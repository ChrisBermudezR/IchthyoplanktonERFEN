import matplotlib.pyplot as plt
import pandas as pd
import pwlf
import numpy as np
import gsw
import math
import geopandas as gpd
from matplotlib.ticker import MaxNLocator
from scipy.optimize import curve_fit
from scipy.interpolate import Rbf
from sklearn.metrics import r2_score
import matplotlib.pyplot as plt
from scipy.signal import find_peaks
import seaborn as sns
from shapely.geometry import Point




    
########################
# Sigmoid function
########################
def fsigmoid(x, a, b):
    return 1.0 / (1.0 + np.exp(-a*(x-b)))

########################
# Normalization
########################
def norm(y, y_min, y_max):
	return (y - y_min)/(y_max-y_min)

########################
# Denormalization
########################
def unnorm(y, y_min, y_max):
	return y*(y_max-y_min)+y_min

    
def NsquaredT(SA, CT, p, lat=None, axis=0):
	# Modified from gsw Nsquared function to get NsquaredT 
	# (release https://github.com/TEOS-10/GSW-Python/releases/tag/v3.6.16.post1)
	if lat is not None:
		if np.any((lat < -90) | (lat > 90)):
			raise ValueError('lat is out of range')
		SA, CT, p, lat = np.broadcast_arrays(SA, CT, p, lat)
		g = gsw.grav(lat, p)
	else:
		SA, CT, p = np.broadcast_arrays(SA, CT, p)
		g = 9.7963
	def axis_slicer(n, sl, axis):
		itup = [slice(None)] * n
		itup[axis] = sl
		return tuple(itup)
	
	db_to_pa = 1e4
	shallow = axis_slicer(SA.ndim, slice(-1), axis)
	deep = axis_slicer(SA.ndim, slice(1, None), axis)
	if lat is not None:
		g_local = 0.5 * (g[shallow] + g[deep])
	else:
		g_local = g
	
	dSA = SA[deep] - SA[shallow]
	dCT = CT[deep] - CT[shallow]
	dp = p[deep] - p[shallow]
	SA_mid = 0.5 * (SA[shallow] + SA[deep])
	CT_mid = 0.5 * (CT[shallow] + CT[deep])
	p_mid = 0.5 * (p[shallow] + p[deep])
	
	specvol_mid, alpha_mid, beta_mid = gsw.specvol_alpha_beta(SA_mid, CT_mid, p_mid)
	
	N2T = ((g_local**2) / (specvol_mid * db_to_pa * dp))
	N2T = N2T * alpha_mid*dCT
	
	return N2T, p_mid

def thermocline(asal, ctemp, pres, lat, m_precision=0.01, threshold=0.2):
    
	########################
	# parameters:
	# 	df: Dataset, columns=['pres','asal','ctemp']
	# 	m_precision: Accuracy in meters
	#	threshold: Threshold for determining MLD and MTD
	########################
        
	x_true = pres 
	y_true = ctemp

    
	########################
	# Calculates the buoyancy frequency squared (N2)
	########################
	N2T, p_mid = NsquaredT(asal, ctemp, pres, lat = lat)
	N2T = np.vstack((N2T, p_mid)).T

	########################
	# Obtain the pressure range where N2T is greater
	########################
	try:
		index = np.argmax(np.abs(N2T[:,0]))
	except:
		return np.nan, np.nan, np.nan, np.nan, np.nan, np.nan, np.nan, np.nan

	max_pres = p_mid[index]*2
	index = x_true < max_pres
	x_true = x_true[index] 
	y_true = y_true[index]
	N2T = N2T[index[:-1]]

	########################
	# Normalize data
	########################
	sign = 1
	if (np.mean(y_true[:2]) > np.mean(y_true[-2:])):
		y_true = -y_true # It is inverted to resemble the function
		sign = -1

	y_min = np.min(y_true) # Min before normalizing
	y_max = np.max(y_true) # Max before normalizing
	y_true = norm(y_true, y_min, y_max) # Normalization

	bounds = ([np.min(y_true), np.min(x_true)], # Lower and upper limits of the parameters
			  [np.max(y_true), np.max(x_true)])

	########################
	# Fit data to sigmoid
	########################
	try:
		popt, pcov = curve_fit(fsigmoid, x_true, y_true, method='dogbox', bounds=bounds)
	except:
		return np.nan, np.nan, np.nan, np.nan, np.nan, np.nan, np.nan, np.nan

	x_pred = np.linspace(np.min(x_true), np.max(x_true), int(np.max(x_true)-np.min(x_true)))
	y_pred = fsigmoid(x_pred, *popt)

	########################
	# Calculate coefficient of determination (R2)
	########################	
	r2 = r2_score(y_true, fsigmoid(x_true, *popt))

	########################
	# Denormalization data
	########################
	y_true = unnorm(y_true, y_min, y_max) # Denormalize real data
	y_pred = unnorm(y_pred, y_min, y_max) # Denormalize predicted data

	########################
	#  Calculate MLD
	######################### 
	pres_10m = 10
	temp_10m = unnorm(fsigmoid(pres_10m, *popt), y_min, y_max)

	pres_mld = np.nan
	temp_mld = np.nan
	for k in np.arange(pres_10m + m_precision, x_true[-1], m_precision): # check every cm
		temp_mld = unnorm(fsigmoid(k, *popt), y_min, y_max)
		if (temp_mld - temp_10m) >= threshold:
			pres_mld = k
			break

    
	########################
	#  Calculate MTD
	######################### 
	pres_deep = x_true[-1]
	temp_deep = unnorm(fsigmoid(pres_deep, *popt), y_min, y_max)
	
	pres_mtd = np.nan
	temp_mtd = np.nan
	if ~np.isnan(pres_mld):
		for k in np.arange(pres_deep + m_precision, pres_mld, -m_precision): # check every cm
			temp_mtd = unnorm(fsigmoid(k, *popt), y_min, y_max)
			if (temp_deep - temp_mtd) >= threshold:
				pres_mtd = k
				break

	if pres_mld > pres_mtd:
		return np.nan, np.nan, np.nan, np.nan, np.nan, np.nan, np.nan, np.nan

    

	return pres_mtd, pres_mld, r2, N2T





def detectar_region_picnoclina(profundidad,densidad, n_segmentos):
    # Ajustar el modelo
    modelo = pwlf.PiecewiseLinFit(profundidad, densidad)
    breakpoints = modelo.fit(n_segmentos)
    
    if len(breakpoints) < 3:
        raise ValueError("No se pudo identificar la picnoclina.")
    
    # Obtener los parámetros del segmento de la picnoclina (entre breakpoints[1] y breakpoints[2])
    pendientes = modelo.calc_slopes()  # Pendientes de todos los segmentos
    segmento_picnoclina = 1  # Asumiendo que es el segundo segmento (índice 1)
    fuerza_picnoclina = pendientes[segmento_picnoclina]  # Gradiente dρ/dz
    
    return fuerza_picnoclina, (breakpoints[1], breakpoints[2]), modelo
    
def detectar_region_haloclina(profundidad, salinidad, n_segmentos):
    # Ajustar el modelo
    modelo = pwlf.PiecewiseLinFit(profundidad, salinidad)
    breakpoints = modelo.fit(n_segmentos)
    
    if len(breakpoints) < 3:
        raise ValueError("No se pudo identificar la haloclina.")
    
    # Obtener la pendiente del segmento de la haloclina (índice 1, asumiendo n_segmentos=3)
    pendientes = modelo.calc_slopes()  # Lista de pendientes de cada segmento
    fuerza_haloclina = pendientes[1]  # Gradiente dS/dz (unidades: PSU/m o g/kg/m)
    
    return fuerza_haloclina, (breakpoints[1], breakpoints[2]), modelo 
    


def round_to_sf(num, sig_figs):
    """
    Redondea un número a una cantidad específica de cifras significativas.
    """
    if num == 0:
        return 0
    # Asegurarse de que es un número antes de intentar formatear
    if not isinstance(num, (int, float)):
        # Devuelve el valor original si no es numérico (como 'Estacion')
        # O podrías lanzar un error si siempre esperas números aquí
        return num
    try:
        # Usar formato .<n>g para cifras significativas y convertir de nuevo a float
        # Esto maneja números grandes y pequeños correctamente.
        formatted_string = f"{num:.{sig_figs}g}"
        return float(formatted_string)
    except (ValueError, TypeError):
        # En caso de algún error inesperado durante el formato/conversión
        return num # Devolver el original o manejar el error
    
def dsigmoid(x, y_min, y_max, x0, k):
        """Derivada de la sigmoidal (dT/dz)."""
        return (y_max - y_min) * (k * np.exp(k * (x - x0))) / (np.exp(k * (x - x0)) + 1)**2

   

def graficar_datos(filtrados, estacion):
    """
    Genera gráficos de perfiles oceanográficos utilizando los datos filtrados.

    Parámetros:
    ----------
    filtrados : dict
        Diccionario con los datos filtrados para temperatura, salinidad y oxígeno.
    estacion : str
        Nombre o identificador de la estación a analizar.

    Retorno:
    -------
    None
        Muestra los gráficos generados.
    """
   
    # Extraer datos del diccionario
    temperatura = filtrados["temperatura"]
    salinidad = filtrados["salinidad"]
    densidad = filtrados["densidad"]
    
    salinity = np.array(salinidad['Value'])  # PSU
    temperature = np.array(temperatura['Value'])  # °C
    density = np.array(densidad['Value'])  # mg/m3
    depth = np.array(temperatura['Profundidad (m)'])  # metros
    longitude = np.array(temperatura['Longitud[deg]'].unique())  # grados decimales
    latitude = np.array(temperatura['Latitud[deg]'].unique())  # grados decimales

    # Verificación de datos
    assert np.all((salinity >= 0) & (salinity <= 42)), "Salinidad fuera de rango"
    assert np.all((temperature >= -2) & (temperature <= 40)), "Temperatura fuera de rango"
    assert np.all(depth >= 0), "Profundidad (m) negativa"
    assert np.all((longitude >= -180) & (longitude <= 180)), "Longitud fuera de rango"
    assert np.all((latitude >= -90) & (latitude <= 90)), "Latitud fuera de rango"

    # Cálculos
    asal = gsw.SA_from_SP(salinity, depth, longitude, latitude)
    ctemp = gsw.CT_from_t(asal, temperature, depth)
    pres_mtd,  pres_mld,  r2, N2T  = thermocline(asal, ctemp, depth, latitude)
    
    pres = depth * 0.1  # Convertir profundidad a presión (dbar)      
    N2T, p_mid = NsquaredT(asal, ctemp, pres, lat=latitude)
    flotacion = np.nanmax(N2T)

    #Calculo de la haloclina
    fuerza_haloclina, (prof_inicio_haloclina, prof_final_haloclina), modelo_halo = detectar_region_haloclina(depth,salinity, n_segmentos=4)
    y_pred_tramos = modelo_halo.predict(depth)
    r2_halo = r2_score(salinity, y_pred_tramos)
        
     #Calculo de la picnoclina
    fuerza_picnoclina, (inicioPicnoclina, finPicnoclina), modelo_picno= detectar_region_picnoclina(depth, density, n_segmentos=4)
    r2_picnoclina = r2_score(density, modelo_picno.predict(depth))     
    
    # Crear la figura y los ejes para las tres gráficas
    fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(15, 8), sharey=True)

    # Graficar temperatura vs. Profundidad (m)
    ax1.plot(temperatura['Value'], temperatura['Profundidad (m)'], color='blue', linewidth=2, label="Perfil de temperatura")
    ax1.set_xlabel('Temperatura (°C)', fontsize=12)
    ax1.set_ylabel('Profundidad (m)', fontsize=12)
    ax1.axhline(y=pres_mld, color="orange", linestyle="--", label=f'Final Capa de mezcla {float(round(pres_mld, 1))} m')
    ax1.axhline(y=pres_mtd, color="red", linestyle="--", label=f'Final Termoclina {float(round(pres_mtd, 1))} m')
    ax1.axhspan(pres_mld, pres_mtd, color='blue', alpha=0.3, label='Termoclina')
    ax1.axhspan(pres_mtd, 0, color='orange', alpha=0.3, label='Capa de mezcla')
    ax1.set_title(f'Estación {estacion}', fontsize=14)
    ax1.invert_yaxis()  # Invertir el eje Y para que la Profundidad (m) aumente hacia abajo
    ax1.xaxis.set_label_position('top')
    ax1.xaxis.tick_top()
    ax1.legend()
    ax1.grid(True)

    # Graficar salinidad vs. Profundidad (m)
    ax2.plot(salinidad['Value'], salinidad['Profundidad (m)'], color='green', linewidth=2, label="Perfil de salinidad")
    ax2.axhline(y=prof_inicio_haloclina, color="grey", linestyle="--", label=f'Inicio Haloclina {float(round(prof_inicio_haloclina, 1))} m')
    ax2.axhline(y=prof_final_haloclina, color="black", linestyle="--", label=f'Final Haloclina {float(round(prof_final_haloclina, 1))} m')
    ax2.axhspan(prof_inicio_haloclina, prof_final_haloclina, color='green', alpha=0.3, label='Haloclina')
    ax2.set_xlabel('Salinidad (UPS)', fontsize=12)
    ax2.set_title(f'Estación {estacion}', fontsize=14)
    ax2.xaxis.set_label_position('top')
    ax2.xaxis.tick_top()
    ax2.legend()
    ax2.grid(True)

    # Graficar oxígeno vs. Profundidad (m)
    ax3.plot(densidad['Value'], densidad['Profundidad (m)'], color='red', linewidth=2,  label="Perfil de densidad")
    ax3.axhline(y=inicioPicnoclina, color="grey", linestyle="--", label=f'Inicio Picnoclina {float(round(inicioPicnoclina, 1))} m')
    ax3.axhline(y=finPicnoclina, color="black", linestyle="--", label=f'Final Picnoclina {float(round(finPicnoclina, 1))} m')
    ax3.axhspan(inicioPicnoclina, finPicnoclina, color='red', alpha=0.3, label='Picnoclina')
    ax3.set_xlabel('Densidad [kg m$^{-3}$]', fontsize=12)
    ax3.set_title(f'Estación {estacion}', fontsize=14)
    ax3.xaxis.set_label_position('top')
    ax3.xaxis.tick_top()
    ax3.legend()
    ax3.grid(True)
    plt.savefig(f'./perfiles/perfil_oceanografico-{estacion}.png', dpi=300, bbox_inches='tight')
    # Ajustar el diseño para evitar solapamientos
    plt.tight_layout()
    plt.show()
    
    resultados = {
    "Estacion": estacion, 
    "Latitud": latitude[0], 
    "Longitud": longitude[0], 
    "Capa de Mezcla": round_to_sf(pres_mld, 4),                     
    "Amplitud Termoclina": round_to_sf(pres_mtd - pres_mld, 4),          
    "Final Termoclina": round_to_sf(pres_mtd, 4), 
    "R2 - Termoclina": round_to_sf(r2, 4),                       # R2 de la sigmoidal
    "Max. Frec. de  Brunt-Väisälä": round_to_sf(flotacion, 4), #s-2/m                
    "Inicio de la Haloclina": round_to_sf(prof_inicio_haloclina, 4), 
    "Final de la Haloclina": round_to_sf(prof_final_haloclina, 4),   
    "Amplitud Haloclina": round_to_sf(prof_final_haloclina - prof_inicio_haloclina, 4),  
    "Fuerza Haloclina": round_to_sf(fuerza_haloclina, 4),           #PSU/m
    "R2 - Haloclina": round_to_sf(r2_halo, 4),                       
    "Inicio de la Picnoclina": round_to_sf(inicioPicnoclina, 4),     
    "Final de la Picnoclina": round_to_sf(finPicnoclina, 4),         
    "Amplitud Picnoclina": round_to_sf(finPicnoclina - inicioPicnoclina, 4),  
    "Fuerza picnoclina": round_to_sf(fuerza_picnoclina, 4),       #kg/m³/m
    "R2 - Picnoclina": round_to_sf(r2_picnoclina, 4)
    }    
    
    frecuencias_BV = {"Estacion": estacion, 
                      "Frecuencia Brunt-Väisälä (s-2)": N2T}    
    
    return resultados, frecuencias_BV  

           
    

    



def grafica_export(filtrados, estacion):
    
    # Extraer datos del diccionario
    temperatura = filtrados["temperatura"]
    salinidad = filtrados["salinidad"]
    oxigeno = filtrados["oxigeno"]
    
    # Crear la figura y los ejes para las tres gráficas
    fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(15, 8), sharey=True)

    # Graficar temperatura vs. Profundidad (m)
    ax1.plot(temperatura['Value'], temperatura['Profundidad (m)'], color='blue', linewidth=2)
    ax1.set_xlabel('Temperatura (°C)', fontsize=12)
    ax1.set_ylabel('Profundidad (m)', fontsize=12)
    ax1.set_title(f'Estación {estacion}', fontsize=14)
    ax1.invert_yaxis()  # Invertir el eje Y para que la Profundidad (m) aumente hacia abajo
    ax1.xaxis.set_label_position('top')
    ax1.xaxis.tick_top()
    ax1.grid(True)

    # Graficar salinidad vs. Profundidad (m)
    ax2.plot(salinidad['Value'], salinidad['Profundidad (m)'], color='green', linewidth=2)
    ax2.set_xlabel('Salinidad (UPS)', fontsize=12)
    ax2.set_title(f'Estación {estacion}', fontsize=14)
    ax2.xaxis.set_label_position('top')
    ax2.xaxis.tick_top()
    ax2.grid(True)

    # Graficar oxígeno vs. Profundidad (m)
    ax3.plot(oxigeno['Value'], oxigeno['Profundidad (m)'], color='red', linewidth=2)
    ax3.set_xlabel('Oxígeno [mg L$^{-1}$]', fontsize=12)
    ax3.set_title(f'Estación {estacion}', fontsize=14)
    ax3.xaxis.set_label_position('top')
    ax3.xaxis.tick_top()
    ax3.grid(True)

    # Ajustar el diseño para evitar solapamientos
    plt.tight_layout()
    # Guardar la figura
    plt.savefig(f'./perfiles/perfil_oceanografico-{estacion}.png', dpi=300, bbox_inches='tight')
    



def graficar_multiejes(filtrados, estacion):
    """
    Genera un gráfico combinado con múltiples ejes horizontales para perfiles oceanográficos 
    y un diagrama T-S.

    Parámetros:
    ----------
    filtrados : dict
        Diccionario con los datos filtrados para temperatura y salinidad.
    estacion : str
        Nombre o identificador de la estación a analizar.

    Retorno:
    -------
    None
        Muestra el gráfico generado.
    """
    
    # Verificar si el diccionario contiene las claves necesarias
    if "temperatura" not in filtrados or "salinidad" not in filtrados:
        raise ValueError("El diccionario de datos debe contener las claves 'temperatura' y 'salinidad'.")

    # Extraer datos
    temperatura = filtrados["temperatura"]
    salinidad = filtrados["salinidad"]
    
    
    # Verificar que las claves necesarias estén en los DataFrames
    for key in ["Value", "Profundidad (m)"]:
        if key not in temperatura or key not in salinidad:
            raise ValueError(f"Las claves '{key}' deben estar en los DataFrames de temperatura y salinidad.")

    # Definir rangos para el diagrama T-S
    mint, maxt = temperatura['Value'].min(), temperatura['Value'].max()
    mins, maxs = salinidad['Value'].min(), salinidad['Value'].max()

    temp = np.linspace(mint - 1, maxt + 1, 25)
    sal = np.linspace(mins - 1, maxs + 1, 25)
    temp, sal = np.meshgrid(temp, sal)

    # Calcular densidad potencial sigma_θ
    sigma_theta = gsw.sigma0(sal, temp)
    levels = np.linspace(sigma_theta.min(), sigma_theta.max(), 15)

    # Convertir valores en DataFrames y arrays
    salinidad_array = salinidad['Value'].to_numpy()
    temperatura_array = temperatura['Value'].to_numpy()
    Profundidad_array = temperatura['Profundidad (m)'].to_numpy()
    #densidad_array = gsw.sigma0(salinidad_array, temperatura_array)

    # Crear figura y subgráficos
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8), gridspec_kw={'width_ratios': [1, 1]})

    # Gráfico 1: Perfiles Oceanográficos
    ax1.invert_yaxis()  # Invertir eje Y (Profundidad (m))
    ax1.set_ylabel('Profundidad (m)', fontsize=12)
    ax1.set_title(f'Perfiles Oceanográficos - Estación {estacion}', fontsize=14)

     # Trazar salinidad
    ax1.plot(salinidad['Value'], salinidad['Profundidad (m)'], color='blue', linewidth=2, label='Salinidad (UPS)')
    ax1.set_xlabel('Salinidad (UPS)', fontsize=12, color='blue')
    ax1.tick_params(axis='x', labelcolor='blue')
    ax1.grid(True)

    # Crear eje secundario para temperatura
    ax1_temp = ax1.twiny()
    ax1_temp.plot(temperatura['Value'], temperatura['Profundidad (m)'], color='red', linewidth=2, label='Temperatura (°C)')
    ax1_temp.set_xlabel('Temperatura (°C)', fontsize=12, color='red')
    ax1_temp.tick_params(axis='x', labelcolor='red')

    # Gráfico 2: Diagrama T-S
    cs = ax2.contour(sal, temp, sigma_theta, colors='black', levels=levels, zorder=1)
    ax2.clabel(cs, fontsize=10, inline=1, fmt='%.1f')

    # Dibujar los puntos de salinidad y temperatura, coloreados por Profundidad (m)
    sc = ax2.scatter(salinidad_array, temperatura_array, c=Profundidad_array, s=35, cmap='nipy_spectral')
    cb = plt.colorbar(sc)
    
    # Configurar etiquetas y título
    ax2.set_xlabel('Salinidad (UPS)')
    ax2.set_ylabel('Temperatura [$^\circ$C]')
    ax2.set_title('Diagrama T-S', y=1.025)
    
    # Configurar los ejes
    ax2.xaxis.set_major_locator(MaxNLocator(nbins=6))
    ax2.yaxis.set_major_locator(MaxNLocator(nbins=8))
    ax2.tick_params(direction='out')
    ax2.xaxis.set_major_locator(MaxNLocator(nbins=6))
    ax2.yaxis.set_major_locator(MaxNLocator(nbins=8))
    ax2.tick_params(direction='out')
    ax2.text(0.02, 0.98, r'$\sigma_\theta$', transform=ax2.transAxes, fontsize=20, verticalalignment='top')

    
    cb.ax.tick_params(direction='out')
    cb.ax.invert_yaxis()
    cb.set_label('Profundidad (m) [m]')

    # Ajustar diseño y mostrar
    plt.tight_layout()
    plt.savefig(f'./perfiles/DiagramasTS_Perfiles_Estacion-{estacion}.png', dpi=300, bbox_inches='tight')
    plt.show()
    



def DiagramaTS(dataset):
    # Extraer datos de salinidad, temperatura y Profundidad (m)
    salinidad = dataset[dataset["Variable"] == "Salinidad (UPS)"]["Value"]
    temperatura = dataset[dataset["Variable"] == "Temperatura (°C)"]["Value"]
    Profundidad = dataset[dataset["Variable"] == "Temperatura (°C)"]["Profundidad (m)"]
    
    # Verificar que las longitudes coincidan
    if len(salinidad) != len(temperatura) or len(salinidad) != len(Profundidad):
        raise ValueError("Las longitudes de salinidad, temperatura y Profundidad no coinciden.")
    
    # Convertir a arrays de numpy para evitar problemas con pandas
    salinidad_array = salinidad.to_numpy()
    temperatura_array = temperatura.to_numpy()
    Profundidad_array = Profundidad.to_numpy()
    
    # Calcular la densidad (opcional, ya que no se usa para el gráfico)
    densidad = gsw.sigma0(salinidad_array, temperatura_array)
    
    # Crear la malla para las curvas de densidad
    mint = np.min(temperatura_array)
    maxt = np.max(temperatura_array)
    mins = np.min(salinidad_array)
    maxs = np.max(salinidad_array)
    
    temp = np.linspace(mint - 1, maxt + 1, 25)
    sal = np.linspace(mins - 1, maxs + 1, 25)
    temp, sal = np.meshgrid(temp, sal)
    sigma_theta = gsw.sigma0(sal, temp)
    levels = np.linspace(sigma_theta.min(), sigma_theta.max(), 10)
    
    # Crear la figura
    fig, ax = plt.subplots(figsize=(8.25, 7))
    
    # Dibujar las curvas de densidad
    cs = ax.contour(sal, temp, sigma_theta, colors='black', levels=levels, zorder=1, extend='both')
    cl = plt.clabel(cs, fontsize=10, inline=1, fmt='%.1f')
    
    # Dibujar los puntos de salinidad y temperatura, coloreados por Profundidad (m)
    sc = ax.scatter(salinidad_array, temperatura_array, c=Profundidad_array, s=35, cmap='nipy_spectral')
    cb = plt.colorbar(sc)
    
    # Configurar etiquetas y título
    ax.set_xlabel('Salinidad (UPS)')
    ax.set_ylabel('Temperatura [$^\circ$C]')
    ax.set_title('Diagrama T-S', y=1.025)
    
    """
    Las masas de agua se nombraron acorde a Malikov y Villegas (2010)
    """
    # Agua Superficial Colombiana Ecuatorial Tropical (ASCET)
    ax.text(34.5, 27, "ASCET", fontsize=12, color="red", fontweight="bold",
        ha="center", va="center", bbox=dict(facecolor="white", alpha=0.7, edgecolor="black"))
    
    # Agua Central Superior Ecuatorial del Pacífico (ACSEP)
    ax.text(35.3, 23, "ACSEP", fontsize=12, color="red", fontweight="bold",
        ha="center", va="center", bbox=dict(facecolor="white", alpha=0.7, edgecolor="black"))
    
    # Agua Intermedia Antartida 
    ax.text(35.7, 15, "AIA", fontsize=12, color="red", fontweight="bold",
        ha="center", va="center", bbox=dict(facecolor="white", alpha=0.7, edgecolor="black"))
    
    # Agua Antartida de Fondo
    ax.text(35.5, 6, "AAF", fontsize=12, color="red", fontweight="bold",
        ha="center", va="center", bbox=dict(facecolor="white", alpha=0.7, edgecolor="black"))
    
    
    # Configurar los ejes
    ax.xaxis.set_major_locator(MaxNLocator(nbins=6))
    ax.yaxis.set_major_locator(MaxNLocator(nbins=8))
    ax.tick_params(direction='out')
    ax.text(0.02, 0.98, r'$\sigma_\theta$', transform=ax.transAxes, fontsize=20, verticalalignment='top')
    cb.ax.tick_params(direction='out')
    cb.ax.invert_yaxis()
    cb.set_label('Profundidad (m) [m]')
    
    # Mostrar el gráfico
    plt.tight_layout()
    plt.show()
        
class MovSTDCalculator:
    def __init__(self, profile, frame, threshold_factor=0.5, min_std=None):
        self.profile = np.array(profile)
        self.frame = frame
        self.threshold_factor = threshold_factor
        self.min_std = min_std if min_std is not None else self.calculate_min_std()
    
    def moving_mean_std(self):
        profile_series = pd.Series(self.profile)
        moving_mean = profile_series.rolling(self.frame, min_periods=1).mean()
        moving_std = profile_series.rolling(self.frame, min_periods=1).std()
        return moving_mean, moving_std
    
    def calculate_std_threshold(self, moving_std):
        return self.threshold_factor * np.nanmax(moving_std)
    
    def calculate_min_std(self):
        return np.nanstd(self.profile)
    
    def detect_transition_depth(self):
        if np.nanstd(self.profile) < self.min_std:
            return np.nan
        
        flat = pd.Series(self.profile).rolling(self.frame, min_periods=1).mean()
        mstd = pd.Series(flat).rolling(self.frame, min_periods=1).std()
        
        idx = np.nanargmax(mstd)
        max_std = mstd[idx]
        
        for i in range(1, idx + 1):
            if mstd[idx - i] < self.threshold_factor * max_std:
                return idx - i
        
        return np.nan
    
    
    
'''
Química   
'''
def filtrado_datos_quimicos(dataset, estacion):
    
    # Filtrar los datos por variable y estación
    oxigeno = dataset[(dataset.Variable == 'Oxigeno [mg/l]') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    clorofila = dataset[(dataset.Variable == 'Clorofila [µg/L]') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    pH = dataset[(dataset.Variable == 'pH') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    salinidad = dataset[(dataset.Variable == 'Salinidad (UPS)') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    tnox = dataset[(dataset.Variable == 'TNOx µM') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    NO2 = dataset[(dataset.Variable == '[NO2--N] µM') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    NO3 = dataset[(dataset.Variable == '[NO3--N] µM') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    PO4 = dataset[(dataset.Variable == '[PO4-3-P] µM') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    SiO2 = dataset[(dataset.Variable == '[SiO2-Si] µM') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    NP = dataset[(dataset.Variable == 'N:P') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    SiP = dataset[(dataset.Variable == 'Si:P') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    SiN = dataset[(dataset.Variable == 'Si:N') & (dataset.Estacion == estacion)].sort_values('Profundidad (m)')
    
    return {
        "oxigeno": oxigeno,
        "clorofila": clorofila,
        "pH": pH,
        "salinidad": salinidad,
        "tnox": tnox,
        "NO2": NO2,
        "NO3": NO3,
        "PO4": PO4,
        "SiO2": SiO2,
        "NP": NP,
        "SiP": SiP,
        "SiN": SiN
    }

def graficar_datos_quimicos(filtrados, estacion):
    """
    Genera gráficos de perfiles oceanográficos utilizando los datos filtrados.

    Parámetros:
    ----------
    filtrados : dict
        Diccionario con los datos filtrados para diferentes parámetros químicos.
    estacion : str
        Nombre o identificador de la estación a analizar.

    Retorno:
    -------
    None
        Muestra los gráficos generados.
    """
    parametros = [
        ("oxigeno", "Oxígeno [mg/l]"),
        ("clorofila", "Clorofila [µg/L]"),
        ("pH", "pH"),
        ("salinidad", "Salinidad (UPS)"),
        ("NO2", "[NO₂--N] [µM]'"),
        ("NO3", "[NO₃--N] [µM]"),
        ("PO4", " [PO₄-3-P] [µM]"),
        ("SiO2", "[SiO₂-Si] [µM]"),
        ("tnox", "TNOx µM"),
        ("NP", "N:P"),
        ("SiP", "Si:P"),
        ("SiN", "Si:N")
        
    ]

    fig, ax = plt.subplots(3, 4, figsize=(15, 18), sharey=True)
    fig.suptitle(f'Perfiles químicos - Estación {estacion}', fontsize=16)

    for ax, (key, xlabel) in zip(ax.flat, parametros):
        if key in filtrados:
            data = filtrados[key]
            ax.plot(data['Value'], data['Profundidad (m)'], color='blue', linestyle='dotted', marker='o', linewidth=1.5)
            
            ax.set_xlabel(xlabel, fontsize=12)
            ax.set_ylabel('Profundidad', fontsize=12)
            ax.xaxis.set_label_position('top')
            ax.xaxis.tick_top()
            ax.grid(True)
    ax.invert_yaxis()
    plt.tight_layout(rect=[0, 0, 1, 0.99])  # Ajustar para que no se sobreponga el título
    plt.show()
    

def filtrado_datos_variables(dataset):
    
    temperatura = dataset[(dataset.Variable == 'Temperatura (°C)')].sort_values('Profundidad (m)')
    densidad = dataset[(dataset.Variable == 'Densidad [mg/m3]')].sort_values('Profundidad (m)')
    oxigeno = dataset[(dataset.Variable == 'Oxigeno [mg/L]')].sort_values('Profundidad (m)')
    clorofila = dataset[(dataset.Variable == 'Clorofila [µg/L]') ].sort_values('Profundidad (m)')
    pH = dataset[(dataset.Variable == 'pH') ].sort_values('Profundidad (m)')
    salinidad = dataset[(dataset.Variable == 'Salinidad (UPS)') ].sort_values('Profundidad (m)')
    tnox = dataset[(dataset.Variable == 'TNOx µM') ].sort_values('Profundidad (m)')
    NO2 = dataset[(dataset.Variable == '[NO2--N] µM') ].sort_values('Profundidad (m)')
    NO3 = dataset[(dataset.Variable == '[NO3--N] µM') ].sort_values('Profundidad (m)')
    PO4 = dataset[(dataset.Variable == '[PO4-3-P] µM') ].sort_values('Profundidad (m)')
    SiO2 = dataset[(dataset.Variable == '[SiO2-Si] µM') ].sort_values('Profundidad (m)')
    NP = dataset[(dataset.Variable == 'N:P') ].sort_values('Profundidad (m)')
    SiP = dataset[(dataset.Variable == 'Si:P') ].sort_values('Profundidad (m)')
    SiN = dataset[(dataset.Variable == 'Si:N') ].sort_values('Profundidad (m)')
    
    return {
        "temperatura": temperatura,
        "densidad": densidad,
        "oxigeno": oxigeno,
        "clorofila": clorofila,
        "pH": pH,
        "salinidad": salinidad,
        "tnox": tnox,
        "NO2": NO2,
        "NO3": NO3,
        "PO4": PO4,
        "SiO2": SiO2,
        "NP": NP,
        "SiP": SiP,
        "SiN": SiN
    }

def graficar_boxplots_por_profundidad(filtrados):
    """
    Genera gráficos de cajas (boxplots) para cada variable química en función de la profundidad.

    Parámetros:
    ----------
    filtrados : dict
        Diccionario con los datos filtrados para diferentes parámetros químicos.
    estacion : str
        Nombre o identificador de la estación a analizar.

    Retorno:
    -------
    None
        Muestra los gráficos generados.
    """
    
    parametros = [
        ("oxigeno", "Oxígeno [mg/l]"),
        ("clorofila", "Clorofila [µg/L]"),
        ("pH", "pH"),
        ("salinidad", "Salinidad (UPS)"),
        ("NO2", "[NO₂--N] [µM]'"),
        ("NO3", "[NO₃--N] [µM]"),
        ("PO4", " [PO₄-3-P] [µM]"),
        ("SiO2", "[SiO₂-Si] [µM]"),
        ("tnox", "TNOx µM"),
        ("NP", "N:P"),
        ("SiP", "Si:P"),
        ("SiN", "Si:N")
        
    ]
    

    fig, ax = plt.subplots(3, 4, figsize=(15, 18), sharey=True)
    fig.suptitle(f'Diagramas de cajas totales', fontsize=16)

    for ax, (key, xlabel) in zip(ax.flat, parametros):
        if key in filtrados:
            data = filtrados[key]
            # Crear el boxplot horizontal
            sns.boxplot(y=data['Profundidad (m)'], x=data['Value'], ax=ax, orient="h", color="skyblue")

            # Configuración del gráfico
            
            ax.set_xlabel(xlabel, fontsize=12)
            ax.set_ylabel('Profundidad', fontsize=12)
            ax.xaxis.set_label_position('top')
            ax.xaxis.tick_top()
            ax.grid(True)

    # Ajustar diseño
    plt.tight_layout(rect=[0, 0, 1, 0.99])
    plt.show()




def interpolacion_IDW(data_entrada, variable, profundidad, color):
    """
    Interpola los datos de una variable química en función de la profundidad.

    Parámetros:
    ----------
    data : DataFrame
        DataFrame con los datos de la variable química.
    variable : str
        Nombre de la variable química a interpolar.
    profundidad : int
        Profundidad a la que se desea interpolar los datos. 
    """
    print("Filtrando datos...")
    # 1. Filtrar datos por variable y profundidad
    data = data_entrada[variable]
    data = data[data['Profundidad (m)'] == profundidad]
    data.dropna(inplace=True)
    
    variable_name = data['Variable'].drop_duplicates().tolist()
    depth = data['Profundidad (m)'].drop_duplicates().tolist()
    
    print("Leyendo capas geográficas...")
    
    continente = gpd.read_file("./sig/GeoLayers.gpkg", layer = "Continente")
    cpc = gpd.read_file("./sig/GeoLayers.gpkg", layer = "cuenca_pacifica")
    AMP = gpd.read_file("./sig/GeoLayers.gpkg", layer = "AMP")
                            
    # Convertir ambas listas a un solo string y eliminar las llaves
    variable_name_str = ', '.join(variable_name)
    depth_str = ', '.join(map(str, depth))

    # 2. Convertir en GeoDataFrame
    print("Convirtiendo a geodataframe...")
    geometry = gpd.points_from_xy(data["Longitud[deg]"], data["Latitud[deg]"])
    gdf = gpd.GeoDataFrame(data, geometry=geometry, crs="EPSG:4326")  # WGS84

    # 3. Extraer coordenadas y valores
    x = gdf.geometry.x.values  # Longitud
    y = gdf.geometry.y.values  # Latitud
    z = gdf["Value"]  # Valores de oxígeno

    # 4. Crear una malla de puntos donde interpolar
    grid_x, grid_y = np.meshgrid(
        np.linspace(-84.5, -77, 1000),  # 100 puntos en X
        np.linspace(1, 7, 1000)   # 100 puntos en Y
    )

    # 5. Aplicar interpolación IDW
    print(f"Interpolando {variable} a {profundidad} m...")
    interp_idw = Rbf(x, y, z, function="inverse")  # Interpolador
    grid_z = interp_idw(grid_x, grid_y)  # Valores interpolados

    # 6. Graficar el mapa interpolado
    fig, ax = plt.subplots(figsize=(8, 6))
    contour = ax.contourf(grid_x, grid_y, grid_z, cmap = color, levels=15)  # Mapa interpolado
    cpc.boundary.plot(ax=ax, edgecolor="black", linewidth=1.5, label="Límites de la CPC")  # Límites de la CPC
    AMP.boundary.plot(ax=ax, edgecolor="grey", linewidth=1.5, label="AMP")  
    plt.colorbar(contour, label=variable_name_str)  # Barra de color
    
    continente.plot(ax=ax, edgecolor="black", linewidth=1.5, label="Límites de Colombia", color="lightgrey")  # Límites de Colombia
    ax.scatter(x, y, color='black')  # Puntos originales
    ax.set_xlabel("Longitud")
    ax.set_ylabel("Latitud")
    ax.set_title(f"Interpolación IDW - {variable_name_str} a profundidad = {depth_str} m")
    ax.set_xlim(-84.5, -77)
    ax.set_ylim(1, 10)
    #ax.legend()
    plt.savefig(f'./perfiles/{variable}_profundidad_{profundidad}.png', dpi=300, bbox_inches='tight')
    plt.show()
    
import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from scipy.interpolate import Rbf
import rasterio
from rasterio.features import rasterize
from rasterio.transform import from_origin
from rasterio.mask import mask
from shapely.geometry import Point, mapping

def interpolacion_IDW_clipped(data_entrada, variable, profundidad, color):
    """
    Interpola los datos de una variable química en función de la profundidad y genera un mapa con el raster resultante.

    Parámetros:
    ----------
    data_entrada : DataFrame
        DataFrame con los datos de la variable química.
    variable : str
        Nombre de la variable química a interpolar.
    profundidad : int
        Profundidad a la que se desea interpolar los datos.
    color : str, optional
        Colormap para la interpolación (default: "viridis").
    """

    # 1. Filtrar datos por variable y profundidad
    data = data_entrada[variable]
    data = data[data['Profundidad (m)'] == profundidad].dropna()

    variable_name = data['Variable'].drop_duplicates().tolist()
    depth = data['Profundidad (m)'].drop_duplicates().tolist()

    # 2. Leer capas geográficas
    continente = gpd.read_file("./sig/GeoLayers.gpkg", layer="Continente")
    cpc = gpd.read_file("./sig/GeoLayers.gpkg", layer="cuenca_pacifica")  # Capa de recorte
    AMP = gpd.read_file("./sig/GeoLayers.gpkg", layer="AMP")

    # 3. Convertir data a GeoDataFrame
    geometry = gpd.points_from_xy(data["Longitud[deg]"], data["Latitud[deg]"])
    gdf = gpd.GeoDataFrame(data, geometry=geometry, crs="EPSG:4326")

    # 4. Extraer coordenadas y valores
    x = gdf.geometry.x.values  # Longitud
    y = gdf.geometry.y.values  # Latitud
    z = gdf["Value"]  # Valores de la variable

    # 5. Crear una malla de interpolación
    grid_x, grid_y = np.meshgrid(
        np.linspace(-84.5, -77, 1000),  # 1000 puntos en X
        np.linspace(1, 7, 1000)  # 1000 puntos en Y
    )

    # 6. Aplicar interpolación IDW
    interp_idw = Rbf(x, y, z, function="inverse")  
    grid_z = interp_idw(grid_x, grid_y)

    # 7. Convertir malla en raster
    resolution = 0.01  # Ajusta según necesites
    minx, miny, maxx, maxy = -84.5, 1, -77, 7  # Extensión del raster

    width = int((maxx - minx) / resolution)
    height = int((maxy - miny) / resolution)
    transform = from_origin(minx, maxy, resolution, resolution)

    raster_array = rasterize(
        [(Point(px, py), value) for px, py, value in zip(grid_x.flatten(), grid_y.flatten(), grid_z.flatten())],
        out_shape=(height, width),
        transform=transform,
        fill=np.nan,
        dtype='float32'
    )

    # 8. Guardar como raster en un GeoTIFF
    raster_path = f"./sig/{variable}_Prof{profundidad}_IDW_raster.tif"
    with rasterio.open(
        raster_path, "w",
        driver="GTiff",
        height=height, width=width,
        count=1,
        dtype=raster_array.dtype,
        crs="EPSG:4326",
        transform=transform
    ) as dst:
        dst.write(raster_array, 1)

    # 9. Recortar el raster con la capa CPC
    with rasterio.open(raster_path) as src:
        cpc_geom = [mapping(geom) for geom in cpc.geometry]  # Convertir geometría a máscara
        clipped_raster, clipped_transform = mask(src, cpc_geom, crop=True, nodata=np.nan)

        # Extraer la banda 1 de clipped_raster
        clipped_raster = clipped_raster[0]  # Convertir (1, altura, ancho) → (altura, ancho)

        # Guardar raster recortado
        clipped_raster_path = f"./sig/{variable}_Prof{profundidad}_IDW_raster_clipped.tif"
        with rasterio.open(
            clipped_raster_path, "w",
            driver="GTiff",
            height=clipped_raster.shape[0],  # Altura
            width=clipped_raster.shape[1],  # Ancho
            count=1,
            dtype=clipped_raster.dtype,
            crs=src.crs,
            transform=clipped_transform,
            nodata=np.nan
        ) as dst:
            dst.write(clipped_raster, 1)  # Ahora tiene la forma correcta

    # 10. Graficar el raster recortado
    fig, ax = plt.subplots(figsize=(8, 6))

    with rasterio.open(clipped_raster_path) as src:
        raster_data = src.read(1)  # Leer la banda 1
        extent = [src.bounds.left, src.bounds.right, src.bounds.bottom, src.bounds.top]

        # Graficar el raster
        img = ax.imshow(raster_data, cmap=color, extent=extent, origin="upper", alpha=0.7)

    # Superponer límites geográficos
    cpc.boundary.plot(ax=ax, edgecolor="black", linewidth=1.5, label="Límites de la CPC")
    AMP.boundary.plot(ax=ax, edgecolor="grey", linewidth=1.5, label="AMP")  
    continente.plot(ax=ax, edgecolor="black", linewidth=1.5, color="lightgrey", label="Límites de Colombia")

    # Añadir puntos originales
    ax.scatter(x, y, color='black', s=10)  

    # Configurar etiquetas y título
    ax.set_xlabel("Longitud")
    ax.set_ylabel("Latitud")
    ax.set_title(f"Interpolación IDW - {variable_name[0]} a profundidad = {depth[0]} m")
    
    # 12. Ajustar límites del mapa
    ax.set_xlim(-84.5, -77)
    ax.set_ylim(1, 10)
    
    # 11. Guardar la figura
    plt.colorbar(img, ax=ax, label=variable_name[0])
    plt.savefig(f'./perfiles/{variable}_profundidad_{profundidad}_clipped.png', dpi=300, bbox_inches='tight')
    plt.show()
    
import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from scipy.interpolate import Rbf
import rasterio
from rasterio.features import rasterize
from rasterio.transform import from_origin
from rasterio.mask import mask
from shapely.geometry import Point, mapping

def interpolacion_estratificacion(data_entrada, variable,  color, labelVar):
    """
    Interpola los datos de una variable química en función de la profundidad y genera un mapa con el raster resultante.

    Parámetros:
    ----------
    data_entrada : DataFrame
        DataFrame con los datos de la variable química.
    variable : str
        Nombre de la variable química a interpolar.
    profundidad : int
        Profundidad a la que se desea interpolar los datos.
    color : str, optional
        Colormap para la interpolación (default: "viridis").
    """

    # 1. Filtrar datos por variable y profundidad
    data = data_entrada
    #data = data[data['Profundidad (m)'] == profundidad].dropna()

    variable_name = variable
    

    # 2. Leer capas geográficas
    continente = gpd.read_file("./sig/GeoLayers.gpkg", layer="Continente")
    cpc = gpd.read_file("./sig/GeoLayers.gpkg", layer="cuenca_pacifica")  # Capa de recorte
    AMP = gpd.read_file("./sig/GeoLayers.gpkg", layer="AMP")

    # 3. Convertir data a GeoDataFrame
    geometry = gpd.points_from_xy(data["Longitud[deg]"], data["Latitud[deg]"])
    gdf = gpd.GeoDataFrame(data, geometry=geometry, crs="EPSG:4326")

    # 4. Extraer coordenadas y valores
    x = gdf.geometry.x.values  # Longitud
    y = gdf.geometry.y.values  # Latitud
    z = gdf[variable]  # Valores de la variable

    # 5. Crear una malla de interpolación
    grid_x, grid_y = np.meshgrid(
        np.linspace(-84.5, -77, 1000),  # 1000 puntos en X
        np.linspace(1, 7, 1000)  # 1000 puntos en Y
    )

    # 6. Aplicar interpolación IDW
    interp_idw = Rbf(x, y, z, function="inverse")  
    grid_z = interp_idw(grid_x, grid_y)

    # 7. Convertir malla en raster
    resolution = 0.01  # Ajusta según necesites
    minx, miny, maxx, maxy = -84.5, 1, -77, 7  # Extensión del raster

    width = int((maxx - minx) / resolution)
    height = int((maxy - miny) / resolution)
    transform = from_origin(minx, maxy, resolution, resolution)

    raster_array = rasterize(
        [(Point(px, py), value) for px, py, value in zip(grid_x.flatten(), grid_y.flatten(), grid_z.flatten())],
        out_shape=(height, width),
        transform=transform,
        fill=np.nan,
        dtype='float32'
    )

    # 8. Guardar como raster en un GeoTIFF
    raster_path = f"./sig/{variable}_IDW_raster.tif"
    with rasterio.open(
        raster_path, "w",
        driver="GTiff",
        height=height, width=width,
        count=1,
        dtype=raster_array.dtype,
        crs="EPSG:4326",
        transform=transform
    ) as dst:
        dst.write(raster_array, 1)

    # 9. Recortar el raster con la capa CPC
    with rasterio.open(raster_path) as src:
        cpc_geom = [mapping(geom) for geom in cpc.geometry]  # Convertir geometría a máscara
        clipped_raster, clipped_transform = mask(src, cpc_geom, crop=True, nodata=np.nan)

        # Extraer la banda 1 de clipped_raster
        clipped_raster = clipped_raster[0]  # Convertir (1, altura, ancho) → (altura, ancho)

        # Guardar raster recortado
        clipped_raster_path = f"./sig/{variable}_IDW_raster_clipped.tif"
        with rasterio.open(
            clipped_raster_path, "w",
            driver="GTiff",
            height=clipped_raster.shape[0],  # Altura
            width=clipped_raster.shape[1],  # Ancho
            count=1,
            dtype=clipped_raster.dtype,
            crs=src.crs,
            transform=clipped_transform,
            nodata=np.nan
        ) as dst:
            dst.write(clipped_raster, 1)  # Ahora tiene la forma correcta

    # 10. Graficar el raster recortado
    fig, ax = plt.subplots(figsize=(8, 6))

    with rasterio.open(clipped_raster_path) as src:
        raster_data = src.read(1)  # Leer la banda 1
        extent = [src.bounds.left, src.bounds.right, src.bounds.bottom, src.bounds.top]

        # Graficar el raster
        img = ax.imshow(raster_data, cmap=color, extent=extent, origin="upper", alpha=0.7)

    # Superponer límites geográficos
    cpc.boundary.plot(ax=ax, edgecolor="black", linewidth=1.5, label="Límites de la CPC")
    AMP.boundary.plot(ax=ax, edgecolor="grey", linewidth=1.5, label="AMP")  
    continente.plot(ax=ax, edgecolor="black", linewidth=1.5, color="lightgrey", label="Límites de Colombia")

    # Añadir puntos originales
    ax.scatter(x, y, color='black', s=10)  

    # Configurar etiquetas y título
    ax.set_xlabel("Longitud")
    ax.set_ylabel("Latitud")
    ax.set_title(f"Interpolación IDW - {variable_name}")
    
    # 12. Ajustar límites del mapa
    ax.set_xlim(-84.5, -77)
    ax.set_ylim(1, 10)
    
    # 11. Guardar la figura
    plt.colorbar(img, ax=ax, label=labelVar)
    plt.savefig(f'./perfiles/{variable}_clipped.png', dpi=300, bbox_inches='tight')
    plt.show()

