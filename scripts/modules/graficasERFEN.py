

def filtrado(dataset, estacion):
    import pandas as pd
    # Filtrar los datos por variable y estación
    temperatura = dataset[(dataset.Variable == 'Temperatura [°C]') & (dataset.Estacion == estacion)].sort_values('Profundidad')
    salinidad = dataset[(dataset.Variable == 'Salinidad [PSU]') & (dataset.Estacion == estacion)].sort_values('Profundidad')
    oxigeno = dataset[(dataset.Variable == 'Oxigeno [mg/l]') & (dataset.Estacion == estacion)].sort_values('Profundidad')
    return {
        "temperatura": temperatura,
        "salinidad": salinidad,
        "oxigeno": oxigeno
    }


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
    import matplotlib.pyplot as plt
    # Extraer datos del diccionario
    temperatura = filtrados["temperatura"]
    salinidad = filtrados["salinidad"]
    oxigeno = filtrados["oxigeno"]

    # Crear la figura y los ejes para las tres gráficas
    fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(15, 8), sharey=True)

    # Graficar temperatura vs. profundidad
    ax1.plot(temperatura['Value'], temperatura['Profundidad'], color='blue', linewidth=2)
    ax1.set_xlabel('Temperatura [°C]', fontsize=12)
    ax1.set_ylabel('Profundidad (m)', fontsize=12)
    ax1.set_title(f'Estación {estacion}', fontsize=14)
    ax1.invert_yaxis()  # Invertir el eje Y para que la profundidad aumente hacia abajo
    ax1.xaxis.set_label_position('top')
    ax1.xaxis.tick_top()
    ax1.grid(True)

    # Graficar salinidad vs. profundidad
    ax2.plot(salinidad['Value'], salinidad['Profundidad'], color='green', linewidth=2)
    ax2.set_xlabel('Salinidad [PSU]', fontsize=12)
    ax2.set_title(f'Estación {estacion}', fontsize=14)
    ax2.xaxis.set_label_position('top')
    ax2.xaxis.tick_top()
    ax2.grid(True)

    # Graficar oxígeno vs. profundidad
    ax3.plot(oxigeno['Value'], oxigeno['Profundidad'], color='red', linewidth=2)
    ax3.set_xlabel('Oxígeno [mg L$^{-1}$]', fontsize=12)
    ax3.set_title(f'Estación {estacion}', fontsize=14)
    ax3.xaxis.set_label_position('top')
    ax3.xaxis.tick_top()
    ax3.grid(True)

    # Ajustar el diseño para evitar solapamientos
    plt.tight_layout()
    plt.show()

def grafica_calidad(filtrados, estacion):
    
    import matplotlib.pyplot as plt
    # Extraer datos del diccionario
    temperatura = filtrados["temperatura"]
    salinidad = filtrados["salinidad"]
    oxigeno = filtrados["oxigeno"]
    
    # Crear la figura y los ejes para las tres gráficas
    fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(15, 8), sharey=True)

    # Graficar temperatura vs. profundidad
    ax1.scatter(temperatura['Quality'], temperatura['Profundidad'], color='blue', s=2, alpha=0.8)
    ax1.set_xlabel('Calidad Temperatura', fontsize=12)
    ax1.set_ylabel('Profundidad (m)', fontsize=12)
    ax1.set_title(f'Estación {estacion}', fontsize=14)
    ax1.invert_yaxis()  # Invertir el eje Y para que la profundidad aumente hacia abajo
    ax1.xaxis.set_label_position('top')
    ax1.xaxis.tick_top()
    ax1.grid(True)

    # Graficar salinidad vs. profundidad
    ax2.scatter(salinidad['Quality'], salinidad['Profundidad'], color='green', s=2, alpha=0.8)
    ax2.set_xlabel('Calidad Salinidad', fontsize=12)
    ax2.set_title(f'Estación {estacion}', fontsize=14)
    ax2.xaxis.set_label_position('top')
    ax2.xaxis.tick_top()
    ax2.grid(True)

    # Graficar oxígeno vs. profundidad
    ax3.scatter(oxigeno['Quality'], oxigeno['Profundidad'], color='red', s=2, alpha=0.8)
    ax3.set_xlabel('Calidad Oxígeno', fontsize=12)
    ax3.set_title(f'Estación {estacion}', fontsize=14)
    ax3.xaxis.set_label_position('top')
    ax3.xaxis.tick_top()
    ax3.grid(True)

    # Ajustar el diseño para evitar solapamientos
    plt.tight_layout()
    # Guardar la figura
    #plt.savefig(f'perfil_oceanografico-{estacion}.png', dpi=300, bbox_inches='tight')
    plt.show()

def grafica_export(filtrados, estacion):
    
    
    import matplotlib.pyplot as plt
    # Extraer datos del diccionario
    temperatura = filtrados["temperatura"]
    salinidad = filtrados["salinidad"]
    oxigeno = filtrados["oxigeno"]
    
    # Crear la figura y los ejes para las tres gráficas
    fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(15, 8), sharey=True)

    # Graficar temperatura vs. profundidad
    ax1.plot(temperatura['Value'], temperatura['Profundidad'], color='blue', linewidth=2)
    ax1.set_xlabel('Temperatura [°C]', fontsize=12)
    ax1.set_ylabel('Profundidad (m)', fontsize=12)
    ax1.set_title(f'Estación {estacion}', fontsize=14)
    ax1.invert_yaxis()  # Invertir el eje Y para que la profundidad aumente hacia abajo
    ax1.xaxis.set_label_position('top')
    ax1.xaxis.tick_top()
    ax1.grid(True)

    # Graficar salinidad vs. profundidad
    ax2.plot(salinidad['Value'], salinidad['Profundidad'], color='green', linewidth=2)
    ax2.set_xlabel('Salinidad [PSU]', fontsize=12)
    ax2.set_title(f'Estación {estacion}', fontsize=14)
    ax2.xaxis.set_label_position('top')
    ax2.xaxis.tick_top()
    ax2.grid(True)

    # Graficar oxígeno vs. profundidad
    ax3.plot(oxigeno['Value'], oxigeno['Profundidad'], color='red', linewidth=2)
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
        Diccionario con los datos filtrados para temperatura, salinidad y oxígeno.
    estacion : str
        Nombre o identificador de la estación a analizar.

    Retorno:
    -------
    None
        Muestra el gráfico generado.
    """
    import matplotlib.pyplot as plt
    import numpy as np
    import gsw

    # Extraer datos
    temperatura = filtrados["temperatura"]
    salinidad = filtrados["salinidad"]
    oxigeno = filtrados["oxigeno"]

    # Crear figura y subplots
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 8), gridspec_kw={'width_ratios': [1, 1]})
    ax1.invert_yaxis()  # Invertir eje Y (profundidad/presión)
    ax1.set_ylabel('Profundidad (m)', fontsize=12)
    ax1.set_title(f'Perfiles Oceanográficos - Estación {estacion}', fontsize=14)

    # Gráfico 1: Temperatura
    ax1.plot(temperatura['Value'], temperatura['Profundidad'], color='blue', linewidth=2, label='Temperatura [°C]')
    ax1.set_xlabel('Temperatura [°C]', fontsize=12, color='blue')
    ax1.tick_params(axis='x', labelcolor='blue')
    ax1.grid(True)

    # Crear eje secundario para Salinidad
    ax1_sal = ax1.twiny()
    ax1_sal.plot(salinidad['Value'], salinidad['Profundidad'], color='green', linewidth=2, label='Salinidad [PSU]')
    ax1_sal.set_xlabel('Salinidad [PSU]', fontsize=12, color='green')
    ax1_sal.tick_params(axis='x', labelcolor='green')

    # Crear tercer eje para Oxígeno
    ax1_oxy = ax1.twiny()
    ax1_oxy.spines['top'].set_position(('outward', 50))  # Desplazar eje hacia afuera
    ax1_oxy.plot(oxigeno['Value'], oxigeno['Profundidad'], color='red', linewidth=2, label='Oxígeno [mg L$^{-1}$]')
    ax1_oxy.set_xlabel('Oxígeno [mg L$^{-1}$]', fontsize=12, color='red')
    ax1_oxy.tick_params(axis='x', labelcolor='red')

    # Diagrama T-S
    Te = np.linspace(0, 30, 30)  # Rango de temperatura para el diagrama T-S
    Se = np.linspace(10, 35.4, 30)  # Rango de salinidad para el diagrama T-S
    Tg, Sg = np.meshgrid(Te, Se)  # Crear malla para contornos
    sigma_theta = gsw.sigma0(Sg, Tg)  # Calcular densidad potencial
    cnt = np.linspace(sigma_theta.min(), sigma_theta.max(), 10)  # Límites de contornos

    cs = ax2.contour(Sg, Tg, sigma_theta, colors='grey', levels=cnt, zorder=1)
    ax2.clabel(cs, inline=True, fontsize=10, fmt='%1.1f')  # Etiquetas de los contornos
    kw = dict(color='darkblue', linestyle="none", marker='*', markersize=5, label='Perfil')
    ax2.plot(salinidad['Value'], temperatura['Value'], **kw)
    ax2.set_xlabel('Salinidad [PSU]', fontsize=12)
    ax2.set_ylabel('Temperatura [°C]', fontsize=12)
    ax2.set_title(f'Diagrama T-S - Estación {estacion}', fontsize=14)
    

    # Ajustar diseño
    plt.tight_layout()
    plt.show()


    