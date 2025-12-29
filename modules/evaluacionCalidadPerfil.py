import matplotlib.pyplot as plt
import numpy as np
from scipy.signal import find_peaks

def filtrado(dataset, estacion):
    """
    Filtra el conjunto de datos por estación y variable, y ordena por profundidad.

    Parámetros:
    - dataset: DataFrame de pandas que contiene los datos.
    - estacion: Nombre o identificador de la estación a filtrar.

    Retorna:
    Un diccionario con DataFrames filtrados para temperatura, salinidad y oxígeno.
    """
    # Filtrar y ordenar datos por variable y estación
    temperatura = dataset[(dataset['Variable'] == 'Temperatura (°C)') & 
                          (dataset['Estacion'] == estacion)].sort_values('Profundidad (m)')
    salinidad = dataset[(dataset['Variable'] == 'Salinidad (UPS)') & 
                        (dataset['Estacion'] == estacion)].sort_values('Profundidad (m)')
    oxigeno = dataset[(dataset['Variable'] == 'Oxígeno [mg/L]') & 
                      (dataset['Estacion'] == estacion)].sort_values('Profundidad (m)')
    densidad = dataset[(dataset['Variable'] == 'Densidad [mg/m3]') & 
                      (dataset['Estacion'] == estacion)].sort_values('Profundidad (m)')
    
    return {
        "temperatura": temperatura,
        "salinidad": salinidad,
        "oxigeno": oxigeno,
        "densidad": densidad
    }

def peaks_graphs(filtrados, estacion, tipo_perfil):
    """
    Genera y guarda gráficos de perfiles de temperatura y salinidad con picos detectados.

    Parámetros:
    - filtrados: Diccionario con DataFrames de temperatura, salinidad y oxígeno filtrados.
    - estacion: Nombre o identificador de la estación.

    Retorna:
    None. Guarda las imágenes de los gráficos en la carpeta './perfiles/'.
    """
    # Extraer datos del diccionario
    temperatura = filtrados["temperatura"]
    salinidad = filtrados["salinidad"]
    
    # Convertir datos a arrays de numpy
    salinity = np.array(salinidad['Value'])  # PSU
    temperature = np.array(temperatura['Value'])  # °C
    depth = np.array(temperatura['Profundidad (m)'])  # metros
    
    # Detectar picos en temperatura y salinidad
    Temp_pks, _ = find_peaks(temperature, prominence=0.02)  
    Sal_pks, _ = find_peaks(salinity, prominence=0.02)  
    
    # Crear la figura y los ejes para las gráficas
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 8), sharey=True)

    # Gráfico de temperatura vs. profundidad
    ax1.plot(temperatura['Value'], temperatura['Profundidad (m)'], color='blue', linewidth=2, label="Perfil de temperatura")
    ax1.plot(temperature[Temp_pks], depth[Temp_pks], 'or', markersize=8, markerfacecolor='r', label='Picos detectados')
    ax1.set_xlabel('Temperatura (°C)', fontsize=12)
    ax1.set_ylabel('Profundidad (m)', fontsize=12)
    ax1.set_title(f'Estación {estacion} - {tipo_perfil}', fontsize=14)
    ax1.invert_yaxis()  # Invertir el eje Y para que la profundidad aumente hacia abajo
    ax1.xaxis.set_label_position('top')
    ax1.xaxis.tick_top()
    ax1.legend()
    ax1.grid(True)

    # Gráfico de salinidad vs. profundidad
    ax2.plot(salinidad['Value'], salinidad['Profundidad (m)'], color='green', linewidth=2, label="Perfil de salinidad")
    ax2.plot(salinity[Sal_pks], depth[Sal_pks], 'or', markersize=8, markerfacecolor='r', label='Picos detectados')
    ax2.set_xlabel('Salinidad [UPS]', fontsize=12)
    ax2.set_title(f'Estación {estacion} - {tipo_perfil}', fontsize=14)
    ax2.xaxis.set_label_position('top')
    ax2.xaxis.tick_top()
    ax2.legend()
    ax2.grid(True)

    # Ajustar el diseño para evitar solapamientos
    plt.tight_layout()
    plt.savefig(f'./perfiles/perfil_oceanografico_picos_{estacion}_{tipo_perfil}.png', dpi=300, bbox_inches='tight')
    plt.show()

def peaks_data(filtrados, estacion, tipo_perfil):
    """
    Detecta picos en los perfiles de temperatura y salinidad, y guarda los resultados en un archivo de texto.

    Parámetros:
    - filtrados: Diccionario con DataFrames de temperatura, salinidad y oxígeno filtrados.
    - estacion: Nombre o identificador de la estación.

    Retorna:
    Una tupla con los índices y profundidades de los picos detectados en temperatura y salinidad.
    """
    # Extraer datos del diccionario
    temperatura = filtrados["temperatura"]
    salinidad = filtrados["salinidad"]
    
    # Convertir datos a arrays de numpy
    salinity = np.array(salinidad['Value'])  # PSU
    temperature = np.array(temperatura['Value'])  # °C
    depth = np.array(temperatura['Profundidad (m)'])  # metros
    
    # Detectar picos en temperatura y salinidad
    Temp_pks, _ = find_peaks(temperature, prominence=0.02)  
    Sal_pks, _ = find_peaks(salinity, prominence=0.02)  
    
    # Obtener profundidades correspondientes a los picos
    depth_temp_pks = depth[Temp_pks]
    depth_sal_pks = depth[Sal_pks]

    # Crear el nombre del archivo con la estación
    filename = f"./resultados_picos/resultados_{estacion}_{tipo_perfil}.txt"

    # Guardar en un archivo de texto con formato de tabla
    with open(filename, "w") as f:
        f.write(f"Estación: {estacion}\n\n")
        f.write("Tabla de picos detectados:\n")
        f.write(f"{'Tipo':<20}{'Índice':<10}{'Profundidad [m]':<15}\n")
        f.write("-" * 45 + "\n")

        # Escribir picos de temperatura
        for idx, depth_value in zip(Temp_pks, depth_temp_pks):
            f.write(f"{'Temperatura':<20}{idx:<10}{depth_value:<15.2f}\n")

        # Escribir picos de salinidad
        for idx, depth_value in zip(Sal_pks, depth_sal_pks):
            f.write(f"{'Salinidad':<20}{idx:<10}{depth_value:<15.2f}\n")

    print(f"Resultados guardados en {filename}")

    return Temp_pks, depth_temp_pks, Sal_pks, depth_sal_pks
