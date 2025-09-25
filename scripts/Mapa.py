import geopandas as gpd
import xarray as xr
import matplotlib.pyplot as plt
from scipy.io import netcdf
import numpy as np
import matplotlib


# Definir los límites del mapa
min_lon, max_lon = -85, -77
min_lat, max_lat = 1, 10

# Leer las capas desde un GeoPackage
# Supongamos que el archivo geopackage tiene varias capas
geopackage_path = "./data/gis/GeoLayers.gpkg"
gdf = gpd.read_file(geopackage_path, layer="Continente")  # Cambia el nombre de la capa

# Filtrar las geometrías dentro del área de interés
gdf_filtered = gdf.cx[min_lon:max_lon, min_lat:max_lat]

# Leer un archivo NetCDF
netcdf_path = "./data/gis/data_1993_04_eastward_sea_water_velocity.nc"
nc_data = xr.open_dataset(netcdf_path)

# Suponiendo que hay variables como latitud, longitud y alguna variable climática en el NetCDF
lons = nc_data['longitude'].values
lats = nc_data['latitude'].values
variable = nc_data['uo'].values  # Reemplaza con el nombre de la variable relevante

# Crear la figura
fig, ax = plt.subplots(figsize=(10, 8))

# Graficar las capas del GeoPackage
gdf_filtered.plot(ax=ax, color='lightgray', edgecolor='black')

# Graficar datos del NetCDF (supongamos que es un mapa de calor)
plt.pcolormesh(lons, lats, variable, cmap='viridis', shading='auto')

# Configurar límites del mapa
ax.set_xlim(min_lon, max_lon)
ax.set_ylim(min_lat, max_lat)

# Añadir título y etiquetas
plt.title("Bahía de panamá")
plt.xlabel("Longitud")
plt.ylabel("Latitud")

# Mostrar el mapa
plt.show()




file2read = netcdf.NetCDFFile(netcdf_path)
temp = file2read.variables["uo"] # var can be 'Theta', 'S', 'V', 'U' etc..
data = temp[:]*1
file2read.close()

plt.contourf(data[t,z,:,:])

