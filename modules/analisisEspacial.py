def filtrado_datos_estratificacion(dataset, lista_variables):
    """
    Filtra el dataset por una lista de variables específicas.

    Parámetros:
    ----------
    dataset : DataFrame
        DataFrame de Pandas conteniendo los datos. Debe tener una columna 'Variable'.
    lista_variables : list
        Lista de strings con los nombres de las variables a filtrar.

    Retorna:
    -------
    dict
        Un diccionario donde las claves son los nombres de las variables en la
        `lista_variables` y los valores son los DataFrames filtrados correspondientes.
        Solo se incluirán las variables que se encuentren en el dataset.
    """
    datos_filtrados = {}
    for variable in lista_variables:
        datos_var = dataset[dataset.Variable == variable]
        if not datos_var.empty:
            datos_filtrados[variable.replace(' ', '_').replace('.', '').replace('[', '').replace(']', '').replace('/', '')] = datos_var
        else:
            print(f"Advertencia: No se encontraron datos para la variable '{variable}'.")
    return datos_filtrados



from sklearn.model_selection import KFold

def evaluar_interpolacion_kfoldcv(x, z, tipo_interp='linear', rbf_function='inverse', cv=5):
    """
    Realiza validación cruzada sobre los datos de entrada para una interpolación determinada.
    """
    r2_scores = []
    rmse_scores = []
    mse_scores = []

    kf = KFold(n_splits=cv, shuffle=True, random_state=42)

    for train_idx, test_idx in kf.split(x):
        x_train, x_test = x[train_idx], x[test_idx]
        y_train, y_test = z[train_idx], z[test_idx]

        if tipo_interp.lower() == 'rbf':
            interp_func = Rbf(x_train[:, 0], x_train[:, 1], y_train, function=rbf_function)
            y_pred = interp_func(x_test[:, 0], x_test[:, 1])
        elif tipo_interp.lower() in ['linear', 'cubic', 'nearest']:
            y_pred = griddata(x_train, y_train, x_test, method=tipo_interp.lower())
        else:
            y_pred = griddata(x_train, y_train, x_test, method='linear')

        if np.any(np.isnan(y_pred)):
            mask = ~np.isnan(y_pred)
            y_test = y_test[mask]
            y_pred = y_pred[mask]

        mse = mean_squared_error(y_test, y_pred)
        rmse = root_mean_squared_error(y_test, y_pred)
        r2 = r2_score(y_test, y_pred)

        mse_scores.append(mse)
        rmse_scores.append(rmse)
        r2_scores.append(r2)

    print(f"\n📊 Validación cruzada ({cv}-folds) - Método: {tipo_interp.upper()}")
    print(f"Promedio MSE: {np.mean(mse_scores):.4f} ± {np.std(mse_scores):.4f}")
    print(f"Promedio RMSE: {np.mean(rmse_scores):.4f} ± {np.std(rmse_scores):.4f}")
    print(f"Promedio R²: {np.mean(r2_scores):.4f} ± {np.std(r2_scores):.4f}")

    return np.mean(r2_scores), np.std(r2_scores), np.mean(rmse_scores), np.std(rmse_scores), np.mean(mse_scores), np.std(mse_scores)

import numpy as np
from scipy.interpolate import griddata, Rbf
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from scipy.stats import pearsonr


def evaluar_interpolacion_loocv(
    x,
    z,
    tipo_interp="linear",
    rbf_function="inverse",
    rbf_epsilon=None,
    rbf_smooth=0.0,
    fallback_nearest=True
):
    """
    Leave-One-Out Cross Validation (LOOCV) for spatial interpolation.

    Parameters
    ----------
    x : ndarray (N, 2)
        Coordinates (x, y).
    z : ndarray (N,)
        Observed values.
    method : str
        'linear', 'cubic', 'nearest', or 'rbf'.
    rbf_function : str
        RBF kernel (used if tipo_interp='rbf').
    rbf_epsilon : float or None
        Shape parameter for RBF.
    rbf_smooth : float
        Smoothing parameter for RBF.
    fallback_nearest : bool
        Use nearest-neighbor if griddata fails.

    Returns
    -------
    dict
        Metrics and pointwise LOOCV results.
    """

    n = len(z)
    y_true = np.zeros(n)
    y_pred = np.full(n, np.nan)

    for i in range(n):
        x_train = np.delete(x, i, axis=0)
        z_train = np.delete(z, i)
        x_test = x[i].reshape(1, -1)

        y_true[i] = z[i]

        try:
            if tipo_interp.lower() == "rbf":
                rbf = Rbf(
                    x_train[:, 0],
                    x_train[:, 1],
                    z_train,
                    function=rbf_function,
                    epsilon=rbf_epsilon,
                    smooth=rbf_smooth
                )
                y_pred[i] = rbf(x_test[:, 0], x_test[:, 1])[0]

            else:
                pred = griddata(x_train, z_train, x_test, method=tipo_interp.lower())

                if np.isnan(pred) and fallback_nearest:
                    pred = griddata(x_train, z_train, x_test, method="nearest")

                y_pred[i] = pred[0] if pred is not None else np.nan

        except Exception:
            y_pred[i] = np.nan

    # ---- valid predictions only ----
    mask = ~np.isnan(y_pred)
    y_true_valid = y_true[mask]
    y_pred_valid = y_pred[mask]

    residuals = y_true_valid - y_pred_valid

    # ---- metrics ----
    mse = mean_squared_error(y_true_valid, y_pred_valid)
    rmse = np.sqrt(mse)
    mae = mean_absolute_error(y_true_valid, y_pred_valid)
    r2 = r2_score(y_true_valid, y_pred_valid)
    r, p_value = pearsonr(y_true_valid, y_pred_valid)

    return {
        "metrics": {
            "R2": r2,
            "Pearson_r": r,
            "Pearson_p": p_value,
            "RMSE": rmse,
            "MSE": mse,
            "MAE": mae
        },
        "pointwise": {
            "y_true": y_true_valid,
            "y_pred": y_pred_valid,
            "residuals": residuals
        },
        "N_total": n,
        "N_valid": len(y_true_valid),
        "N_failed": n - len(y_true_valid)
    }



import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from scipy.interpolate import Rbf, griddata
import rasterio
from rasterio.features import rasterize
from rasterio.transform import from_origin
from rasterio.mask import mask
from shapely.geometry import Point, mapping
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score, root_mean_squared_error

def interpolacion(data_entrada, variable, year, color='plasma', labelVar=None,
                                  tipo_interp='linear', rbf_function='inverse',
                                  validacion_cruzada=False, cv=5):
    """
    Interpola los datos de una variable química sin considerar la profundidad y genera un mapa con el raster resultante.
    Opcionalmente realiza validación cruzada.

    Parámetros:
    ----------
    data_entrada : DataFrame
        DataFrame con los datos de la variable química.
    variable : str
        Nombre de la variable química a interpolar.
    color : str, optional
        Colormap para la interpolación (default: "plasma").
    labelVar : str, optional
        Etiqueta para la barra de color (default: None).
    tipo_interp : str, optional
        Tipo de interpolación a usar ('linear', 'cubic', 'nearest', 'rbf'). Default es 'linear'.
    rbf_function : str, optional
        Función de base radial a usar si tipo_interp es 'rbf'
        ('linear',  'gaussian', 'multiquadric', 'inverse', 'inverse_multiquadric').
        Default es 'inverse'.
    validacion_cruzada : bool, optional
        Indica si se debe realizar validación cruzada (default: False).
    cv : int, optional
        Número de folds para la validación cruzada (default: 5).
    """
    # 1. Filtrar datos por variable
    print("Filtrando datos por variable...")
    data = data_entrada[['Latitud[deg]', 'Longitud[deg]', variable]].copy()

    # Forzar a numérico
    data['Latitud[deg]'] = pd.to_numeric(data['Latitud[deg]'], errors='coerce')
    data['Longitud[deg]'] = pd.to_numeric(data['Longitud[deg]'], errors='coerce')
    data[variable] = pd.to_numeric(data[variable], errors='coerce')

    # Eliminar filas inválidas
    data = data.dropna(subset=['Latitud[deg]', 'Longitud[deg]', variable])


    if data.empty:
        print(f"No hay datos válidos para la variable '{variable}'.")
        return

    variable_name = data['Variable'].unique()[0] if 'Variable' in data.columns else variable
    if labelVar is None:
        labelVar = variable_name

    # 2. Leer capas geográficas
    print("Leyendo capas geográficas...")
    try:
        continente = gpd.read_file("./sig/GeoLayers.gpkg", layer="Continente")
        cpc = gpd.read_file("./sig/GeoLayers.gpkg", layer="cuenca_pacifica")  # Capa de recorte
        AMP = gpd.read_file("./sig/GeoLayers.gpkg", layer="AMP")
    except FileNotFoundError as e:
        print(f"Error al leer las capas geográficas: {e}")
        return

    # 3. Convertir data a GeoDataFrame
    print("Convirtiendo a geodataframe...")
    geometry = gpd.points_from_xy(data["Longitud[deg]"], data["Latitud[deg]"])
    gdf = gpd.GeoDataFrame(data, geometry=geometry, crs="EPSG:4326")

    # 4. Extraer coordenadas y valores
    x = gdf.geometry.x.values  # Longitud
    y = gdf.geometry.y.values  # Latitud
    z = gdf[variable].values  # Valores de la variable
    x_coords = np.column_stack((x, y))

    print(f"Tipo de datos de x: {x.dtype}")
    print(f"Tipo de datos de y: {y.dtype}")
    print(f"Tipo de datos de z: {z.dtype}")

    # Validación cruzada si se solicita
    r2_mean = r2_std = rmse_mean = rmse_std = mse_mean = mse_std = None
    if validacion_cruzada:
        if cv == -1:
            print("⚠️ Usando Leave-One-Out Cross Validation (LOOCV)...")
            resultados = evaluar_interpolacion_loocv(
                x_coords, z, 
                tipo_interp=tipo_interp, 
                rbf_function=rbf_function,
                rbf_epsilon=None,
                rbf_smooth=0.0,
                fallback_nearest=True
            )
        else:
            resultados = evaluar_interpolacion_kfoldcv(
                x_coords, z, tipo_interp=tipo_interp, rbf_function=rbf_function, cv=cv
            )


        r2_mean = resultados["metrics"]["R2"]
        pearsonr = resultados["metrics"]["Pearson_r"]
        pearsonp = resultados["metrics"]["Pearson_p"]
        rmse_mean = resultados["metrics"]["RMSE"]
        mse_mean = resultados["metrics"]["MSE"] 
        
        



    # resultados de la validación cruzada
    print(f"\nResultados de la validación cruzada:")
    print(f"Pearson r: {pearsonr:.2f} (p={pearsonp:.2e})")
    
    y_true = resultados["pointwise"]["y_true"]
    y_pred = resultados["pointwise"]["y_pred"]
    residuals = resultados["pointwise"]["residuals"]
        
    # 5. Crear una malla de interpolación
    grid_x, grid_y = np.meshgrid(
        np.linspace(-84.5, -77, 1000),  # 1000 puntos en X
        np.linspace(1, 7, 1000)  # 1000 puntos en Y
    )

    # 6. Aplicar la interpolación seleccionada
    print(f"Interpolando {variable} con método: {tipo_interp} para el año {year}...")
    if tipo_interp.lower() == 'rbf':
        interp_func = Rbf(x, y, z, function=rbf_function)
        grid_z = interp_func(grid_x, grid_y)
        interp_type_display = f"RBF ({rbf_function})"
    elif tipo_interp.lower() in ['linear', 'cubic', 'nearest']:
        grid_z = griddata(x_coords, z, (grid_x, grid_y), method=tipo_interp.lower())
        interp_type_display = tipo_interp.upper()
    else:
        print(f"Tipo de interpolación '{tipo_interp}' no válido. Usando Linear por defecto.")
        grid_z = griddata(x_coords, z, (grid_x, grid_y), method='linear')
        interp_type_display = 'LINEAR (Default)'

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
    raster_path = f"./sig/{variable}_{year}_{tipo_interp}_{rbf_function}_raster.tif"
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
    try:
        with rasterio.open(raster_path) as src:
            cpc_geom = [mapping(geom) for geom in cpc.geometry]  # Convertir geometría a máscara
            clipped_raster, clipped_transform = mask(src, cpc_geom, crop=True, nodata=np.nan)

            # Extraer la banda 1 de clipped_raster
            clipped_raster = clipped_raster[0]  # Convertir (1, altura, ancho) → (altura, ancho)

            # Guardar raster recortado
            clipped_raster_path = f"./sig/{variable}_{year}{tipo_interp}_{rbf_function}_raster_clipped.tif"
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
                dst.write(clipped_raster, 1)
    except rasterio.RasterioIOError as e:
        print(f"Error al abrir o manipular el raster: {e}")
        return

    # 10. Graficar el raster recortado
    fig, ax = plt.subplots(figsize=(8, 6))

    try:
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
        ax.scatter(x, y, color='black', s=10, label='Datos Originales')

        # Configurar etiquetas y título
        ax.set_xlabel("Longitud")
        ax.set_ylabel("Latitud")
        if validacion_cruzada:
            tipo_cv = "LOOCV" if cv == -1 else f"{cv}-Fold CV"
            titulo = (f"{variable_name} - {year}\n"
                f"Interpolación {interp_type_display} - {tipo_cv}\n"
                      fr"Pearson $\rho$  = {pearsonr:.2f} | "
                      f"p-valor = {pearsonp:.2f}\n"
                      )

        else:
            titulo = f"Interpolación {interp_type_display}\n{variable_name}"
        ax.set_title(titulo)

        # 11. Ajustar límites del mapa
        ax.set_xlim(-84.5, -77)
        ax.set_ylim(1, 8)

        

        # 13. Guardar la figura
        plt.colorbar(img, ax=ax, label=labelVar)
        plt.savefig(f'./outputs/mapas/{variable}_{year}_{tipo_interp}_{rbf_function}_clipped.png', dpi=300, bbox_inches='tight')
        plt.show()
        
       # 14. Graficar resultados de la validación cruzada
        if validacion_cruzada:
            fig, ax = plt.subplots(figsize=(6, 6))

            plt.scatter(y_true, y_pred)
            plt.plot([y_true.min(), y_true.max()],
                    [y_true.min(), y_true.max()],
                    '--')

            plt.xlabel(f"Observado - {variable}")
            plt.ylabel(f"Predicho (LOOCV) - {variable}")
            plt.title(f"LOOCV Análisis de {variable} - {year}\n"
                    f"Interpolación {interp_type_display} - {tipo_cv}\n"
                      fr"$\rho$ Pearson = {pearsonr:.2f}, p-valor = {pearsonp:.2f}"
                      )
            plt.show()
        else:
            print("Validación cruzada no realizada; no se grafican resultados de LOOCV.")

        if validacion_cruzada:
        
            plt.hist(residuals, bins=20)
            plt.xlabel("Residuo")
            plt.ylabel("Frecuencia")
            plt.title(f"Distribución de residuos LOOCV de {variable} - {year}\n"
                       f"Interpolación {interp_type_display} - {tipo_cv}\n"
                      fr"$\rho$ Pearson = {pearsonr:.2f}, p-valor = {pearsonp:.2f}")
            plt.show()
        else:
            print("Validación cruzada no realizada; no se grafican residuos.")

    except rasterio.RasterioIOError as e:
        print(f"Error al abrir el raster para graficar: {e}")
    except Exception as e:
        print(f"Error durante la graficación: {e}")


from scipy.spatial import distance
import numpy as np
import pandas as pd
from pyproj import Transformer
     
def calculate_correlogram(data_entrada, variable, n_lags=50, n_permutations=50, confidence=0.95):
    """
    Calculates and returns a correlogram for spatial data with significance bands.
    """
    # 1. Filtrar datos por variable
    print("Filtrando datos por variable...")
    data = data_entrada[['Latitud[deg]', 'Longitud[deg]', variable]]

    if data.empty:
        print(f"No hay datos válidos para la variable '{variable}'.")
        return

        # Tus datos con coordenadas en grados
    x = data['Longitud[deg]']
    y = data['Latitud[deg]']
    z = data[variable]

    

    # Crear el transformador: WGS84 (EPSG:4326) → MAGNA-SIRGAS / Colombia Oeste-Oeste (EPSG:9377)
    transformer = Transformer.from_crs("EPSG:4326", "EPSG:3114", always_xy=True)

    # Aplicar la transformación
    x_proj, y_proj = transformer.transform(x.values, y.values)

    # Agregar al DataFrame como nuevas columnas
    data['X_proj'] = x_proj
    data['Y_proj'] = y_proj
        
    
    x = data['X_proj']
    y = data['Y_proj']
  
    
    
    
    if not (len(x) == len(y) == len(z)):
        raise ValueError("x, y, and z must have the same length.")

    points = np.column_stack((x, y))
    distances = distance.pdist(points)/ 1000  # Convertir a kilómetros
    z_values = np.array(z)
    lag_size = np.max(distances) / n_lags

    lag_centers = []
    correlations = []
    lower_bounds = []
    upper_bounds = []

    indices_pairs = np.array(np.triu_indices(len(x), 1)).T

    for i in range(n_lags):
        lower_bound = i * lag_size
        upper_bound = (i + 1) * lag_size
        within_lag_indices = np.where((distances >= lower_bound) & (distances < upper_bound))[0]
        selected_pairs = indices_pairs[within_lag_indices]

        if len(selected_pairs) > 0:
            z1 = np.array([z_values[i] for i, j in selected_pairs])
            z2 = np.array([z_values[j] for i, j in selected_pairs])

            if np.std(z1) == 0 or np.std(z2) == 0:
                observed_corr = np.nan
            else:
                observed_corr = np.corrcoef(z1, z2)[0, 1]

            correlations.append(observed_corr)
            lag_centers.append((lower_bound + upper_bound) / 2)

            # Permutation test
            permuted_corrs = []
            for _ in range(n_permutations):
                permuted_z = np.random.permutation(z_values)
                pz1 = np.array([permuted_z[i] for i, j in selected_pairs])
                pz2 = np.array([permuted_z[j] for i, j in selected_pairs])

                if np.std(pz1) == 0 or np.std(pz2) == 0:
                    perm_corr = np.nan
                else:
                    perm_corr = np.corrcoef(pz1, pz2)[0, 1]
                permuted_corrs.append(perm_corr)

            # Filtrar nan
            permuted_corrs = [c for c in permuted_corrs if not np.isnan(c)]

            if len(permuted_corrs) > 0:
                lower = np.percentile(permuted_corrs, (1 - confidence) / 2 * 100)
                upper = np.percentile(permuted_corrs, (1 + confidence) / 2 * 100)
            else:
                lower = np.nan
                upper = np.nan

            lower_bounds.append(lower)
            upper_bounds.append(upper)

    return lag_centers, correlations, lower_bounds, upper_bounds
