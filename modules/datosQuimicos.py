import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


class AnalisisQuimicos:
    """
    Clase para manejar datos químicos de estaciones oceanográficas.

    Atributos:
    ----------
    dataset : DataFrame
        DataFrame que contiene los datos químicos.
    estacion : str
        Nombre o identificador de la estación asociada a esta instancia.

    Métodos:
    -------
    filtrado_datos_quimicos():
        Filtra los datos químicos para la estación específica de la instancia.
    graficar_datos_quimicos():
        Genera gráficos de perfiles oceanográficos utilizando los datos filtrados.
    """

    def __init__(self, dataset, estacion):
        """
        Constructor de la clase DatosQuimicos.

        Parámetros:
        ----------
        dataset : DataFrame
            DataFrame que contiene los datos químicos con columnas como
            'Estacion[#]', 'Variable', 'Profundidad[m]' y la columna de 'Valor'.
        estacion : str
            Nombre o identificador de la estación a analizar con esta instancia.
        """
        self.dataset = dataset
        self.estacion = estacion
        self.filtrados = self.filtrado_estacion()
        self.filtrados_variables = self.filtrado_variables()

    def filtrado_estacion(self):
        """
        Filtra los datos químicos para la estación específica de la instancia.

        Retorna:
        -------
        dict
            Un diccionario donde las claves son los nombres de las variables químicas
            y los valores son DataFrames con las columnas 'Profundidad[m]' y 'Valor'
            para esa variable en la estación especificada, ordenados por profundidad.
        """
        datos_filtrados = {}
        variables_unicas = self.dataset['Variable'].unique()
        for variable in variables_unicas:
            datos_var = self.dataset[(self.dataset['Variable'] == variable) & (self.dataset['Estacion[#]'] == self.estacion)].sort_values('Profundidad (m)')
            if not datos_var.empty:
                # Asumimos que hay una columna 'Value' con los valores de la variable
                # Si tu columna tiene un nombre diferente, ajústalo aquí.
                if 'Value' in datos_var.columns:
                    datos_filtrados[self._generar_clave(variable)] = datos_var[['Profundidad (m)', 'Value']]
                elif variable == 'OD [mg O2/L]':
                    datos_filtrados['oxigeno'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == 'Clorofila [µg/L]':
                    datos_filtrados['clorofila'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == 'pH [Dmnless]':
                    datos_filtrados['pH'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == 'Salinidad[UPS]':
                    datos_filtrados['salinidad'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == 'TNOx [µM]':
                    datos_filtrados['tnox'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == '[NO2--N] [µM]':
                    datos_filtrados['NO2'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == '[NO3--N] [µM]':
                    datos_filtrados['NO3'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == '[PO4-3-P] [µM]':
                    datos_filtrados['PO4'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == '[SiO2-Si] [µM]':
                    datos_filtrados['SiO2'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == 'N:P':
                    datos_filtrados['NP'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == 'Si:P':
                    datos_filtrados['SiP'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
                elif variable == 'Si:N':
                    datos_filtrados['SiN'] = datos_var[['Profundidad (m)', datos_var.columns[-1]]].rename(columns={datos_var.columns[-1]: 'Value'})
            else:
                # Si no hay datos para esta variable en la estación, podemos optar por no incluirla
                # o incluirla con un mensaje de "sin datos" si es necesario.
                # Aquí se opta por no incluirla.
                pass
        return datos_filtrados

    def _generar_clave(self, nombre_variable):
        """
        Genera una clave segura para el diccionario de datos filtrados.
        """
        return nombre_variable.lower().replace(' ', '_').replace('[', '').replace(']', '').replace('"', '').replace("'", '').replace('-', '_').replace('µm', 'um').replace('ups', 'psu')

    def perfiles_estacion(self):
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
        parametros_a_graficar = [
            ("od_mg_o2/l", "OD [mg O$_2$/L]"),
            ("clorofila_µg/l", 'Clorofila [µg/L]'),
            ("ph_dmnless", "pH [Dmnless]"),
            ("salinidadpsu", "Salinidad[UPS]"),
            ("no2__n_um", "[NO$_2$--N] [µM]"),
            ("no3__n_um", "[NO$_3$--N] [µM]"),
            ("po4_3_p_um", "[PO$_4^{-3}$-P] [µM]"),
            ("sio2_si_um", "[SiO$_2$-Si] [µM]"),
            ("tnox_um", "TNOx [µM]"),
            ("n:p", "N:P"),
            ("si:p", "Si:P"),
            ("si:n", "Si:N")
        ]

        fig, ax = plt.subplots(3, 4, figsize=(15, 18), sharey=True)
        fig.suptitle(f'Perfiles químicos - Estación {self.estacion}', fontsize=16)

        for i, (key, xlabel) in enumerate(parametros_a_graficar):
            row = i // 4
            col = i % 4
            if key in self.filtrados:
                data = self.filtrados[key]
                if not data.empty and 'Value' in data.columns and 'Profundidad (m)' in data.columns:
                    ax[row, col].plot(data['Value'], data['Profundidad (m)'], color='blue', linestyle='dotted', marker='o', linewidth=1.5)
                    ax[row, col].set_xlabel(xlabel, fontsize=12)
                    ax[row, col].set_ylabel('Profundidad (m)', fontsize=12)
                    ax[row, col].xaxis.set_label_position('top')
                    ax[row, col].xaxis.tick_top()
                    ax[row, col].grid(True)
                else:
                    ax[row, col].set_xlabel(f"{xlabel}\n(Sin datos)", fontsize=10)
                    ax[row, col].grid(True)
            else:
                ax[row, col].set_xlabel(f"{xlabel}\n(No encontrado)", fontsize=10)
                ax[row, col].grid(True)

        
        plt.gca().invert_yaxis() # Invertir solo el último axes si es necesario
        plt.tight_layout(rect=[0, 0, 1, 0.96])  # Ajustar para que no se sobreponga el título
        plt.show()
        
        
        
    def filtrado_variables(self):
                
       
        oxigeno = self.dataset[(self.dataset.Variable == 'OD [mg O2/L]')].sort_values('Profundidad (m)')
        clorofila = self.dataset[(self.dataset.Variable == 'Clorofila [µg/L]') ].sort_values('Profundidad (m)')
        pH = self.dataset[(self.dataset.Variable == 'pH [Dmnless]') ].sort_values('Profundidad (m)')
        salinidad = self.dataset[(self.dataset.Variable == 'Salinidad[UPS]') ].sort_values('Profundidad (m)')
        NO2 = self.dataset[(self.dataset.Variable == '[NO2--N] [µM]') ].sort_values('Profundidad (m)')
        NO3 = self.dataset[(self.dataset.Variable == '[NO3--N] [µM]') ].sort_values('Profundidad (m)')
        PO4 = self.dataset[(self.dataset.Variable == '[PO4-3-P] [µM]') ].sort_values('Profundidad (m)')
        SiO2 = self.dataset[(self.dataset.Variable == '[SiO2-Si] [µM]') ].sort_values('Profundidad (m)')
        tnox = self.dataset[(self.dataset.Variable == 'TNOx [µM]') ].sort_values('Profundidad (m)')
        NP = self.dataset[(self.dataset.Variable == 'N:P') ].sort_values('Profundidad (m)')
        SiP = self.dataset[(self.dataset.Variable == 'Si:P') ].sort_values('Profundidad (m)')
        SiN = self.dataset[(self.dataset.Variable == 'Si:N') ].sort_values('Profundidad (m)')
        
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
    
    def boxplot_variable(self, variable, xlabel):
        

        # Verifica si la variable está en el diccionario
        if variable not in self.filtrados_variables:
            print(f"Variable '{variable}' no encontrada.")
            return

        data = self.filtrados_variables[variable].copy()

        # Verifica que existan las columnas requeridas
        if 'Value' not in data.columns or 'Profundidad (m)' not in data.columns:
            print(f"La variable '{variable}' no contiene las columnas requeridas.")
            return

        # Asegura que la columna de profundidad sea numérica
        data['Profundidad (m)'] = pd.to_numeric(data['Profundidad (m)'], errors='coerce')

        # Quitar filas con NaN (en caso de errores en conversión)
        data.dropna(subset=['Profundidad (m)', 'Value'], inplace=True)

        # Crear la figura y el eje
        fig, ax = plt.subplots(figsize=(6, 8))
        sns.boxplot(y='Profundidad (m)', x='Value', data=data, orient="h", color="skyblue", ax=ax)

        ax.set_xlabel(xlabel, fontsize=12)
        ax.set_ylabel('Profundidad (m)', fontsize=12)
        
        ax.xaxis.set_label_position('top')
        ax.xaxis.tick_top()
        ax.grid(True)

        # ✅ Invertir eje Y correctamente
        ax.set_ylim(sorted(data['Profundidad (m)'].unique(), reverse=True))
        ax.invert_yaxis()

        plt.tight_layout()
        plt.show()





    def boxplots_totales(self):
        parametros = [
            ("oxigeno", "OD [mg O$_2$/L]"),
            ("clorofila", "Clorofila [µg/L]"),
            ("pH", "pH [Dmnless]"),
            ("salinidad", "Salinidad[UPS]"),
            ("NO2", "[NO$_2$--N] [µM]"),
            ("NO3", "[NO$_3$--N] [µM]"),
            ("PO4", "[PO$_4^{-3}$-P] [µM]"),
            ("SiO2", "[SiO$_2$-Si] [µM]"),
            ("tnox", "TNOx [µM]"),
            ("NP", "N:P"),
            ("SiP", "Si:P"),
            ("SiN", "Si:N")
        ]

        fig, ax = plt.subplots(3, 4, figsize=(15, 18), sharey=True)
        fig.suptitle(f'Diagramas de cajas totales', fontsize=16)

        for axes, (key, xlabel) in zip(ax.flat, parametros):
            if key in self.filtrados_variables:
                data = self.filtrados_variables[key]
                if 'Value' in data.columns:
                    sns.boxplot(y=data['Profundidad (m)'], x=data['Value'], ax=axes, orient="h", color="skyblue")
                    axes.set_xlabel(xlabel, fontsize=12)
                    axes.set_ylabel('Profundidad (m)', fontsize=12)
                    axes.xaxis.set_label_position('top')
                    axes.xaxis.tick_top()
                    axes.grid(True)
                else:
                    axes.set_title(f"{xlabel}\n(Sin columna 'Value')", fontsize=10)

        for axes in ax.flat:
            axes.invert_yaxis()

        plt.tight_layout(rect=[0, 0, 1, 0.99])
        plt.show()
      
        

  
