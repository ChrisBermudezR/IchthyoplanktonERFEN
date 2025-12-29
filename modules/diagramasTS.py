import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import gsw
from matplotlib.ticker import MaxNLocator
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
from sklearn.metrics import silhouette_score



class AnalisisCluster():
    def __init__(self, dataset):
        self.dataset = dataset
        self.salinidad_array = dataset[dataset["Variable"] == "Salinidad (UPS)"]["Value"].to_numpy()
        self.temperatura_array = dataset[dataset["Variable"] == "Temperatura (°C)"]["Value"].to_numpy()
        self.Profundidad_array = dataset[dataset["Variable"] == "Temperatura (°C)"]["Profundidad (m)"].to_numpy()
        
    def filtracionDatos(self):
        self.initial_df = self.dataset.pivot_table(index=['Estacion', 'Profundidad (m)'],
                                       columns='Variable',
                                       values='Value').reset_index()

        self.kmean_df =  self.initial_df[['Temperatura (°C)', 'Salinidad (UPS)', 'Densidad [mg/m3]']]
        
        print(f"Filtracion de datos: {self.kmean_df.shape[0]} filas y {self.kmean_df.shape[1]} columnas")
        print(f"Filtracion de datos finalizada")
        
        return self.kmean_df, self.initial_df
    
    def codo(self):
        # Escalar los datos
        scaler = StandardScaler()
        X_scaled = scaler.fit_transform(self.kmean_df)

        # Determinar el número óptimo de clusters (Método del Codo)
        inertia = []
        for i in range(2, 11):
            kmeans = KMeans(n_clusters=i, random_state=42, n_init=10)
            kmeans.fit(X_scaled)
            inertia.append(kmeans.inertia_)
        print("Iniciando gráfica")
        plt.figure(figsize=(10, 6))
        plt.plot(range(2, 11), inertia, marker='o')
        plt.title('Método del Codo para determinar el número de clusters')
        plt.xlabel('Número de clusters')
        plt.ylabel('Inercia')
        plt.savefig(f'./graficas/DiagramaTS/Prueba_Codo.png', dpi=300, bbox_inches='tight')
        plt.show()
        
    def silueta(self):
        # Escalar los datos
        scaler = StandardScaler()
        X_scaled = scaler.fit_transform(self.kmean_df)
        
        silhouette_scores = []
        for i in range(2, 11):
            kmeans = KMeans(n_clusters=i, random_state=42, n_init=10)
            labels = kmeans.fit_predict(X_scaled)
            silhouette_scores.append(silhouette_score(X_scaled, labels))
        print("Iniciando gráfica")
        plt.figure(figsize=(10, 6))
        plt.plot(range(2, 11), silhouette_scores, marker='o')
        plt.title('Puntaje de Silueta para determinar el número de clusters')
        plt.xlabel('Número de clusters')
        plt.ylabel('Puntaje de Silueta')
        plt.savefig(f'./graficas/DiagramaTS/Prueba_Silueta.png', dpi=300, bbox_inches='tight')
        plt.show()
    
    def kmeans(self, n_clusters=2):
        self.n_clusters = n_clusters
        
        # Normalizar los datos
        scaler = StandardScaler()
        data_scaled = scaler.fit_transform(self.kmean_df)
        

        # Aplicar K-means
        kmeans_final = KMeans(n_clusters=self.n_clusters, random_state=42)
        self.kmean_df['Cluster'] = kmeans_final.fit_predict(data_scaled)
        self.cluster_array = self.kmean_df['Cluster'].to_numpy()
        
        print(self.kmean_df.head())

        # Opcional: Analizar las características de los clusters
        cluster_df = self.kmean_df.groupby('Cluster')[['Temperatura (°C)',  'Salinidad (UPS)', 'Densidad [mg/m3]']].mean()
        print("\nCaracterísticas promedio de los clusters:")
        print(cluster_df)

        unidos= pd.concat([self.initial_df, self.kmean_df['Cluster']], axis=1)

        with pd.ExcelWriter('data/definitivos/kmean_df.xlsx', engine='openpyxl') as writer:
            unidos.to_excel(writer, sheet_name='KMeans_Clusters', index=False)
            cluster_df.to_excel(writer, sheet_name='Cluster_Analysis', index=True)

       
        return self.cluster_array

    def diagramaTS_cluster(self):
        if len(self.salinidad_array) != len(self.temperatura_array) or len(self.salinidad_array) != len(self.cluster_array):
            raise ValueError("Las longitudes de salinidad, temperatura y cluster no coinciden.")

        temp = np.linspace(self.temperatura_array.min() - 1, self.temperatura_array.max() + 1, 25)
        sal = np.linspace(self.salinidad_array.min() - 1, self.salinidad_array.max() + 1, 25)
        temp, sal = np.meshgrid(temp, sal)
        sigma_theta = gsw.sigma0(sal, temp)
        levels = np.linspace(sigma_theta.min(), sigma_theta.max(), 10)

        fig, ax = plt.subplots(figsize=(8.25, 7))
        cs = ax.contour(sal, temp, sigma_theta, colors='black', levels=levels, zorder=1)
        ax.clabel(cs, fontsize=10, inline=1, fmt='%.1f')

        unique_clusters = np.unique(self.cluster_array)
        cmap = plt.cm.get_cmap('jet', len(unique_clusters))
        cluster_colors = {cluster_val: cmap(i) for i, cluster_val in enumerate(unique_clusters)}

        for cluster_val in unique_clusters:
            indices = self.cluster_array == cluster_val
            ax.scatter(self.salinidad_array[indices], self.temperatura_array[indices],
                       c=[cluster_colors[cluster_val]] * np.sum(indices),
                       label=f'Cluster {int(cluster_val)}', s=35)

        ax.set_xlabel('Salinidad (UPS)')
        ax.set_ylabel('Temperatura [$^\circ$C]')
        ax.set_title('Diagrama T-S clusterizado por K-means', y=1.025)
        
        masas = [("ASCET", 28, 26), ("ACSEP", 32.5, 23.5), ("AIA", 33.7, 15), ("AAF", 33.7, 7)]
        for nombre, x, y in masas:
            ax.text(x, y, nombre, fontsize=12, color="red", fontweight="bold",
                    ha="center", va="center", bbox=dict(facecolor="none", alpha=0.7, edgecolor="none", boxstyle="round,pad=0.3"))
            
        ax.legend(title='Cluster')

        plt.savefig('./graficas/DiagramaTS/DiagramaTS_cluster.png', dpi=300, bbox_inches='tight')
        plt.show()



class DiagramasTS():  
    
    def __init__(self, dataset):
        self.dataset = dataset
        self.salinidad_array = dataset[dataset["Variable"] == "Salinidad (UPS)"]["Value"].to_numpy()
        self.temperatura_array = dataset[dataset["Variable"] == "Temperatura (°C)"]["Value"].to_numpy()
        self.Profundidad_array = dataset[dataset["Variable"] == "Temperatura (°C)"]["Profundidad (m)"].to_numpy()
        
    

    def diagramaTS(self):
        if len(self.salinidad_array) != len(self.temperatura_array) or len(self.salinidad_array) != len(self.Profundidad_array):
            raise ValueError("Las longitudes de salinidad, temperatura y profundidad no coinciden.")
        
        # Malla para curvas de densidad
        temp = np.linspace(self.temperatura_array.min() - 1, self.temperatura_array.max() + 1, 25)
        sal = np.linspace(self.salinidad_array.min() - 1, self.salinidad_array.max() + 1, 25)
        temp, sal = np.meshgrid(temp, sal)
        sigma_theta = gsw.sigma0(sal, temp)
        levels = np.linspace(sigma_theta.min(), sigma_theta.max(), 10)
        
        fig, ax = plt.subplots(figsize=(9, 7))
        cs = ax.contour(sal, temp, sigma_theta, colors='black', levels=levels, zorder=1)
        ax.clabel(cs, fontsize=10, inline=1, fmt='%.1f')

        sc = ax.scatter(self.salinidad_array, self.temperatura_array, c=self.Profundidad_array,
                        s=35, cmap='nipy_spectral')
        cb = plt.colorbar(sc)
        
        ax.set_xlabel('Salinidad (UPS)')
        ax.set_ylabel('Temperatura [$^\circ$C]')
        ax.set_title('Diagrama T-S', y=1.025)
        
        

        ax.xaxis.set_major_locator(MaxNLocator(nbins=6))
        ax.yaxis.set_major_locator(MaxNLocator(nbins=8))
        ax.tick_params(direction='out')
        ax.text(0.02, 0.98, r'$\sigma_\theta$', transform=ax.transAxes, fontsize=20, verticalalignment='top')
        cb.ax.tick_params(direction='out')
        cb.ax.invert_yaxis()
        cb.set_label('Profundidad (m)')

        plt.savefig('./graficas/DiagramaTS/DiagramaTS.png', dpi=300, bbox_inches='tight')
        plt.show()

    
