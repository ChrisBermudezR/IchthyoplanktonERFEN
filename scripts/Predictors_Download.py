# -*- coding: utf-8 -*-
"""
script title: Descarga de datos del Copernicus.
Autor: Christian Bermúdez-Rivas 
Objetivo: Descarga de archivos NetCDF del Copernicus: https://resources.marine.copernicus.eu/products
Lenguaje: Python
Fecha: Agosto 2024
Notas:
 No olvidar descargar e instalar el Miniconda: https://docs.conda.io/en/latest/miniconda.html in order to execute this code.
 Para cualquier error consultar este link: https://help.marine.copernicus.eu/en/articles/4947158-what-does-an-error-message-mean#h_9bf2ad6483

"""
from modules import PythonFunctions
import pandas
"""
Descarga de los datos de Panamá
"""
print("Write the password:")
password=(input(str("")))

#cmems_mod_glo_phy_my_0.083deg_P1D-m
#1993 04
#Blue ocean
PythonFunctions.dataDownload("uo", "1993-04-01T00:00:00", "1993-04-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_04_eastward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("vo", "1993-04-01T00:00:00", "1993-04-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_04_northward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("thetao", "1993-04-01T00:00:00", "1993-04-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_04_sea_surface_temperature.nc", password)
PythonFunctions.dataDownload("so", "1993-04-01T00:00:00", "1993-04-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_04_salinity.nc", password)
PythonFunctions.dataDownload("mlotst", "1993-04-01T00:00:00", "1993-04-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_04_mixed_layer.nc", password)
PythonFunctions.dataDownload("zos", "1993-04-01T00:00:00", "1993-04-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_04_sea_surface_height.nc", password)

#Green Ocean
#PythonFunctions.dataDownload("zooc", "1993-04-01T00:00:00", "1993-04-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_1993_04_mass_content_of_zooplankton_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("npp", "1993-04-01T00:00:00", "1993-04-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_1993_04_net_primary_productivity_of_biomass_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("zeu", "1993-04-01T00:00:00", "1993-04-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_1993_04_euphotic_zone_depth.nc", password)


#1993 10
#Blue ocean
PythonFunctions.dataDownload("uo", "1993-10-01T00:00:00", "1993-10-31T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_10_eastward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("vo", "1993-10-01T00:00:00", "1993-10-31T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_10_northward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("thetao", "1993-10-01T00:00:00", "1993-10-31T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_10_sea_surface_temperature.nc", password)
PythonFunctions.dataDownload("so", "1993-10-01T00:00:00", "1993-10-31T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_10_salinity.nc", password)
PythonFunctions.dataDownload("mlotst", "1993-10-01T00:00:00", "1993-10-31T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_10_mixed_layer.nc", password)
PythonFunctions.dataDownload("zos", "1993-10-01T00:00:00", "1993-10-31T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_1993_10_sea_surface_height.nc", password)

#Green Ocean
#PythonFunctions.dataDownload("zooc", "1993-10-01T00:00:00", "1993-10-31T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_1993_10_mass_content_of_zooplankton_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("npp", "1993-10-01T00:00:00", "1993-10-31T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_1993_10_net_primary_productivity_of_biomass_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("zeu", "1993-10-01T00:00:00", "1993-10-31T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_1993_10_euphotic_zone_depth.nc", password)


#2004 09
#Blue ocean
PythonFunctions.dataDownload("uo", "2004-09-01T00:00:00", "2004-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2004_09_eastward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("vo", "2004-09-01T00:00:00", "2004-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2004_09_northward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("thetao", "2004-09-01T00:00:00", "2004-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2004_09_sea_surface_temperature.nc", password)
PythonFunctions.dataDownload("so", "2004-09-01T00:00:00", "2004-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2004_09_salinity.nc", password)
PythonFunctions.dataDownload("mlotst", "2004-09-01T00:00:00", "2004-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2004_09_mixed_layer.nc", password)
PythonFunctions.dataDownload("zos", "2004-09-01T00:00:00", "2004-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2004_09_sea_surface_height.nc", password)

#Green Ocean
#PythonFunctions.dataDownload("zooc", "2004-09-01T00:00:00", "2004-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2004_09_mass_content_of_zooplankton_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("npp", "2004-09-01T00:00:00", "2004-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2004_09_net_primary_productivity_of_biomass_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("zeu", "2004-09-01T00:00:00", "2004-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2004_09_euphotic_zone_depth.nc", password)

#2005-09
#Blue ocean
PythonFunctions.dataDownload("uo", "2005-09-01T00:00:00", "2005-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2005_09_eastward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("vo", "2005-09-01T00:00:00", "2005-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2005_09_northward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("thetao", "2005-09-01T00:00:00", "2005-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2005_09_sea_surface_temperature.nc", password)
PythonFunctions.dataDownload("so", "2005-09-01T00:00:00", "2005-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2005_09_salinity.nc", password)
PythonFunctions.dataDownload("mlotst", "2005-09-01T00:00:00", "2005-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2005_09_mixed_layer.nc", password)
PythonFunctions.dataDownload("zos", "2005-09-01T00:00:00", "2005-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2005_09_sea_surface_height.nc", password)

#Green Ocean
#PythonFunctions.dataDownload("zooc", "2005-09-01T00:00:00", "2005-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2005_09_mass_content_of_zooplankton_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("npp", "2005-09-01T00:00:00", "2005-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2005_09_net_primary_productivity_of_biomass_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("zeu", "2005-09-01T00:00:00", "2005-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2005_09_euphotic_zone_depth.nc", password)


#2006-03
#Blue ocean
PythonFunctions.dataDownload("uo", "2006-03-01T00:00:00", "2006-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_03_eastward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("vo", "2006-03-01T00:00:00", "2006-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_03_northward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("thetao", "2006-03-01T00:00:00", "2006-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_03_sea_surface_temperature.nc", password)
PythonFunctions.dataDownload("so", "2006-03-01T00:00:00", "2006-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_03_salinity.nc", password)
PythonFunctions.dataDownload("mlotst", "2006-03-01T00:00:00", "2006-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_03_mixed_layer.nc", password)
PythonFunctions.dataDownload("zos", "2006-03-01T00:00:00", "2006-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_03_sea_surface_height.nc", password)

#Green Ocean
#PythonFunctions.dataDownload("zooc", "2006-03-01T00:00:00", "2006-03-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2006_03_mass_content_of_zooplankton_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("npp", "2006-03-01T00:00:00", "2006-03-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2006_03_net_primary_productivity_of_biomass_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("zeu", "2006-03-01T00:00:00", "2006-03-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2006_03_euphotic_zone_depth.nc", password)

#2006-09
#Blue ocean
PythonFunctions.dataDownload("uo", "2006-09-01T00:00:00", "2006-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_09_eastward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("vo", "2006-09-01T00:00:00", "2006-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_09_northward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("thetao", "2006-09-01T00:00:00", "2006-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_09_sea_surface_temperature.nc", password)
PythonFunctions.dataDownload("so", "2006-09-01T00:00:00", "2006-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_09_salinity.nc", password)
PythonFunctions.dataDownload("mlotst", "2006-09-01T00:00:00", "2006-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_09_mixed_layer.nc", password)
PythonFunctions.dataDownload("zos", "2006-09-01T00:00:00", "2006-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2006_09_sea_surface_height.nc", password)

#Green Ocean
#PythonFunctions.dataDownload("zooc", "2006-09-01T00:00:00", "2006-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2006_09_mass_content_of_zooplankton_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("npp", "2006-09-01T00:00:00", "2006-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2006_09_net_primary_productivity_of_biomass_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("zeu", "2006-09-01T00:00:00", "2006-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2006_09_euphotic_zone_depth.nc", password)

#2019-03
#Blue ocean
PythonFunctions.dataDownload("uo", "2019-03-01T00:00:00", "2019-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_03_eastward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("vo", "2019-03-01T00:00:00", "2019-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_03_northward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("thetao", "2019-03-01T00:00:00", "2019-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_03_sea_surface_temperature.nc", password)
PythonFunctions.dataDownload("so", "2019-03-01T00:00:00", "2019-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_03_salinity.nc", password)
PythonFunctions.dataDownload("mlotst", "2019-03-01T00:00:00", "2019-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_03_mixed_layer.nc", password)
PythonFunctions.dataDownload("zos", "2019-03-01T00:00:00", "2019-03-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_03_sea_surface_height.nc", password)

#Green Ocean
#PythonFunctions.dataDownload("zooc", "2019-03-01T00:00:00", "2019-03-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2019_03_mass_content_of_zooplankton_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("npp", "2019-03-01T00:00:00", "2019-03-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2019_03_net_primary_productivity_of_biomass_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("zeu", "2019-03-01T00:00:00", "2019-03-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2019_03_euphotic_zone_depth.nc", password)

#2019-09
#Blue ocean
PythonFunctions.dataDownload("uo", "2019-09-01T00:00:00", "2019-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_09_eastward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("vo", "2019-09-01T00:00:00", "2019-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_09_northward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("thetao", "2019-09-01T00:00:00", "2019-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_09_sea_surface_temperature.nc", password)
PythonFunctions.dataDownload("so", "2019-09-01T00:00:00", "2019-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_09_salinity.nc", password)
PythonFunctions.dataDownload("mlotst", "2019-09-01T00:00:00", "2019-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_09_mixed_layer.nc", password)
PythonFunctions.dataDownload("zos", "2019-09-01T00:00:00", "2019-09-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_2019_09_sea_surface_height.nc", password)

#Green Ocean
#PythonFunctions.dataDownload("zooc", "2019-09-01T00:00:00", "2019-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2019_09_mass_content_of_zooplankton_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("npp", "2019-09-01T00:00:00", "2019-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2019_09_net_primary_productivity_of_biomass_expressed_as_carbon_in_sea_water.nc", password)
#PythonFunctions.dataDownload("zeu", "2019-09-01T00:00:00", "2019-09-30T23:59:59","cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i", "data_2019_09_euphotic_zone_depth.nc", password)


#Total

PythonFunctions.dataDownload("uo", "1993-01-01T00:00:00", "2024-06-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_climatologia_eastward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("vo",  "1993-01-01T00:00:00", "2024-06-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_climatologia__northward_sea_water_velocity.nc", password)
PythonFunctions.dataDownload("thetao",  "1993-01-01T00:00:00", "2024-06-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_climatologia__sea_surface_temperature.nc", password)
PythonFunctions.dataDownload("so",  "1993-01-01T00:00:00", "2024-06-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_climatologia__salinity.nc", password)
PythonFunctions.dataDownload("mlotst",  "1993-01-01T00:00:00", "2024-06-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_climatologia__mixed_layer.nc", password)
PythonFunctions.dataDownload("zos",  "1993-01-01T00:00:00", "2024-06-30T23:59:59","cmems_mod_glo_phy_my_0.083deg_P1D-m", "data_climatologia__sea_surface_height.nc", password)
