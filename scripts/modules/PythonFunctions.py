"""
Función para descargar los datos
"""

def dataDownload(variable, fechaInicio, fechaFinal, idDataset, nombreArchivoSalida, password):
            import copernicusmarine
            from pathlib import Path
            script_dir = Path(__file__).resolve().parent
            output_directory = script_dir.parent /'..' / 'data' / 'raw' / 'oceanography' / 'copernicus-data'

            copernicusmarine.subset(
                username= "cbermdezrivas",
                dataset_id=idDataset,
                variables=[variable],
                minimum_longitude=-85,
                maximum_longitude=-76,
                minimum_latitude=0,
                maximum_latitude=10,
                password=password,
                start_datetime=fechaInicio,
                end_datetime=fechaFinal,
                minimum_depth=0.5,
                maximum_depth=0.5,
                output_filename = nombreArchivoSalida,
                output_directory = str(output_directory) 
            )
