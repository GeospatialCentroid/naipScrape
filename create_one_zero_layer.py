import sys
import os
import re

# -------------------------
# 1. QGIS Setup
# -------------------------

# Add Processing plugin path
sys.path.append(r"C:\Program Files\QGIS 3.40.15\apps\qgis-ltr\python\plugins")

from qgis.core import *
from qgis.analysis import QgsNativeAlgorithms
import processing
from processing.core.Processing import Processing

# Initialize QGIS
QgsApplication.setPrefixPath(r"C:\Program Files\QGIS 3.40.15\apps\qgis-ltr", True)
qgs = QgsApplication([], False)
qgs.initQgis()

Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

# -------------------------
# 2. Paths
# -------------------------

input_root = r"C:\Users\C832742681\Documents\qgis_polygon_selection"
output_root = input_root  # keeping same output folder
os.makedirs(output_root, exist_ok=True)

# -------------------------
# 3. Process Each Folder
# -------------------------

for folder_name in os.listdir(input_root):
    folder_path = os.path.join(input_root, folder_name)
    if not os.path.isdir(folder_path):
        continue

    print(f"\nProcessing folder: {folder_name}")

    # -------------------------
    # Build NAIP name
    # -------------------------
    # Remove prefix and suffix
    name_clean = folder_name.replace("aoi_", "").replace("_complete", "")
    # Remove trailing _YYYY
    name_no_year = re.sub(r"_\d{4}$", "", name_clean)
    naip_name = f"oneKM_{name_no_year}"

    # -------------------------
    # Locate and handle files
    # -------------------------

    # Old file path
    old_naming = os.path.join(folder_path, "treesfinal.gpkg")
    # New file name based on grid
    new_name = f"{name_no_year}_treesfinal.gpkg"
    vector_path = os.path.join(folder_path, new_name)
    raster_path = os.path.join(folder_path, f"{naip_name}.tif")

    # 1️⃣ Rename old file if it exists
    if os.path.exists(old_naming):
        print(f"Renaming '{old_naming}' to '{vector_path}'")
        if not os.path.exists(vector_path):  # avoid overwriting
            os.rename(old_naming, vector_path)
        else:
            print(f"Warning: {vector_path} already exists, skipping rename")

    # 2️⃣ Check if vector file exists after renaming
    if not os.path.exists(vector_path):
        print(f"Vector layer '{vector_path}' not found, skipping.")
        continue

    # 3️⃣ Check if raster file exists
    if not os.path.exists(raster_path):
        print(f"NAIP raster not found: {raster_path}, skipping {naip_name}")
        continue

    # -------------------------
    # Load layers
    # -------------------------
    vector_layer = QgsVectorLayer(vector_path, "trees", "ogr")
    raster_layer = QgsRasterLayer(raster_path, "naip")

    if not vector_layer.isValid() or not raster_layer.isValid():
        print("Layer failed to load, skipping.")
        continue


    # -------------------------
    # Output path
    # -------------------------
    output_raster = os.path.join(folder_path, "trees_binary.tif")

    # -------------------------
    # Rasterize (GDAL)
    # -------------------------
    extent = raster_layer.extent()

    extent_string = f"{extent.xMinimum()},{extent.xMaximum()},{extent.yMinimum()},{extent.yMaximum()}"

    params = {
        'INPUT': vector_layer,
        'FIELD': None,
        'BURN': 1,
        'USE_Z': False,
        'UNITS': 1,  # Pixels
        'WIDTH': raster_layer.width(),
        'HEIGHT': raster_layer.height(),
        'EXTENT': extent_string,
        'NODATA': 255,
        'INIT': 0,
        'DATA_TYPE': 0,  # Byte
        'OUTPUT': output_raster
    }

    processing.run("gdal:rasterize", params)
    print(f"Raster created: {output_raster}")

# -------------------------
# Cleanup
# -------------------------
qgs.exitQgis()