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
import zipfile


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
    print(f"\nProcessing folder: {folder_name}")


    # If zip file, extract it
    if folder_name.endswith(".zip"):
        print("is a zip file, skipping")
        continue


    # -------------------------
    # Build NAIP name
    # -------------------------
    # Remove prefix and suffix
    name_clean = (folder_name.replace("aoi_", "").replace("_bundle","").replace("_complete", ""))
    # Remove trailing _YYYY
    name_no_year = re.sub(r"_\d{4}$", "", name_clean)
    naip_name = f"oneKM_{name_no_year}"

    # -------------------------
    # Locate and handle files
    # -------------------------

    # Paths
    correct_name = f"{name_no_year}_treesfinal.gpkg"

    print(correct_name)

    vector_path = os.path.join(folder_path, correct_name)
    old_naming = os.path.join(folder_path, "treesfinal.gpkg")
    raster_path = os.path.join(folder_path, f"{naip_name}.tif")

    # -------------------------
    # Handle old naming
    # -------------------------
    if os.path.exists(vector_path):
        print(f"Correctly named vector exists: {vector_path}")
        vector_layer = QgsVectorLayer(vector_path, "trees", "ogr")
        if not vector_layer.isValid():
            print("Vector layer failed to load, skipping.")
            continue
        burn_value = 1  # polygons will be burned as 1
    elif os.path.exists(old_naming):
        print(f"Renaming '{old_naming}' to '{vector_path}'")
        os.rename(old_naming, vector_path)
        vector_layer = QgsVectorLayer(vector_path, "trees", "ogr")
        if not vector_layer.isValid():
            print("Vector layer failed to load after rename, skipping.")
            continue
        burn_value = 1  # polygons will be burned as 1
    else:
        print(f"No vector file found in {folder_path}, will create empty raster")
        vector_layer = None
        burn_value = 0  # entire raster will be zeros

    # -------------------------
    # Check raster
    # -------------------------
    if not os.path.exists(raster_path):
        print(f"NAIP raster not found: {raster_path}, skipping {naip_name}")
        continue


    # -------------------------
    # Output path
    # -------------------------
    output_raster = os.path.join(folder_path, "trees_binary.tif")

    # -------------------------
    # Rasterize (GDAL)
    # -------------------------
    raster_layer = QgsRasterLayer(raster_path, "naip")

    if not raster_layer.isValid():
        print("Raster layer failed to load, skipping.")
        continue

    extent = raster_layer.extent()
    extent_string = f"{extent.xMinimum()},{extent.xMaximum()},{extent.yMinimum()},{extent.yMaximum()}"

    output_raster = os.path.join(folder_path, "trees_binary.tif")

    print("Raster extent:", raster_layer.extent())
    print("Vector extent:", vector_layer.extent())

    # -------------------------
    # 1. If vector exists → process it
    # -------------------------
    if vector_layer:

        # 1. Split multipolygons
        vector_layer = processing.run("native:multiparttosingleparts", {
            'INPUT': vector_layer,
            'OUTPUT': 'memory:'
        })['OUTPUT']

        # 2. Fix geometries
        vector_layer = processing.run("native:fixgeometries", {
            'INPUT': vector_layer,
            'OUTPUT': 'memory:'
        })['OUTPUT']

        # 3. Rebuild IDs in memory (no file needed)
        vector_layer = processing.run("native:refactorfields", {
            'INPUT': vector_layer,
            'FIELDS_MAPPING': [
                {
                    'name': 'id',
                    'type': 4,  # Integer
                    'length': 10,
                    'precision': 0,
                    'expression': '@row_number'
                }
            ],
            'OUTPUT': 'memory:'
        })['OUTPUT']

        print("Feature count after fix:", vector_layer.featureCount())

        # 4. Rasterize directly from memory layer
        extent = raster_layer.extent()
        extent_string = f"{extent.xMinimum()},{extent.xMaximum()},{extent.yMinimum()},{extent.yMaximum()}"

        params = {
            'INPUT': vector_layer,  # memory layer
            'FIELD': None,
            'BURN': 1,
            'USE_Z': False,
            'UNITS': 1,
            'WIDTH': raster_layer.rasterUnitsPerPixelX(),
            'HEIGHT': raster_layer.rasterUnitsPerPixelY(),
            'EXTENT': extent_string,
            'NODATA': 255,
            'INIT': 0,
            'DATA_TYPE': 0,  # Byte
            'ALL_TOUCHED': True,
            'OUTPUT': output_raster
        }

        processing.run("gdal:rasterize", params)

    # -------------------------
    # 2. No vector → create empty raster
    # -------------------------
    else:
        continue
    #     print("Creating empty raster (all zeros)")
    #
    #     extent = raster_layer.extent()
    #     extent_string = f"{extent.xMinimum()},{extent.xMaximum()},{extent.yMinimum()},{extent.yMaximum()}"
    #
    #     params_empty = {
    #         'EXTENT': extent_string,
    #         'WIDTH': raster_layer.width(),
    #         'HEIGHT': raster_layer.height(),
    #         'BURN': 0,
    #         'DATA_TYPE': 0,  # Byte
    #         'NODATA': 0,
    #         'OUTPUT': output_raster
    #     }
    #
    #     processing.run("gdal:createrasterlayerfromextent", params_empty)
    #
    # print(f"Raster created: {output_raster}")

import numpy as np
from osgeo import gdal

ds = gdal.Open(output_raster)
band = ds.GetRasterBand(1)
arr = band.ReadAsArray()

print("Unique values:", np.unique(arr))

# -------------------------
# Cleanup
# -------------------------
qgs.exitQgis()