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

input_root = r"C:\Users\C832742681\Documents\binary_test"


# -------------------------
# 3. Process Each Folder
# -------------------------

for folder_name in os.listdir(input_root):
    folder_path = os.path.join(input_root, folder_name)
    print(f"\nProcessing folder: {folder_name}")


    # If zip file, extract it
    if folder_name.endswith(".zip"):
        zip_path = os.path.join(input_root, folder_name)

        # Create extraction folder (same name without .zip)
        extract_folder = os.path.join(input_root, folder_name.replace(".zip", ""))

        # Only extract if not already extracted
        if not os.path.exists(extract_folder):
            print(f"Extracting zip: {folder_name}")
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                zip_ref.extractall(extract_folder)
        else:
            print(f"Already extracted: {extract_folder}")

        folder_path = extract_folder
        folder_name = os.path.basename(extract_folder)

        if not os.path.isdir(folder_path):
            print(f"Skipping non-directory: {folder_path}")
            continue

        # Handle nested folder inside zip
        subfolders = [f.path for f in os.scandir(folder_path) if f.is_dir()]

        if len(subfolders) == 1:
            print(f"Using inner folder: {subfolders[0]}")
            folder_path = subfolders[0]
            folder_name = os.path.basename(folder_path)
        else:
            # Now treat extracted folder like normal folder
            folder_path = extract_folder
            folder_name = os.path.basename(extract_folder)


    # -------------------------
    # Build NAIP name
    # -------------------------
    # Clean folder name
    name_clean = (
        folder_name
        .replace("aoi_", "")
        .replace("_bundle", "")
        .replace("_complete", "")
        .replace("grid_", "")
    )

    # Extract year
    year_match = re.search(r"_(\d{4})$", name_clean)

    if year_match:
        year = year_match.group(1)
        grid_id = name_clean[:year_match.start()]
    else:
        raise ValueError(f"No year found in {folder_name}")

    files = os.listdir(folder_path)

    # Case 1: oneKM exists
    onekM_name = f"oneKM_{grid_id}.tif"

    if onekM_name in files:
        naip_name = onekM_name

    # Case 2: fallback to NAIP naming
    else:
        naip_name = f"naip_{year}_id_{grid_id}_wgs84.tif"


    print("Using:", naip_name)

    # -------------------------
    # Locate and normalize vector file
    # -------------------------

    correct_name = f"{grid_id}_treesfinal.gpkg"
    vector_path = None
    correct_path = os.path.join(folder_path, correct_name)

    # Scan folder for any matching file (case-insensitive)
    for f in os.listdir(folder_path):
        f_lower = f.lower()

        if f_lower == "treesfinal.gpkg":
            # Old naming → rename
            old_path = os.path.join(folder_path, f)

            if not os.path.exists(correct_path):
                print(f"Renaming '{f}' → '{correct_name}'")
                os.rename(old_path, correct_path)
                vector_path = correct_path
            else:
                print(f"Both old and new exist, using: {correct_path}")
                vector_path = correct_path

            break

        elif f_lower.endswith("_treesfinal.gpkg"):
            # Already correctly named (or close enough)
            vector_path = os.path.join(folder_path, f)

            break

    # Final check
    if vector_path:
        print(f"Using vector: {vector_path}")
        vector_layer = QgsVectorLayer(vector_path, "trees", "ogr")

        if not vector_layer.isValid():
            print("Vector layer failed to load, skipping.")
            vector_layer = None
        else:
            burn_value = 1
    else:
        print(f"No vector file found in {folder_path}, will create empty raster")
        vector_layer = None
        burn_value = 0

    # -------------------------
    # Check raster
    # -------------------------
    raster_path = os.path.join(folder_path, f"{naip_name}")

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

    print("Vector exists:", vector_layer is not None)
    print("Raster exists:", os.path.exists(raster_path))

    if vector_layer:
        params = {
            'INPUT': vector_layer,
            'FIELD': None,
            'BURN': burn_value,
            'USE_Z': False,
            'UNITS': 1,
            'WIDTH': raster_layer.rasterUnitsPerPixelX(),
            'HEIGHT': raster_layer.rasterUnitsPerPixelY(),
            'EXTENT': extent_string,
            'NODATA': 255,
            'INIT': 0,
            'DATA_TYPE': 0,  # Byte
            'OUTPUT': output_raster
        }
        processing.run("gdal:rasterize", params)
    else:
        print("Creating empty raster (all zeros)")

        params_empty = {
            'INPUT_A': raster_path,
            'BAND_A': 1,
            'FORMULA': '0',
            'NO_DATA': 255,
            'RTYPE': 0,  # Byte
            'OUTPUT': output_raster
        }

        processing.run("gdal:rastercalculator", params_empty)

    # -------------------------
    # Final Output Check
    # -------------------------
    if os.path.exists(output_raster):
        raster_check = QgsRasterLayer(output_raster, "output_check")

        if raster_check.isValid():
            print(f"✅ SUCCESS: Raster created and valid: {output_raster}")
        else:
            print(f"❌ ERROR: Raster file exists but failed to load: {output_raster}")
    else:
        print(f"❌ ERROR: Raster was NOT created: {output_raster}")


# -------------------------
# Cleanup
# -------------------------
qgs.exitQgis()