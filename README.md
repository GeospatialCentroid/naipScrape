# naipScrape: High-Performance Midstream & Spatial Processing Pipeline

A high-performance R-based spatial pipeline developed to support agroforestry sampling and machine learning workflows. This repository automates the discovery, targeted chunk-downloading, processing, and visual optimization of high-resolution **National Agriculture Imagery Program (NAIP)** multi-band data, as well as the generation of segmentations for ground truth training sites.

By utilizing cloud-native GeoTIFF reads over GDAL virtual file systems (`/vsicurl/`) and parallel execution, this pipeline minimizes local disk usage and network overhead, running entirely database-free via decentralized, thread-safe tracking JSONs.

---

## 🛠️ Workflow 1: Bulk NAIP Imagery Downloader (`src/bulk_download.R`)

### 📌 Core Purpose
Designed for parallel, high-throughput acquisition of NAIP imagery across thousands of Area of Interest (AOI) grid cells. It streams only the required spatial crop of each tile directly from the **Microsoft Planetary Computer STAC API**, completely avoiding the need to download huge, multi-gigabyte raw scene files.

### 🔄 How the Pipeline Operates
1. **Target Loading & Batching**: Loads a target coordinate/grid table (e.g., Albers Equal Area 100km subgrids) and divides it into manageable batches (default size = 50) to maintain system headroom.
2. **Parallel Processing**: Spawns concurrent worker nodes using the `future` and `furrr` multisession engines (default = 10 workers).
3. **Decentralized State Check**: Each worker checks for a local `status.json` file inside the AOI's target folder. If all target years are already marked successful, it immediately skips the AOI, avoiding redundant API calls.
4. **Dynamic Year Fallback**: Queries the STAC API for available years. If a target year (e.g., `2012`, `2016`, `2020`) is missing, it dynamically falls back to adjacent years in a prioritized order (`target`, `target - 1`, `target - 2`, `target + 1`).
5. **GDAL `/vsicurl/` Targeted Crop**: Streams only the pixels falling inside the buffered AOI using GDAL's virtual file system. Crops the raw bands on-the-fly to a specified margin (e.g., 250 meters).
6. **Mosaicking & Resampling**: To eliminate standard coordinate origin and resolution discrepancies between different NAIP tiles, workers resample each raw cropped tile to a master 1m resolution template grid *before* mosaicking them using `terra::mosaic(fun = "mean")`.
7. **Buffer Masking**: Applies a circular, rounded-corner mask using the buffered AOI geometry to isolate the exact area of interest.
8. **QGIS Visualization Optimizations**:
   - **Band 4 Alpha Fix**: Explicitly overrides GDAL's default behavior (which treats the 4th Near-Infrared band as a transparency mask) by calling GDAL translate to set band 4's color interpretation to `undefined`. This ensures proper color bands and NIR visibility.
   - **Bbaked Header Stats**: Computes min-max statistics using GDAL info (`-stats`) and bakes them directly into the GeoTIFF headers. This enables QGIS to render the imagery **instantly** with perfect color stretching, without needing to scan the file.
9. **No-Lock Status Logging**: Writes a local, pretty-printed `status.json` inside the AOI's output folder containing exact fallback years, STAC collection datetimes, and unique STAC item IDs.

### 📂 Expected Results & Folder Structure
```text
/run/media/dan/T7/naip_bulk_export/
└── naip_batch_1/
    └── <aoi_id>/
        ├── naip_1.5km_<aoi_id>_<actual_year>.tif  <- High-res, 4-band (RGB+NIR), QGIS-optimized, rounded mask
        ├── aoi-<aoi_id>.gpkg                       <- Original geometry vector
        └── status.json                             <- Decentralized progress footprint
```

---

## 🌲 Workflow 2: Ground Truth Training Site & SNIC Generator (`src/produce_groundTruthSites.R`)

### 📌 Core Purpose
Designed to prepare highly flexible agroforestry training sites by generating both buffered and unbuffered NAIP imagery, as well as executing **Simple Non-Iterative Clustering (SNIC) superpixel segmentation** for downstream machine learning classification.

### 🔄 How the Pipeline Operates
1. **Site Input**: Loads an established candidate training site CSV or generates random spatial samples within a Major Land Resource Area (MLRA) polygon boundaries.
2. **Parallel Cluster Allocation**: Establishes a parallel cluster using the `foreach` and `doParallel` packages.
3. **STAC Discovery**: For each site, queries the STAC API and resolves fallback years identically to the Bulk Downloader.
4. **Buffered & Unbuffered Imagery Generation**:
   - Downloads, merges, and exports a **1.5km buffered crop** (buffered by 250m) for contextual evaluation.
   - Downloads, merges, and exports a tight **1km unbuffered crop** (no buffer) to serve as the direct modeling workspace.
5. **SNIC Superpixel Segmentation**: 
   Runs Simple Non-Iterative Clustering (SNIC) directly on the 1km NAIP raster using R's `snic` package. It generates cohesive, edge-aligned polygon clusters (superpixels) representing localized crop, tree, or grassland boundaries.
6. **Data Packaging**: Collects the vectors, raw segmentations, and NAIP products, and packages them into a clean, unified export folder ready for analysis or model input.

### 📂 Expected Results & Folder Structure
```text
data/exportData/
└── aoi_<aoi_id>_<actual_year>/
    ├── naip_1.5km_<aoi_id>_<actual_year>.tif     <- Contextual buffered image (QGIS-optimized)
    ├── naip_1km_<aoi_id>_<actual_year>.tif       <- Modeling core image (QGIS-optimized)
    ├── snic_clusters_<aoi_id>_<actual_year>.tif   <- SNIC Superpixel segmentations
    └── <associated geopackages & site vectors>
```

---

## 📊 Progress Reporting & Utilities (`function/getSTATUS.R`)

Since the pipeline operates in a decentralized, database-free manner, progress can be monitored and managed using these highly efficient, on-disk utilities:

### 1. `compileStatus(local_working_dir)`
Crawls your T7 drive or export folder, reads all the distributed `status.json` tracker files, and compiles them into a single, clean, flat R data frame.
* ** Tabular Flattening**: Automatically flattens nested JSON metadata.
* **Spreadsheet Ready**: Outputs columns such as `y1_actual_year`, `y1_capture_dates` (timestamps), `y1_item_ids`, and `y1_naip_states`.
* **Overlap Merging**: If an AOI is covered by multiple overlapping raw scenes, their timestamps and item IDs are concatenated using semicolon-separation (e.g., `2012-07-22T18:00:00Z; 2012-07-22T19:00:00Z`), making it fully compatible with CSV exports and Excel.

### 2. `clearStatus(local_working_dir)`
Deletes all distributed `status.json` files on disk. Useful for forcing the pipeline to perform a clean retry/re-discovery across your active directories.
