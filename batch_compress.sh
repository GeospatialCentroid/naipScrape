#!/bin/bash

# 1. Explicitly define the target folder
# Using $HOME ensures it expands correctly to /home/dune (or your user home path)
TARGET_DIR="$HOME/trueNAS/work/naipScrape/data/naipExports"

# 2. Safely navigate to the folder before doing anything else
# The '|| exit 1' part stops the script if the folder doesn't exist, preventing accidents
cd "$TARGET_DIR" || { echo "Error: Could not find directory $TARGET_DIR"; exit 1; }

# Set how many files you want per compressed archive
BATCH_SIZE=100

# Gather all .tif files into an array
files=( *.tif )
total_files=${#files[@]}

# Check if there are actually files to process
if [ "$total_files" -eq 0 ] || [ "${files[0]}" = "*.tif" ]; then
    echo "No .tif files found in $TARGET_DIR."
    exit 1
fi

echo "Found $total_files .tif files in $TARGET_DIR."
echo "Grouping and compressing into batches of $BATCH_SIZE..."

# Counter for naming the compressed files
archive_num=1

# Loop through the array in steps of BATCH_SIZE
for (( i=0; i<$total_files; i+=BATCH_SIZE )); do
    # Slice the array to get the next batch
    batch=( "${files[@]:$i:$BATCH_SIZE}" )
    
    # Format the archive name (e.g., batch_001.tar.gz)
    archive_name=$(printf "batch_%03d.tar.gz" "$archive_num")
    
    echo "Compressing $archive_name with ${#batch[@]} files..."
    
    # Create the compressed tar file
    tar -czf "$archive_name" "${batch[@]}"
    
    ((archive_num++))
done

echo "Batch compression complete in $TARGET_DIR!"