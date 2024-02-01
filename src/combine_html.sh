#!/bin/bash

# Create main directory to store subfolders
main_directory="/home/spinney/scratch/coventure/output/reports"
mkdir -p "$main_directory"

# Function to copy HTML files to subfolders
copy_html_to_subfolder() {
    local analysis_folder="$1"
    local subfolder_name="$2"
    
    # Create subfolder
    mkdir -p "$main_directory/$subfolder_name"
    
    # Find and copy HTML files to subfolder
    find "$analysis_folder" -name "*.html" -exec cp {} "$main_directory/$subfolder_name" \;
}

# Copy HTML files for each analysis type
copy_html_to_subfolder "/home/spinney/scratch/coventure/models/anxiety/html_output" "anxiety"
copy_html_to_subfolder "/home/spinney/scratch/coventure/models/depression/html_output" "depression"
copy_html_to_subfolder "/home/spinney/scratch/coventure/models/depado_dashed/html_output" "depado_dashed"
copy_html_to_subfolder "/home/spinney/scratch/coventure/models/depado_undashed/html_output" "depado_undashed"
copy_html_to_subfolder "/home/spinney/scratch/coventure/models/sdq/html_output" "sdq"

# Create a zip file containing the subfolders
zip -r analysis_html.zip "$main_directory"

echo "HTML files organized into subfolders and zipped successfully!"
