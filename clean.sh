#!/bin/bash

directory_path=$PWD
file_extensions=(".o" ".hi" ".exe") 

# Loop through each subdirectory in the specified directory recursively
/usr/bin/find "$directory_path" -type d | while read -r dir; do
    # Loop through each file in the current subdirectory with specified extensions
    for extension in "${file_extensions[@]}"; do
        /usr/bin/find "$dir" -type f -name "*${extension}" | while read -r file; do
            echo "Deleting file: $file"
            rm "$file"
        done
    done
done

echo "Deletion process completed."