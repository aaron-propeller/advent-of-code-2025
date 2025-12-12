#!/bin/bash

# Script to collect all Z3 output into a single file
output_file="all_z3_output.txt"

echo "Collecting Z3 output from all machine files..."
echo "===============================================" > "$output_file"

# Process all machine_*.smt2 files in numerical order
for file in $(ls machine_*.smt2 | sort -V); do
    if [[ -f "$file" ]]; then
        echo "Processing $file..."
        echo "" >> "$output_file"
        echo "=== $file ===" >> "$output_file"
        
        # Run Z3 and capture output
        z3 "$file" >> "$output_file" 2>&1
    fi
done

echo "All Z3 output saved to $output_file"
echo "Total files processed: $(ls machine_*.smt2 | wc -l)"