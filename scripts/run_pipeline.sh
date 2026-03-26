#!/bin/bash
# ============================================================
# LQAS Pipeline Local Test Script
# Runs the pipeline locally (for testing before GitHub)
# ============================================================

set -e

echo "🚀 Running LQAS Pipeline Locally..."

# Create directories if they don't exist
mkdir -p data/input data/output data/archive logs reports

# Check if Python is installed
if ! command -v python &> /dev/null; then
    echo "❌ Python is not installed"
    exit 1
fi

# Check if R is installed
if ! command -v R &> /dev/null; then
    echo "❌ R is not installed"
    exit 1
fi

# Install Python dependencies
echo "📦 Installing Python dependencies..."
pip install -r src/python/requirements.txt

# Fetch data from ONA
echo "📡 Fetching data from ONA..."
python src/python/fetch_ona_data.py --config config/config.yml

# Process with R
echo "🔄 Processing data with R..."
Rscript src/r/process_lqas.R --input-dir data/input --output-dir data/output

# Combine results
echo "🔗 Combining results..."
Rscript -e "
library(dplyr)
library(readr)
files <- list.files('data/output/', pattern = '\\.csv$', full.names = TRUE)
if (length(files) > 0) {
    combined <- purrr::map_dfr(files, read_csv, show_col_types = FALSE)
    write_csv(combined, 'data/final_lqas_data.csv')
    message('✅ Combined ', nrow(combined), ' rows')
} else {
    message('⚠️ No output files found')
}
"

echo "✅ Local pipeline complete!"
echo "📊 Results saved to data/final_lqas_data.csv"