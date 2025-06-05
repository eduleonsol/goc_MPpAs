# Create the target directory if it doesn't exist
dir.create("data/raster/biophysical/mean_carbon_stock/", recursive = TRUE, showWarnings = FALSE)

# Define the Dropbox download URL (force direct download with dl=1)
url <- "https://www.dropbox.com/scl/fi/gxjyqn3l2dibu4t4o4u73/Mean-carbon_stock.tif?rlkey=n1x37ufuxe2f125mxk16chyil&dl=1"
destfile <- "data/raster/biophysical/mean_carbon_stock/Mean-carbon_stock.tif"

# Download the file
download.file(url, destfile, mode = "wb")



# Create the target directory if it doesn't exist
dir.create("data/shp/socioeconomic/human_wellbeing/", recursive = TRUE, showWarnings = FALSE)

# Define the Dropbox download URL (force direct download with dl=1)
url <- "https://www.dropbox.com/scl/fi/k3uswlo8hl6xc2ri90vsi/human_wellbeing.7z?rlkey=bm2555f9y3fz1nntfv1kjz9wg&dl=1"
destfile <- "data/shp/socioeconomic/human_wellbeing/human_wellbeing.7z"

# Download the file
download.file(url, destfile, mode = "wb")

# Extract the 7z file using the 'archive' package (recommended for cross-platform)
if (!requireNamespace("archive", quietly = TRUE)) {
  install.packages("archive")
}
archive::archive_extract(destfile, dir = "data/shp/socioeconomic/human_wellbeing/")
