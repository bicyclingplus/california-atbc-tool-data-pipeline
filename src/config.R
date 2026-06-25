# Set working directory to Box project root
setwd("C:/Users/Dillon/Box/_Projects/Caltrans_BC2")

# Source all functions from the local git repo (NOT from Box).
# The pipeline runs with the working directory set to the Box project root so
# that data paths ("data_raw/...") and the _targets store resolve to Box.
tar_source("C:/Users/Dillon/projects/california-atbc-tool-data-pipeline/src/functions")