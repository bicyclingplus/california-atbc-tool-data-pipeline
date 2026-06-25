setwd("C:/Users/Dillon/Box/_Projects/Caltrans_BC2")
Sys.setenv(TAR_CONFIG = "_targets.yaml")
LOCAL <- "C:/Users/Dillon/projects/california-atbc-tool-data-pipeline"
NUM_THREADS <- as.integer(Sys.getenv("TUNE_THREADS", "3"))
