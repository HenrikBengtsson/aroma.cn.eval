library("R.menu");

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 1. Data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (interactive()) {
  tumorTypes <- c("GBM", "OV");
  tumorType <- textMenu(tumorTypes, title="Tumor type:", value=TRUE);
  rm(tumorTypes);
  dataSets <- c(
    "TCGA,%s,BeadStudio,BAF",
    "TCGA,%s,BeadStudio,XY",
    "TCGA,%s,BeadStudio,XY+CalMaTe",
    "TCGA,%s,Birdseed,ismpolish",
    "TCGA,%s,CRMAv2",
    "TCGA,%s,CRMAv2+CalMaTe"
  );
  dataSets <- sprintf(dataSets, tumorType);
  dataSet <- textMenu(dataSets, title="Data set:", value=TRUE);
  rm(dataSets);
} else {
  tumorType <- args$tumorType;
  if (is.null(tumorType)) {
    throw("Command line argument --tumorType not given.");
  }
  dataSet <- args$dataSet;
  if (is.null(dataSet)) {
    throw("Command line argument --dataSet not given.");
  }
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 2. Threshold for genotype confidence scores
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (interactive()) {
  confQuantiles <- c(1.00, 0.90);
  confQuantile <- textMenu(confQuantiles, title="Genotype confidence score threshold:", value=TRUE);
  rm(confQuantiles);
} else {
  confQuantile <- args$confQuantile;
  if (is.null(confQuantile)) {
    cat("Command line argument --confQuantile not given. Defaults to 1.0.\n");
    confQuantile <- 1.0;
  }
}
confQuantile <- Arguments$getDouble(confQuantile, range=c(0,1));
confQuantileTag <- sprintf("conf=%.0f", 100*confQuantile);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Infer chip type for data set directory
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
rootPath <- "totalAndFracBData"
path <- file.path(rootPath, dataSet);
path <- Arguments$getReadablePath(path);
paths <- list.files(path=path, full.names=TRUE);
paths <- paths[sapply(paths, FUN=isDirectory)];
stopifnot(length(paths) == 1);
chipType <- basename(paths);
rm(path, paths);

targetChipType <- chipType;


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Pattern of data sets to be analyzed
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
methodPattern <- "^(raw|TBN)";


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Change-points of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (tumorType == "OV") {
  regions <- c(
    "TCGA-23-1027:Chr2@108-140,cp=124+/-0.5,s=0/1",
    "TCGA-23-1027:Chr2@125.0-157.0,cp=141.0+/-0.5,s=1/3",
    "TCGA-23-1027:Chr10@80-109,cp=94+/-0.5,s=0/2", ## deletion
    "TCGA-23-1027:Chr10@106.5-113.5,cp=110+/-0.5,s=2/3", ## deletion -> CN LOH
    "TCGA-23-1027:Chr2@55-75.0,cp=65.0+/-0.5,s=0/1" ## "FALSE BREAKPOINT"
  );
} else if (tumorType == "GBM") {
  regions <- c(
    "TCGA-02-0001:Chr2@35-74,cp=57+/-1,s=1/2",
    "TCGA-02-0001:Chr2@75-110,cp=96+/-1,s=1/4",
    "TCGA-02-0001:Chr2@100-130,cp=110+/-1,s=4/0",
    "TCGA-02-0001:Chr13@0-70,cp=45+/-1,s=0/2"
  );
}
