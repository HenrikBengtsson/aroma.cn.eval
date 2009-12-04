# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rootPath <- "totalAndFracBData"

dataSets <- c(
  "TCGA,GBM,onePair",
  "TCGA,GBM,onePair,Broad,ismpolish",
  "TCGA,OV,testSet,pairs,Broad,ismpolish",
  "TCGA,OV,testSet,pairs,Broad,ACC,ra,-XY,BPN,-XY,AVG,FLN,-XY",
  "TCGA,OV,testSet,pairs,Stanford,BeadStudio,XY",
  "TCGA,OV,testSet,pairs,Stanford,BeadStudio,BAF",
  "TumorBoostPaper,Broad,CRMAv2"
);

flavors <- c("v1", "v2", "v3", "v4", "v5");

if (interactive()) {
  require("R.menu") || throw("Package not loaded: R.menu");
  dataSet <- textMenu(dataSets, title="Data set:", value=TRUE);
  flavor <- "v4";
#  flavor <- textMenu(flavors, title="TumorBoost normalization flavor:", value=TRUE);
  confQuantiles <- c(1.00, 0.95, 0.90);
  confQuantile <- textMenu(confQuantiles, title="Genotype confidence score threshold:", value=TRUE);
} else {
  dataSet <- dataSets[4];
  flavor <- "v4";
  confQuantile <- 1.0;
}

confQuantileTag <- sprintf("conf=%.0f", 100*confQuantile);


# Infer chip type for data set directory
path <- file.path(rootPath, dataSet);
path <- Arguments$getReadablePath(path);
paths <- list.files(path=path, full.names=TRUE);
paths <- paths[sapply(paths, FUN=isDirectory)];
stopifnot(length(paths) == 1);
chipType <- basename(paths);

targetChipType <- chipType;
methodPattern <- sprintf("^(raw|TBN,%s)", flavor);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Change-points of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
regions <- c(
             "TCGA-04-1335:Chr9@0.0-120.0,cp=55.0+/-18.0,s=0/3",
             "TCGA-04-1336:Chr2@120.0-230.0,cp=176.0+/-2.0,s=0/2",
             "TCGA-23-1027:Chr2@108-140,cp=124+/-0.5,s=0/1",
             "TCGA-23-1027:Chr2@125.0-157.0,cp=141.0+/-0.5,s=1/3",
             "TCGA-23-1027:Chr2@25.55-75.0,cp=65.0+/-0.5,s=0/1", ## "FALSE BREAKPOINT"
             "TCGA-23-1027:Chr1@165-245.0,cp=201.0+/-0.5,s=0/1",
             "TCGA-23-1027:Chr1@0-120,cp=60+/-0.5,s=0/1",   ## CN LOH within a deletion (+more)
             "TCGA-23-1027:Chr10@80-109,cp=94+/-0.5,s=0/2", ## deletion
##              "TCGA-23-1027:Chr10@106.4-113.6,cp=110+/-0.25,s=2/3", ## deletion -> CN LOH
             "TCGA-23-1027:Chr10@106.5-113.5,cp=110+/-0.5,s=2/3", ## deletion -> CN LOH
             "TCGA-23-1027:Chr2@55.0-75.0,cp=65.0+/-0.5,s=0/1", ## "FALSE BREAKPOINT #2"
             "TCGA-04-1335:Chr5@0.70-140.0,cp=105.0+/-18.0,s=0/1",
             "TCGA-12-0620:Chr17@0-50.0,cp=23.0+/-2.0,s=0/3"
             );

## regions <- regions[c(3:4, 8:9, 5)]
## regions <- regions[4];
## regions <- regions[12];

regions <- c(
             "TCGA-23-1027:Chr2@108-140,cp=124+/-0.5,s=0/1",
             "TCGA-23-1027:Chr2@125.0-157.0,cp=141.0+/-0.5,s=1/3",
             "TCGA-23-1027:Chr10@80-109,cp=94+/-0.5,s=0/2", ## deletion
             "TCGA-23-1027:Chr10@106.5-113.5,cp=110+/-0.5,s=2/3", ## deletion -> CN LOH
             "TCGA-23-1027:Chr2@55-75.0,cp=65.0+/-0.5,s=0/1" ## "FALSE BREAKPOINT"
             );

## regions <- regions[4];
