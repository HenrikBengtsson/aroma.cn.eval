library("R.menu");

dataSet <- "TCGA,OV";
dataSetTags <- "CRMAv2";
#dataSetTags <- "BeadStudio,XY";
dataSetF <- paste(c(dataSet, dataSetTags), collapse=",");

rootPath <- "totalAndFracBData/"
rootPath <- Arguments$getReadablePath(rootPath);

# Assert that data set exists
path <- file.path(rootPath, dataSetF);
path <- Arguments$getReadablePath(path);

## # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## # 2. Post-processing methods, e.g. TumorBoost & CalMaTe
## # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## knownPostProcessing <- c("TBN"="TumorBoost", "CalMaTe"="CalMaTe");
## postTags <- names(knownPostProcessing);
## if (interactive()) {
##   postTags <- selectMenu(postTags, title="Postprocessing methods:", selected=TRUE);
## }
## postProcessing <- knownPostProcessing[postTags];


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Infer chip type for data set directory
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
path <- file.path(rootPath, dataSetF);
path <- Arguments$getReadablePath(path);
paths <- list.files(path=path, full.names=TRUE);
paths <- paths[sapply(paths, FUN=isDirectory)];
stopifnot(length(paths) == 1);
chipType <- basename(paths);
rm(path, paths);



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Change-points of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
regions <- c(
  "TCGA-23-1027:Chr2@108-140,cp=124+/-0.5,s=0/1",
  "TCGA-23-1027:Chr2@125.0-157.0,cp=141.0+/-0.5,s=1/3",
  "TCGA-23-1027:Chr10@80-109,cp=94+/-0.5,s=0/2", ## deletion
  "TCGA-23-1027:Chr10@106.5-113.5,cp=110+/-0.5,s=2/3", ## deletion -> CN LOH
  "TCGA-23-1027:Chr2@55-75.0,cp=65.0+/-0.5,s=0/1" ## "FALSE BREAKPOINT"
);

if (debug) regions <- regions[4];

regionsList <- lapply(regions, FUN=parseRegion);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Sample of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sampleNames <- sapply(regionsList, FUN=function(region) region$name);
uSampleNames <- sort(unique(sampleNames));
sampleName <- sampleNames[1];
if (length(uSampleNames) > 1 && interactive()) {
  sampleName <- selectMenu(uSampleNames, title="Sample to investigate:", selected=TRUE);
}

keep <- (sampleNames == sampleName);
rm(sampleNames);
if (any(!keep)) {
  regions <- regions[keep];
  regionsList <- regionsList[keep];
}
rm(keep);
