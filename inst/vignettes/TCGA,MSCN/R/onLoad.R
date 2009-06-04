############################################################################
#
############################################################################
library("aroma.cn");
library("aroma.cn.eval");

setOption(aromaSettings, "output/checksum", TRUE);
setOption(aromaSettings, "output/path", FALSE);
setOption(aromaSettings, "output/ram", FALSE);



tagsList <- list("MSKCC", "Harvard", "Stanford", "Broad");
dataSet <- "TCGA,GBM,testSet,pairs"; 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Load raw CN data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (!exists("dsList", mode="list")) {
  dsList <- lapply(tagsList, FUN=function(tags) {
    AromaUnitTotalCnBinarySet$byName(dataSet, tags=tags, chipType="*");
  });

  # Set the names
  names(dsList) <- sapply(dsList, FUN=getFullName);

  # Keep only common samples (just in case)
  names <- Reduce(intersect, lapply(dsList, FUN=getNames));
  dsList <- lapply(dsList, FUN=extract, names);
}


tags <- Reduce(intersect, lapply(dsList, FUN=getTags));
sites <- sapply(dsList, FUN=function(ds) setdiff(getTags(ds), tags));
platforms <- sapply(dsList, FUN=getPlatform);
chipTypes <- sapply(dsList, FUN=getChipType);
names <- cbind(site=sites, platform=platforms, chipType=chipTypes);


############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################
