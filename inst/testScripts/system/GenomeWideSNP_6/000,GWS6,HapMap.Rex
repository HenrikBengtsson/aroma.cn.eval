if (interactive()) savehistory();
library("aroma.cn.eval");

log <- verbose <- Arguments$getVerbose(-8, timestamp=TRUE);
robust <- FALSE;

chipType <- "GenomeWideSNP_6";

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Identify which units in the full CDF are in the default CDF
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library("aroma.affymetrix");
cdf <- AffymetrixCdfFile$byChipType(chipType);
cdfF <- AffymetrixCdfFile$byChipType(chipType, tags="Full");
keep <- is.finite(indexOf(cdf, names=getUnitNames(cdfF)));
poolOfUnits <- whichVector(keep);
rm(keep, cdf, cdfF);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Load raw CN data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
tagsList <- list("CRMAv2", "GTC", "dChip,AVG,ref=median", "dChip,MBEI,ref=median");
dataSet <- "HapMap270,6.0,CEU,founders,three";

dsList <- lapply(tagsList, FUN=function(tags) {
  AromaUnitTotalCnBinarySet$byName(dataSet, tags=tags, chipType=chipType);
});
# Keep only common samples (just in case)
names <- Reduce(intersect, lapply(dsList, FUN=getNames));
dsList <- lapply(dsList, FUN=extract, names);
print(dsList);


tags <- Reduce(intersect, lapply(dsList, FUN=getTags));
methods <- sapply(dsList, FUN=function(ds) {
  paste(setdiff(getTags(ds), c(tags, "ref=median")), collapse=",");
})
platforms <- sapply(dsList, FUN=getPlatform);
chipTypes <- sapply(dsList, FUN=getChipType);
