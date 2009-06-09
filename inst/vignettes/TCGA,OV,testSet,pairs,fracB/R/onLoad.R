############################################################################
#
############################################################################
library("aroma.cn.eval");

setOption(aromaSettings, "output/checksum", TRUE);
setOption(aromaSettings, "output/path", FALSE);
setOption(aromaSettings, "output/ram", FALSE);


dataSet <- "TCGA,GBM,onePair";
chipType <- "GenomeWideSNP_6";
targetChipType <- chipType;



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Setting up data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (!exists("dsList", mode="list")) {
  pattern <- sprintf("^%s(|,TBN.*)$", dataSet)
  dsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=pattern);

  dsList <- lapply(dsList, FUN=function(ds) {
    setFullNamesTranslator(ds, function(names, ...) {
      pattern <- "^(TCGA-[0-9]{2}-[0-9]{4})-([0-9]{2}[A-Z])[-]*(.*)";
      gsub(pattern, "\\1,\\2,\\3", names);
    }); 
  })


  # Keep only tumors
  dsList <- lapply(dsList, function(ds) {
    types <- sapply(ds, function(df) getTags(df)[1]);
    keep <- grep("^01[A-Z]$", types);
    ds <- extract(ds, keep);
    ds;
  });

  # Set the names
  names <- names(dsList);
  names <- sapply(names, FUN=strsplit, split=",", fixed=TRUE);
  commonTags <- Reduce(intersect, names);
  names <- sapply(names, FUN=setdiff, commonTags);
  names <- sapply(names, FUN=paste, collapse=",");
  names[names == ""] <- "raw";
  names(dsList) <- names;
}

if (length(dsList) == 0) {
  rm(dsList);
  throw("No matching data sets found.");
}


if (!exists("gsN")) {
  gsN <- AromaUnitFracBCnBinarySet$byName(dataSet, tags="Birdseed", chipType="*", paths="totalAndFracBData"); 
  setFullNamesTranslator(gsN, function(names, ...) {
    pattern <- "^(TCGA-[0-9]{2}-[0-9]{4})-([0-9]{2}[A-Z])[-]*(.*)";
    gsub(pattern, "\\1,\\2,\\3", names);
  }); 
  print(gsN);
}


############################################################################
# HISTORY:
# 2009-06-08
# o Created from aroma.aroma.cn.eval/inst/vignettes/GSE13372,2CHCC1143,fracB.

############################################################################
