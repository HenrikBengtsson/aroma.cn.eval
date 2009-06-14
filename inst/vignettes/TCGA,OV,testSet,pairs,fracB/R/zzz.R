############################################################################
#
############################################################################
## for the fullNamesTranslator
fntFUN <- function(names, ...) {
  pattern <- "^(TCGA-[0-9]{2}-[0-9]{4})-([0-9]{2}[A-Z])[-]*(.*)";
  gsub(pattern, "\\1,\\2,\\3", names);
}; 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Setting up fracB data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (!exists("dsList", mode="list")) {
  pattern <- sprintf("^%s(|,TBN.*)$", dataSet)
  dsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=pattern, type="fracB", rootPath=rootPath);

  dsList <- lapply(dsList, setFullNamesTranslator, fntFUN);

  tds <- AromaUnitTotalCnBinarySet$byName(dataSet, chipType=chipType, paths=rootPath);
  setFullNamesTranslator(tds, fntFUN)
  
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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Setting up total CN data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (!exists("acs") || !inherits(acs, "AromaUnitTotalCnBinarySet")) {
  acs0 <- AromaUnitTotalCnBinarySet$byName(dataSet, chipType=chipType, paths=rootPath, verbose=verbose);
  setFullNamesTranslator(acs0, fntFUN);
  
  types <- sapply(acs0, function(df) getTags(df)[1]);
  isTumor <- grep("^01[A-Z]$", types);
  isNormal <- grep("^1[01][A-Z]$", types);
  
  acsN <- extract(acs0, isNormal);
  acsT <- extract(acs0, isTumor);
  rm(acs0);
  
  exportTotalCnRatioSet(acsT, acsN, verbose=verbose);
  
  acs <- AromaUnitTotalCnBinarySet$byName(dataSet, chipType=chipType, paths="rawCnData", verbose=verbose);
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Setting up normal genotype data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (!exists("gsN")) {
  gsN <- AromaUnitGenotypeCallSet$byName(dataSet, tags="Birdseed", chipType="*"); 
  setFullNamesTranslator(gsN, function(names, ...) {
    pattern <- "^(TCGA-[0-9]{2}-[0-9]{4})-([0-9]{2}[A-Z])[-]*(.*)";
    gsub(pattern, "\\1,\\2,\\3", names);
  }); 
  print(gsN);
}



############################################################################
# HISTORY:
# 2009-06-13
# o More cleanups.
# 2009-06-09
# o Genotype calls now assumed to be stored in AromaUnitGenotypeCallFile:s.
# 2009-06-08
# o Created from aroma.aroma.cn.eval/inst/vignettes/GSE13372,2CHCC1143,fracB.
############################################################################
