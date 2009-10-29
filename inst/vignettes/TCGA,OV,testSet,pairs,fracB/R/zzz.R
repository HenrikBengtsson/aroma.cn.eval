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
if (!exists("fracBDsList", mode="list")) {
  verbose && enter(verbose, "Loading 'fracBDsList'");

  pattern <- sprintf("^%s(|,TBN.*)$", dataSet);
  fracBDsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=pattern, type="fracB", rootPath=rootPath);

  fracBDsList <- lapply(fracBDsList, setFullNamesTranslator, fntFUN);
  verbose && print(verbose, fracBDsList);
  
  verbose && enter(verbose, "Identifying normals");
  dsN0 <- fracBDsList[[dataSet]];
  types <- sapply(dsN0, function(df) getTags(df)[1]);
  keep <- grep("^1[01][A-Z]$", types);
  dsN <- extract(dsN0, keep);
  verbose && print(verbose, dsN);
  verbose && exit(verbose);

  ## Keep only tumors
  verbose && enter(verbose, "Keeping only tumors");
  fracBDsList <- lapply(fracBDsList, function(ds) {
    types <- sapply(ds, function(df) getTags(df)[1]);
    keep <- grep("^01[A-Z]$", types);
    ds <- extract(ds, keep);
    ds;
  });
  verbose && print(verbose, fracBDsList);
  verbose && exit(verbose);

  # Set names
  verbose && enter(verbose, "Updating data set names");
  names <- names(fracBDsList);
  names <- sapply(names, FUN=strsplit, split=",", fixed=TRUE);
  commonTags <- Reduce(intersect, names);
  names <- sapply(names, FUN=setdiff, commonTags);
  names <- sapply(names, FUN=paste, collapse=",");
  names[names == ""] <- "raw";
  names(fracBDsList) <- names;
  verbose && exit(verbose);

  verbose && exit(verbose);
}

# Sanity check
if (length(fracBDsList) == 0) {
  rm(fracBDsList);
  throw("No matching data sets found.");
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Setting up total CN data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (!exists("cnDsList", mode="list")) {
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
  }
  
  pattern <- sprintf("^%s(|.lnk)$", dataSet)
  cnDsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=pattern, type="total", rootPath="rawCnData", verbose=verbose);
}

# Sanity check
if (length(cnDsList) == 0) {
  rm(cnDsList);
  throw("No matching data sets found.");
}


# - - - - - - - - - - - - - - - - - - - 
# Setting up normal genotype data set
# - - - - - - - - - - - - - - - - - - - 
if (!exists("gcDsList", mode="list")) {
  pattern <- sprintf("^%s,", dataSet)
  gcDsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=pattern, type="genotypes", rootPath="callData", verbose=verbose);

  gcDsList <- lapply(gcDsList, setFullNamesTranslator, fntFUN);

  ## Keep only normals
  gcDsList <- lapply(gcDsList, function(ds) {
    types <- sapply(ds, function(df) getTags(df)[1]);
    keep <- grep("^1[01][A-Z]$", types);
    ds <- extract(ds, keep);
    ds;
  });

  # Set names
  names <- names(gcDsList);
  names <- sapply(names, FUN=strsplit, split=",", fixed=TRUE);
  commonTags <- Reduce(intersect, names);
  commonTags <- setdiff(commonTags, genTags);
  names <- lapply(names, FUN=setdiff, commonTags);
  names <- sapply(names, FUN=paste, collapse=",");
  names(gcDsList) <- names;

  # keep those who match genTags and update genTags accordingly
  m <- match(genTags, names);
  genTags <- genTags[which(!is.na(m))];
  gcDsList <- gcDsList[genTags];
  if (sum(is.na(m))) {
    warning("No matching genotype data set found for tag: ", paste(genTags[is.na(m)], collapse=","));
  }
}

# Sanity check
if (length(gcDsList) == 0) {
  rm(gcDsList);
  throw("No matching data sets found.");
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up normal genotype call confidence scores data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if (!exists("gcsDsList", mode="list")) {
  pattern <- sprintf("^%s,", dataSet)
  gcsDsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=pattern, type="confidenceScores", rootPath="callData", verbose=verbose);

  gcsDsList <- lapply(gcsDsList, setFullNamesTranslator, fntFUN);

  ## Keep only normals
  gcsDsList <- lapply(gcsDsList, function(ds) {
    types <- sapply(ds, function(df) getTags(df)[1]);
    keep <- grep("^1[01][A-Z]$", types);
    ds <- extract(ds, keep);
    ds;
  });

  # Set names
  names <- names(gcsDsList);
  names <- sapply(names, FUN=strsplit, split=",", fixed=TRUE);
  commonTags <- Reduce(intersect, names);
  commonTags <- setdiff(commonTags, genTags);
  names <- sapply(names, FUN=setdiff, commonTags);
  names <- sapply(names, FUN=paste, collapse=",");
  names(gcsDsList) <- names;

  # keep those who match genTags
  m <- match(genTags, names);
  gcsDsList <- gcsDsList[genTags[which(!is.na(m))]];
  if (sum(is.na(m))) {
    warning("No matching genotype confidence score data set found for tag: ", paste(genTags[is.na(m)], collapse=","));
  }
}
if (length(gcsDsList) == 0) {
    # No genotypes confidence scores available
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Coerce genotype calls and confidence scores into a list
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



############################################################################
# HISTORY:
# 2009-06-18
# o Now use 'loadAllDataSets' to retrieve genotype calls and confidence scores.
# 2009-06-18
# o Now use 'loadAllDataSets' to retrieve total copy number data.
# 2009-06-13
# o More cleanups.
# 2009-06-09
# o Genotype calls now assumed to be stored in AromaUnitGenotypeCallFile:s.
# 2009-06-08
# o Created from aroma.aroma.cn.eval/inst/vignettes/GSE13372,2CHCC1143,fracB.
############################################################################
