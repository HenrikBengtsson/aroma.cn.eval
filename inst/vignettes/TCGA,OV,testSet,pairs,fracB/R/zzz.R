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
  while(TRUE) {
    ns <- sapply(names, FUN=length);
    if (any(ns == 0))
      break;
    first <- unname(sapply(names, FUN=function(x) x[1]));
    if (length(unique(first)) > 1)
      break;
    names <- lapply(names, FUN=function(x) x[-1]);
  }
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

ds <- cnDsList[[1]];
platform <- getPlatform(ds);
chipType <- getChipType(ds);
chipTypeEsc <- gsub("_", "\\_", chipType, fixed=TRUE);

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
  while(length(names[[1]]) > 0 && length(names[[2]]) > 0) {
    if (names[[1]][1] != names[[2]][1])
      break;
    names[[1]] <- names[[1]][-1];
    names[[2]] <- names[[2]][-1];
  }
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

  if (length(gcsDsList) > 0) {
    # Set names
    names <- names(gcsDsList);
    names <- sapply(names, FUN=strsplit, split=",", fixed=TRUE);
    while(length(names[[1]]) > 0 && length(names[[2]]) > 0) {
      if (names[[1]][1] != names[[2]][1])
        break;
      names[[1]] <- names[[1]][-1];
      names[[2]] <- names[[2]][-1];
    }
    names <- sapply(names, FUN=paste, collapse=",");
    names(gcsDsList) <- names;
  }

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Sanity checks
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Assert that only one sample is studied
regionsList <- lapply(regions, FUN=parseRegion);
sampleNames <- sapply(regionsList, FUN=function(x) x$name);
sampleName <- sampleNames[1];
stopifnot(all(sampleNames == sampleName));


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Filter out the data sets matching the method pattern
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (!is.null(methodPattern)) {
  verbose && cat(verbose, "Method pattern: ", methodPattern);

  dsList <- fracBDsList;
  verbose && print(verbose, names(dsList));
  keep <- (regexpr(methodPattern, names(dsList)) != -1);
  verbose && print(verbose, keep);
  dsList <- dsList[keep];
  # Sanity check
  if (length(dsList) == 0) {
    throw("No data sets remaining after name pattern filtering.");
  }
  fracBDsList <- dsList;
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Drop all samples but the one of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cnDsList <- lapply(cnDsList, FUN=function(ds) {
  ds <- extract(ds, indexOf(ds, sampleName));
});
print(cnDsList);
fracBDsList <- lapply(fracBDsList, FUN=function(ds) {
  ds <- extract(ds, indexOf(ds, sampleName));
});
print(fracBDsList);

# Infer the tumor and normal type
df <- getFile(cnDsList[[1]], 1);
tags <- getTags(df);
tags <- grep("^[0-9]{2}[A-Z]$", tags, value=TRUE);
tags <- sort(tags);
tumorType <- tags[1];
normalType <- tags[2];



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Document path
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
docTags <- c(platform, chipType, docTags);
if (confQuantile < 1) {
  docTags <- c(docTags, confQuantileTag);
}
docTags <- paste(docTags, collapse=",");
docPath <- sprintf("doc,%s", docTags);
docPath <- Arguments$getWritablePath(docPath);
docName <- sprintf("BengtssonH_2009c-SupplementaryNotes,%s", docTags);

pdfName <- sprintf("%s.pdf", docName);
pdfPathname <- filePath(docPath, pdfName);

figPath <- file.path(docPath, "figures", "col");
figForce <- 3;
figDev <- function(..., force=(figForce > 0)) { epsDev(..., path=figPath, force=force) }
figDev <- function(..., force=(figForce > 0)) { pngDev(..., device=png, path=figPath, force=force) }

docTags <- strsplit(docTags, split=",", fixed=TRUE)[[1]];

############################################################################
# HISTORY:
# 2009-12-06
# o Updated to also handle BeadStudio genotype calls.
# 2009-12-04
# o Now only one sample per data set is kept.
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
