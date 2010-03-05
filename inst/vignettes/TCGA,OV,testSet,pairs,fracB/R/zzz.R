############################################################################
#
############################################################################
## for the fullNamesTranslator
fntFUN <- function(names, ...) {
  pattern <- "^(TCGA-[0-9]{2}-[0-9]{4})-([0-9]{2}[A-Z])[-]*(.*)";
  gsub(pattern, "\\1,\\2,\\3", names);
};

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 1. Setting up fracB data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (!exists("fracBDsList", mode="list")) {
  verbose && enter(verbose, "Loading 'fracBDsList'");

  pattern <- sprintf("^%s(|,TBN.*)$", dataSet);
  fracBDsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=pattern, type="fracB", rootPath=rootPath);
  rm(pattern);

  fracBDsList <- lapply(fracBDsList, setFullNamesTranslator, fntFUN);
  verbose && print(verbose, fracBDsList);
  
  verbose && enter(verbose, "Identifying normals");
  dsN0 <- fracBDsList[[dataSet]];
  types <- sapply(dsN0, function(df) getTags(df)[1]);
  keep <- grep("^1[01][A-Z]$", types);
  dsN <- extract(dsN0, keep);
  verbose && print(verbose, dsN);
  rm(dsN0, types);
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
  names[(names == "") | sapply(names, length)==0] <- "raw";
  names(fracBDsList) <- names;
  rm(names, ns, first);
  verbose && exit(verbose);

  verbose && exit(verbose);
}

# Sanity check
if (length(fracBDsList) == 0) {
  throw("No matching data sets found.");
}

# Filter out the data sets matching the method pattern
if (!is.null(methodPattern)) {
  verbose && enter(verbose, "Filtering out data sets matching the method pattern");
  verbose && cat(verbose, "Method pattern: ", methodPattern);
  verbose && cat(verbose, "Before:");
  verbose && print(verbose, fracBDsList);

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
  rm(dsList);
  verbose && cat(verbose, "After:");
  verbose && print(verbose, fracBDsList);
  verbose && exit(verbose);
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 2. Setting up total CN data set
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
    rm(acs0, types, isTumor, isNormal);
    
    exportTotalCnRatioSet(acsT, acsN, verbose=verbose);
    rm(acsN, acsT);
  }
  
  pattern <- sprintf("^%s(|.lnk)$", dataSet)
  cnDsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=pattern, type="total", rootPath="rawCnData", verbose=verbose);
  rm(pattern);
}

# Sanity check
if (length(cnDsList) == 0) {
  throw("No matching data sets found.");
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Setting up normal genotype data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (!exists("gcDsList", mode="list")) {
  pattern <- sprintf("^%s,", dataSet)
  gcDsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=pattern, type="genotypes", rootPath="callData", verbose=verbose);
  rm(pattern);

  gcDsList <- lapply(gcDsList, setFullNamesTranslator, fntFUN);

  ## Keep only normals
  gcDsList <- lapply(gcDsList, function(ds) {
    types <- sapply(ds, function(df) getTags(df)[1]);
    keep <- grep("^1[01][A-Z]$", types);
    ds <- extract(ds, keep);
    ds;
  });

  # Set names by caller algorithm
  names <- names(gcDsList);
  names <- gsub(".*,", "", names);
  names(gcDsList) <- names;
  rm(names);

  # keep those who match genTags and update genTags accordingly
  m <- match(genTags, names(gcDsList));
  if (sum(is.na(m))) {
    warning("No matching genotype data set found for tag: ", paste(genTags[is.na(m)], collapse=","));
  }
  genTags <- genTags[!is.na(m)];
  gcDsList <- gcDsList[genTags];
  rm(m);
}

# Sanity check
if (length(gcDsList) == 0) {
  throw("No matching data sets found.");
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Setting up normal genotype call confidence scores data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (confQuantile < 1) {
  if (!exists("gcsDsList", mode="list")) {
    pattern <- sprintf("^%s,", dataSet)
    gcsDsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=pattern, type="confidenceScores", rootPath="callData", verbose=verbose);
    rm(pattern);
  
    gcsDsList <- lapply(gcsDsList, setFullNamesTranslator, fntFUN);
  
    ## Keep only normals
    gcsDsList <- lapply(gcsDsList, function(ds) {
      types <- sapply(ds, function(df) getTags(df)[1]);
      keep <- grep("^1[01][A-Z]$", types);
      ds <- extract(ds, keep);
      ds;
    });
  
    # Set names by caller algorithm
    names <- names(gcsDsList);
    names <- gsub(".*,", "", names);
    names(gcsDsList) <- names;
    rm(names);
  
    # keep those who match genTags
    m <- match(genTags, names(gcsDsList));
    gcsDsList <- gcsDsList[genTags[!is.na(m)]];
    if (sum(is.na(m))) {
      warning("No matching genotype confidence score data set found for tag: ", paste(genTags[is.na(m)], collapse=","));
    }
    rm(m);
  }
  
  # Drop non existing confidence scores
  gcsDsList <- gcsDsList[sapply(gcsDsList, FUN=length) > 0];
  
  # Sanity check
  if (length(gcsDsList) == 0) {
    throw("Stratification on genotype confidence scores is requests but there are no confidence score files available: ", confQuantile);
  }

  # Drop incomplete cases
  if (length(gcsDsList) > 0) {
    keep <- intersect(names(gcDsList), names(gcsDsList));
    gcDsList <- gcDsList[keep];
    gcsDsList <- gcsDsList[keep];
    rm(keep);

    # Sanity check
    if (length(gcDsList) == 0) {
      throw("After matching genotypes with available confidence scores, there is no data.");
    }
  }
} else {
  # Empty dummy
  gcsDsList <- list();
} # if (confQuantile < 1)


genTags <- names(gcDsList);
names <- names(fracBDsList);
names <- strsplit(names, split=",", fixed=TRUE);
keep <- sapply(names, FUN=function(tags) {
  is.element("raw", tags) || any(is.element(genTags, tags))
});
fracBDsList <- fracBDsList[keep];
rm(keep);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Sanity checks
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Assert that only one sample is studied
regionsList <- lapply(regions, FUN=parseRegion);
sampleNames <- sapply(regionsList, FUN=function(x) x$name);
sampleName <- sampleNames[1];
stopifnot(all(sampleNames == sampleName));
rm(regions, sampleNames);



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
rm(df, tags);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Document path
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ds <- cnDsList[[1]];
platform <- getPlatform(ds);
chipType <- getChipType(ds);
chipTypeEsc <- gsub("_", "\\_", chipType, fixed=TRUE);
rm(ds);

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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# CLEAN UP
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rm(fntFUN);


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
