############################################################################
#
############################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 1. Setting up fracB data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (!exists("dataList", mode="list")) {
  verbose && enter(verbose, "Loading PairedPSCNData list");

  pscnList <- loadPairedPSCNDataList(sampleName, dataSet=dataSet, tags=dataSetTags, chipType=chipType, fnt=fntFUN, verbose=-10);

  # Sanity check
  if (length(pscnList) == 0) {
    throw("No data sets found.");
  }

  # Add TumorBoost after "raw"
  key <- names(pscnList)[1];  # e.g. "CRMAv2";
  pscn <- pscnList[[key]];
  pscnTBN <- normalizeTumorBoost(pscn);
  pscn$betaT <- pscnTBN$betaTN;
  rm(pscnTBN);

  names <- names(pscnList);
  at <- which(key == names);
  key <- sprintf("%s,TumorBoost", key);
  names <- insert(names, ats=at+1, values=key);
  pscnList <- pscnList[names];
  names(pscnList) <- names;
  pscnList[[key]] <- pscn;
  rm(pscn, key, names);
    
  dataList <- pscnList;

  verbose && exit(verbose);
}


# Sanity check
stopifnot(all(sapply(dataList, FUN=inherits, "PairedPSCNData")));


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Document path
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
data <- dataList[[1]];
platform <- getPlatform(data);
chipType <- getChipType(data, fullname=FALSE);
rm(data);

docTags <- c(platform, chipType, docTags);
docTags <- paste(docTags, collapse=",");
docPath <- sprintf("doc,%s", docTags);
docPath <- Arguments$getWritablePath(docPath);
docName <- sprintf("BengtssonH_2009c-SupplementaryNotes,%s", docTags);

pdfName <- sprintf("%s.pdf", docName);
pdfPathname <- filePath(docPath, pdfName);

figPath <- file.path(docPath, "figures", "col");
setOption("devEval/args/path", figPath);

docTags <- unlist(strsplit(docTags, split=",", fixed=TRUE));



############################################################################
# HISTORY:
# 2012-03-15
# o Now adding TumorBoost data set.
# 2012-03-14
# o Now loading PairedPSCNData objects.
# 2012-02-24 [PN]
# o Now able to load several TCN data sets.
# 2012-02-19
# o Now the data set name pattern for loading fracB data is generated
#   from the 'postProcessing' strings.
# 2011-03-18
# o Prepared to utilize new devEval().
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
