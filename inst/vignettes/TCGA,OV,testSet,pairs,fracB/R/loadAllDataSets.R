############################################################################
#
############################################################################
loadAllDataSets <- function(dataSet, chipType="*", pattern=NULL, ..., rootPath="totalAndFracBData", type=c("fracB", "total", "genotypes", "confidenceScores"), verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'dataSet':
  dataSet <- Arguments$getCharacter(dataSet);

  # Argument 'chipType':
  chipType <- Arguments$getCharacter(chipType);

  # Argument 'pattern':
  if (is.null(pattern)) {
    pattern <- sprintf("^%s,.*", dataSet);
  }
  pattern <- Arguments$getRegularExpression(pattern);

  # Argument 'rootPath':
  rootPath <- Arguments$getReadablePath(rootPath, mustExist=TRUE);

  # Argument 'type':
  type <- match.arg(type, c("fracB", "total", "genotypes", "confidenceScores"))

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }


  verbose && enter(verbose, "loadAllDataSets()");


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Identify all data sets
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  verbose && enter(verbose, "Scanning directory for data sets");
  verbose && cat(verbose, "Path: ", rootPath);
  verbose && cat(verbose, "Pattern: ", pattern);
  # Search for directories and links
  paths <- list.files(path=rootPath, pattern=pattern, full.names=TRUE);
  verbose && cat(verbose, "Located paths:");
  verbose && print(verbose, paths);
  paths <- sapply(paths, FUN=Arguments$getReadablePath);
  paths <- paths[sapply(paths, FUN=isDirectory)];
  dataSets <- basename(paths);
  verbose && cat(verbose, "Located data sets:");
  verbose && print(verbose, dataSets);
  verbose && exit(verbose);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Setup data sets
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  dsList <- list();
  verbose && enter(verbose, "Loading data sets");
  for (kk in seq(along=dataSets)) {
    dataSet <- dataSets[kk];
    verbose && enter(verbose, sprintf("Data set #%d ('%s') of %d",
                                          kk, dataSet, length(dataSets)));
    verbose && cat(verbose, "Type: ", type);

    if (type=="total") {
      ds <- AromaUnitTotalCnBinarySet$byName(dataSet, chipType=chipType, paths=rootPath);
    } else if (type=="fracB") {
      ds <- AromaUnitFracBCnBinarySet$byName(dataSet, chipType=chipType, paths=rootPath);
    } else if (type=="genotypes") {
      ds <- AromaUnitGenotypeCallSet$byName(dataSet, chipType=chipType); 
    } else if (type=="confidenceScores") {
      ds <- AromaUnitSignalBinarySet$byName(dataSet, chipType=chipType, pattern="confidenceScores", paths=rootPath);
    }
    if (length(ds)) {
      dsList[[kk]] <- ds;
      verbose && print(verbose, ds);
    } else {
      verbose && cat(verbose, "No such data set found.");
    }
    verbose && exit(verbose);
  } # for (kk ...)
  verbose && exit(verbose);

  # Drop empty data sets
  dsList <- dsList[sapply(dsList, FUN=length) > 0];

  # Set the names
  names <- sapply(dsList, FUN=getFullName);
  names(dsList) <- names;

  verbose && cat(verbose, "Loaded data sets:");
  verbose && print(verbose, dsList);

  # Sanity check
  ns <- sapply(dsList, FUN=function(ds) nbrOfUnits(getFile(ds,1)));
  nbrOfUnits <- ns[1];
  if (!all(ns == nbrOfUnits)) {
    verbose && print(verbose, dsList);
    verbose && print(verbose, ns);
#    throw("INTERNAL ERROR: The loaded data sets does not have the same number of units.");
  }

  verbose && exit(verbose);

  dsList;
} # loadAllDataSets()



############################################################################
# HISTORY:
# 2011-03-18
# o Added sanity checks for the result of loadAllDataSets().
# 2009-02-23
# o Created.
############################################################################
