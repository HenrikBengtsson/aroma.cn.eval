############################################################################
#
############################################################################
loadAllDataSets <- function(dataSet, chipType="*", pattern=NULL, ..., rootPath="totalAndFracBData") {
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


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Identify all data sets
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("Scanning directory for data sets:\n");
  cat("Path: ", rootPath, "\n");
  cat("Pattern: ", pattern, "\n");
  dataSets <- list.files(path=rootPath, pattern=pattern);
  print(dataSets);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Setup data sets
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  dsList <- list();
  for (kk in seq(along=dataSets)) {
    dataSet <- dataSets[kk];
    ds <- AromaUnitFracBCnBinarySet$byName(dataSet, chipType=chipType, paths=rootPath);
    dsList[[kk]] <- ds;
  }

  # Set the names
  names <- sapply(dsList, FUN=getFullName);
  names(dsList) <- names;

  dsList;
} # loadAllDataSets()



############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################
