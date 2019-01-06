############################################################################
#
############################################################################
loadAllDataSets <- function(dataSet, chipType="*", pattern=NULL, ..., rootPath="rawCnData") {
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
  dataSets <- list.files(path=rootPath, pattern=pattern);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Setup data sets
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  dsList <- list();
  for (kk in seq_along(dataSets)) {
    dataSet <- dataSets[kk];
    ds <- AromaUnitTotalCnBinarySet$byName(dataSet, chipType=chipType);
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
