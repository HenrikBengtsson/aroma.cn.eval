loadAllDataSetsPSCN <- function(dataSet, tags=NULL, chipType="*", pattern=NULL, ..., rootPath="totalAndFracBData", cleanNames=TRUE, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'dataSet':
  dataSet <- Arguments$getCharacter(dataSet);

  # Argument 'tags':
  tags <- Arguments$getTags(tags);

  dataSet0 <- dataSet;
  dataSet <- paste(c(dataSet, tags), collapse=",");

  # Argument 'chipType':
  chipType <- Arguments$getCharacter(chipType);

  # Argument 'pattern':
  if (is.null(pattern)) {
    pattern <- sprintf("^%s(|,.*)$*", dataSet);
  }
  pattern <- Arguments$getRegularExpression(pattern);

  # Argument 'rootPath':
  rootPath <- Arguments$getReadablePath(rootPath, mustExist=TRUE);

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }


  verbose && enter(verbose, "loadAllDataSetsPSCN()");


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
  for (kk in seq_along(dataSets)) {
    dataSet <- dataSets[kk];
    verbose && enter(verbose, sprintf("Data set #%d ('%s') of %d",
                                          kk, dataSet, length(dataSets)));

    dsT <- AromaUnitTotalCnBinarySet$byName(dataSet, chipType=chipType, paths=rootPath);
    dsB <- AromaUnitFracBCnBinarySet$byName(dataSet, chipType=chipType, paths=rootPath);
    dsListKK <- list(total=dsT, fracB=dsB);
    # Drop empty data sets
    dsListKK <- dsListKK[sapply(dsListKK, FUN=length) > 0];

    if (length(dsListKK) == 2) {
      verbose && print(verbose, dsListKK);

      # Sanity checks
      ns <- sapply(dsListKK, FUN=function(ds) nbrOfUnits(getFile(ds,1)));
      if (!all(ns == ns[1])) {
        verbose && print(verbose, dsList);
        verbose && print(verbose, ns);
        throw("INTERNAL ERROR: The loaded data sets does not have the same number of units.");
      }

      dsList[[kk]] <- dsListKK;
    } else if (length(dsListKK) == 0) {
      verbose && cat(verbose, "No such data set found.");
    } else {
      verbose && printf(verbose, "Found only the '%s' data set. Ignoring data set: %s\n", hpaste(names(dsListKK)), dataSet);
    }
    verbose && exit(verbose);
  } # for (kk ...)
  verbose && exit(verbose);

  # Set the names
  names <- sapply(dsList, FUN=function(dsListKK) {
    ds <- dsListKK[[1]];
    getFullName(ds);
  });
  names(dsList) <- names;

  if (cleanNames) {
    names <- names(dsList);
    names <- gsub(sprintf("^%s", dataSet0), "", names);
    names <- gsub("^,", "", names);
    names[nchar(names) == 0] <- "raw";
    names(dsList) <- names;
  }

  verbose && cat(verbose, "Loaded data sets:");
  verbose && print(verbose, dsList);

  verbose && exit(verbose);

  dsList;
} # loadAllDataSetsPSCN()


loadPairedPSCNDataList <- function(sampleName, ..., fnt=NULL, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'sampleName':
  sampleName <- Arguments$getCharacter(sampleName);

  # Argument 'fnt':
  if (!is.null(fnt)) {
    stopifnot(is.function(fnt));
  }

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  verbose && enter(verbose, "Loading all available PSCNData");
  dsList <- loadAllDataSetsPSCN(..., verbose=less(verbose, 5));

  verbose && enter(verbose, "Extracting sample of interest");
  verbose && cat(verbose, "Sample name: ", sampleName);
  dsList <- lapply(dsList, FUN=function(dsListKK) {
    dsListKK <- lapply(dsListKK, FUN=function(ds) {
      # Apply fullnames translator?
      if (!is.null(fnt)) {
        ds <- setFullNamesTranslator(ds, fnt);
      }

      # Keep only sample of interest
      ds <- extract(ds, indexOf(ds, names=sampleName));
      # Sort lexicographically
      ds <- sortBy(ds);
      ds;
    });
    # Drop empty data sets
    dsListKK <- dsListKK[sapply(dsListKK, FUN=length) > 0];
    dsListKK;
  });
  # Drop empty data sets
  dsList <- dsList[sapply(dsList, FUN=length) > 0];
  verbose && exit(verbose);

  verbose && enter(verbose, "Identify the tumor and normal sample");
  dsList <- lapply(dsList, FUN=function(dsListKK) {
    dsListKK <- lapply(dsListKK, FUN=function(ds) {
      # Assumption check
      stopifnot(length(ds) == 2);
      idxs <- c(T="T", N="N");
      idxs <- sapply(names(idxs), FUN=function(tag) {
        which(sapply(ds, hasTag, tag));
      });
      # Sanity check
      stopifnot(length(idxs) == 2 && all(is.finite(idxs)));
      # Order (T,N)
      ds <- extract(ds, idxs);
      ds;
    });
    dsListKK;
  });
  verbose && exit(verbose);


  verbose && enter(verbose, "Extracting PairedPSCNData objects");
  dataList <- list();
  for (kk in seq_along(dsList)) {
    key <- names(dsList)[kk];
    dsTCN <- dsList[[key]]$total;
    verbose && enter(verbose, sprintf("Data set #%d ('%s') of %d", kk, key, length(dsList)));

    # Extract (chromosome, x) positions
    ugp <- getAromaUgpFile(dsTCN);
    gp <- readDataFrame(ugp);

    # Extract tumor-normal PSCN data
    data <- extractPSCNArray(dsTCN, verbose=less(verbose, 5));
    dimnames(data)[[3]] <- c("T", "N");

    # Setup PairedPSCNData
    data <- PairedPSCNData(
              chromosome=gp$chromosome, x=gp$position,
              CT=data[,"total","T"], CN=data[,"total","N"],
              betaT=data[,"fracB","T"], betaN=data[,"fracB","N"],
              name=sampleName
            );

    data <- setPlatform(data, getPlatform(ugp));
    data <- setChipType(data, getChipType(ugp));

    dataList[[key]] <- data;
    verbose && exit(verbose);
  } # for (kk ...)
  verbose && exit(verbose);

  verbose && enter(verbose, "Calling SNP and genotypes");
  for (kk in seq_along(dataList)) {
    key <- names(dataList)[kk];
    verbose && enter(verbose, sprintf("Data set #%d ('%s') of %d", kk, key, length(dataList)));
    data <- dataList[[key]];

    data <- callSNPs(data, verbose=less(verbose, 10));
    data <- callNaiveGenotypes(data, verbose=less(verbose, 10));

    isSNP <- data$isSNP;
    muN <- data$muN;
    data$isHet <- isSNP & (muN == 1/2);
    rm(isSNP, muN);

    dataList[[key]] <- data;
    verbose && exit(verbose);
  } # for (kk ...)
  verbose && exit(verbose);


  verbose && enter(verbose, "Adding virtual (C,rho) fields");
  for (kk in seq_along(dataList)) {
    key <- names(dataList)[kk];
    verbose && enter(verbose, sprintf("Data set #%d ('%s') of %d", kk, key, length(dataList)));
    data <- dataList[[key]];

    data$C <- function(data, ...) {
      CT <- data$CT;
      CN <- data$CN; 
      C <- 2 * CT / CN;
      C;
    };

    data$rho <- function(data, ...) {
      betaT <- data$betaT;
      isHet <- data$isHet;
      rho <- 2*abs(betaT - 1/2);
      rho[!isHet] <- NA;
      rho;
    };

    dataList[[key]] <- data;
    verbose && exit(verbose);
  } # for (kk ...)
  verbose && exit(verbose);


  verbose && print(verbose, dataList);
  verbose && exit(verbose);

  dataList;
} # loadPairedPSCNDataList()


############################################################################
# HISTORY:
# 2011-03-18
# o Added loadPairedPSCNDataList(). Woohoo!
# o Added sanity checks for the result of loadAllDataSets().
# 2009-02-23
# o Created.
############################################################################
