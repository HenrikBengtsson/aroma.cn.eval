extractHeterozygous <- function(signalList, genotypeCalls, confidenceScores=NULL, confQuantile=0.90, ..., verbose=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'genotypeCalls':
  className <- "AromaUnitGenotypeCallSet";
  if (!inherits(genotypeCalls, className)) {
    throw(sprintf("Argument 'genotypeCalls' is not of class %s: %s"), 
                                       className, class(genotypeCalls)[1]);
  }

  # Argument 'confidenceScores':
  if (!is.null(confidenceScores)) {
#    className <- "AromaUnitGenotypeCallSet";
#    if (!inherits(confidenceScores, className)) {
#      throw(sprintf("Argument 'confidenceScores' is not of class %s: %s"), 
#                                   className, class(confidenceScores)[1]);
#    }
  }

  # Argument 'confQuantile':
  if (!is.null(confQuantile)) {
    confQuantile <- Arguments$getDouble(confQuantile, range=c(0,1));
  }

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  verbose && enter(verbose, "Extracting heterozygous subset");

  signals <- signalList[[1]];

  name <- getName(signals);
  # Sanity check
  stopifnot(!is.null(name));
  
  name <- gsub(",.*", "", name);
  verbose && cat(verbose, "Name: ", name);

  idx <- indexOf(genotypeCalls, name);
  gf <- getFile(genotypeCalls, idx);
  verbose && print(verbose, gf);

  if (!is.null(confidenceScores) && confQuantile < 1) {
    idx <- indexOf(confidenceScores, name);
    csf <- getFile(confidenceScores, idx);
    verbose && print(verbose, csf);
  }

  signalList <- lapply(signalList, FUN=function(signals) {
    verbose && enter(verbose, "Extracting heterozygous subset");

    # Identify heterozygous SNPs
    units <- signals$unit;
    isHet <- isHeterozygous(gf, units=units, drop=TRUE);
    verbose && cat(verbose, "Heterozygous loci:");
    verbose && summary(verbose, isHet);
    hets <- whichVector(isHet);
    verbose && str(verbose, hets);
    rm(isHet);

    # Keep the SNPs with higest genotype confidence score
    if (!is.null(confidenceScores) && confQuantile < 1) {
      cs <- csf[units[hets], drop=TRUE];
      q <- quantile(cs, 1-confQuantile);
      keep <- (cs >= q);
      hets <- hets[keep];
    }
    
    signals <- extractSubset(signals, hets);
    verbose && exit(verbose);

    signals;
  });

  verbose && print(verbose, signalList);

  verbose && exit(verbose);

  signalList;
} # extractHeterozygous()


############################################################################
# HISTORY:
# 2009-07-04
# o Added argument 'pattern'.
# 2009-04-30
# o Created from getCnList.R.
############################################################################
