setMethodS3("extractRawAlleleBFractions", "AromaUnitFracBCnBinaryFile", function(this, chromosome, range=NULL, units=NULL, ..., verbose=FALSE) {
  # Argument 'units':
  if (!is.null(units)) {
    units <- Arguments$getIndices(units, range=c(1, nbrOfUnits(this)));
    units <- sort(unique(units));
  }

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  } 


  verbose && enter(verbose, "Extracting RawAlleleBFractions");

  name <- getFullName(this);
  verbose && cat(verbose, "Name: ", name);

  verbose && cat(verbose, "Chromosome: ", chromosome);
  verbose && enter(verbose, "Identifying units on chromosome");
  ugp <- getAromaUgpFile(this, ..., verbose=less(verbose,50));
  verbose && print(verbose, ugp);
  units2 <- getUnitsAt(ugp, chromosome=chromosome, range=range, ..., 
                                            verbose=less(verbose,5));
  verbose && cat(verbose, "Units:");
  verbose && str(verbose, units2);

  # Keeping only a subset of units?
  if (!is.null(units)) {
    verbose && enter(verbose, "Keeping only units of interest");
    keep <- is.element(units2, units);
    verbose && cat(verbose, "Keeping:");
    verbose && summary(verbose, keep);
    units2 <- units2[keep];
    rm(keep);
    verbose && cat(verbose, "Units:");
    verbose && str(verbose, units);
    verbose && exit(verbose);
  }
  units <- units2;
  rm(units2);

  # Identify hets?
  if (exists("gsN")) {
    verbose && enter(verbose, "Keeping only loci for which the normal is heterozygous");
    idx <- indexOf(gsN, getName(this));
    gf <- getFile(gsN, idx);
    print(gf);
    mu <- gf[units,1,drop=TRUE];
    ## Stratify on heterozygous normal genotypes
    keep <- whichVector(isHeterozygous(gf, units=units, drop=TRUE));
    
    units <- units[keep];
    verbose && cat(verbose, "Units:");
    verbose && str(verbose, units);
    verbose && exit(verbose);
  }

  verbose && cat(verbose, "Genomic positions:");
  pos <- getPositions(ugp, units=units);
  verbose && str(verbose, pos);  
  verbose && exit(verbose);

  verbose && enter(verbose, "Extracting data");
  beta <- extractMatrix(this, units=units, drop=TRUE, verbose=less(verbose,5));
  verbose && str(verbose, beta);

  fracBs <- RawAlleleBFractions(x=pos, y=beta, chromosome=chromosome, name=name);

  # Add annotation data
  fracBs$platform <- getPlatform(this);
  fracBs$chipType <- getChipType(this);
  fracBs$fullname <- getFullName(this);

  verbose && exit(verbose);

  verbose && exit(verbose);

  fracBs;
})



############################################################################
# HISTORY:
# 2009-06-10
# o Now using isHeterozygous() [was alias isHeterozygote()].
# o Created from extractRawCopyNumbers.AromaUnitFracBCnBinaryFile.R.
# 2009-06-09
# o Hets now inferred using 'isHeterozygote'.
# 2009-04-30
# o Created. [AD HOC TEMP!!!]
############################################################################
