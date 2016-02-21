if (interactive()) savehistory();
library("aroma.cn");
library("aroma.cn.eval");

log <- verbose <- Arguments$getVerbose(-8, timestamp=TRUE);
rootPath <- "totalAndFracBData";

dataSet <- "TCGA,GBM,onePair";
sampleName <- "TCGA-12-0620";
chromosome <- 17;
region <- c(0.0,55.0)*1e6;
states <- c(0,+1); 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Load the raw (tumor,normal) data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ds <- AromaUnitFracBCnBinarySet$byName(dataSet, chipType="*", paths=rootPath);
setFullNamesTranslator(ds, function(names, ...) {
  pattern <- "^(TCGA-[0-9]{2}-[0-9]{4})-([0-9]{2}[A-Z])[-]*(.*)";
  gsub(pattern, "\\1,\\2,\\3", names);
});
print(ds);

pair <- indexOf(ds, sampleName);
stopifnot(length(pair) == 2);

# Order as (tumor,normal)
types <- sapply(extract(ds,pair), FUN=function(df) getTags(df)[1]);
o <- order(types);
types <- types[o];
pair <- pair[o];

# Extract (tumor, normal) pair
dsPair <- extract(ds, pair);
dsT <- extract(dsPair, 1);
dsN <- extract(dsPair, 2);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Load the normalized tumor data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
dsTN <- AromaUnitFracBCnBinarySet$byName(dataSet, tags="TBN,Birdseed", chipType="*", paths=rootPath);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Load the genotype call set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
gsN <- AromaUnitFracBCnBinarySet$byName(dataSet, tags="Birdseed", chipType="*", paths=rootPath);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Create an list of matched data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
dsList <- list(normal=dsN, tumor=dsT, tumorN=dsTN, callsN=gsN);
dsList <- lapply(dsList, FUN=function(ds) {
  ds <- setFullNamesTranslator(ds, function(names, ...) {
    pattern <- "^(TCGA-[0-9]{2}-[0-9]{4})-([0-9]{2}[A-Z])[-]*(.*)";
    gsub(pattern, "\\1,\\2,\\3", names);
  });
  idxs <- indexOf(ds, getNames(dsList$normal));
  extract(ds, idxs);
});


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Plot the data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ugp <- getAromaUgpFile(dsList$tumor);
units <- getUnitsOnChromosome(ugp, chromosome=chromosome, region=region);
pos <- getPositions(ugp, units=units);


kk <- 1;
dfList <- lapply(dsList, FUN=getFile, kk);
beta <- lapply(dfList, FUN=function(df) df[units,1,drop=TRUE]);
beta <- as.data.frame(beta);
beta <- as.matrix(beta);

isHet <- (beta[,"callsN"] == 1/2);
beta <- beta[isHet,,drop=FALSE];
pos <- pos[isHet];

cols <- as.integer(1+2*beta[,"callsN"]);

rho <- abs(beta - 1/2);
rho <- rho[,c("tumor", "tumorN")];

cnList <- list();
for (kk in 1:ncol(rho)) {
  cnList[[kk]] <- RawCopyNumbers(cn=rho[,kk], x=pos);
}
truth <- function(x, ...) {
  t <- integer(length(x));
  cps <- 23.5e6;
  t[x > cps] <- +1L;
  t;
} # truth()
cnList <- lapply(cnList, FUN=SegmentedCopyNumbers, states=truth);

fits <- lapply(cnList, FUN=fitRoc, states=c(0,1));
devSet(2);
plot(NA, xlim=c(0,0.5), ylim=c(0.5,1));
for (kk in seq_along(fits)) {
  lines(fits[[kk]]$roc, lwd=2, col=kk);
}

fcnList <- list(mean, appendVarArgs(stats::median.default));
for (jj in seq_along(fcnList)) {
  devSet(2+jj);
  FUN <- fcnList[[jj]];
  cnSList <- lapply(cnList, FUN=function(cn) {
    binnedSmoothingByState(cn, by=10e3, FUN=FUN);
  });
  fits <- lapply(cnSList, FUN=fitRoc, states=c(0,1));
  plot(NA, xlim=c(0,0.5), ylim=c(0.5,1));
  for (kk in seq_along(fits)) {
    lines(fits[[kk]]$roc, lwd=2, col=kk);
  }
}
	