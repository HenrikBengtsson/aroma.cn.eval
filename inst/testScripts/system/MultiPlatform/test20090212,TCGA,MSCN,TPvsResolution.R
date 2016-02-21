if (interactive()) savehistory();
library("aroma.cn.eval");

log <- verbose <- Arguments$getVerbose(-8, timestamp=TRUE);

tagsList <- list("MSKCC", "Harvard", "Stanford", "Broad");
tagsList <- lapply(tagsList, FUN=c, "mscn");
dataSet <- "TCGA,GBM,testSet,pairs";

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Load raw CN data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
dsList <- lapply(tagsList, FUN=function(tags) {
  AromaUnitTotalCnBinarySet$byName(dataSet, tags=tags, chipType="*");
});
# Keep only common samples (just in case)
names <- Reduce(intersect, lapply(dsList, FUN=getNames));
dsList <- lapply(dsList, FUN=extract, names);
print(dsList);


tags <- Reduce(intersect, lapply(dsList, FUN=getTags));
sites <- sapply(dsList, FUN=function(ds) setdiff(getTags(ds), tags));
platforms <- sapply(dsList, FUN=getPlatform);
chipTypes <- sapply(dsList, FUN=getChipType);
names <- cbind(site=sites, platform=platforms, chipType=chipTypes);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Region of interest with known CN states
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Sample of interest
array <- "TCGA-06-0178-01Avs10B";

# Chromosomal region of interest
chromosome <- 10;
region <- c(93.18,136.38)*1e6;

# A function give the true CN state for a given set of positions
truth <- function(x, ...) {
  t <- integer(length(x));

  # Location of change points
  xCp <- 114.77e6;
  # Neutral
  t[x <= xCp] <- 0L;

  # Gain
  t[x >= xCp] <- +1L;

  # Unknown (safety region of 500kb each side)
  dx <- 500e3;
  t[xCp-dx < x & x < xCp+dx] <- NA;

  t;
} # truth()

name <- getNames(extract(dsList[[1]], array))[1];
print(name);

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Extract data from region of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
cnList <- lapply(dsList, FUN=function(ds) {
  df <- getFile(extract(ds, array), 1);
  cn <- extractRawCopyNumbers(df, chromosome=chromosome, region=region);
  cn <- SegmentedCopyNumbers(cn, states=truth);  
  cn;
})
names(cnList) <- apply(names, MARGIN=1, paste, collapse="\n");
print(cnList);
nbrOfTracks <- length(dsList);

# Range of loci covered in all platforms
xRange <- range(sapply(cnList, FUN=xRange));



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# TP rate at fixe FP rate at various resolutions (in kb)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
devSet("TPvResolution2");
states <- c(0,1);
fpRate <- 0.02;
binWidths <- c(5,10,20,30,40,50,60,70,80,90,100)*1e3;
binWidths <- seq(from=1,to=50,by=3)*1e3;
xlim <- c(0,max(binWidths))/1e3;
tplim <- c(0,1);
tplab <- "True-positive rate";
xlab <- "Width of bins (in kb)";
par(mar=c(3,3,2,1)+0.1, mgp=c(1.4,0.4,0));
plot(NA, xlim=xlim, ylim=tplim, xlab=xlab, ylab=tplab);
stext(side=3, pos=0, name);
trackCols <- seq_len(nbrOfTracks);
labels <- apply(names[,-2], MARGIN=1, paste, collapse="/");
legend("bottomright", col=trackCols, pch=19, labels, cex=0.8, bty="n");

naValue <- as.double(NA);
tpRates <- matrix(naValue, nrow=length(binWidths), ncol=nbrOfTracks);
colnames(tpRates) <- apply(names, MARGIN=1, paste, collapse=";");
colnames(tpRates) <- names(cnList);
res <- cbind(data.frame(binWidth=binWidths), tpRates);

for (ww in seq_along(binWidths)) {
  binWidth <- binWidths[ww];

  # Smooth CNs using consecutive bins of given width (in kb)
  cnSList <- lapply(cnList, FUN=function(cn) {
    cnS <- binnedSmoothingByState(cn, from=xRange[1]+binWidth/2, to=xRange[2]+binWidth/2, by=binWidth, verbose=log);
    cnS <- extractSubsetByState(cnS, states=states);
    cnS;
  })
  print(cnSList);

  fits <- lapply(cnSList, FUN=function(cnS) {
    findRocTpAtFp(cnS, fpRate=fpRate, states=states, recall=states[1]);
  });

  tpRates <- sapply(fits, FUN=function(fit) fit[["tpRateEst"]]);
  cols <- match(names(tpRates), colnames(res));
  row <- whichVector(res[,"binWidth"] == binWidth);
  res[row,cols] <- tpRates;

  x <- rep(binWidth/1e3, nbrOfTracks);
  points(x, y=tpRates, pch=19, col=trackCols);
} # for (ww ...)

o <- order(res[,"binWidth"]);
res <- res[o,,drop=FALSE];

for (kk in seq_len(nbrOfTracks)) {
  binWidths <- res[,"binWidth"];
  tpRates <- res[,kk+1];
  lines(binWidths/1e3, tpRates, lwd=2, col=trackCols[kk]);
}

