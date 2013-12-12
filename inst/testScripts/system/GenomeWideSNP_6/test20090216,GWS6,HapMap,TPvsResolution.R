if (interactive()) savehistory();
library("aroma.cn.eval");

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Region of interest with known CN states
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Sample and region of interest
array <- "NA06985"; chromosome <- 2; region <- c(81,86)*1e6; states <- c(-1,0);
array <- "NA06985"; chromosome <- 14; region <- c(19.25,19.80)*1e6; states <- c(0,+1);
#array <- "NA06991"; chromosome <- 22; region <- c(23.5,25.0)*1e6; states <- c(0,+1);
#array <- "NA06993"; chromosome <- 1; region <- c(147.3,149.0)*1e6; states <- c(0,+1);
#array <- "NA11830"; chromosome <- 15; region <- c(29.0,31.5)*1e6; states <- c(0,+1);
#array <- "NA11839"; chromosome <- 22; region <- c(23.5,25.0)*1e6; states <- c(0,+1);
#array <- "NA07022"; chromosome <- 4; region <- c(116.0,122.0)*1e6; states <- c(0,+1);
 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Extract data from region of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
cnList <- lapply(dsList, FUN=function(ds) {
  df <- getFile(extract(ds, array), 1);
  subset <- NULL;
  if (getChipType(df) != chipType) {
    subset <- poolOfUnits;
  }
  cn <- extractRawCopyNumbers(df, chromosome=chromosome, region=region, units=subset);
  cn <- SegmentedCopyNumbers(cn, states=truth);
  cn;
})
names(cnList) <- sprintf("%s\n%s\n%s", methods, platforms, chipTypes);
print(cnList);
nbrOfTracks <- length(cnList);

# Range of loci covered in all platforms
xRange <- range(sapply(cnList, FUN=xRange));  



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# TP rate at fixe FP rate at various resolutions (in kb)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
devSet("TPvResolution");
states <- c(-1,0);
fpRate <- 0.05;
binWidths <- c(0,0.1,0.5,1,2,4,6,8,10,15,20)*1e3;
xlim <- c(0,max(binWidths))/1e3;
tplim <- c(0,1);
tplab <- "True-positive rate";
xlab <- "Width of bins (in kb)";
par(mar=c(3,3,2,1)+0.1, mgp=c(1.4,0.4,0));
plot(NA, xlim=xlim, ylim=tplim, xlab=xlab, ylab=tplab);
stext(side=3, pos=0, name);
trackCols <- seq(length=nbrOfTracks);
labels <- methods;
legend("bottomright", col=trackCols, pch=19, labels, cex=0.8, bty="n");

naValue <- as.double(NA);
tpRates <- matrix(naValue, nrow=length(binWidths), ncol=nbrOfTracks);
colnames(tpRates) <- names(cnList);
res <- cbind(data.frame(binWidth=binWidths), tpRates);

for (ww in seq(along=binWidths)) {
  binWidth <- binWidths[ww];

  if (binWidth > 0) {
    # Smooth CNs using consecutive bins of given width (in kb)
    cnSList <- lapply(cnList, FUN=function(cn) {
      cnS <- binnedSmoothingByState(cn, from=xRange[1], to=xRange[2], by=binWidth, verbose=log);
      cnS <- extractSubsetByState(cnS, states=states);
      cnS;
    })
  } else {
    cnSList <- cnList;
  }
  print(cnSList);

  fits <- lapply(cnSList, FUN=function(cnS) {
    findRocTpAtFp(cnS, fpRate=fpRate, states=states, recall=states[1]);
  });

  tpRates <- sapply(fits, FUN=function(fit) fit[["tpRateEst"]]);
  cols <- match(names(tpRates), colnames(res));
  row <- whichVector(res[,"binWidth"] == binWidth);
  res[row,cols] <- tpRates;

  if (binWidth == 0) {
    binWidth <- diff(xRange(cnS))/(nbrOfLoci(cnS)-1);
    binWidths[ww] <- binWidth;
  }

  x <- rep(binWidth/1e3, nbrOfTracks);
  points(x, y=tpRates, pch=19, col=trackCols);
} # for (ww ...)

o <- order(res[,"binWidth"]);
res <- res[o,,drop=FALSE];

for (kk in seq(length=nbrOfTracks)) {
  binWidths <- res[,"binWidth"];
  tpRates <- res[,kk+1];
  lines(binWidths/1e3, tpRates, lwd=2, col=trackCols[kk]);
}
