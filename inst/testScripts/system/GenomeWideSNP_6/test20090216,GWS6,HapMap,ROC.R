if (interactive()) savehistory();
library("aroma.cn.eval");


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Region of interest with known CN states
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Sample and region of interest
#array <- "NA06985"; chromosome <- 2; region <- c(81,86)*1e6; states <- c(-1,0);
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


# Range of loci covered in all platforms
xRange <- range(sapply(cnList, FUN=xRange));

robust <- TRUE;

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot raw CNs along chromosome
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mlim <- c(-1,1)*2;
xlim <- xRange;

# The name of the tumor/normal pair
name <- getNames(dsList[[1]])[array];


devSet("panels");
layout(seq_along(cnList));
par(mar=c(4.2,4.2,1.3,2.1));
for (kk in seq_along(cnList)) {
  cn <- cnList[[kk]];
  plot(cn, xlim=xlim, ylim=Mlim);
  stext(side=3, pos=0, cex=0.8, name);
  stext(side=3, pos=1, cex=0.8, sprintf("Chr%02d (n=%d)", 
                                               chromosome, nbrOfLoci(cn)));

  binWidth <- 50e3;
  cnS <- binnedSmoothingByState(cn, from=xRange[1], to=xRange[2], by=binWidth, byPosition=FALSE, robust=robust);
  points(cnS, cex=1, col="white");
  points(cnS, cex=0.5, col="orange"); 
  points(cnS, cex=0.5); 
  lines(cnS, lwd=3, col="orange"); 
  label <- strsplit(names(cnList)[kk], split="\n")[[1]];
  label <- label[1];
  label <- sprintf("Raw and %g-kb binned CNs from %s", binWidth/1e3, label);
  stext(side=3, pos=0, cex=0.8, label);
}



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# ROC performances at various resolutions (in kb)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
devSet("ROC");
binWidths <- c(1,2,5,10)*1e3;
binWidths <- c(0,1,2,3)*1e3;
layout(matrix(seq_along(binWidths), ncol=2, byrow=TRUE));
par(mar=c(3,3,2,1)+0.1, mgp=c(1.4,0.4,0));
fpLim <- c(0,0.20);
for (ww in seq_along(binWidths)) {
  binWidth <- binWidths[ww];

  if (binWidth > 0) {
    # Smooth CNs using consecutive bins of given width (in kb)
    cnSList <- lapply(cnList, FUN=function(cn) {
      cnS <- binnedSmoothingByState(cn, from=xRange[1], to=xRange[2], by=binWidth, byPosition=FALSE, robust=robust);
      cnS <- extractSubsetByState(cnS, states=states);
      cnS;
    }) 
    binLabel <- sprintf("Bin width %g kb", binWidth/1e3);
  } else {
    cnSList <- cnList;
    binLabel <- sprintf("Full resolution");
  }
  print(cnSList);

  fits <- lapply(cnSList, FUN=function(cnS) {
    cat("Number of missing values: ", sum(is.na(cnS$cn)), "\n", sep="");
    fitRoc(cnS, states=states, recall=states[1]);
  });

  for (kk in seq_along(fits)) {
    fit <- fits[[kk]];
    roc <- fit$roc;
    if (kk == 1) {
      plot(roc, type="l", lwd=3, col=kk, xlim=fpLim, ylim=sort(1-fpLim));
      abline(a=0, b=1, lty=3);
      stext(side=3, pos=1, binLabel);
    } else {
      lines(roc, lwd=3, col=kk);
    }
  } # for (kk ...)
} # for (ww ...)

labels <- strsplit(names(cnSList), split="\n");
labels <- sapply(labels, FUN=function(s) s[1]);
legend("bottomright", col=seq_along(labels), pch=19, labels, cex=0.8, bty="n");
