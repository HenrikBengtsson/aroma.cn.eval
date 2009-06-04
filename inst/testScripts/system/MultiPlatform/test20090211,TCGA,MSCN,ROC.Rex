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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Extract data from region of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
cnList <- lapply(dsList, FUN=function(ds) {
  df <- getFile(extract(ds, array), 1);
  cn <- extractRawCopyNumbers(df, chromosome=chromosome, region=region);
  cn <- SegmentedCopyNumbers(cn, states=truth);  
  cn;
})
names(cnList) <- sprintf("%s\n%s\n%s", sites, platforms, chipTypes);
print(cnList);

# Range of loci covered in all platforms
xRange <- range(sapply(cnList, FUN=xRange));


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot raw CNs along chromosome
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mlim <- c(-1,1)*1;
xlim <- xRange;

# The name of the tumor/normal pair
name <- getNames(dsList[[1]])[array];


devSet("panels");
layout(seq(along=cnList));
par(mar=c(4.2,4.2,1.3,2.1));
for (kk in seq(along=cnList)) {
  cn <- cnList[[kk]];
  plot(cn, xlim=xlim, ylim=Mlim);
  stext(side=3, pos=0, cex=0.8, name);
  stext(side=3, pos=1, cex=0.8, sprintf("Chr%02d (n=%d)", 
                                               chromosome, nbrOfLoci(cn)));

  binWidth <- 50e3;
  cnS <- binnedSmoothingByState(cn, from=xRange[1], to=xRange[2], by=binWidth);
  points(cnS, cex=1, col="white");
  points(cnS, cex=0.5, col="blue"); 
  label <- strsplit(names(cnList)[kk], split="\n")[[1]];
  label <- paste(label[-3], collapse="/");
  label <- sprintf("Raw and %g-kb binned CNs from %s", binWidth/1e3, label);
  stext(side=3, pos=0, cex=0.8, label);
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# ROC performances at various resolutions (in kb)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
devSet("ROC");
states <- c(0,1);
binWidths <- c(10,25,50,100)*1e3;
layout(matrix(seq(along=binWidths), ncol=2, byrow=TRUE));
par(mar=c(3,3,2,1)+0.1, mgp=c(1.4,0.4,0));
fpLim <- c(0,0.25);
for (ww in seq(along=binWidths)) {
  binWidth <- binWidths[ww];

  # Smooth CNs using consecutive bins of given width (in kb)
  cnSList <- lapply(cnList, FUN=function(cn) {
    cnS <- binnedSmoothingByState(cn, from=xRange[1], to=xRange[2], by=binWidth);
    cnS <- extractSubsetByState(cnS, states=states);
    cnS;
  })
  print(cnSList);

  fits <- lapply(cnSList, FUN=function(cnS) {
    fitRoc(cnS, states=states, recall=states[1]);
  });

  for (kk in seq(along=fits)) {
    fit <- fits[[kk]];
    roc <- fit$roc;
    if (kk == 1) {
      plot(roc, type="l", lwd=3, col=kk, xlim=fpLim, ylim=sort(1-fpLim));
      abline(a=0, b=1, lty=3);
      stext(side=3, pos=1, sprintf("Bin width %g kb", binWidth/1e3));
    } else {
      lines(roc, lwd=3, col=kk);
    }
  } # for (kk ...)
} # for (ww ...)

labels <- strsplit(names(cnSList), split="\n");
labels <- sapply(labels, FUN=function(s) paste(s[-3], collapse="/"));
legend("bottomright", col=seq(along=labels), pch=19, labels, cex=0.8, bty="n");

