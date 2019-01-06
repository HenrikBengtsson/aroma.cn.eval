if (interactive()) savehistory();
library("aroma.cn.eval");
library("R.cache");


dataset <- "GSE13372";
chipType <- "GenomeWideSNP_6";


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Known CN aberrant regions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
truth <- function(x, chromosome, name, ...) {
  name <- gsub(",.*", "", name);
  res <- integer(length(x));
  state <- -1L;
  cps <- NULL;
  if (chromosome == 1) {
    cps <- c(103.8,107.7)*1e6;
    dx <- 200e3;
    state <- -1L;
  } else if (chromosome == 3) {
    cps <- c(85.3,91)*1e6;
    dx <- 100e3;
    state <- +1L;
  } else if (chromosome == 4) {
    cps <- c(63.4,65.8)*1e6;
    dx <- 50e3;
    state <- +1L;
  } else if (chromosome == 10) {
    cps <- c(60,65.3)*1e6;
    dx <- 100e3;
    state <- +1L;
  } else if (chromosome == 11) {
    cps <- c(78.1,80.2)*1e6;
    dx <- 100e3;
    state <- +1L;
  } else if (chromosome == 12) {
    cps <- c(56.0,59.9)*1e6;
    dx <- 100e3;
    state <- +1L;
  }

  if (length(cps) > 0) {
    res[cps[1] <= x & x < cps[2]] <- state;
    dx <- rep(dx, length.out=2);
    for (kk in seq_along(cps)) {
      res[cps[kk]-dx[kk] <= x & x < cps[kk]+dx[kk]] <- NA;
    }
  }
  res;
} # truth()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Regions of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
regions <- c(
  "GSM337641:Chr1@98-112",
  "GSM337641:Chr3@80-92",
  "GSM337641:Chr4@60.5-68.5",
  "GSM337641:Chr10@61-69",
  "GSM337641:Chr11@78.1-83",
  "GSM337641:Chr12@57-63"
);

#region <- regions[1];
#region <- regions[4];
library("R.menu");
region <- selectMenu(regions);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Infer sample name, chromsome, and region
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pattern <- "^(.*):Chr([0-9]+)@([.0-9]+)-([.0-9]+)";
array <- gsub(pattern, "\\1", region);
chromosome <- as.integer(gsub(pattern, "\\2", region));
start <- as.double(gsub(pattern, "\\3", region));
stop <- as.double(gsub(pattern, "\\4", region));
region <- c(start, stop)*1e6;


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up annotation data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rootPath <- "rawCnData";
rootPath <- Arguments$getReadablePath(rootPath, mustExist=TRUE);

if (!exists("unitsF", mode="numeric")) {
  key <- list("fullUnitsAlsoInDefault", chipType=chipType);
  dirs <- c("aroma.affymetrix", chipType);
  unitsF <- loadCache(key=key, dirs=dirs);
  if (is.null(unitsF)) {
    library("aroma.affymetrix");
    cdf <- AffymetrixCdfFile$byChipType(chipType);
    cdfF <- AffymetrixCdfFile$byChipType(chipType, tags="Full");
    unitNames <- getUnitNames(cdf);
    units <- indexOf(cdfF, names=unitNames);
    stopifnot(length(units) == nbrOfUnits(cdf));
    saveCache(units, key=key, dirs=dirs);
  }
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (!exists("dsList", mode="list")) {
  pattern <- sprintf("^%s,.*,pairs", dataset);
  datasets <- list.files(path=rootPath, pattern=pattern);
  if (length(datasets) == 0L) {
    throw(sprintf("Failed to located any '%s' data sets under '%s'.", pattern, rootPath));
  }

  dsList <- list();
  for (kk in seq_along(datasets)) {
    dataset <- datasets[kk];
    ds <- AromaUnitTotalCnBinarySet$byName(dataset, chipType=chipType);
    dsList[[kk]] <- ds;
  }

  # Set the names
  names <- sapply(dsList, FUN=getFullName);
  names <- gsub("^GSE13372(,HCC1143|),", "", names);
  names <- gsub(",pairs$", "", names);
  names <- gsub("ACC,(ra,|)-XY,BPN,-XY,(AVG|RMA),A[+]B,FLN,-XY", "CRMAv2,\\2", names);
  names <- gsub("ACC,-XY,v1,(AVG|RMA),A[+]B,FLN,-XY", "CRMAv1,\\1", names);

  names(dsList) <- names;
  print(dsList);
}
stopifnot(length(dsList) > 0L)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Extract CNs for sample and genomic region of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dfList <- lapply(dsList, FUN=getFile, indexOf(ds, array));
cnList <- lapply(dfList, FUN=function(df) {
  if (regexpr("Full", getChipType(df)) != -1) {
    units <- unitsF;
  } else {
    units <- NULL;
  }
  cn <- extractRawCopyNumbers(df, chromosome=chromosome, region=region, units=units);
  SegmentedCopyNumbers(cn, states=truth);
});
print(cnList);
stopifnot(length(cnList) > 0L)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Infer states
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cn <- cnList[[1]];
states <- sort(unique(getStates(cn)));
states <- states[is.finite(states)];
stopifnot(length(states) == 2);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Plot CN ratios along genome
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fig <- sprintf("%s,chr%02d", getName(dfList[[1]]), chromosome);
devSet(fig, width=8, height=8);
nbrOfSources <- length(cnList);
subplots(nbrOfSources, ncol=1);
par(mar=c(3,3,0.5,0.5)+0.1);

xRange <- range(sapply(cnList, FUN=xRange));
for (kk in seq_along(cnList)) {
  cn <- cnList[[kk]];
  name <- names(cnList)[kk];
  plot(cn, cex=1.5, xlim=xRange);
  sd <- estimateStandardDeviation(cn);
  stext(side=3, pos=0.5, line=-1.5, cex=0.8, sprintf("SD=%.3g", sd));
  stext(side=3, pos=0, line=-1.1, name);
  if (kk == 1) {
    stext(side=3, pos=1, line=-1.1, sprintf("Chr%02d", getChromosome(cn)));
  }
  binWidth <- 3e3;
  cn <- extractSubsetByState(cn, states=states);
  cnS <- binnedSmoothingByState(cn, from=xRange[1], to=xRange[2], by=binWidth);
  points(cnS, cex=1.2, col="white");
  points(cnS, cex=1, col="orange");
}
devDone(fig);


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ROC performances at various resolutions (in kb)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
devSet("ROC");
binWidths <- c(0,1,2,5)*1e3;
layout(matrix(seq_along(binWidths), ncol=2, byrow=TRUE));
par(mar=c(3,3,2,1)+0.1, mgp=c(1.4,0.4,0));
fpLim <- c(0,0.50);
for (ww in seq_along(binWidths)) {
  binWidth <- binWidths[ww];

  if (binWidth > 0) {
    # Smooth CNs using consecutive bins of given width (in kb)
    cnSList <- lapply(cnList, FUN=function(cn) {
      cnS <- binnedSmoothingByState(cn, from=xRange[1], to=xRange[2], by=binWidth);
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
    if (kk == 1) {
      labels <- strsplit(names(cnSList), split="\n");
      labels <- sapply(labels, FUN=function(s) s[1]);
      legend("bottomright", col=seq_along(labels), pch=19, labels, cex=0.8, bty="n");
    }
  } # for (kk ...)
} # for (ww ...)


############################################################################
# HISTORY:
# 2009-02-22
# o Created.
############################################################################
