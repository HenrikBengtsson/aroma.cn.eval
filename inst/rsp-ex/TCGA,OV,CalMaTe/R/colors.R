getMethodColor <- function(method, what=c("TCN", "DH", "BAF"), default="#999999", human=FALSE, ...) {
  what <- match.arg(what);
  if (what =="BAF") what <- "DH";

  # Setup color scheme
  colnames <- c("TCN", "DH");
  rownames <- c("*", "*,TumorBoost", "*,CalMaTe", "*,CalMaTe,v1", "*,CalMaTe,v2");
  dimnames <- list(rownames, colnames);
  dim <- sapply(dimnames, FUN=length);
  map <- array(default, dim=dim, dimnames=dimnames);
  map["*",]            <- c("#999999", "#000000");
  map["*,TumorBoost",] <- c("#ff9999", "#ff0000");
  map["*,CalMaTe",]    <- c("#9999ff", "#0000ff");
  map["*,CalMaTe,v1",] <- map["*,CalMaTe",];
  map["*,CalMaTe,v2",] <- map["*,CalMaTe",];

  # Pick value
  methodS <- gsub("^([^,]*)", "*", method);
  methodS <- gsub(",XY", "", methodS); # AD HOC
  res <- map[methodS,what];
  res[is.na(res)] <- default;

  if (human) {
    map <- c("#000000"="black", "#999999"="gray",
             "#ff0000"="red", "#ff9999"="light red",
             "#0000ff"="blue", "#9999ff"="light blue");
    res2 <- map[res];
    ok <- !is.na(res2);
    res[ok] <- res2[ok];
  }

  res;
} # getMethodColor()


getMethodLineType <- function(method, what=c("TCN", "DH", "BAF"), default=1, human=FALSE, ...) {
  what <- match.arg(what);
  if (what =="BAF") what <- "DH";

  # Setup color scheme
  colnames <- c("TCN", "DH");
  rownames <- c("*", "*,TumorBoost", "*,CalMaTe", "*,CalMaTe,v1", "*,CalMaTe,v2");
  dimnames <- list(rownames, colnames);
  dim <- sapply(dimnames, FUN=length);
  map <- array(default, dim=dim, dimnames=dimnames);
  map["*",]            <- c(2, 2);
  map["*,TumorBoost",] <- c(3, 3);
  map["*,CalMaTe",]    <- c(1, 1);
  map["*,CalMaTe,v1",] <- map["*,CalMaTe",];
  map["*,CalMaTe,v2",] <- map["*,CalMaTe",];

  # Pick value
  methodS <- gsub("^([^,]*)", "*", method);
  methodS <- gsub(",XY", "", methodS); # AD HOC
  res <- map[methodS,what];
  res[is.na(res)] <- default;

  if (human) {
    labels <- c("blank"=0, "solid"=1, "dashed"=2, "dotted"=3, "dotdash"=4, "longdash"=5, "twodash"=6);
    idxs <- match(res, labels);
    ok <- is.finite(idxs);
    res[ok] <- names(labels)[idxs[ok]];
  }

  res;
} # getMethodLineType()


getMethodLegends <- function(methods, ..., lty=TRUE, col=TRUE, nuanse=TRUE, quote=TRUE, human=TRUE, collapse=TRUE) {
  s <- NULL;
  if (lty) {
    ltys <- getMethodLineType(methods, ..., human=human);
    s <- paste(s, ltys, sep=" ");
  }
  if (col) {
    cols <- getMethodColor(methods, ..., human=human);
    if (!nuanse) {
      cols <- gsub("(dark|light)", "", cols);
    }
    s <- paste(s, cols, sep=" ");
  }

  if (quote) {
    methods <- sprintf("'%s'", methods);
  }

  legends <- methods;

  if (!is.null(s)) {
    s <- gsub("(^ | $)", "", s);
    s <- gsub("  ", " ", s);
    legends <- sprintf("%s (%s)", methods, s);
  }

  if (collapse) {
    legends <- hpaste(legends, lastCollapse=", and ", maxHead=Inf);
  }

  legends;
} # getMethodLegends()
