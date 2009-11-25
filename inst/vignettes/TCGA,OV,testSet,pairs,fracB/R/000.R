library("aroma.cn.eval");
library("xtable");
library("MASS");

setOption(aromaSettings, "output/checksum", TRUE);
setOption(aromaSettings, "output/path", FALSE);
setOption(aromaSettings, "output/ram", FALSE);

pd <- packageDescription("aroma.core");
stopifnot(compareVersion(pd$Version, "1.1.2") >= 0);
