library("aroma.cn.eval")

regions <- c(
  "TCGA-23-1027:Chr2@108-140,cp=124+/-0.5,s=0/1",
  "TCGA-23-1027:Chr2@125.0-157.0,cp=141.0+/-0.5,s=1/3",
  "TCGA-23-1027:Chr10@80-109,cp=94+/-0.5,s=0/2", ## deletion
  "TCGA-23-1027:Chr10@106.5-113.5,cp=110+/-0.5,s=2/3", ## deletion -> CN LOH
  "TCGA-23-1027:Chr2@55-75.0,cp=65.0+/-0.5,s=0/1", ## "FALSE BREAKPOINT"
  "TCGA-02-0001:Chr2@35-74,cp=57+/-1,s=1/2",
  "TCGA-02-0001:Chr2@75-110,cp=96+/-1,s=1/4",
  "TCGA-02-0001:Chr2@100-130,cp=110+/-1,s=4/0",
  "TCGA-02-0001:Chr13@0-70,cp=45+/-1,s=0/2"
)

for (kk in seq_along(regions)) {
  region <- regions[kk]
  reg <- parseRegion(region)
  cat(sprintf("Region string: %s\n", region))
  cat("Parsed region:\n")
  str(reg)
}
