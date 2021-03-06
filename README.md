# GRC
Gene regulation/correlation with gene and gene sets.
---
title: "example"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{example}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

```{r downstream}
library(GRC)

data=system.file("data","data.Rdata", package="GRC")
load(data) 
gene="100134869"
res=regulator(gene,data)
plot.reg.down(res,type="circle")

```

```{r upstream}
gene="100134869"
res=regulator(gene,data,type=2)
res=Top(res,10)
plot.reg.up(res,type="circle")

```

```{r The correlation between gene expression and pathway score }
 gg=list(TEST=res$target[c(12:30)])
GP(data,gg)
```
```{r  Synthetic lethal analysis}
load('data/data.Rdata') 
data=t(data)
data=data[,1]
mutation=rep(c(2,1,0),c(20,18,7))
sample=rownames(data)
data=cbind(sample,data)
data=cbind(data,mutation)

colnames(data)=c('sample','expression','mutation') 
SL(data=data)           
```


