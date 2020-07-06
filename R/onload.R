#load dependent packages

.onLoad<-function(lib, pkg) {
  packageStartupMessage("")
  warning("package 'GRC' was built under R version 3.6.1")

  if(!require(ggplot2)){
    install.packages("ggplot2",repos = "http://cran.us.r-project.org")
  }
  if(!require(pheatmap)){
    install.packages("pheatmap",repos = "http://cran.us.r-project.org")
  }
  if(!require(scatterplot3d)){
    install.packages("scatterplot3d",repos = "http://cran.us.r-project.org")
  }
  if(!require(igraph)){
    install.packages("igraph",repos = "http://cran.us.r-project.org")
  }
  if(!require(plotrix)){
    install.packages("plotrix",repos = "http://cran.us.r-project.org")
  }

  if (!requireNamespace("BiocManager", quietly = TRUE)){
    install.packages("BiocManager",repos = "http://cran.us.r-project.org")

  }

  if(!require(GSVA)){
    BiocManager::install("GSVA")
  }
  if(!require(GSEABase)){
    BiocManager::install("GSEABase")
  }


}

.First <- function(){
  cat("Hello!") # startup message

}
