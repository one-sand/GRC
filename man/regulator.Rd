\name{regulator}
\alias{regulator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function for regulation between gane and genes
}
\description{
A function for regulation between gane and genes.
}
\usage{
regulator(target, data, plot = TRUE, type = "1", scale = TRUE, group = "med", FC = 2, FDR = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target}{
the gene that will be check in expression profile.
}
  \item{data}{
the dataset for gene expression. Consisting of a matrix.
}
  \item{plot}{
a logical value for ploting results.default TRUE, print the figure to window.
}
  \item{type}{
The ragulation type for target. if type=1,  thie model will caculate the downstream genes for target gene; if type=2, it will caculate the  upstream genes for target gene.
}
  \item{scale}{
a logical value indicating whether the expression profile needs to normalized by log2(X+1).default scale=TRUE.
}
  \item{group}{
%%     ~~The group for the target gene expresion profile. default is median and group="med"; it also can set "quar" means  gene expression value less than a quartile and more than three quartile.
}
  \item{FC}{
The fold change for difference between target gene and other genes.
}
  \item{FDR}{
The False discovery rate for results.if type=2,the FDR eual to P-value.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
