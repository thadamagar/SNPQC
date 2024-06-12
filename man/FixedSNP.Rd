\name{AvgFreq}
\alias{AvgFreq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FixedSNP
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AvgFreq(genoM)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genoM}{
%%     ~~Describe \code{genoM} here~~
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

## The function is currently defined as
function (genoM) 
{
    freqAvg <- rowMeans(genoM, na.rm = TRUE)
    p <- freqAvg/2
    p_filtered <- p[is.finite(p)]
    fixedSNPs <- sum(p == 0, na.rm = TRUE) + sum(p == 1, na.rm = TRUE)
    list_fixedSNPs <- which(p == 0 | p == 1)
    return(list(Number_of_fixedSNPs = fixedSNPs, list_of_fixed_SNPs = list_fixedSNPs))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
