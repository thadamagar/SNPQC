\name{filter_genotype_matrix}
\alias{filter_genotype_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
filter_genotype_matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
filter_genotype_matrix(genoM, snp_callrate_threshold = 0.7, hetObs_threshold = 0.3, sample_callrate_threshold = 0.7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genoM}{
%%     ~~Describe \code{genoM} here~~
}
  \item{snp_callrate_threshold}{
%%     ~~Describe \code{snp_callrate_threshold} here~~
}
  \item{hetObs_threshold}{
%%     ~~Describe \code{hetObs_threshold} here~~
}
  \item{sample_callrate_threshold}{
%%     ~~Describe \code{sample_callrate_threshold} here~~
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
function (genoM, snp_callrate_threshold = 0.7, hetObs_threshold = 0.3, 
    sample_callrate_threshold = 0.7) 
{
    freqAvg <- rowMeans(genoM, na.rm = TRUE)
    p <- freqAvg/2
    fixedSNPs <- which(p == 0 | p == 1)
    SNPcalls <- rowSums(!is.na(genoM))/ncol(genoM)
    hetObs <- colSums(genoM == 1, na.rm = TRUE)/nrow(genoM)
    sample_calls <- colSums(!is.na(genoM))/nrow(genoM)
    filtered_genoM <- genoM[-fixedSNPs, ]
    filtered_genoM <- filtered_genoM[which(SNPcalls > snp_callrate_threshold), 
        ]
    filtered_genoM <- filtered_genoM[, which(hetObs < hetObs_threshold)]
    filtered_genoM <- filtered_genoM[, which(colSums(!is.na(filtered_genoM))/nrow(filtered_genoM) > 
        sample_callrate_threshold)]
    return(filtered_genoM)
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
