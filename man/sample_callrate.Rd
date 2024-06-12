\name{sample_callrate}
\alias{sample_callrate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sample_callrate
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sample_callrate(genoM, threshold = 0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genoM}{
%%     ~~Describe \code{genoM} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
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
function (genoM, threshold = 0.9) 
{
    sample_calls <- colSums(!is.na(genoM))/nrow(genoM)
    highsamplecall <- sum(sample_calls >= threshold)/ncol(genoM) * 
        100
    low_call_samples <- data.frame(Sample = names(sample_calls[sample_calls < 
        threshold]), CallRate = sample_calls[sample_calls < threshold], 
        row.names = NULL)
    return(list(sample_call_rate = sample_calls, high_sample_call_rate_percentage = highsamplecall, 
        low_call_rate_samples = low_call_samples))
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
