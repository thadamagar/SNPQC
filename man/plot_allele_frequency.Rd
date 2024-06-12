\name{plot_allele_frequency}
\alias{plot_allele_frequency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot_allele_frequency
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_allele_frequency(p_filtered)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p_filtered}{
%%     ~~Describe \code{p_filtered} here~~
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
function (p_filtered) 
{
    library(ggplot2)
    ggplot(data.frame(p_filtered), aes(x = p_filtered)) + geom_histogram(binwidth = 0.03, 
        fill = "blue", color = "black") + xlab("Allele Frequency") + 
        ggtitle("Distribution of Allele Frequencies") + theme_minimal()
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
