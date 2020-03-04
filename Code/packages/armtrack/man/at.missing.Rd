\name{at.missing}
\alias{at.missing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Arm Tracker Data Reading
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function allows you to imput the missing data with the previous non-missing value of each variable when the % of missing data is less than a value (default 5%).
}
\usage{
at.missing(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{x} here~~
Dataset to do the missing treatment
}
  \item{perc}{
%%     ~~Describe \code{x} here~~
By default 5%, the maximum % of missing where we would imput data.
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
\item{d2}{A dataframe with the Arm Tracker Data without missing data or the same if there exists so much missing data.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Cecilia Siliato
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
