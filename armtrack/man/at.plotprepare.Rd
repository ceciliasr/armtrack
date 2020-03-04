\name{at.plotprepare}
\alias{at.plotprepare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Arm Tracker Data Reading
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
 This function prepares the Arm Tracker database to perform a 3D ggplot.
}
\usage{
at.plotprepare(data,freq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
Needed in order to know the dataframe file to prepare
}
  \item{freq}{
%%     ~~Describe \code{freq} here~~
The frequency (Hz) that we need on data. Default: 2Hz
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
\item{dd}{A dataframe prepared to perform a ggplot in 3D}
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
