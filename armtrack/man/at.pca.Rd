\name{at.pca}
\alias{at.pca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Arm Tracker Data Reading
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
 This function allows you to perform a pca to a DB and obtain a list with the reconstructed database using the first n components until obtain more than a value (given) of cumulative proportion.
}
\usage{
at.pca(data,val)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
Needed in order to know the dataframe file to perform PCA
}
  \item{val}{
%%     ~~Describe \code{val} here~~
Threshold cumulative proportion value to take into account (Default: 0.9)
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
\item{dd}{A list of the first reconstructed dataframes with the PCs until reach the 'val' cumulative proportion value}
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
