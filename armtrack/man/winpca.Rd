\name{winpca}
\alias{winpca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Temporal windos Principal Component analysis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function allows you to perform a pca in a database which is like a time series.
You indicate the quantity of 'windows' or intervals of time that you want and it 
will perform a pca for each interval.

}
\usage{
winpca(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{x} here~~
Dataset to do the PCAs
}
  \item{var}{
%%     ~~Describe \code{x} here~~
Variable of dataframe that indicates the time
}
  \item{interval}{
%%     ~~Describe \code{x} here~~
Number of interval in which you would like to devide the dataset
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
\item{plot}{Scatterplots of PCA performed in each interval and variable contribution to the first 4 PCs}
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
