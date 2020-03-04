\name{at.read}
\alias{at.read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Arm Tracker Data Reading
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function allows you to read Arm Tracker Data in .mat format
}
\usage{
at.read(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
Needed in order to know the name (and path) of the mat data file you want to read
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
\item{dd}{A dataframe with the Arm Tracker Data. The quaternions are written as 4 different variables with a subindex 1, 2, 3 or 4. The same happens with tridimensional data.}
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
