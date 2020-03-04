\name{at.angle}
\alias{at.angle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Arm Tracker Data Reading
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function allows you to obtain the angle of a quaternion in radians and add this as a new variable on the input dataset
}
\usage{
at.angle(data, var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{x} here~~
Input dataset where we can find the variable
}
  \item{var}{
%%     ~~Describe \code{x} here~~
Variable of the dataset to treat. This variable must be the first component of the quaternion because is the one obtaining the angle information.
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
\item{dd}{A dataframe with the Arm Tracker Data adding the angle value of the variable indicated.}
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
