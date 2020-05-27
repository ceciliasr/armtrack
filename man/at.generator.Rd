\name{at.generator}
\alias{at.generator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Arm Tracker Data Reading
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
 This function allows you to create a temporal window with a generator. It is useful for example in order to apply then deep learning models such as GRU or LSTM.
}
\usage{
at.generator(data,lookback,delay,min_index,max_index,shuffle,batch_size,step)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
Matrix of data where the last column is the target variable
}
  \item{lookback}{
%%     ~~Describe \code{val} here~~
Time steps that the window should look before
}
  \item{delay}{
%%     ~~Describe \code{val} here~~
Time steps that the window should after before in order to put the correct action
}
  \item{min_index, max_index}{
%%     ~~Describe \code{val} here~~
Data points earlier than min_index or later than max_index will not be used in the output sequences. This is useful to reserve part of the data for test or validation.
}
  \item{shuffle}{
%%     ~~Describe \code{val} here~~
Data should be shuffled or not? (FALSE by default)
}
  \item{batch_size}{
%%     ~~Describe \code{val} here~~
Quantity of rows for each generator iteration
}
  \item{step}{
%%     ~~Describe \code{val} here~~
Period between successive individual timesteps within sequences. For rate r, timesteps data[i],data[i-r], ... data[i - length] are used for create a sample sequence.
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
\item{dd}{An object that can be passed to generator based training functions}
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
