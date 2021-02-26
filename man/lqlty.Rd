\name{lqlty}
\alias{lqlty}
\title{
Labour Quality 
}
\description{
It calculates the auality of labour against expected quality as as percentages
}
\usage{
lqlty(sprt, tprt)
}
\arguments{
  \item{sprt}{ a positive integer representing number of outputs produced with standard 
  quality - the sellable}
  \item{tprt}{a positive integer, representing total number of outputs produced, either 
  standard or substandard}
}
\value{
a string of character giving labour quality as a percentage 
}
\references{
Overall Labor Effectiveness, Wikipedia 
}
\author{
Abubakar Akanbi
}
\examples{
## Two employees (workforce) produce 670 good units during a shift
## 700 Units were started in order to produce the 670 good units
lqlty(sprt = 670, tprt = 700)
}
