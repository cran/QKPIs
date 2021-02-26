\name{lperfm}
\alias{lperfm}
\title{
Labour Performance 
}
\description{
It measures, as a percentage, performance of employees against a set standard of performance 
in terms of labour output
}
\usage{
lperfm(twk, tbr, tdwn, stdr, pprd)
}
\arguments{
  \item{twk}{the time measured in hours to produce the standard output}
  \item{tbr}{the time allocated for break measured in hours}
  \item{tdwn}{the downtime measured in hours when there is breakdown that prevents production}
  \item{stdr}{the set standard output}
  \item{pprd}{the sellable portion of output produced by the employees within the time to
   work}
}
\details{
The output in this regard extends beyond physical object. It could be service rendering that
is supposed to bring  in a certain  amount of money within the time to work against the
actual amount of money brought in through the service rendered within the time. The function 
calculates the performance based on a minute time to produce the output 
}
\value{
a character string showing the Labour Performance as a percentage 
}
\references{
Overall Labor Effectiveness, Wikipedia 
}
\author{
Abubakar Akanbi
}
\examples{
## Two employees (workforce) are scheduled to 8-hour shift with a 1/2 hour scheduled break and
## there is unscheduled downtime of 2 hours. The standards rate for the part being produced 
## is 60 unitsper hour. The workforce produced 700 units during shift
lperfm( twk = 16, tbr =1, tdwn = 2, stdr = 60, pprd = 700)
}
