\name{lavail}
\alias{lavail}
\title{
Labour Availability 
}
\description{
It assesses, as a percentage, the time employees are available to perform their duty against
the time they are bound  
}
\usage{
lavail(twk, tbr, tdwn)
}
\arguments{
  \item{twk}{the total time, measured in hours, the employees are supposed to work}
  \item{tbr}{the time measured in hours for break at work}
  \item{tdwn}{the time, measured in hours, that the employees are not working due to machine 
   or material downtime}
}
\details{The function could be applied to any organisation where labour is utilized in any 
form, such as a purely service establishment
}
\value{
a string giving the Labour Availability as a percentage 
}
\references{
Overall Labor Effectiveness, Wikipedia 
}
\author{
Abubakar Akanbi
}
\examples{
## Two employees (workforce) are scheduled to work 8 hour shifts.
## The normal shift includes a scheduled 1/2 hour break.
## The employees experience 1 hour  of scheduled downtime
lavail(twk = 8, tbr = 0.5, tdwn = 1)
}
