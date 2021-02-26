\name{ole}
\alias{ole}
\title{
Overall Labour Effectiveness 
}
\description{
It calculates the Overall Labour Effectiveness as a percent from Labour Availability, Labour
Performance and Labour Quality  
}
\usage{
ole(lavail, lperfm, lqlty)
}
\arguments{
  \item{lavail}{the percent value returned by the function lavail}
  \item{lperfm}{the percent value regurned by the function lperfm}
  \item{lqlty}{he percent value regurned by the function lqlty}
}
\details{
Each argument  in the function ole is a self-contained key performance metric function, which 
is considered independently  
}
\value{
a string of character giving Overall Labour Effectiveness as a percentage
}
\references{
Overall Labor Effectiveness, Wikipedia 
}
\author{
Abubakar Akanbi
}
\examples{
ole(lavail = 86.67, lperfm = 89.74, lqlty =  95.71)
}
