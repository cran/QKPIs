\name{saa}
\alias{saa}
\title{
Sales Margin
}
\description{
It calculates the diffrence between any current sales amount and a target sales amount. Sales
are presented as amounts of money 
}
\usage{
saa(status, tgoal)
}
\arguments{
  \item{status}{any current sales amount under review}
  \item{tgoal}{the target sales amount to be achieved}
}
\details{
The function is specifically meant for spot sales amount to ascertain its performance 
}
\value{ 
a string describing the current sale as percentage of the target and their difference
}
\author{
Abubakar Akanbi
}

\examples{
## Current sales = 56000, target sales = 45520
## status = 56000 , tgoal = 45520
saa(status = 56000, tgoal = 45520)
}
