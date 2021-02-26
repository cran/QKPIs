\name{sam}
\alias{sam}
\title{Monthly Sales }
\description{
It calculates average sales per month, if there is a series of sales in a month and returns 
the average and total sales for a period, normally a year. It produces the visual of
performance in a line plot for the period under review.  
}
\usage{
sam(status, tgoal, dtev)
}
\arguments{
  \item{status}{a dataframe containing the average sales amount or a series of amounts for
   each month}
  \item{tgoal}{the target amount of sales for the period}
  \item{dtev}{1 or logical TRUE. It is an optional value. When it is not provided the 
   function considers the the status argument as the average sales amount for the period}
}
\details{The actual sales key performance indicator is usually presented in a visual by 
known Buisness Intelligence apps, as such the lines plot produced by the function contains 
the key indicators. And the period must start from january. Months or times without sales
reckoning must be filled with zeros or NAs, if the table is read with read.table function. 
The function only reckons with yearly sales starting from January and ending in December      
}
\value{
    \item{comp1 }{a line plot graphic of performance}
    \item{comp2 }{a string describing whether sales target is achieved or otherwise}
    \item{comp3 }{a dataframe object containing the period sales average and total}
}
\author{
Abubakar Akanbi
}
\examples{
df2 = data.frame(Jan = c(23455, 56432, 64344), Feb = c(21145, 43213, 76433), 
Mar = c(2005,  86432, 64344), April = c(40149, 67746, 43203), May = c(85438, 05847,6849),
June = c(5565,44253, 78253), July = c(85724, 87346, 33332), Aug = c(45782, 5279, 6235))
sam(status = df2, tgoal = 54635, dtev = 1)
}
