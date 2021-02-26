\name{QKPIs-package}
\alias{QKPIs-package}
\alias{QKPIs}
\docType{package}
\title{
Quantitative Key Performance Indicators 
}
\description{
It assesses quantitatively key performance indicators for sales and overall labour 
effectiveness 
}
\details{
Its purpose is to present the metrics of performance for sales of a company and salespersons  
on one hand and overall performance of  employees in the company or an organisation on the 
other. For sales metrics, the amin function, sam, takes three arguments: status, tgoal and 
dtev. The main function for Overall Labour Effectiveness is ole and takes three arguments - 
lavail, lperfm and qlty
Functions: sam, saa, spp, ole, lavail, lperfm, lqlty 
}
\author{
Abubakar Alkanbi

Maintainer: Abubakar Alkanbi
}
\references{
Overall Labor Effectiveness, Wikipedia, accessed on 19 January, 2021
}
\examples{
## For Sales metric
df1 = data.frame(jan =c(23455, 56432, 64344), Feb = c(21145, 43213, 76433), Mar = c(2355, 
86432, 64344), April= c(41149, 67746, 43203), May= c(85438, 05847,6849), 
June = c(5565,44253, 78253))
sam(status = df1, tgoal = 54635, dtev = 1)

## Overall Labour Effectiveness
## A workforce experiences, Availability of 87%, the Work Center Performance of 89.74% and
## Work Center Quality of 96%.
ole(lavail = 86.67, lperfm = 89.74, lqlty = 95.71)
}
