\name{spp}
\alias{spp}
\title{
Sales Per Person
}
\description{
It shows the performance  of a salesperson(s) in terms of sales amount achieved against the 
target 
}
\usage{
spp(sdt, ss)
}
\arguments{
  \item{sdt}{a sales amount targetted}
  \item{ss}{a sales amount achieved by the salesperson(s) in the second column of a dataframe 
   containing names/ID of salesperson(s) in the first column} 
}
\details{
Colour coding is used to display the performance of a salesperson in relation to a target for
the barchart visual produced by the function. A bar coloured orange indicates that the 
salesperson has met the target; a green bar indicates the salesperson has exceeded the target and a red bar 
 and a red bar indicates the person is below the target. Sales amouns are in monetary values
of a given currency
}
\value{
  \item{comp1 }{a barplot showing the performance of the salesperson(s)}
  \item{comp2 }{a dataframe object of three columns for names/ID, sales amount, sales as
   percentage of target for the salesperson(s)}
}

\author{
Abubakar Akanbi
}

\examples{
df3 = data.frame(name=c('john','kelvin', 'ayo'), amnount= c(5084,8685,2058))
spp(df3, 5084)
}
