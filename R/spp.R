spp <-
function(sdt, ss){
  assign("psl", c(NULL), envir = .BaseNamespaceEnv)
  for (i in 1:length(sdt[[2]])){psl <- append(psl, sdt[[2]][i] - ss, length(psl))}
  SS <- rep(ss, length(psl))
  fcol1 <- function(sp){
  sp = sdt[[2]]
  assign("rpl1", c(NULL), envir = .BaseNamespaceEnv)
  for(i in 1:length(sp))
     {if(sp[i] > ss){rpl1 <- append(rpl1, "green", length(rpl1))}
       else
       if(sp[i] < ss){ rpl1 <- append(rpl1, "red", length(rpl1))}
       else{ rpl1 <- append(rpl1, "orange", length(rpl1))}
    }
  rpl1 
}
 df <- data.frame(Sales_per_person = sdt[[2]], Expected_Sales = SS, Name = sdt[[1]] )
 barplot(sdt[[2]], col = fcol1(df), names.arg = sdt[[1]], xlab = "Names/ID", ylab  = "Sales") 
 legend("topright", legend = c("beyond target", "target", "below target"), fill = c("green","orange", "red"))

 assign("status", c(NULL), envir = .BaseNamespaceEnv)
 for(i in 1:length(psl)){status <- append(status, round(sdt[[2]][i]/ss * 100, 0), length(status))}
    print(length(status)); df1 = data.frame(sdt, Percent_of_target = status)
 df1
}
