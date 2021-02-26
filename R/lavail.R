lavail <-
function(twk, tbr, tdwn){
 twk <- twk*60
 assign("tav", 0, envir = .BaseNamespaceEnv)
  if (!(missing(tbr))) {tbr <- tbr*60; tschl <- twk - tbr}
  else {tschl <- twk} 

 if (!(missing(tdwn))) {tdwn<- tdwn*60; tav <- tschl - tdwn} 
 else {tav <- tschl} 
lav = round(tav/tschl*100, 2)
paste0("Labour availability = ", lav,"%")
}
