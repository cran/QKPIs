lperfm <-
function(twk, tbr, tdwn, stdr, pprd){
 twk <- twk*60; stdr <- stdr/60; mpp <- stdr
 assign("tav", 0, envir = .BaseNamespaceEnv)

  if (!(missing(tbr)) && !(missing(tdwn))) {tbr <- tbr*60; tdwn <- tdwn*60; tav <- twk - tbr - tdwn}
  else {tav <- twk} 
  if((missing(tdwn)) && !(missing(tbr))) {tbr <- tbr*60; tav <- twk - tbr}
  else {tav <- twk}
  if((missing(tbr)) && !(missing(tdwn))) {tdwn <- tdwn*60; tav <- twk - tdwn}
  else {tav <- twk}
 if ((missing(tbr)) && (missing(tdwn))) {tbr <-tbr*60; tav <- twk}
  else{tav <- twk - tbr - tdwn}
 
 trqrd <- pprd*mpp
 perfm <- round(trqrd/tav*100, 2)
 paste0("Labour performance  = ", perfm,"%")
}
