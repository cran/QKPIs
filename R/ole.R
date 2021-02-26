ole <-
function(lavail, lperfm, lqlty){
 le = round((lavail*lperfm*lqlty/10000), 2)
 paste0("Overall Labour Effectiveness  = ",  le,"%")
}
