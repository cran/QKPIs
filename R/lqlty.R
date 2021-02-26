lqlty <-
function(sprt,tprt){
 qlty <- round(sprt/tprt*100, 2)
paste0("Labour quality  = ", qlty,"%")
}
