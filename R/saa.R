saa <-
function(status, tgoal){
   dfr = status -  tgoal 
   pc.tm = signif((status/tgoal)*100, 2)
   paste0("The current sales is ", pc.tm ,"% of target sales with a difference of: ", dfr)
 }
