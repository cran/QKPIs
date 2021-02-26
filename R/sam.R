sam <-
function(status, tgoal, dtev){
 assign("snm", c(NULL), envir = .BaseNamespaceEnv); 
 assign("dfr", NULL, envir = .BaseNamespaceEnv)
  if(is.data.frame(status))
   {nm  <- ncol(status)
   for (i in 1:nm) {snm <- append(snm,sum(status[[i]]), length(snm))}; avst <-  sum(snm)/ncol(status)
    dfr = avst -  tgoal}
   else
    {dfr <- status - tgoal}

 pc.dfr  = (dfr/tgoal)*100
 pc.tm = signif((avst/tgoal)*100, 2)
 pc.tm1 = signif((sum(snm)/tgoal)*100, 2)
 mdp <- barplot(snm, beside = T, plot = FALSE)
 mdp <- seq(colMeans(mdp), by = colMeans(mdp), colMeans(mdp) * length(snm))

 if((dfr > 0 && is.vector(snm)) && ( (!(missing(dtev)))) ) 
   {plot(mdp, snm, type = "b", lwd = 3, main = "Sales", 
    xlab = "Monthly Sales", ylab = "Amount", xaxt = "n", col = "green")
   axis(1, mdp, month.abb[dtev:length(snm)])
   legend("topright", c("Green is Good (+ve)"), lty = 1, lwd = 2, bty = "n", col = "green")
   mop = par("usr")
   x = (mop[1] + mop[2])/2; y = (mop[3] + mop[4])/2
   text(x, y, paste0(signif(eval(expression( (dfr/tgoal)*100)), 2),"% ahead"), col = "darkgreen")
   list(paste0("The average sales target, at this time, is exceeded by an amount of ", dfr),
    data.frame(Status_average_sales = avst, Total_sales = sum(snm))) }

  else

  if ((dfr < 0 && is.vector(snm)) && ( (!(missing(dtev))))  )
   {plot(mdp, snm, type = "b", lwd = 3, main = "Sales", 
    xlab = "Monthly Sales", ylab = "Amount", xaxt = "n", col = "red")
    axis(1, mdp, month.abb[dtev:length(snm)])
    legend("topright", c("Red is Bad (-ve)"), lty = 1, lwd = 2, bty = "n", col = "red")
    mop = par("usr")
    x = (mop[1] + mop[2])/2; y = (mop[3] + mop[4])/2
    text(x, y, paste0(signif(eval(expression( (dfr/tgoal)*100)), 2),"% behind"), col = "red")
    list(paste0("An amount of ", dfr," is required to meet the average sales target at this time"),
        data.frame(Status_average_sales = avst, Total_sales = sum(snm))) }

  else

  if((dfr == 0 && is.vector(snm)) && ( (!(missing(dtev))))  )
   {plot(mdp, snm, type = "b", lwd = 3, main = "Sales", 
    xlab = "Monthly Sales", ylab = "Amount", xaxt = "n", col = "orange")
   axis(1, mdp, month.abb[dtev:length(snm)])
   legend("topright", c("Orange is Neutral"), lty = 1, lwd = 2, bty = "n", col = "orange")
   mop = par("usr")
   x = (mop[1] + mop[2])/2; y = (mop[3] + mop[4])/2
   text(x, y, c("Target is achieved"), col = "orange") 
   list(paste0("The average sales target is achieved at this time"),
     data.frame(Status_average_sales = avst, Total_sales = sum(snm)))}

  else
  if(missing(dtev)){list(paste0("The current sales is ", 
    pc.tm,"% of target sales with a difference of: ", dfr ), Annual_sales = sum(snm))}
 }
