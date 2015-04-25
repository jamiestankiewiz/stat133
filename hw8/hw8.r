xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

#######################################################################################################
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
   
   #==== genBootY uses CASE RESAMPLING of the data
   #==== see HW8.pdf #1
   
   
#tapply(x, y, function(y) sample(y, length(y), replace=T))
   tempy<- tapply(y,x, function(y) sample(y, length(y), replace=T))
   return(as.vector(unlist(tempy)))
#return(tapply(y,x, sample, replace=T, size=length(y)))

#yvals <- sample(y, length(y), replace=T)
#tapply(x, y, function(y) sample(y, length(y), replace=T))
}

#genBootY(c(1,2,2,3,4), c(4,5,6,7,8))
########################################################################################################


genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
   
  
   
   tempy<- tapply(err,fit, function(err) sample(err, length(err), replace=T))
   y <- fit + tempy
  return(y)
}



############################################################################
fitModel = function(x, y, degree = 1){
  #as## use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
   
  ### NOTE: degree=1 is for LINE MODEL
  ### NOTE: degree=2 is for QUADRATIC MODEL
  ### QUESTION:  why is DEGREE always set to 1???
   if(degree==1){
   mymodel <- lm(y ~ x)
   
   fitted <- mymodel$fitted
   err <- mymodel$residuals
   
   newmodel <- genBootR(fitted, err)
   
   
   coeff <- mymodel$coefficients
   }
   
   else{
      mymodel <- lm(y~x + I(x^2))
      
      fitted <- mymodel$fitted
      err <- mymodel$residuals
      
      newmodel <- genBootR(fitted, err)
      
      
      coeff <- mymodel$coefficients
   }
   
  return(coeff)
  
} #end all


##############################################################
oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  

 
  ### Use fitModel to fit a model to this bootstrap Y 
   #####
   oneBoot = function(data, fit = NULL, degree = 1){
      ###  data are either your data (from call to getData)
      ###  OR fit and errors from fit of line to data
      ###  OR fit and errors from fit of quadratic to data  
      ### Use fitModel to fit a model to this bootstrap Y 
      
      #case1/2
      if(degree==1){
         
         #condition 1
         if(is.null(fit) == FALSE){
            ##generates random y vals
            dataline <- genBootY(data[,2],data[,1])  
            ##gives you new coeffs
            return(fitModel(data[,1], dataline, degree=1)) 
         }
         
         #condition 2
         else{
            #you DO have a model
            ynew <- genBootR(fit[,1], fit[,2])
            return(fitModel(data[,1], ynew, degree=1))
         }
      }
      
      #case2/2
      if (degree==2) {
         
         #condition 1
         if(is.null(fit) == FALSE){
            ##generates random y vals
            dataline <- genBootY(data[,2],data[,1])  
            ##gives you new coeffs
            return(fitModel(data[,1], dataline, degree=2)) 
         }
         
         #condition 2
         else{
            #you DO have a model
            ynew <- genBootR(fit[,1], fit[,2])
            return(fitModel(data[,1], ynew, degree=2))
         }
      }
   }

#############################################################
repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
   
   #==== first, this will be in a for loop for 1000 times
   #==== then inside loop, seems like data will be loaded for a call to ONEREP
   #==== but it will call ONEREP 4x and each time the call will be set up for each of 4 variations
   #==== also NOTE that each run returns 4 coefficients
   #==== which is why the output is a LIST of 4 sets of co-efficients
   
   
   #####Set up condition (making fit matrix)
   mymodel <- lm(y ~ x)
   
   fitted <- mymodel$fitted
   err <- mymodel$residuals

   fit <- matrix(0, nrow=length(fitted), ncol=2)
   fit[,1] <- fitted
   fit[,2] <- err
   
   
   mata <- matrix(0, 2, 1000)
   matb< <- matrix(0,2, 1000)
   matc <- matrix(0,3,1000)
   matd <- matrix(0,3,1000)
   
   
   listof4[1] <- mata
   
   listof4[2] <- matb
   
   listof4[3] <- matc
   
   listof4[4] <- matd
   
   # set up loop for B=1000 times
   for(i in 1:B) {
   
      #first setup the LIST of 4 as a blank
      listof4[i] <- vector("list", 4)
      
      
      #call ONEREP 4x for each of 4 variations
      #and save output of each into that LIST of 4
      
      # number 1 variation: basic genBootY
      
    
         mata[,i] <- oneBoot(data, fit=NULL, degree=1) 
      
      
      # number 2 variation: basic genBootR
      
         matb[,i] <- oneBoot(data, fit=NULL, degree=2)

      # number 3 variation: LINE version
      
     
         matc[,i] <- oneBoot(data, fit, degree=1)
      
      # number 4 variation: quADRatic X2

         matd[,i] <- oneBoot(data, fit, degree=2)
   }

   
   for (i in 1:4){
listof4[[i]] <- data.frame(listof4[i]) 
}

return(listof4)
} 


##########################################################################
bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data

  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out

     #plot each of 1000 curves from coefficients
     plot(x,y)
     mapply(plot,coeff, add=T)
     
     
  #now write the 1 true data
  plot(trueCoeff,y, add=T)
}


################################################################
### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}

#####################################################
runSim

#done
