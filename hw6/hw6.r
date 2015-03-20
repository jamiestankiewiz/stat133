# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

## TESTING INITS (not needed when turned into a function)
## n.doctors <- 10
## p= 0.9
## n.days <- 10



#this is initializing the doctors of who originially adopted the drug
initial.doctors <- sample(0:1, size=n.doctors, replace=T, prob=c(.9,.1))


sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)
  
  m <- matrix(0,n.doctors,n.days)
  
  #STEP 2; set the first col = init
  m[,1] = initial.doctors
  # inital matrix done
  
  
  #STEP3; create a col loop
  for (col in 1:n.days) {
    
#    print("Top of COL loop")
    
    if((col+1) > n.days) {
      print(c("IF TEST col+1 > n.days", col+1, n.days))
      break 
    }
    
    else {  
      
#      print(c("ELSE TEST col+1 > n.days", col+1, n.days))
      
      #STEP 4: copy current col to the next col
      for(row in 1:n.doctors) {
        #      browser()
#        print(c("copying", row, col, "to", row, col+1))
        
        #copy to cell (row k,New Col) from cell (row k,Old Col)
        m[row,col+1] <-  m[row,col]
        #      browser()
      } #end copy loop
      
      #STEP5; select 2 doctors at random
      doc <- sample(1:n.doctors, 2)
      doc1 <- doc[1]
      doc2 <- doc[2]
#      print(c("picked 2 DOCTORS:", doc1, doc2))
#      print(c("---------------->DOCTORS:", m[doc1,col+1], m[doc2,col+1]))
      
      #STEP6; Choose from 1 of 4 possible conditions
      
#      print("START CHOOSING 1 of 4 CONDITIONS")
      
      #condition 1, both are 0
      if((m[doc1,col+1] == 0) & (m[doc2,col+1] == 0)){
#        print("Choosing CONDITION 1:")
#        print("0,0 do nothing")
#        print(c("OLD DOCS:",m[doc1,col+1], m[doc2,col+1]))
#        print(c("NEW DOCS:",m[doc1,col+1], m[doc2,col+1]))
        
#        browser()
      }
      
      #condition 2, doc1 ==1, doc2==0
      if((m[doc1,col+1] == 1) & (m[doc2,col+1] == 0)){
#        print("Choosing CONDITION 2:")
#        print(c("OLD DOCS:",m[doc1,col+1], m[doc2,col+1]))
        
        #doc1 ==1
        #do nothing
        
        #doc2 == 0
#        print("run prob func")
        newCELL <- sample(0:1, 1, prob = c(1-p, p))
        m[doc2, col+1] = newCELL
        
#        print(c("NEW DOCS:",m[doc1,col+1], m[doc2,col+1]))
        
#        browser()    
      }
      
      #conditon 3; doc1==0, doc2==1
      if((m[doc1,col+1] == 0) & (m[doc2,col+1] == 1)){
#        print("Choosing CONDITION 3:")
#        print(c("OLD DOCS:",m[doc1,col+1], m[doc2,col+1]))
        
        #doc1 ==0
#        print("run prob func")
        #    sample(0:1, 1, prob = c(1-p, p))
        newCELL <- sample(0:1, 1, prob = c(1-p, p))
        m[doc1, col+1] = newCELL
        
        #doc2 ==1
        #do nothing
        
#        print(c("NEW DOCS:",m[doc1,col+1], m[doc2,col+1]))
        
#        browser()      
      }
      
      #condition 4; doc1==doc2==1
      if((m[doc1,col+1] == 1) & (m[doc2,col+1] == 1)){
#        print("Choosing CONDITION 4:")
#        print(c("OLD DOCS:",m[doc1,col+1], m[doc2,col+1]))
        
#        print("both are 1s, so do nothing")
        
#        print(c("NEW DOCS:",m[doc1,col+1], m[doc2,col+1]))
        
#        browser()
        
      }
      
    } # end else loop
    print(p)
#    browser()
    
  }

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output
print(m)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

vec5 <- vector(mode="integer", length=n.days)
for (i in 1:n.days) {
  vec5[i] <- sum(mat5[,i])
}

vec4 <- vector(mode="integer", length=n.days)
for (i in 1:n.days) {
  vec4[i] <- sum(mat4[,i])
}

vec3 <- vector(mode="integer", length=n.days)
for (i in 1:n.days) {
  vec3[i] <- sum(mat3[,i])
}

vec2 <- vector(mode="integer", length=n.days)
for (i in 1:n.days) {
  vec2[i] <- sum(mat2[,i])
}


vec1 <- vector(mode="integer", length=n.days)
for (i in 1:n.days) {
  vec1[i] <- sum(mat1[,i])
}


plot((1:n.days),vec1, type="b", col="red", main="Number of doctors that have adopted drug each day", xlab="Day", ylab="Number of doctors that have adopted drug")
lines(x=1:n.days, vec2, type="b", col="orange")
lines(x=1:n.days, vec3, type="b", col="green")
lines(x=1:n.days, vec4, type="b", col="blue")
lines(x=1:n.days, vec5, type="b")
lty = c(1,1)
col= c("red", "orange", "green", "blue", "black")
legend(3,16.5, c("p=.9", "p=.7", "p=.5", "p=.3", "p=.1"), lty=lty, col=col)
