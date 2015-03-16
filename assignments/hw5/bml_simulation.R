#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.


bml.init <- function(r, c, p){
  # INIT MATRIX v1
  
  m<-matrix(0,r,c)
  
  for(i in 1:(trunc(p*r*c))) {
    free=1
    while(free==1){
      ROW = sample(1:r,1)
      COL = sample(1:c,1)
      #r=10
      #c=8
      
      print(c("Iteration: ", i))
      
      #print(c(r,c))
      print(c("Cell: ", ROW, COL, "currently shows:",m[ROW,COL]))
      #    browser()
      
      if(m[ROW,COL] == 0) {
        #space is free, so put a car 
        m[ROW,COL] = sample(1:2,1) 
        #bail out of hit loop
        free = 0
        
        print(c("Cell: ", ROW, COL, "now       shows:",m[ROW,COL]))
        #    browser()
      } 
      #so if was a car there, re-run inner loop
      #so don't need else statement
    }
  }
  return(m)
  #return(m)
}


 ###################################################################
###################################################################
bml.step <- function(m){
  
  
  r <- nrow(m)
  c <- ncol(m)
  
  #save the opening grid for later comparison
  mSTART = m
  
  #This is the TEST SWITCH
  mTEST=TRUE
  mTEST=FALSE
  
  #This is the BROWSER TEST SWITCH
  mBROWSE=TRUE
  mBROWSE=FALSE
  
  #This switch stops after every iteration
  mEACHITERATION=TRUE
  
  #FIRST, DEFINE MATRIX GRID
  endROW= r
  endCOL= c
  
  #SECOND, PRESERVE ORIGINAL SIZE OF MATRIX
  ORIGendROW=endROW
  ORIGendCOL=endCOL
  
  #DEFINE START ITERATION number (example start at 2)
  mSTARTITERATION=1
  
  #DEFINE NUMBER OF ITERATIONS
  ITERATIONS=2
  
  #this is the iteration loop
  for(i in mSTARTITERATION:ITERATIONS) {
    #for(i in 1:ITERATIONS) {
    
    #TEST IF ODD or EVEN
    #this is a little harder than it sounds
    #so create a simple test function
    #if (i %% 10 == any(c(1,3,5,7,9)))
    if(i %% 2 == 0) {
      
      
      
      #---------------------------------
      # THIS IS THE EVEN ITERATION CODE
      # which cycles thru ROWS first
      #---------------------------------
      
      if(mTEST==TRUE) {
        print("===========================")
        print(c("STARTING ODD/EVEN AS: ",i))
        print("===========================")
        if(mBROWSE==TRUE) {
          browser()
        }
      } #--end mTEST    
      
      
      #DEFINE CAR COLOR
      CARCOLOR=2
      
      
      #go thru inner COL LOOP 1-END
      #define the first COL loop
      for(curCOL in 1:endCOL) {
        
        
        if(mTEST==TRUE) {
          print("===========================")
          print(c("STARTING COLUMN: ",curCOL))
          print("===========================")
          if(mBROWSE==TRUE) {
            browser()
          }
        } #--end mTEST    
        
        
        
        
        #define curROW
        curROW=1
        #reset the endROW to end size of matrix
        endROW=ORIGendROW
        
        while (curROW <= endROW) {
          #        while (curROW < endROW) {
          #        for (curROW in 1:endROW) {
          
          
          #START AT ROW 1
          #curROW=1
          
          #print(c("got to col loop", curCOL))
          #AT THIS POINT, I KNOW THE CURRENT CELL
          # WHICH IS AT curROW,curCOL
          if(mTEST==TRUE) {
            print(c("got to current cell ", curROW, curCOL))
          }
          
          #FIRST, test if the current cell is a 2 meaning a BLUE CAR is there
          if (m[curROW,curCOL] == CARCOLOR) {
            
            
            #DEFINE TEST CELL FOR THIS MOVE
            testROW=curROW-1
            testCOL=curCOL
            
            if(mTEST==TRUE) {
              print(c("defined test row: ",testROW))
              if(mBROWSE==TRUE) {
                browser()
              }    
            } #--end mTEST
            
            #TEST IF the test cell IS OFF MAP
            #!!!!## THIS IS THE SPECIAL CASE!!!!!!
            #remember that if it goes into the special case, 
            #the endROW will potentially change.
            
            if (testROW < 1) {
              #move test row back to bottom
              testROW=endROW
              
              if(mTEST==TRUE) {
                print("test: off map")
                if(mBROWSE==TRUE) {
                  browser()
                }
              } #--end mTEST
              
              #DO a checking loop
              #check if cells above it are 0,1 or 2
              #redo loop if it = 1, otherwise exit checking loop
              #CYCLE TEST CELL UP UNTIL DID NOT FIND A 2
              
              while(m[testROW,testCOL]==CARCOLOR) {
                
                #move test cell up for special case
                testROW=(testROW-1)
              } #--end check line of cars in special mode
              
              
              ## if it got here, there is NOT a 2 here (Not a BLUE CAR HERE)
              if(m[testROW,testCOL]==1) {
                #do nothing because it is blocked by a RED CAR
                
                if(mTEST==TRUE) {
                  print("Car was BLOCKED")
                  if(mBROWSE==TRUE) {
                    browser()
                  }    
                }  #--end mTEST         
              } #--endif TEST BLOCKED
              
              else { #-- BLUE CAR IS IN THE MIDDLE OF MATRIX
                #this is equivalent to:  if (testROW >=1 )
                #this means the space MUST have been a 0
                #SO PUT A BLUE CAR HERE (BLUE=2)
                m[testROW, testCOL] = 2
                #go back to original 1,1 and zero out
                m[curROW, curCOL] = 0
                
                if(mTEST==TRUE) {
                  print("MOVED CAR SPECIAL CASE")
                  if(mBROWSE==TRUE) {
                    browser()
                  }    
                } #--end mTEST
                #--endif NOT A SAME COLOR CAR HERE 
              } #--endif TESTROW OFF MAP          
              
              
              #----------------------------------------------------
              #move END to new endROW rather than end of matrix row
              #----------------------------------------------------
              
              #so NEW ENDROW is the cell above where the SPECIAL CASE CAR GOT MOVED TO
              endROW=(testROW-1)
              #endROW=(testROW)
              #endROW=(testROW+1)
              
              if(mTEST==TRUE) {
                print("CHANGED END ROW")
                if(mBROWSE==TRUE) {
                  browser()
                }
              }  #--end mTEST
            } #--endif CAR IS RIGHT COLOR
            
            else {
              #move car NOT SPECIAL CASE
              #THERE IS A FREE SPACE TO MOVE TO
              if (m[testROW, testCOL] == 0) {
                #PUT A BLUE CAR HERE (BLUE=2)
                m[testROW, testCOL] = 2
                #go back to current car and zero out
                m[curROW, curCOL] = 0
                # if the testCELL is 1 or 2, it must not have moved 
                # because we started from the top. so don't do anything.
                
                if(mTEST==TRUE) {
                  print("MOVED CAR =NOT SPECIAL CASE")
                  if(mBROWSE==TRUE) {
                    browser()
                  }
                } #--end mTEST
              } #--endif MOVE CAR
            } #--endif CAR IS SAME COLOR
          } #--end while ROW LOOP
          
          #------------------------
          #increment current row
          #------------------------
          curROW=curROW+1
          #once this has finished, it will return to the ROW loop. 
          #we still need to consider that the endROW has changed if it went into the special case.  
          
          if(mTEST==TRUE) {
            print(c("INCREMENTED CURRENT ROW", curROW))
            if(mBROWSE==TRUE) {
              browser()
            }
          } #--end mTEST 
        } #--end while ROW LOOP
        
      } #--end for COLUMN LOOP 1 to endCOL
    } #--endif ODD or EVEN
    
   ########################################################
    
    else {
      
      
      
      if(mTEST==TRUE) {
        print("=================================")
        print(c("GOT TO START ODD ITERATION",ITERATIONS))
        print("=================================")
        if(mBROWSE==TRUE) {
          browser()
        }
      } #--end mTEST  
      
      
      
      #TEST IF ODD or EVEN
      #this is a little harder than it sounds
      #so create a simple test function
      #if (i %% 10 == any(c(1,3,5,7,9)))
      if(i %% 2 == 1) {
        
        
        
        #---------------------------------
        # THIS IS THE EVEN ITERATION CODE
        # which cycles thru COLUMNS first
        #---------------------------------
        
        if(mTEST==TRUE) {
          print("===========================")
          print(c("STARTING ODD/EVEN AS: ",i))
          print("===========================")
          if(mBROWSE==TRUE) {
            browser()
          }
        } #--end mTEST    
        
        
        #DEFINE CAR COLOR AS RED (RED=1)
        CARCOLOR=1
        
        
        #go thru inner ROW LOOP 1-END
        #define the first ROW loop
        for(curROW in 1:endROW) {
          
          
          if(mTEST==TRUE) {
            print("===========================")
            print(c("STARTING ROW: ",curROW))
            print("===========================")
            if(mBROWSE==TRUE) {
              browser()
            }
          } #--end mTEST    
          
          
          
          
          #define curCOL
          curCOL=1
          #reset the endCOL to end size of matrix
          endCOL=ORIGendCOL
          
          while (curCOL <= endCOL) {
            #        while (curROW < endROW) {
            #        for (curROW in 1:endROW) {
            
            
            #START AT COL 1
            #curCOL=1
            
            #print(c("got to ROW LOOP", curROW))
            #AT THIS POINT, I KNOW THE CURRENT CELL
            # WHICH IS AT curROW,curCOL
            if(mTEST==TRUE) {
              print(c("got to current cell ", curROW, curCOL))
            }
            
            #FIRST, test if the current cell is a 1 meaning a RED CAR is there
            if (m[curROW,curCOL] == CARCOLOR) {
              
              
              #DEFINE TEST CELL FOR THIS MOVE
              testROW=curROW
              testCOL=curCOL-1
              
              if(mTEST==TRUE) {
                print(c("defined test COL: ",testCOL))
                if(mBROWSE==TRUE) {
                  browser()
                }    
              } #--end mTEST
              
              #TEST IF the test cell IS OFF MAP
              #!!!!## THIS IS THE SPECIAL CASE!!!!!!
              #remember that if it goes into the special case, 
              #the endCOL will potentially change.
              
              if (testCOL < 1) {
                #move test col back to bottom
                testCOL=endCOL
                
                if(mTEST==TRUE) {
                  print("test: off map")
                  if(mBROWSE==TRUE) {
                    browser()
                  }
                } #--end mTEST
                
                #DO a checking loop
                #check if cells above it are 0,1 or 2
                #redo loop if it = 2, otherwise exit checking loop
                #CYCLE TEST CELL UP UNTIL DID NOT FIND A 2
                
                while(m[testROW,testCOL]==CARCOLOR) {
                  
                  #move test cell LEFT for special case
                  testCOL=(testCOL-1)
                } #--end check line of cars in special mode
                
                
                ## if it got here, there is NOT a 1 here (NOT A RED CAR HERE) 
                if(m[testROW,testCOL]==2) {
                  #do nothing because it is blocked by a BLUE car
                  
                  if(mTEST==TRUE) {
                    print("Car was BLOCKED")
                    if(mBROWSE==TRUE) {
                      browser()
                    }    
                  }  #--end mTEST         
                } #--endif TEST BLOCKED
                
                else { #-- RED CAR IS IN THE MIDDLE OF MATRIX
                  #this is equivalent to:  if (testCOL >=1 )
                  #this means the space MUST have been a 0
                  #SO PUT A RED CAR HERE (RED=1)
                  m[testROW, testCOL] = 1
                  #go back to original 1,1 and zero out
                  m[curROW, curCOL] = 0
                  
                  if(mTEST==TRUE) {
                    print("MOVED CAR SPECIAL CASE")
                    if(mBROWSE==TRUE) {
                      browser()
                    }    
                  } #--end mTEST
                  #--endif NOT A SAME COLOR CAR HERE 
                } #--endif TESTROW OFF MAP          
                
                
                #----------------------------------------------------
                #move END to new endCOL rather than end of matrix row
                #----------------------------------------------------
                
                #so NEW ENDCOL is the cell above where the SPECIAL CASE CAR GOT MOVED TO
                endCOL=(testCOL-1)
                #endCOL=(testCOL)
                #endCOL=(testCOL+1)
                
                if(mTEST==TRUE) {
                  print("CHANGED END COL")
                  if(mBROWSE==TRUE) {
                    browser()
                  }
                }  #--end mTEST
              } #--endif CAR IS RIGHT COLOR
              
              else {
                #move car NOT SPECIAL CASE
                #SO CELL IS FREE
                if (m[testROW, testCOL] == 0) {
                  #SO PUT A BLUE CAR HERE (RED=1)
                  m[testROW, testCOL] = 1
                  #go back to current car and zero out
                  m[curROW, curCOL] = 0
                  # if the testCELL is 1 or 2, it must not have moved because we started from the top. 
                  # so don't do anything.
                  
                  if(mTEST==TRUE) {
                    print("MOVED CAR =NOT SPECIAL CASE")
                    if(mBROWSE==TRUE) {
                      browser()
                    }
                  } #--end mTEST
                } #--endif MOVE CAR
              } #--endif CAR IS SAME COLOR
            } #--end while ROW LOOP
            
            #------------------------
            #increment current COL
            #------------------------
            curCOL=curCOL+1
            #once this has finished, it will return to the COL loop. 
            #we still need to consider that the endROW has changed if it went into the special case.  
            
            if(mTEST==TRUE) {
              print(c("INCREMENTED CURRENT COL", curCOL))
              if(mBROWSE==TRUE) {
                browser()
              }
            } #--end mTEST 
          } #--end while COL LOOP
          
        } #--end for ROW LOOP 1 to endROW
      } #--endif ODD or EVEN
      
      
    } #--end if ODD or EVEN
    
    image(m)  
  } #--end FOR ITERATIONS
  
  
  #save post manipulation of m
  mEND = m
  
  
  grid.new <- all(mSTART == mEND)
  
  return(list(m, grid.new))
}


#####################################################################
#####################################################################


bml.sim <- function(r, c, p, LOOPS){
  
  m <-  bml.init(r,c,p)
  
  k=1
  while( k <= LOOPS ) {
    
    print(c("LOOP:",k))
    
    #SAVE BEGINNING MATRIX FOR COMPARISON
    mSTARTCOMP <- m
    #    print("START")
    #    print(mSTARTCOMP)
    
    bml.step(m)
    
    
    #SAVE ENDING MATRIX FOR COMPARISON
    mENDCOMP <- bml.step(m)[[1]]
    #    print("END")
    #    print(mENDCOMP)
    
    m <- bml.step(m)[[1]]
    
    if(all(mSTARTCOMP == mENDCOMP)){
      print(c("SIMULATION reached GRIDLOCK at ITERATION:",k))
      return(k)
    }
    
    #increment LOOP COUNTER
    k=k+1 
    
  }  #--end while loop
  
  print("free flowing state. never reached gridlock.")
  # return k-1 since it just incremented to rowEND+1
  return(k-1)
  
} #end function loop


###########################################################################
##########################################################################

