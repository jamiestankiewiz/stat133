#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)



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


#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  
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
  
  
  #DEFINE NUMBER OF ITERATIONS
  ITERATIONS=2
  
  #this is the iteration loop
  for(i in 1:ITERATIONS) {
    
    #TEST IF ODD or EVEN
    #EVEN case
    if(i %% 2 == 0) {
      
      
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
      
      
      #col loop
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
            #!!!! THIS IS THE SPECIAL CASE!!!!!!
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
              #CYCLE TEST CELL UP UNTIL DID NOT FIND A 1
              
              while(m[testROW,testCOL]==CARCOLOR) {
                
                #move test cell up for special case
                testROW=(testROW-1)
              } #--end check line of cars in special mode
              
              
              ## if it got here, there is NOT a 2 here (Not a BLUE CAR HERE)
              if(m[testROW,testCOL]==1) {
                #do nothing because it is blocked by a RED car
                
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
                #SO PUT A BLUE CAR HERE (RED=1)
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
              
              
              #still within the special case:
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
            } #--endif CAR IS IN SPECIAL CASE
            
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
    ########################################################
    
    #odd case
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
      #odd case, moving red cars
      
      if(i %% 2 == 1) {
        
        
        if(mTEST==TRUE) {
          print("===========================")
          print(c("STARTING ODD/EVEN AS: ",i))
          print("===========================")
          if(mBROWSE==TRUE) {
            browser()
          }
        } #--end mTEST    
        
        
        #DEFINE CAR COLOR AS RED(RED=1)
        CARCOLOR=1
        
        
        #row loop is now the outer loop
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
                  #SO PUT A RED CAR HERE (RED = 1)
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
                
                
                #so NEW ENDCOL is the cell above where the SPECIAL CASE CAR GOT MOVED TO
                endCOL=(testCOL-1)
                
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
                  #SO PUT A RED CAR HERE (RED = 1)
                  m[testROW, testCOL] = 1
                  #go back to current car and zero out
                  m[curROW, curCOL] = 0
                  # if the testCELL is 1 or 2, it must not have moved because we started from the top. 
                  #so don't do anything.
                  
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
    
    
  } #--end FOR ITERATIONS
  


grid.new <-   
   return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  
  bml.init(r,c,p)
 
  
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
  
  
  #DEFINE NUMBER OF ITERATIONS
  ITERATIONS=2
  
  #this is the iteration loop
  for(i in 1:ITERATIONS) {
    
    #TEST IF ODD or EVEN
    #EVEN case
    if(i %% 2 == 0) {
      
      
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
      
      
      #col loop
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
            #!!!! THIS IS THE SPECIAL CASE!!!!!!
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
              #CYCLE TEST CELL UP UNTIL DID NOT FIND A 1
              
              while(m[testROW,testCOL]==CARCOLOR) {
                
                #move test cell up for special case
                testROW=(testROW-1)
              } #--end check line of cars in special mode
              
              
              ## if it got here, there is NOT a 2 here (Not a BLUE CAR HERE)
              if(m[testROW,testCOL]==1) {
                #do nothing because it is blocked by a RED car
                
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
                #SO PUT A BLUE CAR HERE (RED=1)
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
              
              
              #still within the special case:
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
            } #--endif CAR IS IN SPECIAL CASE
            
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
    ########################################################
    
    #odd case
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
      #odd case, moving red cars
      
      if(i %% 2 == 1) {
        
        
        if(mTEST==TRUE) {
          print("===========================")
          print(c("STARTING ODD/EVEN AS: ",i))
          print("===========================")
          if(mBROWSE==TRUE) {
            browser()
          }
        } #--end mTEST    
        
        
        #DEFINE CAR COLOR AS RED(RED=1)
        CARCOLOR=1
        
        
        #row loop is now the outer loop
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
                  #SO PUT A RED CAR HERE (RED = 1)
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
                
                
                #so NEW ENDCOL is the cell above where the SPECIAL CASE CAR GOT MOVED TO
                endCOL=(testCOL-1)
                
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
                  #SO PUT A RED CAR HERE (RED = 1)
                  m[testROW, testCOL] = 1
                  #go back to current car and zero out
                  m[curROW, curCOL] = 0
                  # if the testCELL is 1 or 2, it must not have moved because we started from the top. 
                  #so don't do anything.
                  
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
    
    
  } #--end FOR ITERATIONS
  
  
  
 
}