# Copyright (c) 2015 by Sean Downey
# Authors: Sean Downey (sean@codexdata.com) and Guowei Sun (gwsun@umd.edu)
# This software is distributed under GPL-3.

italian <-
  
  
  function(x, mark=FALSE, m1=NULL, m2=NULL){
    
    VOWELS = c("a", "à", "e", "è", "é", "i", "ì", "í", "o", "ò", "ó", "u", "ù", "ú")
    I_VOWELS = c("i", "ì", "í")
    SOFT_VOWELS = c("e", "è", "é", "i", "ì", "í")
    HARD_VOWELS = c("a", "à", "o", "ò", "ó", "u", "ù", "ú")
    
    CONSONANTS = c("b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s" ,"t", "v", "w", "x", "y", "z")
    VOICED_CONSONANTS = c("b","d","g","m")
    
    STANDARD_CONSONANTS = c("b","d","f","l","m","n","p","r","t","v")
    DYNAMIC_CONSONANTS = c("c","g")
    LOAN_CONSONANTS = c("j","k","w","x","y")
    EXCEPTIONS = c("q","h","s","z")
    
    PUNCTUATION = c("'")
    
    
    geminate<-function(x,index) {
      if (length(x) == index) {return(FALSE)}
      return(x[index] == x[index+1])
    }
    
    map<-map(m1,m2)
    ipaRep<-""
    index<-1
    xWord<-unlist(strsplit(tolower(x), ""))
    
    
    while (index <= length(xWord)) {
      
      if(xWord[index] %in% STANDARD_CONSONANTS) {
        if (geminate(xWord, index)) {
          ipaRep = append(ipaRep, xWord[index])
          ipaRep = append(ipaRep, ":")
          index<-index+2
          next
        } 
        else{
          ipaRep = append(ipaRep, xWord[index])
          index<-index+1
          next
        }
      }
      
      if(xWord[index] %in% EXCEPTIONS) {
        if (xWord[index] == "q" && xWord[index+1] == "u" && xWord[index+2] %in% VOWELS) {
          ipaRep = append(ipaRep, "k")
          ipaRep = append(ipaRep, "w")
          index<-index+2
          next
        }
        if (xWord[index] == "z") {
          if (geminate(xWord, index)) {
            ipaRep = append(ipaRep, "t")
            ipaRep = append(ipaRep, "s")
            index<-index+2
            next
          } else{
            ipaRep = append(ipaRep, "d")
            ipaRep = append(ipaRep, "z")
            index<-index+1
            next
          }
        }
        if (xWord[index] == "s") {
          if (geminate(xWord, index)) {
            ipaRep = append(ipaRep, "s")
            ipaRep = append(ipaRep, ":")
            index<-index+2
            next
          }
          if (xWord[index +1] %in% VOICED_CONSONANTS) {
            ipaRep = append(ipaRep, "z")
            index<-index+1
            next
          } else {
            if (xWord[index +1] == "c") {
              if (xWord[index +2] %in% SOFT_VOWELS) {
                ipaRep = append(ipaRep, "ʃ") # i error
                index<-index+2
                next
              } else {
                ipaRep = append(ipaRep, "s")
                ipaRep = append(ipaRep, "k")
                index<-index+2
                next
              }
              
            } else {
              
              if (xWord[index -1] %in% VOWELS && xWord[index +1] %in% VOWELS){
                ipaRep = append(ipaRep, "z")
                index<-index+1
                next
              }
              else {
                ipaRep = append(ipaRep, "s")
                index<-index+1
                next
              }
              
            }
          }
            
        }
        if (xWord[index] == "h") {
          index<-index+1
          next
        }
      }
      
      if(xWord[index] %in% LOAN_CONSONANTS) {
        if (xWord[index] == "j" || xWord[index] == "y") {
          ipaRep = append(ipaRep, "j")
          index<-index+1
          next
        }
        if (xWord[index] == "k") {
          ipaRep = append(ipaRep, "k")
          index<-index+1
          next
        }
        if (xWord[index] == "w") {
          ipaRep = append(ipaRep, "w")
          index<-index+1
          next
        }
        if (xWord[index] == "x") {
          ipaRep = append(ipaRep, "k")
          ipaRep = append(ipaRep, "s")
          index<-index+1
          next
        }
      }
      
      if(xWord[index] %in% DYNAMIC_CONSONANTS) {
        gemStatus = FALSE
        
        if (geminate(xWord, index)) {
          index<-index+1
          gemStatus = TRUE
        }
        
        if (xWord[index + 1] == "h") {
          if (xWord[index] == "c") {
            ipaRep = append(ipaRep, "k")
            index<-index+2
            if (gemStatus) {
              ipaRep = append(ipaRep, ":")
            }
            next
          }
          if (xWord[index] == "g") {
            ipaRep = append(ipaRep, "g")
            index<-index+2
            if (gemStatus) {
              ipaRep = append(ipaRep, ":")
            }
            next
          }
        }
        if (xWord[index+1] %in% SOFT_VOWELS) {
          if (xWord[index] == "c") {
            ipaRep = append(ipaRep, "t")
            ipaRep = append(ipaRep, "ʃ")
            if (xWord[index+1] == "i" && xWord[index+2] %in% HARD_VOWELS) {
              index<-index+2
            } else {
              index<-index+1
            }
            next
          }
          if (xWord[index] == "g") {
            ipaRep = append(ipaRep, "d")
            ipaRep = append(ipaRep, "ʒ")
            if (xWord[index+1] == "i" && xWord[index+2] %in% HARD_VOWELS) {
              index<-index+2
            } else {
              index<-index+1
            }
            next
          }
        } else {
          if (xWord[index] == "c") {
            ipaRep = append(ipaRep, "k")
            if (gemStatus) {
              ipaRep = append(ipaRep, ":")
            }
            index<-index+1
            next
          }
          if (xWord[index] == "g") {
            if (xWord[index +1] == "l") {
              ipaRep = append(ipaRep, "ʎ")
              index<-index+3
              next
            } else {
              ipaRep = append(ipaRep, "g")
              index<-index+1
              next
            }
          }
        }
        
      }
      
      if(xWord[index] %in% VOWELS) {
        if (xWord[index] == "a" || xWord[index] == "à") {
          ipaRep = append(ipaRep, "a")
          index<-index+1
          next
        }
        if (xWord[index] == "e" || xWord[index] == "è" || xWord[index] == "é") {
          ipaRep = append(ipaRep, "e")
          index<-index+1
          next
        }
        if (xWord[index] == "ì" || xWord[index] == "í") {
          ipaRep = append(ipaRep, "i")
          index<-index+1
          next
        }
        if (xWord[index] == "i") {
          if (xWord[index+1] %in% VOWELS) {
            ipaRep = append(ipaRep, "j")
            index<-index+1
            next
          } else {
            ipaRep = append(ipaRep, "i")
            index<-index+1
            next
          }
        }
        if (xWord[index] == "o" || xWord[index] == "ò" || xWord[index] == "ó") {
          ipaRep = append(ipaRep, "o")
          index<-index+1
          next
        }
        if (xWord[index] == "u" || xWord[index] == "ù" || xWord[index] == "ú") {
          ipaRep = append(ipaRep, "u")
          index<-index+1
          next
        }
      }
      
      return("Invalid italian word.")
      
    }
    
    return(paste((ipaRep), collapse=""))
    

  }
