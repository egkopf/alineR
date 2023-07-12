# Copyright (c) 2015 by Sean Downey
# Authors: Sean Downey (sean@codexdata.com) and Guowei Sun (gwsun@umd.edu)
# This software is distributed under GPL-3.

encode.ALINE <-
  function(x, mark=FALSE){
    map<-read.csv("./data/aline_mapset_workingcopy.csv")
    diacritics<-read.csv("./data/aline_diacritic_set.csv")

    v<-intToUtf8(c(65:90, 97:122), multiple=T) 
    `%nin%` <- function (x, table) match(x, table, nomatch = 0L) == 0L 
    
    sapply(x, USE.NAMES=T, FUN=function (word){
      out<-""
      for (i in unlist(strsplit(word, "")) ){
        
        catch<-FALSE
        
        for(j in 1:nrow(map)){ 
          if( utf8ToInt(i) == map$U.Val[j] ){
            i <- intToUtf8(unlist(strsplit(as.character(map$A.Val[j]), " ")))
            catch<-TRUE
            break
          } 
        }

        if (!catch) {
          for (j in 1:nrow(diacritics)) {
            if( utf8ToInt(i) == diacritics$U.Val[j] ){
              i <- intToUtf8(unlist(strsplit(as.character(diacritics$A.Val[j]), " ")))
              catch<-TRUE
              break
            }
          }
        }
        
        if (!catch && i %nin% v){
          # message(paste("Invalid character: ",i,"in ",word))
          i <- ifelse(mark,'@','') 
        }
        
        out<-append(out,i)
      }
      
      paste(out,sep='',collapse='')
    } 
    )
  }
