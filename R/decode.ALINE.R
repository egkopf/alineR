# Copyright (c) 2015 by Sean Downey
# Authors: Sean Downey (sean@codexdata.com) and Guowei Sun (gwsun@umd.edu)
# This software is distributed under GPL-3.

decode.ALINE <-
function(x,y) {
  map<-read.csv("./data/aline_mapset_workingcopy.csv")
  diacritics<-read.csv("./data/aline_diacritic_set.csv")
  
  splitx<-strsplit(x,"")
  splity<-strsplit(y,"")  
  
  # put in the first token split by character
  t<-splitx[[1]]
  
  # make sure every character is present
  j<-1
  while (j<=length(t)) {
    catch<-FALSE
    
    for(i in 1:length(map$U.Val)){
      if(t[j]==intToUtf8(map$U.Val[i])){
        catch=TRUE
        break
      }
    }
    if(!catch){
      invalid = TRUE
      
      for(i in 1:length(diacritics$U.Val)){
        if(t[j]==intToUtf8(diacritics$U.Val[i])){
          if ((diacritics$Aline[i] == "") || (strtoi(strsplit(diacritics$A.Val[i][1], " ")[1][[1]][1]) <= 90)) {
            t<-t[-j]
          }
          invalid = FALSE
        }
      }
      if (invalid) {
        message(paste("Invalid character:",t[j],"dropped in alignment"))
        t<-t[-j]
      }
    }
    j<-j+1
  }
  
  #put it back in?
  splitx[[1]]<-t
  
  num<-length(splity[[1]])
  J<-rep(NA,length(splitx[[1]])) 
  j=1    
  
  # creates the array of present slot indexes in 'splity'
  for(i in 1:(num-1))
  {
    # if there's a space in the next spot
    if (!splity[[1]][i]==" " && (i==1||splity[[1]][i-1]==" ")) # modified by EK to fix off-by-one error
    {
      J[j]=i
      j=j+1    
    }
  }

 aligned_y=rep(" ",num)
  
p=0
  
 for (i in 1:(length(J)))
 { 
   if(splity[[1]][J[i]]=="|"||splity[[1]][J[i]]=="<"||splity[[1]][J[i]]=="-")   
   {
     # stick it in the same slot
     aligned_y[J[i]]<-splity[[1]][J[i]]
     p=p+1
   }
   else{
     # possible error with codes that are two characters long
   aligned_y[J[i]]=splitx[[1]][(i-p)]   
   }
 }
  z<-paste(aligned_y,sep=' ',collapse='')
  return(z)
}
