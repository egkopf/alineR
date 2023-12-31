raw.alignment <-function(s,Syllabic=20,Place=70,Stop=90,Voice=15,Nasal=15,Retroflex=15,Lateral=5,Aspirated=1,Long=5,High=5,Back=5,Round=5,sk=15)
  {
    
    ### Input Arguments ###  
    features<-as.integer(c(Syllabic,Place,Stop,Voice,Nasal,Retroflex,Lateral,Aspirated,Long,High,Back,Round))
    
    ### C++ interface ####
    z<-.Call("exchange",s,features,as.integer(sk * -100)) # modified by EK to match (conv) of other gP constants
    m<-list(s,z[[3]],z[[1]],z[[2]])
    names(m)<-c("word pair","similarity score","alignment1","alignment2")
    return(m)    
  }
