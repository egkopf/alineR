raw.alignment <-function(s,Syllabic=10,Place=40,Stop=50,Voice=5,Nasal=5,Retroflex=5,Lateral=5,Aspirated=1,Long=5,High=5,Back=5,Round=5,sk=-1000)
  {
    
    ### Input Arguments ###  
    features<-as.integer(c(Syllabic,Place,Stop,Voice,Nasal,Retroflex,Lateral,Aspirated,Long,High,Back,Round))
    
    ### C++ interface ####
    z<-.Call("exchange",s,features,as.integer(sk))
    m<-list(s,z[[3]],z[[1]],z[[2]])
    names(m)<-c("word pair","similarity score","alignment1","alignment2")
    return(m)    
  }
