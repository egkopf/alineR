show.map<-function(){
  map<-read.csv("./data/aline_mapset_workingcopy.csv")
  # map<-read.csv("./data/aline_diacritic_set.csv")
  show.map<-cbind(apply(data.frame(map$U.Val),MARGIN=1,FUN=intToUtf8),map,deparse.level=0)
  colnames(show.map)[1]<-"IPA"
  return(show.map)
}
