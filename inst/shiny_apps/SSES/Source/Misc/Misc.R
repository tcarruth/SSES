
namconv<-function(nam){
  nam<-gsub(" ","_",nam)
  nam<-gsub("[.]","",nam)
  nam<-gsub(",","",nam)
  substr(nam,1,15)[[1]]
}
