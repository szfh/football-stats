for(data in ls(pattern="_raw")){
  write.csv(
    data,
    file=paste0("data/fbref/",data,".csv"),
    row.names=FALSE
  )
}