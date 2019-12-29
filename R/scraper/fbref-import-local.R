for(data in list.files("./data/fbref/", pattern=".csv")){
  print(data)
  read.csv(
    paste0("./data/fbref/",data)
  )
}

list.files("./data/fbref/", pattern=".csv")
