# for(data in ls(pattern="_raw")){
#   write.csv(
#     data, file=paste0("data/",data,".csv"), row.names=FALSE
#   )
# }

# n <- length(ls(pattern="_raw"))
# list1 <- lapply((1:n), function(i) data.frame(ls(pattern="_raw")))
# list1


# list_data <- ls(pattern="_raw")
# n <- length(ls(pattern="_raw"))

# for(i in seq_along(ls(pattern="_raw"))){
#   # print(i)
#   print(list_data[[i]])
# }


# for(data in list_data){
#   print(data)
# }

# a_raw <- data.frame(x = rnorm(10), y = rnorm(10))
# b_raw <- data.frame(x = rnorm(10), y = rnorm(10))
# c_raw <- data.frame(x = rnorm(10), y = rnorm(10))


# df_list <- lapply(1:4, function(i)  data.frame(x = rnorm(10), y = rnorm(10)) )
# names(df_list) <- paste0(letters[1:4],"_raw")

# save each new data frame as an individual .csv file based on its name
# lapply(1:length(my_list), function(i) write.csv(my_list[[i]], 
#                                                 file = paste0("data/",names(my_list[i]), ".csv"),
#                                                 row.names = FALSE))
# https://stackoverflow.com/questions/26707724/writing-multiple-data-frames-into-csv-files-using-r
# https://stackoverflow.com/questions/17018138/using-lapply-to-apply-a-function-over-list-of-data-frames-and-saving-output-to-f?rq=1

# saveRDS(ls(pattern="_raw"))
# length(ls(pattern="_raw"))

# raw_data_list <- as.list(ls(pattern="_raw"))
# sapply(raw_data_list, function(x) saveRDS(get(x), file=paste0("./data/",x,".rds")))

sapply(as.list(ls(pattern="_raw")),save_data)