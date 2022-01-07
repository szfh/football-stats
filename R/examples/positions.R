{
  source(here::here("R","library.R"),encoding="utf-8")
  source(here("R","join","join.R"),encoding="utf-8")
  data <- join()
}
{
  positions <-
    data$fbref$match_lineups %>%
    filter(Matchday>=as.Date("2017-08-01")) %>%
    # filter(Matchday<=as.Date("2021-12-31")) %>%
    select(Player=Player_Name,Position=Pos,Min) %>%
    mutate(Position=str_sub(Position,1,2)) %>%
    filter(!is.na(Position)) %>%
    # mutate(Pos=case_when(
    #   Pos=="LB" ~ "FB",
    #   Pos=="RB" ~ "FB",
    #   Pos=="WB" ~ "FB",
    #   Pos=="DM" ~ "CM",
    #   Pos=="LM" ~ "AM",
    #   Pos=="RM" ~ "AM",
    #   Pos=="LW" ~ "AM",
    #   Pos=="RW" ~ "AM",
    #   Pos=="FW" ~ "ST",
    #   TRUE ~ Pos
    # )) %>%
    # mutate(Pos=factor(Pos,levels=c("GK","CB","FB","CM","AM","ST"))) %>%
    group_by(Player,Position) %>%
    summarise(Min=sum(Min,na.rm=TRUE),.groups="drop") %>%
    group_by(Player) %>%
    slice_max(Min, with_ties=FALSE) %>%
    ungroup() %>%
    select(Player,Position)
  
  positions %>%
    select(Position) %>%
    distinct() %>%
    print(n=Inf)
  
  write.csv(positions,here("positions.csv"),row.names=FALSE)
}
