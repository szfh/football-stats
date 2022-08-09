age_contract <- function(url=NA){

  transfermarket_positions <- function(positions){
    positions <-
      positions %>%
      as_tibble_col(column_name="position") %>%
      mutate(position_new=case_when(
        position=="Goalkeeper" ~ "GK",
        position=="Defender - Centre-Back" ~ "CB",
        position=="Defender - Left-Back" ~ "FB",
        position=="Defender - Right-Back" ~ "FB",
        position=="midfield - Defensive Midfield" ~ "CM",
        position=="midfield - Central Midfield" ~ "CM",
        position=="midfield - Left Midfield" ~ "AM",
        position=="midfield - Right Midfield" ~ "AM",
        position=="attack - Left Winger" ~ "AM",
        position=="attack - Right Winger" ~ "AM",
        position=="attack - Centre-Forward" ~ "ST",
        TRUE ~ position))
    
    return(positions$position_new)
  }
  
  peak_ages <-
    tribble(
      ~"position",~"age_start",~"age_end",
      "GK",27,33,
      "CB",26,30,
      "FB",23,27,
      "CM",24,27,
      "AM",25,28,
      "ST",26,29,
    ) %>%
    mutate(position=factor(position,levels=c("GK","CB","FB","CM","AM","ST")))
  
  team_urls <- 
    c("https://www.transfermarkt.com/fc-southampton/startseite/verein/180",
      "https://www.transfermarkt.co.uk/fc-southampton-u21/startseite/verein/36546")

  player_urls <- team_urls %>%
    map(tm_team_player_urls) %>%
    .[[1]] #%>%
    # unlist()
  
  squad_bio <- player_urls %>%
    map_dfr(tm_player_bio)
  
  squad_bio_type_converted <-
    squad_bio %>%
    # select(player_name,date_of_birth,joined,contract_expires,position,URL) %>%
    mutate(across(c(date_of_birth,joined,contract_expires),as.Date)) %>%
    type_convert()
  
  squad_data <-
    squad_bio_type_converted %>%
    mutate(position=transfermarket_positions(position)) %>%
    mutate(position=factor(position,levels=c("GK","CB","FB","CM","AM","ST"))) %>%
    mutate(age_now=date_of_birth %--% today()/years()) %>%
    mutate(age_contract_expiry=date_of_birth %--% contract_expires/years()) %>%
    mutate(player_name=fct_reorder(player_name,age_now))
  
  plot <-
    squad_data %>%
    ggplot() +
    geom_rect(data=peak_ages,aes(xmin=age_start,xmax=age_end,ymin=-Inf,ymax=Inf), fill="black", alpha=0.15) +
    geom_segment(aes(y=player_name,yend=player_name,x=age_now+0.2,xend=age_contract_expiry-0.2),alpha=0.8,size=0.4) +
    geom_point(aes(y=player_name,x=age_now),size=2,alpha=0.75,shape=23,fill="darkred") +
    geom_point(aes(y=player_name,x=age_contract_expiry),size=2,alpha=0.75,shape=23,fill="royalblue") +
    facet_grid(position ~ ., scales="free", space="free") +
    theme[["solar"]]() +
    theme(
      plot.title=element_markdown(size=rel(0.8)),
      axis.title=element_blank(),
      axis.text.x=element_markdown(size=rel(0.8)),
      axis.text.y=element_markdown(size=rel(0.6)),
      strip.text.y=element_markdown(angle=0),
      plot.caption=element_markdown(size=5),
    ) +
    labs(
      title="Southampton age <b style='color:darkred'>now</b> and at <b style='color:royalblue'>end of contract</b>",
      caption="data from Transfermarkt | peak age shaded | @saintsbynumbers"
    ) +
    scale_x_continuous(breaks=seq(0,100,2),expand=expansion(add=0.3)) +
    NULL
  
  return(plot)
}