# visualisation of current ages and contracts across a squad

# loosely based on layouts from Opta and The Athletic
# https://twitter.com/OptaAnalyst/status/1479062293833031681
# https://twitter.com/ChrisDHWaugh/status/1531888022341435392

# data from Transfermarkt, accessed using worldfootballR
# https://jaseziv.github.io/worldfootballR/index.html
{
  library(tidyverse)
  library(worldfootballR)
  library(lubridate)
  library(ggtext)
  library(here)
}
{
  # enter your team url from transfermarkt here
  team_url <- "https://www.transfermarkt.com/fc-southampton/startseite/verein/180/"
  team_url_u23 <- "https://www.transfermarkt.co.uk/fc-southampton-u21/startseite/verein/36546"
  player_urls <- tm_team_player_urls(team_url)
  player_urls_u23 <- tm_team_player_urls(team_url_u23)
  player_bio <- tm_player_bio(player_urls)
  player_bio_u23 <- tm_player_bio(player_urls_u23)
}
{
  # data frame of all first team players and selected U23 players
  player_bio_combined <-
    bind_rows(
      player_bio,
      player_bio_u23 %>%
        filter(player_name %in% c("Dynel Simeu","Jake Vokins","Thierry Small","Kayne Ramsay","Caleb Watts","Kgaogelo Chauke","Will Ferry","Kazeem Olaigbe"))
    )
  
  # keep useful columns and auto-detect data types (particularly dates)
  player_bio_type_converted <-
    player_bio_combined %>%
    select(player_name,position,date_of_birth,contract_expires,date_of_last_contract_extension) %>%
    type_convert()
  
  # update data with changes that haven't been listed in Transfermarkt yet
  player_bio_updated <-
    player_bio_type_converted %>%
    # fix date of birth
    mutate(date_of_birth=case_when(
      player_name=="Lyanco" ~ as.Date("1997-2-1"),
      TRUE ~ date_of_birth
    )) %>%
    # new contracts
    mutate(contract_expires=case_when(
      player_name=="Willy Caballero" ~ contract_expires+years(1),
      player_name=="Alex McCarthy" ~ contract_expires+years(1),
      player_name=="Jack Stephens" ~ contract_expires+years(2),
      TRUE ~ contract_expires
    ))
  
  # custom function to simplify positions
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
        position=="midfield - Attacking Midfield" ~ "AM",
        position=="Attack - Left Winger" ~ "AM",
        position=="Attack - Right Winger" ~ "AM",
        position=="Attack - Centre-Forward" ~ "ST",
        TRUE ~ "Other"))
    
    return(positions$position_new)
  }
  
  player_data <-
    player_bio_updated %>%
    # simplify positions
    mutate(position=transfermarket_positions(position)) %>%
    # positions in order
    mutate(position=factor(position,levels=c("GK","CB","FB","CM","AM","ST"))) %>%
    # calculate age now and age at contract expiry
    mutate(age_now=date_of_birth %--% today()/years()) %>%
    mutate(age_contract_expiry=date_of_birth %--% contract_expires/years()) %>%
    # players ordered by age now
    mutate(player_name=fct_reorder(player_name,age_now))
  
  # create peak age data
  peak_ages <-
    tribble(
      ~"position",~"age_start",~"age_end",
      "GK",27,31,
      "CB",26,30,
      "FB",25,28,
      "CM",24,28,
      "AM",24,28,
      "ST",25,29,
    ) %>%
    mutate(
      age_start=age_start-0.3,
      age_end=age_end+0.3
    ) %>%
    mutate(position=factor(position,levels=c("GK","CB","FB","CM","AM","ST")))
}
{
  # ready to visualise
  player_data %>%
    ggplot() +
    geom_rect(data=peak_ages,aes(xmin=age_start,xmax=age_end,ymin=-Inf,ymax=Inf), fill="black", alpha=0.15) +
    geom_segment(aes(y=player_name,yend=player_name,x=age_now+0.1,xend=age_contract_expiry-0.1),alpha=0.8,linewidth=0.4) +
    geom_point(aes(y=player_name,x=age_now),alpha=0.75,shape=23,fill="darkred") +
    geom_point(aes(y=player_name,x=age_contract_expiry),alpha=0.75,shape=23,fill="royalblue") +
    facet_grid(position ~ ., scales="free", space="free") +
    theme_minimal() +
    theme(
      plot.title=element_markdown(size=rel(0.8)),
      axis.title=element_blank(),
      axis.text.x=element_markdown(size=rel(0.8)),
      axis.text.y=element_markdown(size=rel(0.6)),
      strip.text.y=element_markdown(angle=0,size=rel(0.8)),
      plot.caption=element_markdown(size=5),
    ) +
    labs(
      title="Southampton age <b style='color:darkred'>now</b> and at <b style='color:royalblue'>end of contract</b>",
      caption="data from Transfermarkt | peak age shaded"
    ) +
    scale_x_continuous(breaks=seq(0,100,2),expand=expansion(add=0.25)) +
    scale_y_discrete()
  
  # save_path <- here("plots","team","ages-contracts.jpg")
  # ggsave(filename=save_path,plot=last_plot())
}
