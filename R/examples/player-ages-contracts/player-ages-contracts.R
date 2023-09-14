# visualisation of current ages and contracts across a squad
# the script to generate the visualisation is functionised to accept arguments which modify the plot

# loosely based on layouts from Opta and The Athletic
# https://twitter.com/OptaAnalyst/status/1479062293833031681
# https://twitter.com/ChrisDHWaugh/status/1531888022341435392

# data from transfermarkt.com, accessed using worldfootballR
# https://jaseziv.github.io/worldfootballR/index.html
{
  library(tidyverse)
  library(worldfootballR)
  library(glue)
  library(lubridate)
  library(ggtext)
  library(here)
  
  # function to generate visualisation
  age_contract <- function(team_name="",team_url,players_to_add=tibble(),players_to_facet=c(),include_peak_age=FALSE){
    
    # custom function to simplify transfermarkt positions
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
          position=="Attack - Second Striker" ~ "ST",
          position=="Attack - Centre-Forward" ~ "ST",
          TRUE ~ position))
      
      return(positions$position_new)
    }
    
    # define the "peak age" for each position
    peak_ages <-
      tribble(
        ~"position",~"age_start",~"age_end",
        "GK",27,33,
        "CB",26,30,
        "FB",24,27,
        "CM",24,27,
        "AM",25,28,
        "ST",25,28,
      ) %>%
      mutate(position=factor(position,levels=c("GK","CB","FB","CM","AM","ST"))) %>%
      mutate(facet=1)
    
    player_url <- team_url %>%
      map(tm_team_player_urls)
    
    squad_bio <- player_url %>%
      map_dfr(tm_player_bio)
    
    squad_bio_type_converted <-
      squad_bio %>%
      # add the "on_loan_from" column if it doesn't exist
      {if("on_loan_from" %in% names(.)) . else mutate(., on_loan_from=NA)} %>%
      select(player_name,date_of_birth,contract_expires,position,current_club,on_loan_from,URL) %>%
      mutate(across(c(date_of_birth,contract_expires),as.Date)) %>%
      type_convert()
    
    squad_data <-
      squad_bio_type_converted %>%
      mutate(position=transfermarket_positions(position)) %>%
      # add column for loan in / loan out
      mutate(
        loan_in = case_when(
          !is.na(on_loan_from) ~ TRUE,
          .default = FALSE
        ),
        loan_out = FALSE,
        .keep="unused"
      ) %>%
      # bind rows extra players (usually loaned out) if given in function argument
      bind_rows(players_to_add) %>%
      # add extra facets if given in function argument
      mutate(facet=case_when(
        player_name %in% players_to_facet ~ 2,
        .default=1
      )) %>%
      # calculate age now and at contract expiry using lubridate package
      mutate(age_now=date_of_birth %--% today()/years()) %>%
      mutate(age_contract_expiry=date_of_birth %--% contract_expires/years())
    
    plot <-
      squad_data %>%
      # change colour for loan players
      mutate(player_name=case_when(
        loan_in ~ glue("<b style='color:royalblue'>{player_name}</b>"),
        loan_out ~ glue("<b style='color:darkred'>{player_name}</b>"),
        .default=glue("{player_name}")
      )) %>%
      mutate(position=factor(position,levels=c("GK","CB","FB","CM","AM","ST"))) %>%
      mutate(player_name=fct_reorder(player_name,age_now)) %>%
      ggplot() +
      # add peak age shading if specified in function argument
      {if(include_peak_age) geom_rect(data=peak_ages,aes(xmin=age_start-0.2,xmax=age_end+0.2,ymin=-Inf,ymax=Inf), fill="black", alpha=0.15) else NA} +
      geom_segment(aes(y=player_name,yend=player_name,x=age_now+0.15,xend=age_contract_expiry-0.15),alpha=0.8,linewidth=0.4) +
      geom_point(aes(y=player_name,x=age_now),size=2,alpha=0.75,shape=23,fill="darkred") +
      geom_point(aes(y=player_name,x=age_contract_expiry),size=2,alpha=0.75,shape=23,fill="royalblue") +
      facet_grid(rows=vars(position),cols=vars(facet), scales="free", space="free") +
      theme_minimal() +
      theme(
        plot.title=element_markdown(size=rel(0.8)),
        axis.title=element_blank(),
        axis.text.x=element_markdown(size=rel(0.8)),
        axis.text.y=element_markdown(size=rel(0.6)),
        strip.text.x=element_blank(),
        strip.text.y=element_markdown(angle=0),
        plot.caption=element_markdown(size=5),
      ) +
      labs(
        title=glue("{team_name} squad <b style='color:darkred'>age now</b> and <b style='color:royalblue'>end of contract</b>"),
        caption="transfermarkt.com data | <b style='color:darkred'>loaned in</b> | <b style='color:royalblue'>loaned out</b> | peak value shaded"
      ) +
      scale_x_continuous(breaks=seq(0,100,1),expand=expansion(add=c(0.25,0.25))) +
      scale_y_discrete(expand=expansion(add=c(0.75,0.75))) +
      NULL
    
    return(plot)
  }
}
{
  team_url_sfc <- c("https://www.transfermarkt.com/fc-southampton/startseite/verein/180")
  team_url_cfc <- c("https://www.transfermarkt.com/fc-chelsea/startseite/verein/631")
  team_url_whu <- c("https://www.transfermarkt.co.uk/west-ham-united/startseite/verein/379")
  team_url_lfc <- c("https://www.transfermarkt.co.uk/fc-liverpool/startseite/verein/31")
  team_url_shu <- c("https://www.transfermarkt.co.uk/sheffield-united/startseite/verein/350")
  team_url_ltfc <- c("https://www.transfermarkt.co.uk/luton-town/startseite/verein/1031")
  team_url_cpfc <- c("https://www.transfermarkt.co.uk/crystal-palace/startseite/verein/873")
  team_url_avfc <- c("https://www.transfermarkt.co.uk/aston-villa/startseite/verein/405")
  
  players_to_add_sfc <-
    tribble(
      ~"player_name",~"date_of_birth",~"contract_expires",~"position",~"loan_in",~"loan_out",
      "Armel Bella-Kotchap",as.Date("2001-12-11"),as.Date("2026-6-30"),"CB",FALSE,TRUE,
      "Romain Perraud",as.Date("1997-9-22"),as.Date("2025-6-30"),"FB",FALSE,TRUE,
      "Duje Caleta-Car",as.Date("1996-9-17"),as.Date("2026-6-30"),"CB",FALSE,TRUE,
      "Lyanco",as.Date("1997-2-1"),as.Date("2025-6-30"),"CB",FALSE,TRUE,
      "Mateusz Lis",as.Date("1997-2-27"),as.Date("2027-6-30"),"GK",FALSE,TRUE,
    )
  
  players_to_add_cfc <-
    tribble(
      ~"player_name",~"date_of_birth",~"contract_expires",~"position",~"loan_in",~"loan_out",
      "Romelu Lukaku",as.Date("1993-5-13"),as.Date("2026-6-30"),"ST",FALSE,TRUE,
      "Kepa Arrizabalaga",as.Date("1994-10-3"),as.Date("2025-6-30"),"GK",FALSE,TRUE,
      "Hakim Ziyech",as.Date("1993-3-19"),as.Date("2025-6-30"),"AM",FALSE,TRUE,
      "Andrey Santos",as.Date("2004-5-3"),as.Date("2030-6-30"),"CM",FALSE,TRUE,
      "Angelo",as.Date("2004-12-21"),as.Date("2028-6-30"),"AM",FALSE,TRUE,
      "Lewis Hall",as.Date("2004-9-8"),as.Date("2026-6-30"),"FB",FALSE,TRUE,
      "David Datro Fofana",as.Date("2002-12-22"),as.Date("2029-6-30"),"ST",FALSE,TRUE,
      "Gabriel Slonina",as.Date("2004-5-15"),as.Date("2028-6-30"),"GK",FALSE,TRUE,
      "Diego Moreira",as.Date("2004-8-6"),as.Date("2028-6-30"),"AM",FALSE,TRUE,
    )
  
  players_to_add_lfc <-
    tribble(
      ~"player_name",~"date_of_birth",~"contract_expires",~"position",~"loan_in",~"loan_out",
      "Fábio Carvalho",as.Date("2002-8-30"),as.Date("2027-6-30"),"AM",FALSE,TRUE,
      "Sepp van der Berg",as.Date("2001-12-20"),as.Date("2026-6-30"),"CB",FALSE,TRUE,
      "Calvin Ramsay",as.Date("2003-7-31"),as.Date("2027-6-30"),"FB",FALSE,TRUE,
      "Nathaniel Phillips",as.Date("1997-3-21"),as.Date("2025-6-30"),"CB",FALSE,TRUE,
      "Rhys Williams",as.Date("2001-2-3"),as.Date("2026-6-30"),"CB",FALSE,TRUE,
    )
  
  players_to_add_shu <- tribble(~"player_name",~"date_of_birth",~"contract_expires",~"position",~"loan_in",~"loan_out")
  players_to_add_ltfc <- tribble(~"player_name",~"date_of_birth",~"contract_expires",~"position",~"loan_in",~"loan_out")
  players_to_add_cpfc <- tribble(~"player_name",~"date_of_birth",~"contract_expires",~"position",~"loan_in",~"loan_out")
  players_to_add_avfc <- tribble(~"player_name",~"date_of_birth",~"contract_expires",~"position",~"loan_in",~"loan_out")
  
  players_to_facet_cfc <- c("Thiago Silva")
  
  age_contract_sep_23_sfc <- age_contract(team_name="Southampton",team_url_sfc,players_to_add_sfc,players_to_facet=c(),include_peak_age=TRUE)
  # age_contract_sep_23_cfc <- age_contract(team_name="Chelsea",team_url_cfc,players_to_add_cfc,players_to_facet_cfc,include_peak_age=TRUE)
  # age_contract_sep_23_whu <- age_contract(team_name="West Ham Utd",team_url_whu,include_peak_age=TRUE)
  # age_contract_sep_23_lfc <- age_contract(team_name="Liverpool",team_url_lfc, players_to_add_lfc,include_peak_age=TRUE)
  # age_contract_sep_23_shu <- age_contract(team_name="Sheffield Utd",team_url_shu, players_to_add_shu,include_peak_age=TRUE)
  # age_contract_sep_23_ltfc <- age_contract(team_name="Luton Town",team_url_ltfc, players_to_add_ltfc,include_peak_age=TRUE)
  # age_contract_sep_23_cpfc <- age_contract(team_name="Crystal Palace",team_url_cpfc, players_to_add_cpfc,include_peak_age=TRUE)
  # age_contract_sep_23_avfc <- age_contract(team_name="Aston Villa",team_url_avfc, players_to_add_avfc,include_peak_age=TRUE)
  
  # save_path <- here("plots","age_contract_sep_23_sfc.jpg")
  # ggsave(filename=save_path,plot=last_plot())
}