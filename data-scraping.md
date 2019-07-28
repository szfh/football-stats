data-scraping
================
saintsnumbers
2019-07-26

## Introduction

This is a R conversion of a tutorial by [FC
Python](http://twitter.com/FC_Python). I take no credit for the idea and
have their blessing to make this conversion. All text is a direct copy
unless changes were relevant. Please follow them on twitter and if you
have a desire to learn Python then they are a fantastic resource\!

Before starting the article, I’m obliged to mention that web scraping is
a grey area legally and ethicaly in lots of circumstances. Please
consider the positive and negative effects of what you scrape before
doing so\!

Warning over. Web scraping is a hugely powerful tool that, when done
properly, can give you access to huge, clean data sources to power your
analysis. The applications are just about endless for anyone interested
in data. As a professional analyst, you can scrape fixtures and line-up
data from around the world every day to plan scouting assignments or
alert you to youth players breaking through. As an amateur analyst, it
is quite likely to be your only source of data for analysis.

This tutorial is just an introduction for R scraping. It will take you
through the basic process of loading a page, locating information and
retrieving it. Combine the knowledge on this page with for loops to
cycle through a site and HTML knowledge to understand a web page, and
you’ll be armed with just about any data you can find.

Let’s fire up our package & get started. We’ll need the ‘rvest’ package,
so make sure you that installed.

Our process for extracting data is going to go something like this:

  - Load the webpage containing the data.
  - Locate the data within the page and extract it.
  - Organise the data into a dataframe
  - For this example, we are going to take the player names and values
    for the most expensive players in a particular year. You can find
    the page that we’ll use
    [here](https://www.transfermarkt.co.uk/transfers/transferrekorde/statistik/top/plus/0/galerie/0?saison_id=2000).

The following sections will run through each of these steps
individually.

## Load the webpage containing the data

This is pretty easy with **rvest**. Just set the page variable to the
page we want to scrape and the pass in through the read\_html
function.

``` r
page <- "https://www.transfermarkt.co.uk/transfers/transferrekorde/statistik/top/plus/0/galerie/0?saison_id=2000"
 
scraped_page <- read_html(page)
```

## Locate the data within a page & extract it

To fully appreciate what we are doing here, you probably need a basic
grasp of HTML – the language that structures a webpage. As simply as I
can put it for this article, HTML is made up of elements, like a
paragraph or a link, that tell the browser what to render. For scraping,
we will use this information to tell our program what information to
take.

You can inspect the source code of the page or use SelectorGadget to
help us tell our scraping code where the information is that we want to
grab.

Take another look at the page we are scraping. We want two things – the
player name and the transfer value.

Using **SelectorGadget** we can find the following node locations:

Player Name = \#yw2 .spielprofil\_tooltip Transfer Value =
.rechts.hauptlink a

Extracting the data is then quiet easy. Reading the code left to right
the\_page -\> the\_nodes -\> the\_text -\> as\_text. Each time storing
them as objects with
\<-

``` r
PlayerNames  <- scraped_page %>% html_nodes("#yw2 .spielprofil_tooltip") %>% html_text() %>% as.character()
TransferValue <- scraped_page %>% html_nodes(".rechts.hauptlink a") %>% html_text() %>% as.character()
```

## Organise the data into a dataframe

This is pretty simple as we now have two objects PlayersNames and
TransferValues. So we just add them to the construction of a dataframe.

``` r
df <- data.frame(PlayerNames, TransferValue)
```

and then display the top 5 items of the dataframe with :

``` r
head(df)
```

    ##         PlayerNames TransferValue
    ## 1         Luís Figo       £54.00m
    ## 2     Hernán Crespo       £51.13m
    ## 3     Marc Overmars       £36.00m
    ## 4 Gabriel Batistuta       £32.54m
    ## 5    Nicolas Anelka       £31.05m
    ## 6     Rio Ferdinand       £23.40m

## Summary

This article has gone through the absolute basics of scraping, we can
now load a page, identify elements that we want to scrape and then
process them into a dataframe.

There is more that we need to do to scrape efficiently though. Firstly,
we can apply a for loop to the whole program above, changing the initial
webpage name slightly to scrape the next year – I’ll let you figure out
how\!

You will also need to understand more about HTML, particularly class and
ID selectors, to get the most out of scraping. Regardless, if you’ve
followed along and understand what we’ve achieved and how, then you’re
in a good place to apply this to other pages.

# Scraping Lists Through Transfermarkt and Saving Images in R

This is a R conversion of a tutorial by [FC
Python](https://twitter.com/FC_Python). I take no credit for the idea
and have their blessing to make this conversion. All text is a direct
copy unless changes were relevant. Please follow them on twitter and if
you have a desire to learn Python then they are a fantastic resource\!

In this tutorial, we’ll be looking to develop our scraping knowledge
beyond just lifting text from a single page. Following through the
article, you’ll learn how to scrape links from a page and iterate
through them to take information from each link, speeding up the process
of creating new datasets. We will also run through how to identify and
download images, creating a database of every player in the Premier
League’s picture. This should save 10 minutes a week for anyone
searching in Google Images to decorate their pre-match presentations\!

This tutorial builds on the [first
tutorial](https://github.com/FCrSTATS/ScrappingTutorials/blob/master/1.ScrapingTransferFeeData.rmd)
in our scraping series, so it is strongly recommended that you
understand the concepts there before starting here.

Let’s import our package and get started. Rvest is the only package we
need to install.

Our aim is to extract a picture of every player in the Premier League.
We have identified Transfermarkt as our target, given that each player
page should have a picture. Our secondary aim is to run this in one
piece of code and not to run a new command for each player or team
individually. To do this, we need to follow this process:

1)  Locate a list of teams in the league with links to a squad list –
    then save these links

2)  Run through each squad list link and save the link to each player’s
    page

3)  Locate the player’s image and save it to our local computer

For what seems to be a massive task, we can distill it down to three
main tasks. Below, we’ll break each one down.

## Locate a list of team links and save them

The [Premier League
page](https://www.transfermarkt.co.uk/premier-league/startseite/wettbewerb/GB1)
is the obvious place to start. As you can see, each team name is a link
through to the squad page.

All we need to do is use Selector Gadget to identify the names of the
nodes that we want to scrape.

Finally, we append these links to the transfermarkt domain so that we
can call them on their
own.

``` r
page <- "https://www.transfermarkt.co.uk/premier-league/startseite/wettbewerb/GB1"
scraped_page <- read_html(page)
teamLinks <- scraped_page %>% html_nodes(".hide-for-pad .vereinprofil_tooltip") %>% html_attr("href")
teamLinks <- paste0("https://www.transfermarkt.co.uk", teamLinks)
head(teamLinks)
```

    ## [1] "https://www.transfermarkt.co.uk/manchester-city/startseite/verein/281/saison_id/2019"  
    ## [2] "https://www.transfermarkt.co.uk/fc-liverpool/startseite/verein/31/saison_id/2019"      
    ## [3] "https://www.transfermarkt.co.uk/tottenham-hotspur/startseite/verein/148/saison_id/2019"
    ## [4] "https://www.transfermarkt.co.uk/fc-chelsea/startseite/verein/631/saison_id/2019"       
    ## [5] "https://www.transfermarkt.co.uk/manchester-united/startseite/verein/985/saison_id/2019"
    ## [6] "https://www.transfermarkt.co.uk/fc-arsenal/startseite/verein/11/saison_id/2019"

## Run through each squad and save the player links

So we now have 20 team links. We will now iterate through each of these
team links and do the same thing, only this time we are taking player
links and not squad links. Take a look through the code below, but
you’ll notice that it is very similar to the last chunk of
instructions – the key difference being that we will run it within a
loop to go through all 20 teams in one go.

``` r
PlayerLinks <- list()
#was i in length(PlayerLinks)
for (i in 1:2) {
  page <- teamLinks[i]
  scraped_page <- read_html(page)
  temp_PlayerLinks <- scraped_page %>% html_nodes(".hide-for-small .spielprofil_tooltip") %>% html_attr("href")
  PlayerLinks <- append(PlayerLinks, temp_PlayerLinks)
}
head(PlayerLinks)
```

    ## [[1]]
    ## [1] "/ederson/profil/spieler/238223"
    ## 
    ## [[2]]
    ## [1] "/claudio-bravo/profil/spieler/40423"
    ## 
    ## [[3]]
    ## [1] "/aymeric-laporte/profil/spieler/176553"
    ## 
    ## [[4]]
    ## [1] "/john-stones/profil/spieler/186590"
    ## 
    ## [[5]]
    ## [1] "/nicolas-otamendi/profil/spieler/54781"
    ## 
    ## [[6]]
    ## [1] "/eliaquim-mangala/profil/spieler/90681"

(done for 2 teams only)

We have to make some quick changes to the PlayerLinks list to make them
easier to use in the next step. Firstly, unlist them. Finally, we append
these links to the transfermarkt domain so that we can call them on
their own.

``` r
PlayerLinks <- unlist(PlayerLinks)
PlayerLinks <- paste0("https://www.transfermarkt.co.uk" , PlayerLinks)
head(PlayerLinks)
```

    ## [1] "https://www.transfermarkt.co.uk/ederson/profil/spieler/238223"        
    ## [2] "https://www.transfermarkt.co.uk/claudio-bravo/profil/spieler/40423"   
    ## [3] "https://www.transfermarkt.co.uk/aymeric-laporte/profil/spieler/176553"
    ## [4] "https://www.transfermarkt.co.uk/john-stones/profil/spieler/186590"    
    ## [5] "https://www.transfermarkt.co.uk/nicolas-otamendi/profil/spieler/54781"
    ## [6] "https://www.transfermarkt.co.uk/eliaquim-mangala/profil/spieler/90681"

## Locate and save each player’s image

We now have a lot of links for players…

513 links, in fact\! We now need to iterate through each of these links
and save the player’s picture.

Hopefully you should now be comfortable with the process to download and
process a webpage, but the second part of this step will need some
unpacking – locating the image and saving it.

Once again, we are locating elements in the page. We grab the image by
the node and use the “src” as the input to html\_attr(). This gives us
the url of the image. We also grab the player’s name using the “h1”
node. The we use the download.file function to grab the image and save
it with the filename of the player. The image then downloads to the
working directory you have set in R.

``` r
#was i in length(teamLinks)
for (i in 1:2) {
  page <- PlayerLinks[i]
  scraped_page <- read_html(page)
  Player <- scraped_page %>% html_node("h1") %>% html_text() %>% as.character()
  Image_Title <- paste0(Player,".jpg")
  Image_url <- scraped_page %>% html_node(".dataBild img") %>% html_attr("src")
  download.file(Image_url,Image_Title, mode = 'wb')
}
```

… and job done\! We now have a catalog of player images. To help test
what you have learnt try and add the players club name to the filename
of each image i.e. “AymericLaporte\_ManchesterCity.jpg” …. maybe helpful
when cataloging the images for future use.
