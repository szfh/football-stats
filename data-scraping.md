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

This is pretty easy with rvest. Just set the page variable to the page
we want to scrape and the pass in through the read\_html function.