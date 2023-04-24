
## @saintbynumbers football data and analytics repository

### Structure

-   [R/raw/](https://github.com/szfh/football-stats/tree/main/R/raw) for
    scripts to get data from public sources and save to a .RDS file.
    **[See note
    below](https://github.com/szfh/football-stats#note-about-scraping)**.
-   [R/join/](https://github.com/szfh/football-stats/tree/main/R/join)
    for data preparation and import to the R workspace.
-   [R/plot/](https://github.com/szfh/football-stats/tree/main/R/plot)
    for scripts to create and save images.
-   [R/library.R](https://github.com/szfh/football-stats/blob/main/R/library.R)
    for packages.
-   [R/themes.R](https://github.com/szfh/football-stats/blob/main/R/themes.R)
    for themes.
-   [R/examples/](https://github.com/szfh/football-stats/blob/main/R/examples)
    for self-contained example scripts.
-   [positions.csv](https://github.com/szfh/football-stats/blob/main/positions.csv)
    for a .csv file of positions based on FBRef match data. For EPL
    players between **2017-08-01** and **2022-10-01**.
-   [@saintsbynumbers](https://twitter.com/saintsbynumbers) for the
    author. I [blog](https://szfh.github.io/) about twice a year.

### Note about scraping

Most data in this repository is collected using
[worldfootballR](https://cran.r-project.org/web/packages/worldfootballR/index.html)
by [Jason Zivkovic](https://www.dontblamethedata.com/). This is done by
checking public data sources for new data, downloading, and then adding
to an existing dataset.

If you use scraping tools including any in this repository, **you must
use them responsibly**. Download data once, save locally, and work from
there. Credit the data sources in your work. Data is provided for free,
so don’t ruin it for everyone.

### Data visualisation

<center>
<img src="./images/plot1.jpg" width="60%">
<img src="./images/plot2.jpg" width="60%">
<img src="./images/plot3.jpg" width="60%">
<img src="./images/plot4.jpg" width="60%">
</center>

### Useful resources

1.  [Friends of
    Tracking](https://www.youtube.com/channel/UCUBFJYcag8j2rm_9HkrrA7w)
2.  [Free Statsbomb
    Data](https://statsbomb.com/2021/11/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/)
3.  [Devin Pleuler Analytics
    Handbook](https://github.com/devinpleuler/analytics-handbook)