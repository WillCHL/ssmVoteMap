---
Same Sex Marriage Survey Vote Map
---

## Background

I had been looking for a good coding project for a while, to try and build some confidence before I attempted a hackathon, when a friend posted a link to a great map based representation of the  Australian Marriage Law Postal Survey results.  It was a nice clean map that provided a mouse-over pop-up of the yes/no percentage for each electorate which I found it really interesting, but I also wanted some more information.  Information on things like the number of voters, the member holding that seat, the party they are from, what suburbs where in each electorate, and maybe even some census data.  Thus the project was formed.

The site is built with Shiny, a web app language based on R developed by  RStudio which can run R code in the background to generate an html page a browsers can view.  The actual layout code is like a cross between R, html and javascript but it is relatively easy to learn, particularly if you have some experience in R and html.


## Data Sources

* Vote data: https://marriagesurvey.abs.gov.au/results/downloads.html
* Region boundries and names: http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202017
* Parliment members: https://en.wikipedia.org/wiki/Members_of_the_Australian_House_of_Representatives,_2016%E2%80%932019
* Census data: https://datapacks.censusdata.abs.gov.au/datapacks/


## Workflow




## Disclaimer

This project wasn't set-up using git or with the intention of making the code public and as a result, it was a bit messy at the end of the project.  Some time after the project was completed I went through the code to tidy things up and add comments so they may more sense to me (and hopefully others).  Hopefully everything still works but keep in mind that some things have moved a little, and a number of pieces of the processing script were not run from start to finish.
