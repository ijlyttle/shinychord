This is a demonstration of how to use [shinychords](http://ijlyttle.github.io/shinychord/) to read and parse a csv file that contains a time-series, then to plot the data using a [dygraph](https://rstudio.github.io/dygraphs/) into a shiny app.

If you don't have available a csv file with a time series, you can download [wx_ames.csv](http://ijlyttle.github.io/shinychord/doc/wx_ames.csv). This file describes the weather in Ames, Iowa, for the month of January 2014. The data was obtained from the [Weather Underground API](http://www.wunderground.com/weather/api), using Alex Shum's [rwunderground package](https://cran.r-project.org/web/packages/rwunderground/index.html).

To read and parse the csv file, the development version of Hadley's [readr](https://github.com/hadley/readr) package is used.

Dygraphs are used to plot time-series, although we are using a dataframe to supply the data.

The code for the functions `ch_read_delim` and `ch_dygraph`, used to generate the shinychords, can be found at [Github](https://github.com/ijlyttle/shinychord/blob/master/R/), as can its package [shinychord](http://github.com/ijlyttle/shinychord/). 


