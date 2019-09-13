# Installation Instructions

For anybody that wants to install and use the functions in this package, follow these steps to install and use them in R:

1. You need to install the devtools package in order to install this package from github:

`install.packages('devtools')`

2. You can now install both this package and the magrittr package that it relies on (run all four):

`devtools::install_github('https://github.com/palmerimatthew/EPScraper')`

`install.packages('magrittr')`

`library(EPScraper)`

`library(magrittr)`

You should be good to go now! To make sure that everything worked properly, you should be able to type 'EP_' into the console, and see it populate with functions from the EPScraper package.

If You ever need help on one of these functions (or any function in R), you can put '?' before the function in the console, and you will be help text appear in the lower right window of rStudio (for example, '?grep').
