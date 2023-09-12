
<!-- badges: start
[![R-CMD-check](https://github.com/jvcasillas/rult/workflows/R-CMD-check/badge.svg)](https://github.com/jvcasillas/rult/actions)  
[![CodeFactor](https://www.codefactor.io/repository/github/jvcasillas/rult/badge)](https://www.codefactor.io/repository/github/jvcasillas/rult)
badges: end -->

## rult <img src='https://raw.githubusercontent.com/jvcasillas/hex_stickers/master/stickers/rult.png' align='right' width='275px'/>

A simple interface to RU lexTALE data  
Last update: 2023-09-12

### Overview

Use this package to interface with the `lextale_ru` repo and download
data  
from specific terms or courses.

### Installation

You can install the development version from GitHub with:

    install.packages("devtools")
    devtools::install_github("jvcasillas/rult")

### Use

Use the `download_lextale_data()` function to get data from the server.
By default, the function will download a `lextale` folder to the working
directory of your R session:

``` r
library("rult")
download_lextale_data()
```

If you would like to download the data to a specific location in your
working directory, you can use the `destination` argument:

``` r
library("rult")
download_lextale_data(
  destination = "data"
)
```

You can use the `apply_filter` argument to select a subset of the data
on the server. For example, if you only want the data from Fall 2023,
you would use the following code:

``` r
library("rult")
download_lextale_data(
  destination = "data", 
  apply_filter = "term == 'fa_2023'"
)
```

Note that the term *must* be specified as with “fa”, “sp” or “su”
following by an underscore, i.e., `_`, and then the year, e.g.,
“sp_2024”, “su_2023”, etc.

It is also possible to filter by course:

``` r
library("rult")
download_lextale_data(
  destination = "data", 
  apply_filter = "course = '940:132 Intermediate Spanish'"
)
```

Here is a full list of course options:

- 940:101 Elementary Spanish I
- 940:102 Elementary Spanish II
- 940:121 Spanish Review and Continuation
- 940:131 Intermediate Spanish
- 940:132 Intermediate Spanish
- 940:160 Spanish in the World
- 940:201 Spanish for Heritage Speakers II
- 940:202 Culture and Composition for Heritage Speakers
- 940:203 Spanish Conversation and Composition
- 940:204 Culture and Composition
- 940:205 Spanish for the Health Professions
- 940:206 Spanish for Business I
- 940:215 Introduction to Hispanic Literature
- 940:225 Spanish for the Health Professions II
- 940:250 Sports in Latin America and the Caribbean
- 940:261 Introduction to the Study of Language
- 940:313 Advanced Spanish Conversation and Contemporary Issues
- 940:325 Advanced Grammar and Composition
- 940:326 Advanced Composition and Introduction to Translation Studies
- 940:331 Literature & Culture of the Hispanic Culture
- 940:332 Literature & Culture of the Hispanic Caribbean
- 940:333 Power & Nation-Building in Spanish America
- 940:334 Modernity and New Identities in Spanish America
- 940:335 Literature & Culture of Spain
- 940:336 Literature & Culture of Spain
- 940:342 Women Writers of the Hispanic World
- 940:346 Spanish Film
- 940:348 Latin American Cinema
- 940:353 Spanish for Community Engagement
- 940:360 Spanish for Commerce
- 940:362 Spanish Phonetics and Phonology
- 940:363 Bilingualism in the Spanish-Speaking World
- 940:364 Contrastive Analysis in Spanish and English
- 940:366 Methods of Teaching Spanish K-12
- 940:367 Sociolinguistics in the Spanish-Speaking World
- 940:368 The Bilingual Mind
- 940:389 Spanish Lab for Oral Proficiency I
- 940:390 Topics in Hispanic Literature and Culture
- 940:402 Intro to Translation of Specialized Texts
- 940:403/4 Civilization of Spanish America
- 940:405/6 Civilization of Spain
- 940:419 Dialectology of the Spanish-Speaking World
- 940:450 Spanish-American Theater
- 940:462 Environment and Literature in Hispanic Culture
- 940:471 Internship in Translation & Interpreting
- 940:477 Court Interpreting
- 940:479 Translation Media and Technology
- 940:488 Topics in Spanish Linguistics
- 940:489-492 Tpics in Hispanic Literature and Culture
- 940:491 Civilization of Spanish America
- 940:491/597 - Hispanic Women’s Voices
- 940:493/494 Independent Study
- 940:499 Oral Proficiency Interview
- 940:589 Topics in Hispanic Linguistics
- Graduate
