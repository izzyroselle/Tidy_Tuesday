Nobel
================

Prep
----

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
```

Import data
-----------

``` r
1595
```

    ## [1] 1595

``` r
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   laureate_id = col_double(),
    ##   laureate_name = col_character(),
    ##   prize_year = col_double(),
    ##   title = col_character(),
    ##   pub_year = col_double(),
    ##   paper_id = col_double(),
    ##   doi = col_character(),
    ##   journal = col_character(),
    ##   affiliation = col_character(),
    ##   is_prize_winning_paper = col_character(),
    ##   category = col_character()
    ## )

``` r
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   prize_year = col_double(),
    ##   category = col_character(),
    ##   prize = col_character(),
    ##   motivation = col_character(),
    ##   prize_share = col_character(),
    ##   laureate_id = col_double(),
    ##   laureate_type = col_character(),
    ##   full_name = col_character(),
    ##   birth_date = col_date(format = ""),
    ##   birth_city = col_character(),
    ##   birth_country = col_character(),
    ##   gender = col_character(),
    ##   organization_name = col_character(),
    ##   organization_city = col_character(),
    ##   organization_country = col_character(),
    ##   death_date = col_date(format = ""),
    ##   death_city = col_character(),
    ##   death_country = col_character()
    ## )

Data Wrangling
--------------

``` r
n <- nobel_winners
np <- nobel_winner_all_pubs
head(np)
```

    ## # A tibble: 6 x 11
    ##   laureate_id laureate_name prize_year title pub_year paper_id doi  
    ##         <dbl> <chr>              <dbl> <chr>    <dbl>    <dbl> <chr>
    ## 1       20001 stoddart, j         2016 a mo…     1991   1.98e9 10.1…
    ## 2       20001 stoddart, j         2016 chem…     1993   1.96e9 10.1…
    ## 3       20001 stoddart, j         2016 form…     1981   1.96e9 10.1…
    ## 4       20001 stoddart, j         2016 sing…     2005   2.10e9 10.1…
    ## 5       20001 stoddart, j         2016 synt…     1974   2.10e9 10.1…
    ## 6       20001 stoddart, j         2016 a st…     1982   1.96e9 10.1…
    ## # … with 4 more variables: journal <chr>, affiliation <chr>,
    ## #   is_prize_winning_paper <chr>, category <chr>

``` r
ne <- aggregate(pub_year ~ laureate_name, data = np, min)
g <- np[which(np$is_prize_winning_paper == 'YES'),]
g2 <- g[, c(1,2,3, 5),]
nbig <- aggregate(pub_year ~ laureate_name, data = np, max)
b <- left_join(g2, ne, by = 'laureate_name') %>% left_join(., nbig, by = 'laureate_name')
names (b) [4] <- 'prizepub'
names (b) [5] <- 'firstpub'
names (b) [6] <- 'lastpub'
f <- np[, c(2, 11)]
b2 <- left_join(b, f, by = 'laureate_name')
b3 <- b2[which(!duplicated(b2$laureate_id)),]
```

NB: I'm using the np dataset, which means I can only do the Medicine, Chemistry and Physics prizes. \#\#Graphing

We know Nobel prizes are offset, often by a long time, from the publication year of the winning paper.

``` r
ggplot(b3, aes(x = prizepub, y = prize_year, color = category)) + geom_point() + coord_cartesian(xlim = c(1880, 2020), ylim = c(1880, 2020)) + geom_abline(slope = 1, intercept = 0) + ggtitle('Delay in Nobel Prizes') + xlab('Publication Year of Winning Paper') + ylab('Year Prize Awarded')
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](nobel_files/figure-markdown_github/cars-1.png)

Diagnosing a problem with the data
----------------------------------

But look - there's a blue (physics) point below the 45 degree line (which denotes a Nobel Prize awarded the same year as the cited paper was published), which is impossible. Let's investigate that.

``` r
b3[which(b3$prizepub > 1960 & b3$prize_year < 1920),]
```

    ## # A tibble: 1 x 7
    ##   laureate_id laureate_name prize_year prizepub firstpub lastpub category
    ##         <dbl> <chr>              <dbl>    <dbl>    <dbl>   <dbl> <chr>   
    ## 1       10189 wien, w             1911     1983     1902    1983 physics

Because I know that's about where the outlying point is.

Then to generalise and make sure there aren't other impossible values (with regard to this error, not to other errors) there:

``` r
b3[which(b3$prizepub > b3$prize_year),]
```

    ## # A tibble: 1 x 7
    ##   laureate_id laureate_name prize_year prizepub firstpub lastpub category
    ##         <dbl> <chr>              <dbl>    <dbl>    <dbl>   <dbl> <chr>   
    ## 1       10189 wien, w             1911     1983     1902    1983 physics

This shows that the laureate was Wien, who won his prize in 1911. Now I want to see if it's a problem with the source data or some error I introduced in my processing, by looking at this entry in the original dataframe:

``` r
np[which(np$laureate_id == 10189),]
```

    ## # A tibble: 4 x 11
    ##   laureate_id laureate_name prize_year title pub_year paper_id doi  
    ##         <dbl> <chr>              <dbl> <chr>    <dbl>    <dbl> <chr>
    ## 1       10189 wien, w             1911 Wien…     1983  NA      <NA> 
    ## 2       10189 wien, w             1911 uber…     1902   2.01e9 10.1…
    ## 3       10189 wien, w             1911 uber…     1904   2.06e9 10.1…
    ## 4       10189 wien, w             1911 uebe…     1902   2.06e9 10.1…
    ## # … with 4 more variables: journal <chr>, affiliation <chr>,
    ## #   is_prize_winning_paper <chr>, category <chr>

This shows that the problem existed in the original data, which says that Wien won his Nobel prize in 1911 (true) for a paper published in 1983 (...less so.)

So now I know there's something wrong with the data, so I'll be excluding outliers in future but otherwise continuing since this is for fun and learning.

Now I want to look at how far through their careers laureates published the paper that would win them the Nobel prize. Firstly, looking at years since their first paper:

``` r
ggplot(b3[which(b3$prizepub - b3$firstpub < 50),], aes(x = category, y = prizepub - firstpub, fill = category)) + geom_boxplot() + ylab('Years Between First Publication and Winning Publication') + xlab('Category') + labs(subtitle = 'In physics, Nobel laureates hit their stride soon after first publication.') + ggtitle('Incubation Period of the Lesser Spotted Nobel Laureate') + coord_flip() + theme(legend.position = 'none') + theme_classic()
```

![](nobel_files/figure-markdown_github/unnamed-chunk-7-1.png)

yadyada. Now looking at it percentage-wise:

``` r
ggplot(b3[which(b3$prizepub - b3$firstpub < 50),], aes(x = category, y = (prizepub - firstpub)/(lastpub - firstpub), fill = category)) + geom_boxplot() + ylab('Winning paper publication date as fraction of time from first to last paper') + xlab('Category') + labs(subtitle = 'In physics, Nobel laureates hit their stride soon after first publication.') + ggtitle("How far through a laureate's career is their winning paper published?") + coord_flip()  + theme_classic() + theme(legend.position = 'none')
```

    ## Warning: Removed 29 rows containing non-finite values (stat_boxplot).

![](nobel_files/figure-markdown_github/unnamed-chunk-8-1.png)

We can see that physics laureates publish their winning paper soonest after their first paper, followed by biologists, then chemists. But is that because their winning paper is early or their first paper is late? Let's check using their age at time of publication of first paper.

To do that, let's get their year of birth by extracting it from their birth date:

``` r
n2 <- separate(n, birth_date, c('birthyear', NA, NA), sep = "-") 
```

Now the problem is, how to join the laureate dataset with the papers dataset, when neither the names nor the IDs match? Perhaps for individual prizes I could use a combination of year prize awarded and category, but I'd like something better. Spoiler alert: Couldn't find a way, unfortunately, although I did check whether there were any major differences in the raw year of first publication across category and didn't see any. Let me know if you have a way of connecting them!

``` r
ggplot(b3, aes(x = category, y = prizepub)) + geom_boxplot()
```

    ## Warning: Removed 1 rows containing non-finite values (stat_boxplot).

![](nobel_files/figure-markdown_github/unnamed-chunk-10-1.png) Okay, now let's lay out the year of their first publication, winning publication, last publication and prize award together. I want to use geom\_point() and show the different types as different colours, which means I'll need to first put this data into long format using gather().

``` r
head(b3)
```

    ## # A tibble: 6 x 7
    ##   laureate_id laureate_name prize_year prizepub firstpub lastpub category 
    ##         <dbl> <chr>              <dbl>    <dbl>    <dbl>   <dbl> <chr>    
    ## 1       20001 stoddart, j         2016     1991     1965    2018 chemistry
    ## 2       20002 feringa, b          2016     1999     1976    2014 chemistry
    ## 3       20003 sauvage, j          2016     1983     1969    2014 chemistry
    ## 4       20004 modrich, p          2015     1989     1971    2014 chemistry
    ## 5       20005 lindahl, t          2015     1974     1962    2012 chemistry
    ## 6       20006 sancar, a           2015     1983     1978    2017 chemistry

``` r
b4 <- gather(b3, Type, Year, prize_year:lastpub)
head(b4)
```

    ## # A tibble: 6 x 5
    ##   laureate_id laureate_name category  Type        Year
    ##         <dbl> <chr>         <chr>     <chr>      <dbl>
    ## 1       20001 stoddart, j   chemistry prize_year  2016
    ## 2       20002 feringa, b    chemistry prize_year  2016
    ## 3       20003 sauvage, j    chemistry prize_year  2016
    ## 4       20004 modrich, p    chemistry prize_year  2015
    ## 5       20005 lindahl, t    chemistry prize_year  2015
    ## 6       20006 sancar, a     chemistry prize_year  2015

What I want here is to have each laureate's name on a line on the y axis, as if I was doing a lollipop plot or joyplot. Unfortunately, there are like 1000 laureates, so I'm going to just pick some, namely the Medicine & Physiology laureates from 2000 to 2019. This is arbitrary; I just played around to see what fit nicely onto the graph.

The basic working version of this is here. Note the filtering for outliers - the 50 year mark is fairly arbitrary and may get rid of some legitimate data, but it does exclude obviously wrong ones.

``` r
ggplot(b4[which(b3$prize_year %in% c(2000:2019) & b4$category == 'medicine' & b3$prizepub - b3$firstpub < 50),], aes(x = Year, y = laureate_name, color = Type, size = 2)) + geom_point()
```

![](nobel_files/figure-markdown_github/unnamed-chunk-12-1.png)

However, I wanted to make some changes for cosmetic and information purposes, particularly ordering the names by year of prize. Unfortunately, having converted my data to long format to color code the types of publication meant that I no longer had prize\_year available to use to order the name factors, so I made b5, which has prize\_year added to it.

``` r
priz <- b3[, c(1,3)]
b5 <- left_join(b4, priz, by = 'laureate_id')
```

This means I can make:

``` r
ggplot(b5[which(b3$prize_year %in% c(2000:2019) & b5$category == 'medicine' & b3$prizepub - b3$firstpub < 50),], aes(x = Year, y = reorder(laureate_name, prize_year), color = Type)) + geom_point() 
```

![](nobel_files/figure-markdown_github/unnamed-chunk-14-1.png)

And with cosmetic alterations (e.g. decreasing opacity to 0.8 because some of the points overlap) and labelling:

``` r
ggplot(b5[which(b3$prize_year %in% c(2000:2019) & b5$category == 'medicine' & b3$prizepub - b3$firstpub < 50),], aes(x = Year, y = reorder(laureate_name, prize_year), color = Type, size = 2, alpha = .8)) + geom_point() + guides(size = FALSE, alpha = FALSE) + ylab('Laureate name') + ggtitle('Milestones in Nobel Laureate Publications')
```

![](nobel_files/figure-markdown_github/unnamed-chunk-15-1.png)
