The COVID-19 pandemic’s gendered impact on academic productivity
================
Megan Frederickson
18/04/2020

# How is COVID-19 affecting the productivity of male versus female academics?

There is debate on Twitter about how the COVID-19 pandemic is affecting
submissions to journals, and expecially whether there are differences
between male and female scholars. There are anectodal observations that
submissions from female scholars are down in March, 2020, presumably
because women are disproportionately shouldering increased caregiving
burdens because of school and childcare closures. I wanted to find out
it these anecdotes are borne out by data.

I don’t not have access to journal submission data directly, so I
decided to use data from large preprint servers, where academics often
archive their papers as they submit them for peer-review to journals.

First, I loaded some useful packages.

``` r
library(tidyverse) #includes ggplot2, dplyr, readr, stringr
library(knitr)
library(cowplot)
library(car)
library(nlme)
library(lme4)
library(glmm)
library(gender)
library(png)
library(reticulate)
library(aRxiv)
library(lubridate)
library(anytime)
library(zoo)
library(viridis)
```

Next, I scraped submission data from arXiv (<https://arxiv.org/>), which
is the main preprint server for physics, math, computer science,
statistics, and other quantitative disciplines. I scraped all records
for March 15-April 15, 2020, during the COVID-19 pandemic, and for the
same date range in 2019.

``` r
#Not run
#Get all submissions between March 15, 2020 and April 15, 2020 (during the COVID-19 pandemic)
n.2020 <- arxiv_count(query = 'submittedDate:[20200315 TO 20200415]')
df.2020.1 <- arxiv_search(query = 'submittedDate:[20200315 TO 20200321]', limit=n.2020, batchsize=1000)
df.2020.2 <- arxiv_search(query = 'submittedDate:[20200322 TO 20200328]', limit=n.2020, batchsize=1000)
df.2020.3 <- arxiv_search(query = 'submittedDate:[20200329 TO 20200403]', limit=n.2020, batchsize=1000)
df.2020.4 <- arxiv_search(query = 'submittedDate:[20200404 TO 20200409]', limit=n.2020, batchsize=1000)
df.2020.5 <- arxiv_search(query = 'submittedDate:[20200410 TO 20200412]', limit=2000, batchsize=500)
df.2020.6 <- arxiv_search(query = 'submittedDate:[20200413 TO 20200415]', limit=2000, batchsize=500)
df.2020.full <- rbind(df.2020.1, df.2020.2, df.2020.3, df.2020.4, df.2020.5, df.2020.6)
write.csv(df.2020.full, file="arxiv_2020_data.csv")

#Get all submission between March 15, 2019 and April 15, 2019 (the same dates last year)
n.2019 <- arxiv_count(query = 'submittedDate:[20190315 TO 20190415]')
df.2019.1 <- arxiv_search(query = 'submittedDate:[20190315 TO 20190322]', limit=n.2019, batchsize=1000)
df.2019.2 <- arxiv_search(query = 'submittedDate:[20190323 TO 20190329]', limit=n.2019, batchsize=1000)
df.2019.3 <- arxiv_search(query = 'submittedDate:[20190330 TO 20190405]', limit=n.2019, batchsize=1000)
df.2019.4 <- arxiv_search(query = 'submittedDate:[20190406 TO 20190412]', limit=n.2019, batchsize=1000)
df.2019.5 <- arxiv_search(query = 'submittedDate:[20190413 TO 20190415]', limit=n.2019, batchsize=500)
df.2019.full <- rbind(df.2019.1, df.2019.2, df.2019.3, df.2019.4, df.2019.5)
write.csv(df.2019.full, file="arxiv_2019_data.csv")
```

I then tidied the data by splitting strings of author names to identify
the first and last author of each preprint. I also calculated the number
of authors per preprint.

``` r
df.2020 <- read.csv("arxiv_2020_data.csv") #Read in data
df.2019 <- read.csv("arxiv_2019_data.csv")

df.2020$author.n <- str_count(df.2020$authors, pattern = "\\|")+1 #Count author number
df.2019$author.n <- str_count(df.2019$authors, pattern = "\\|")+1 

max(df.2020$author.n) #Max author number 
```

    ## [1] 914

``` r
max(df.2019$author.n) 
```

    ## [1] 1186

``` r
kable(df.2020 %>% group_by(author.n) %>% summarize(n=n()))
```

| author.n |    n |
| -------: | ---: |
|        1 | 1829 |
|        2 | 2876 |
|        3 | 2692 |
|        4 | 1791 |
|        5 | 1148 |
|        6 |  707 |
|        7 |  387 |
|        8 |  268 |
|        9 |  183 |
|       10 |  101 |
|       11 |   94 |
|       12 |   70 |
|       13 |   48 |
|       14 |   34 |
|       15 |   35 |
|       16 |   24 |
|       17 |   24 |
|       18 |   17 |
|       19 |   14 |
|       20 |   13 |
|       21 |    7 |
|       22 |   15 |
|       23 |    5 |
|       24 |    8 |
|       25 |    2 |
|       26 |    7 |
|       27 |    4 |
|       28 |    6 |
|       29 |    2 |
|       30 |    4 |
|       31 |    4 |
|       32 |    2 |
|       33 |    2 |
|       35 |    2 |
|       36 |    3 |
|       37 |    4 |
|       38 |    2 |
|       40 |    2 |
|       44 |    2 |
|       45 |    1 |
|       46 |    4 |
|       47 |    1 |
|       48 |    1 |
|       50 |    2 |
|       51 |    4 |
|       52 |    2 |
|       53 |    1 |
|       54 |    1 |
|       56 |    1 |
|       58 |    2 |
|       59 |    1 |
|       60 |    2 |
|       61 |    1 |
|       63 |    2 |
|       68 |    1 |
|       73 |    1 |
|       74 |    1 |
|       77 |    1 |
|       78 |    1 |
|       79 |    2 |
|       81 |    2 |
|       83 |    1 |
|       84 |    1 |
|       87 |    1 |
|       90 |    1 |
|       92 |    1 |
|       94 |    3 |
|       97 |    2 |
|      100 |    1 |
|      105 |    1 |
|      123 |    1 |
|      127 |    2 |
|      136 |    1 |
|      142 |    1 |
|      148 |    1 |
|      155 |    1 |
|      176 |    1 |
|      180 |    1 |
|      197 |    1 |
|      199 |    1 |
|      205 |    1 |
|      208 |    1 |
|      229 |    1 |
|      236 |    1 |
|      315 |    1 |
|      330 |    1 |
|      360 |    1 |
|      362 |    1 |
|      365 |    1 |
|      378 |    1 |
|      506 |    1 |
|      912 |    1 |
|      914 |    1 |

``` r
kable(df.2019 %>% group_by(author.n) %>% summarize(n=n()))
```

| author.n |    n |
| -------: | ---: |
|        1 | 1639 |
|        2 | 2680 |
|        3 | 2467 |
|        4 | 1622 |
|        5 |  948 |
|        6 |  598 |
|        7 |  329 |
|        8 |  207 |
|        9 |  150 |
|       10 |  113 |
|       11 |   93 |
|       12 |   56 |
|       13 |   47 |
|       14 |   32 |
|       15 |   24 |
|       16 |   32 |
|       17 |   21 |
|       18 |   20 |
|       19 |   16 |
|       20 |   11 |
|       21 |    7 |
|       22 |    9 |
|       23 |    8 |
|       24 |   11 |
|       25 |    5 |
|       26 |    5 |
|       27 |    3 |
|       28 |   10 |
|       29 |    6 |
|       30 |    5 |
|       31 |    5 |
|       32 |    6 |
|       33 |    1 |
|       34 |    4 |
|       35 |    3 |
|       36 |    2 |
|       37 |    4 |
|       38 |    3 |
|       39 |    4 |
|       40 |    1 |
|       41 |    2 |
|       44 |    3 |
|       45 |    1 |
|       47 |    1 |
|       48 |    1 |
|       49 |    4 |
|       50 |    3 |
|       52 |    2 |
|       54 |    1 |
|       55 |    1 |
|       56 |    3 |
|       57 |    1 |
|       59 |    1 |
|       60 |    1 |
|       62 |    2 |
|       63 |    1 |
|       65 |    1 |
|       66 |    1 |
|       69 |    1 |
|       71 |    1 |
|       75 |    1 |
|       76 |    1 |
|       78 |    1 |
|       79 |    2 |
|       83 |    2 |
|       85 |    2 |
|       88 |    1 |
|       93 |    1 |
|       94 |    1 |
|       98 |    1 |
|      107 |    1 |
|      115 |    1 |
|      124 |    1 |
|      127 |    2 |
|      132 |    1 |
|      139 |    1 |
|      143 |    1 |
|      160 |    1 |
|      167 |    1 |
|      175 |    1 |
|      176 |    1 |
|      203 |    1 |
|      227 |    1 |
|      264 |    1 |
|      303 |    1 |
|      348 |    1 |
|      461 |    1 |
|      470 |    1 |
|      482 |    1 |
|      483 |    1 |
|      846 |    1 |
|      851 |    2 |
|      853 |    2 |
|      872 |    1 |
|     1135 |    1 |
|     1186 |    1 |

``` r
df.2020$first.author <- sapply(strsplit(as.character(df.2020$authors), "|", fixed=TRUE), head, 1) #Extract first author 
df.2019$first.author <- sapply(strsplit(as.character(df.2019$authors), "|", fixed=TRUE), head, 1)

df.2020$last.author <- sapply(strsplit(as.character(df.2020$authors), "|", fixed=TRUE), tail, 1) #Extract last author
df.2019$last.author <- sapply(strsplit(as.character(df.2019$authors), "|", fixed=TRUE), tail, 1) 

df.2020$first.author.first.name <- sapply(strsplit(df.2020$first.author, " "), head, 1) #Extract first author first name
df.2019$first.author.first.name <- sapply(strsplit(df.2019$first.author, " "), head, 1)

df.2020$last.author.first.name <- sapply(strsplit(df.2020$last.author, " "), head, 1) #Extract last author first name
df.2019$last.author.first.name <- sapply(strsplit(df.2019$last.author, " "), head, 1)

#all.authors.2020 <- unlist(strsplit(as.character(df.2020$authors), "|", fixed=TRUE))
#all.authors.first.names.2020 <- sapply(strsplit(all.authors.2020, " "), head, 1)

#all.authors.2019 <- unlist(strsplit(as.character(df.2019$authors), "|", fixed=TRUE))
#all.authors.first.names.2019 <- sapply(strsplit(all.authors.2019, " "), head, 1)
```

Then I cleaned up the categories and dates for later
use.

``` r
df.2020$cat <- sapply(strsplit(as.character(df.2020$primary_category), ".", fixed=TRUE), head, 1) #Lump similar categories
df.2019$cat <- sapply(strsplit(as.character(df.2019$primary_category), ".", fixed=TRUE), head, 1)

df.2019$date <- as.Date(df.2019$submitted)
df.2020$date <- as.Date(df.2020$submitted)
```

I assigned gender to author first names using the gender package
(<https://github.com/ropensci/gender>). This package returns the
probability that a name is male or female by comparing the name to a
names in a database; I used the U.S. Social Security baby names
database. I started by assigning gender to first and last authors of
preprints.

Please note: this is a brute force and rather crude method of predicting
gender, and it has many limitations, as discussed by the package authors
on their GitHub repo and included links. By using this method, I am not
assuming that individuals are correctly gendered in the resulting
dataset, but merely that it provides insight into gender’s effects
across the aggregated population of preprint authors.

``` r
gender.2020 <- NULL
gender.2020 <- gender(df.2020$first.author.first.name, method = "ssa")
```

    ## Warning: `data_frame()` is deprecated as of tibble 1.1.0.
    ## Please use `tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
gender.2020 <- unique(gender.2020[ , c(1,2,4)])
df.2020 <- merge(df.2020, gender.2020, by.x = "first.author.first.name",  by.y ="name", all = TRUE)
colnames(df.2020)[which(colnames(df.2020) == "proportion_male")] <- "first.author.prop.male"
colnames(df.2020)[which(colnames(df.2020) == "gender")] <- "first.author.gender"

gender.2020 <- NULL
gender.2020 <- gender(df.2020$last.author.first.name, method = "ssa")
gender.2020 <- unique(gender.2020[ , c(1,2,4)])
df.2020 <- merge(df.2020, gender.2020, by.x = "last.author.first.name",  by.y ="name", all = TRUE)
colnames(df.2020)[which(colnames(df.2020) == "proportion_male")] <- "last.author.prop.male"
colnames(df.2020)[which(colnames(df.2020) == "gender")] <- "last.author.gender"

gender.2019 <- NULL
gender.2019 <- gender(df.2019$first.author.first.name, method = "ssa")
gender.2019 <- unique(gender.2019[ , c(1,2,4)])
df.2019 <- merge(df.2019, gender.2019, by.x = "first.author.first.name",  by.y ="name", all = TRUE)
colnames(df.2019)[which(colnames(df.2019) == "proportion_male")] <- "first.author.prop.male"
colnames(df.2019)[which(colnames(df.2019) == "gender")] <- "first.author.gender"

gender.2019 <- NULL
gender.2019 <- gender(df.2019$last.author.first.name, method = "ssa")
gender.2019 <- unique(gender.2019[ , c(1,2,4)])
df.2019 <- merge(df.2019, gender.2019, by.x = "last.author.first.name",  by.y ="name", all = TRUE)
colnames(df.2019)[which(colnames(df.2019) == "proportion_male")] <- "last.author.prop.male"
colnames(df.2019)[which(colnames(df.2019) == "gender")] <- "last.author.gender"
```

Next, I predicted the genders of ALL preprint authors, and summarized
the data as the number of male and female authors of each preprint,
regardless of order. This code takes a while to run, so it is not run
when knitting this markdown document.

``` r
#Not run
split.names <- function(x){strsplit(as.character(x), "|", fixed=TRUE)}  
df.2020$split.names <- lapply(df.2020$authors, split.names)
tmp <- NULL
for(i in 1:length(df.2020$authors)){
  tmp[i] <- as.vector(gender(word(unlist(df.2020$split.names[[i]]), 1), method="ssa")[, 4])
  df.2020$male.n[i] <- as.numeric(str_count(as.character(tmp[i]), pattern = paste(sprintf("\\b%s\\b", "male"))))
  df.2020$female.n[i] <- as.numeric(str_count(as.character(tmp[i]), pattern = paste(sprintf("\\b%s\\b", "female"))))
}
df.2020$prop.female.authors <- df.2020$female.n/(df.2020$female.n+df.2020$male.n)
df.2020$prop.female.authors.ex.single <- ifelse((df.2020$female.n+df.2020$male.n) > 1, df.2020$prop.female.authors, NA)
df.2020.output <- as.data.frame(apply(df.2020,2,as.character))
write.csv(df.2020.output, "arxiv_2020_gender.csv")

split.names <- function(x){strsplit(as.character(x), "|", fixed=TRUE)}  
df.2019$split.names <- lapply(df.2019$authors, split.names)
tmp <- NULL
for(i in 1:
length(df.2019$authors)){
  tmp[i] <- as.vector(gender(word(unlist(df.2019$split.names[[i]]), 1), method="ssa")[, 4])
  df.2019$male.n[i] <- as.numeric(str_count(as.character(tmp[i]), pattern = paste(sprintf("\\b%s\\b", "male"))))
  df.2019$female.n[i] <- as.numeric(str_count(as.character(tmp[i]), pattern = paste(sprintf("\\b%s\\b", "female"))))
}

df.2019$prop.female.authors <- df.2019$female.n/(df.2019$female.n+df.2019$male.n)
df.2019$prop.female.authors.ex.single <- ifelse((df.2019$female.n+df.2019$male.n) > 1, df.2019$prop.female.authors, NA)
df.2019.output <- as.data.frame(apply(df.2019,2,as.character))
write.csv(df.2019.output, "arxiv_2019_gender.csv")
```

## First authors

I calculated what proportion of first authors are female, and then
visualized this proportion over time, comparing the same date range in
2019 and 2020. There appears to be no differences, at least aggregated
across all arxiv preprints (e.g., without considering field, etc.)

``` r
df.2020 <- read.csv("arxiv_2020_gender.csv") #Read in data
df.2019 <- read.csv("arxiv_2019_gender.csv")

#Rerun
df.2019$date <- as.Date(df.2019$submitted)
df.2020$date <- as.Date(df.2020$submitted)

sum.2019 <- subset(df.2019, !is.na(first.author.gender)) %>% group_by(date) %>% summarize(n=n(), female.n = length(which(first.author.gender =="female")), female.per = female.n/n)
sum.2019 <- ungroup(sum.2019)
sum.2019$year <- "2019"
sum.2019$date.noyear <- paste0(month(sum.2019$date), "/", day(sum.2019$date))

sum.2020 <- subset(df.2020, !is.na(first.author.gender)) %>% group_by(date) %>% summarize(n=n(), female.n = length(which(first.author.gender =="female")), female.per = female.n/n)
sum.2020 <- ungroup(sum.2020)
sum.2020$year <- "2020"
sum.2020$date.noyear <- paste0(month(sum.2020$date), "/", day(sum.2020$date))

summary <- rbind(sum.2019, sum.2020)
summary$anydate <- anydate(summary$date.noyear)

p1 <- ggplot(data=summary, aes(x=as.Date(anydate), y=female.per, color=year))+geom_point()+ylab("Female (%)")+xlab("Date")+labs(color="Year")+geom_smooth(method="lm", se=FALSE)+ggtitle("First authors")+theme_cowplot()
p1
```

    ## `geom_smooth()` using formula 'y ~ x'

![](README_files/figure-gfm/Visualize%20first%20author%20data-1.png)<!-- -->

## Last authors

I then visualized what proportion of last authors are female and how
this changes over time, again comparing the same date range in 2019 and
2020. Here, there does appear to be fewer female last authors in 2020
than in
2019.

``` r
sum.2019 <- subset(df.2019, !is.na(last.author.gender)) %>% group_by(date) %>% summarize(n=n(), female.n = length(which(last.author.gender =="female")), female.per = female.n/n)
sum.2019 <- ungroup(sum.2019)
sum.2019$year <- "2019"
sum.2019$date.noyear <- paste0(month(sum.2019$date), "/", day(sum.2019$date))

sum.2020 <- subset(df.2020, !is.na(last.author.gender)) %>% group_by(date) %>% summarize(n=n(), female.n = length(which(last.author.gender =="female")), female.per = female.n/n)
sum.2020 <- ungroup(sum.2020)
sum.2020$year <- "2020"
sum.2020$date.noyear <- paste0(month(sum.2020$date), "/", day(sum.2020$date))

summary <- rbind(sum.2019, sum.2020)
summary$anydate <- anydate(summary$date.noyear)
summary$weekday <- weekdays(summary$date)

p2 <- ggplot(data=summary, aes(x=anydate, y=female.per, color=year))+geom_point()+ylab("Female (%)")+xlab("Date")+labs(color="Year")+geom_smooth(method="lm", se=FALSE)+ggtitle("Last authors")+theme_cowplot()
p2
```

    ## `geom_smooth()` using formula 'y ~ x'

![](README_files/figure-gfm/Visualize%20last%20author%20data-1.png)<!-- -->

``` r
p3 <- plot_grid(p1, p2, nrow=2, labels=c("A", "B"))
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

``` r
p3
```

![](README_files/figure-gfm/Visualize%20last%20author%20data-2.png)<!-- -->

``` r
save_plot("plot.png", p3, base_height=5, base_width=6)
```

I was curious how this effect plays out by field, so I aggregated the
category information provided by arXiv into six main categories:
physics, math, stats, computer science, economics, and electrical
engineering. Only three categories have enough records to really use
reliably: physics, math, and computer science. It looks like the decline
in female last authorship in 2020 is driven in large part by physics and
math, but not computer science.

``` r
df.full <- rbind(df.2019, df.2020)
df.full$year <- as.factor(year(df.full$date))
df.full$big.cat <- ifelse((df.full$cat == "astro-ph" | df.full$cat == "cond-mat" | df.full$cat == "gr-qc" | df.full$cat == "hep-ex" | df.full$cat == "physics" | df.full$cat == "nucl-ex"| df.full$cat == "quant-ph" | df.full$cat == "nucl-th" | df.full$cat == "hep-ph" | df.full$cat == "nlin" | df.full$cat == "hep-th" | df.full$cat == "hep-lat"), "physics", ifelse(df.full$cat == "stat", "stats", ifelse(df.full$cat == "math" | df.full$cat == "math-ph", "math", ifelse(df.full$cat=="cs", "cs", ifelse(df.full$cat == "q-bio", "q-bio", ifelse(df.full$cat=="q-fin" | df.full$cat == "econ", "econ", ifelse(df.full$cat=="eess", "eess", NA)))))))

big.sum.last <- subset(df.full, !is.na(last.author.gender)) %>% group_by(year, big.cat) %>% summarize(total = n(), female.n = length(which(last.author.gender =="female")), female.per = female.n/total)

big.sum.first <- subset(df.full, !is.na(first.author.gender)) %>% group_by(year, big.cat) %>% summarize(total = n(), female.n = length(which(first.author.gender =="female")), female.per = female.n/total)

min = 500

p4 <- ggplot()+geom_point(data=subset(big.sum.last, total > min), aes(x=year,y=female.per), size=5, color="blue")+geom_line(data=subset(big.sum.last, total > min), aes(x=year,y=female.per, group=1), size=1, color="blue")+facet_wrap(~big.cat)
p4
```

![](README_files/figure-gfm/Last%20authors%20by%20field-1.png)<!-- -->

## All authors

Next I wanted to look at authorship in general, because we see different
patterns for first and last authors, but conventions regarding author
order are not consistent across fields.

### Sole authors

How many preprints were sole-authored by a male versus a female academic
in Mar/Apr, 2020, compared to the same dates last
year?

``` r
summary <- df.full %>% group_by(year, male.n, female.n) %>% summarize(n = n())
summary$total.n <- summary$male.n+summary$female.n
summary.sole.authors <- subset(summary, total.n == 1)
sole.wide <- spread(summary.sole.authors, year, n)
sole.wide$gender <- ifelse(sole.wide$male.n == 0, "Female", "Male")
sole.long <- gather(sole.wide, year, number, `2019`:`2020`)
sole.wide$per.dif <- (sole.wide$`2020`/sole.wide$`2019`)*100-100

p5 <- ggplot(data=sole.long, aes(fill=as.factor(year), y=number, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Preprints (no.)")+labs(fill="Year")+scale_fill_viridis(option="cividis", discrete=TRUE)+annotate("text", x=c(1,2),  y=c(650,2600), label = paste0("+", round(sole.wide$per.dif, 1), "%"))+ggtitle("1 author")+theme(legend.position = c(0.1, 0.85))
p5
```

![](README_files/figure-gfm/Sole%20authors-1.png)<!-- -->

### Two authors

``` r
summary.two.authors <- subset(summary, total.n == 2)
two.wide <- spread(summary.two.authors, year, n)
two.wide$gender <- ifelse(two.wide$male.n == 0, "All female", ifelse(two.wide$female.n == 0, "All male", "Mixed"))
two.long <- gather(two.wide, year, number, `2019`:`2020`)
two.wide$per.dif <- (two.wide$`2020`/two.wide$`2019`)*100-100

p6 <- ggplot(data=two.long, aes(fill=as.factor(year), y=number, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Preprints (no.)")+labs(fill="Year")+scale_fill_viridis(option="cividis", discrete=TRUE)+ggtitle("2 authors")+theme(legend.position = "none")
p6
```

![](README_files/figure-gfm/All%20authors-1.png)<!-- -->

### Three or more authors

``` r
summary.three.authors <- subset(summary, total.n >= 3)
three.wide <- spread(summary.three.authors, year, n)
three.wide$gender <- ifelse(three.wide$male.n == 0, "All female", ifelse(three.wide$female.n == 0, "All male", "Mixed"))
three.wide.sum <- three.wide %>% group_by(gender) %>% summarize(sum.2019 = sum(`2019`, na.rm=TRUE), sum.2020 = sum(`2020`, na.rm=TRUE))
three.long <- gather(three.wide.sum, year, number, sum.2019:sum.2020)

p7 <- ggplot(data=three.long, aes(fill=as.factor(year), y=number, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Preprints (no.)")+labs(fill="Year")+scale_fill_viridis(option="cividis", discrete=TRUE)+ggtitle("3+ authors")+theme(legend.position = "none")
p7
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
p8 <- plot_grid(p5,p6,p7, labels=c("A", "B", "C"))
save_plot("plot2.png", p8, base_height=6, base_width=8)
```

``` r
summary$female.n <- as.factor(summary$female.n)
sum.sum.female <- subset(summary, total.n > 0 & female.n != 0) %>% group_by (year, female.n) %>% summarize(total.pubs = sum(n, na.rm=TRUE))
sum.sum.female.wide <- spread(sum.sum.female, year, total.pubs)
sum.sum.female.wide$female.n <- as.numeric(as.character(sum.sum.female.wide$female.n))
sum.sum.female.wide$female.n.trunc <- ifelse(sum.sum.female.wide$female.n >= 5, "5+", sum.sum.female.wide$female.n)
sum.sum.female.wide$female.n.trunc <- as.factor(sum.sum.female.wide$female.n.trunc)
female <- sum.sum.female.wide %>% group_by(female.n.trunc) %>% summarize(sum.2019 = sum(`2019`, na.rm=TRUE), sum.2020 = sum (`2020`, na.rm=TRUE))
levels(female$female.n.trunc) <- c("1", "2", "3", "4", "5+", "Overall")
female <- rbind(female, c("Overall", sum(female$sum.2019), sum(female$sum.2020)))
female$per.dif <- (as.numeric(female$sum.2020)/as.numeric(female$sum.2019))*100-100
female$gender <-"Female"
colnames(female)[1] <- "Male or female authors (no.)"

summary$male.n <- as.factor(summary$male.n)
sum.sum.male <- subset(summary, total.n > 0 & male.n != 0) %>% group_by (year, male.n) %>% summarize(total.pubs = sum(n, na.rm=TRUE))
sum.sum.male.wide <- spread(sum.sum.male, year, total.pubs)
sum.sum.male.wide$male.n <- as.numeric(as.character(sum.sum.male.wide$male.n))
sum.sum.male.wide$male.n.trunc <- ifelse(sum.sum.male.wide$male.n >= 5, "5+", sum.sum.male.wide$male.n)
sum.sum.male.wide$male.n.trunc <- as.factor(sum.sum.male.wide$male.n.trunc)
male <- sum.sum.male.wide %>% group_by(male.n.trunc) %>% summarize(sum.2019 = sum(`2019`, na.rm=TRUE), sum.2020 = sum (`2020`, na.rm=TRUE))
levels(male$male.n.trunc) <- c("1", "2", "3", "4", "5+", "Overall")
male <- rbind(male, c("Overall", sum(male$sum.2019), sum(male$sum.2020)))
male$per.dif <- (as.numeric(male$sum.2020)/as.numeric(male$sum.2019))*100-100
male$gender <- "Male"
colnames(male)[1] <- "Male or female authors (no.)"

all <- rbind(female, male)
all$dif <- as.numeric(all$sum.2020) - as.numeric(all$sum.2019)
all.long <- gather(all, year, number, sum.2019:sum.2020)
all.long$year <- as.factor(all.long$year)
levels(all.long$year) <- c("2019", "2020")


p9 <- ggplot(data=all, aes(fill=gender, x=`Male or female authors (no.)`, y=per.dif))+ylab("Percent change, 2019 to 2020")+geom_bar(position="dodge", stat="identity")+theme_cowplot()
p9
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
p10 <- ggplot(data=all, aes(fill=gender, x=`Male or female authors (no.)`, y=dif))+ylab("Change in number of preprints, 2019 to 2020")+geom_bar(position="dodge", stat="identity")+theme_cowplot()
p10
```

![](README_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
p11 <- ggplot(data=all.long, aes(fill=year, x=`Male or female authors (no.)`, y=as.numeric(number)))+labs(fill="Year")+ylab("Preprints (no.)")+geom_bar(position="dodge", stat="identity")+theme_cowplot()+scale_fill_viridis(discrete=TRUE)+facet_wrap(~gender)
p11
```

![](README_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->
