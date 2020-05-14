COVID-19’s gendered impact on academic productivity
================
Megan Frederickson
May 11, 2020

With schools and daycares closed because of COVID-19, many academics are
currently working from home with their kids underfoot. Writing in
*Nature,* [Minello](https://www.nature.com/articles/d41586-020-01135-9)
suggested the pandemic is disproportionately affecting the productivity
of female academics, because women often do more caregiving than men.

I quantified this effect by analyzing data on preprint submissions to
[arXiv](https://arxiv.org/) and [bioRxiv](https://www.biorxiv.org/), two
preprint servers that together cover many STEM fields. Peer review takes
time, so it is still too soon to see COVID-19’s effects on the numbers
of journal articles published by female versus male academics. However,
a growing number of academics make their submitted or in-progress
manuscripts available on preprint servers, meaning it might be possible
to measure the pandemic’s effect on preprint submissions in real time.

I used these packages:

``` r
#Load packages
library(tidyverse) #includes ggplot2, dplyr, readr, stringr
library(knitr)
library(cowplot)
library(gender)
library(aRxiv)
library(rbiorxiv)
library(lubridate)
library(anytime)
```

## arXiv submissions

I scraped submission data from arXiv, a preprint server for physics,
math, computer science, statistics, and other quantitative disciplines.
I used the aRxiv package to scrape the data, see:

Karthik, R. and K. Broman (2019). aRxiv: Interface to the arXiv API. R
package version 0.5.19. <https://CRAN.R-project.org/package=aRxiv>

I began by scraping all records for March 15-April 15, 2020, during the
COVID-19 pandemic, and for the same date range in 2019. Then, I expanded
to scrape all the data for Jan 1, 2020 to April 30, 2020, inclusive. I
scraped the data in batches, as recommended in the aRxiv package
tutorial. For brevity, I am not reproducing the code here, but it is
available in the [R markdown
file](https://github.com/drfreder/pandemic-pub-bias/blob/master/README.Rmd)
included in this repo.

Next, I assigned gender to author names using the gender package, see:

Mullen, L. (2019). gender: Predict Gender from Names Using Historical
Data. R package version 0.5.3, <https://github.com/ropensci/gender>.

This package returns the probability that a name is male or female by
comparing the name to names in a database; I used the U.S. Social
Security Administration baby names database.

Please note: this is a brute force method of predicting gender, and it
has many limitations, as discussed by the package authors on their
GitHub repo and included links. By using this method, I am not assuming
that individuals are correctly gendered in the resulting dataset, but
merely that it provides insight into gender’s effects in aggregate
across the population of preprint authors. This approach clearly
mis-genders or excludes some individual authors, but it can reveal
gender bias in a large enough dataset.

I predicted the genders of all preprint authors, and summarized the data
as the number of male and female authors of each preprint, regardless of
author order. This code takes a while to run, so it is not run when
knitting this markdown document.

``` r
#Not run
#First combine data for year-by-year comparison
df.2020 <- read.csv("Data/arxiv_2020_data.csv") #Read in data
df.2019 <- read.csv("Data/arxiv_2019_data.csv")
df.full <- rbind(df.2019, df.2020) #Combine in one dataframe

#Next combine data for early 2020 comparison
df.early2020 <- read.csv("Data/arxiv_early2020_data.csv")
df.update <- read.csv("Data/arxiv_update2020_data.csv")
df.update.2 <- read.csv("Data/arxiv_update2020_2_data.csv")
df.all2020 <- rbind(df.2020, df.early2020, df.update, df.update.2) #Early 2020 data

split.names <- function(x){strsplit(as.character(x), "|", fixed=TRUE)} #Function to split strings of author names

#For the year-over-year dataset
df.full$split.names <- lapply(df.full$authors, split.names) #Apply function

all_first_names <- word(unlist(df.full$split.names),1) #Make a list of all author first names
gender <- gender(all_first_names, method = "ssa") #Predict gender
gender <- unique(gender[ , c(1,2,4)]) #Keep only unique names

#This loop is an inelegant way of counting the number of male and female authors for each paper
tmp <- NULL
for(i in 1:length(df.full$authors)){
  tmp <- as.data.frame(word(unlist(df.full$split.names[[i]]), 1))
  colnames(tmp) <- "name"
  tmp <- merge(tmp, gender, by="name", all.x=TRUE, all.y=FALSE)
  df.full$male.n[i] <- sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "male")))), na.rm=TRUE)
  df.full$female.n[i] <-  sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "female")))), na.rm=TRUE)
}

df.full.output <- as.data.frame(apply(df.full,2,as.character)) 
write.csv(df.full.output, "Data/arxiv_full_gender.csv") #Save data

#Same for the early 2020 dataset
df.all2020$split.names <- lapply(df.all2020$authors, split.names)

tmp <- NULL
all_first_names <- word(unlist(df.all2020$split.names),1)
gender <- gender(all_first_names, method = "ssa")
gender <- unique(gender[ , c(1,2,4)])

for(i in 1:length(df.all2020$authors)){
  tmp <- as.data.frame(word(unlist(df.all2020$split.names[[i]]), 1))
  colnames(tmp) <- "name"
  tmp <- merge(tmp, gender, by="name", all.x=TRUE, all.y=FALSE)
  df.all2020$male.n[i] <- sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "male")))), na.rm=TRUE)
  df.all2020$female.n[i] <- sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "female")))), na.rm=TRUE)
}

df.all2020.output <- as.data.frame(apply(df.all2020,2,as.character))
write.csv(df.all2020.output, "Data/arxiv_all2020_gender.csv") #Save data
```

Next, I calculated some summary statistics for the arXiv dataset I
assembled.

``` r
df.full <- read.csv("Data/arxiv_full_gender.csv") #Read in data
df.full <- df.full[!duplicated(df.full), ] #Remove duplicates, if any
df.full$author.n <- str_count(df.full$authors, pattern = "\\|")+1 #Count author number
df.full$year <- as.factor(year(as.Date(df.full$submitted))) #Extract year

df.all2020 <- read.csv("Data/arxiv_all2020_gender.csv") #Read in data
df.all2020 <- df.all2020[!duplicated(df.all2020), ] #Remove duplicated rows, if any
df.all2020$author.n <- str_count(df.all2020$authors, pattern = "\\|")+1 #Count author number
df.all2020$year <- as.factor(year(as.Date(df.all2020$submitted))) #Extract year

all.arxiv <- rbind(df.full, df.all2020) #Combine datasets
all.arxiv <- all.arxiv[!duplicated(all.arxiv), ] #Remove duplicates
total.preprints <- length(all.arxiv$id) #Total number of preprints
total.authors <- sum(all.arxiv$author.n) #Total number of authors
total.authors.with.gender <- sum(all.arxiv$male.n+all.arxiv$female.n) #Total number of authors with gender inferred
per.gender <- round((total.authors.with.gender/total.authors)*100, 1) #Percent of authors with gender

year.arxiv.preprints <- length(df.full$"id") #Total number of preprints for year-over-year comparison
year.arxiv.authors <- sum(df.full[, "male.n"]+df.full[, "female.n"]) #Authors with gender inferred for year-over-year comparison
```

There are 83118 preprints in the full arXiv dataset, with a total of
399769 non-unique authors. I inferred the gender of 192332 authors, or
48.1%, with the rest omitted from subsequent analyses. This a lower
success rate for predicting author gender for the arXiv dataset than for
the bioRxiv dataset (see below), reflecting the fact that arXiv
preprints are more likely to list large consortia as authors (e.g., CMS
Collaboration), have authors who provide only first initials, or have
authors who have names not in the U.S. Social Security names database.

For just the comparison of March 15-April 15, 2020 with the same dates
in 2019, there are 28711 arXiv preprints with 67309 authors for whom I
inferred
gender.

### Comparing arXiv preprint authors between Mar/Apr 2019 and Mar/Apr 2020, by gender

How many male versus female authors of preprints were there in Mar/Apr
2020, compared to the same dates last year? Note: this is not the number
of unique authors; it includes authors who submitted multiple
preprints.

``` r
df.full$year <- as.factor(year(as.Date(df.full$submitted))) #Extract year
all <- as.data.frame(ungroup(df.full %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
all.long <- gather(all, Gender, number, Female:Male) #Make wide data long
all.t <- as.data.frame(t(all[,-1])) #Transpose
colnames(all.t) <- c("2019", "2020") #Fix column names
all.t$per.dif.1920 <- ((all.t$`2020`-all.t$`2019`)/(all.t$`2019`))*100 #Calculate percent change, 2020 over 2019
yr.labels = c("Mar. 15 - Apr. 15, 2019", "Mar. 15 - Apr. 15, 2020") #Set legend labels
colours1 = c("#f4a582","#ca0020") #Set colours
colours2 = c("#92c5de", "#0571b0") 
fontsize = 10

#Make figure comparing 2020 to 2019
p1 <- ggplot(data=all.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values=colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank(), legend.text = element_text(size=fontsize))+ggplot2::annotate("text", x=c(1, 2),  y=c(9000,29000), label = paste0("+", round(all.t$per.dif.1920[1:2], 1), "%"))+labs(title="arXiv", subtitle="all authors")+guides(fill=guide_legend(nrow=2))
p1 
```

![](README_files/figure-gfm/Visualize%20arXiv%20year-over-year%20data-1.png)<!-- -->

arXiv preprint submissions are up overall, but the number of male
authors is currently growing faster than the number of female authors.
Comparing preprint submissions in late March and early April 2020 to the
same dates in 2019, the number of male authors has grown more than the
number of female authors, both as a percent change and in absolute
terms.

The same data, but in tabular form:

``` r
#Or in tabular form
colnames(all.long) <- c("Year", "Gender", "Number of authors submitting arXiv preprints in Mar/Apr, by gender")
kable(all.long)
```

| Year | Gender | Number of authors submitting arXiv preprints in Mar/Apr, by gender |
| :--- | :----- | -----------------------------------------------------------------: |
| 2019 | Female |                                                               6968 |
| 2020 | Female |                                                               7157 |
| 2019 | Male   |                                                              25768 |
| 2020 | Male   |                                                              27416 |

### Comparing single-authored arXiv preprints between Mar/Apr 2019 and Mar/Apr 2020, by gender

How many arXiv preprints were single-authored by a female versus a male
academic in Mar/Apr, 2020, compared to the same dates last
year?

``` r
sole.authors <- as.data.frame(ungroup(subset(df.full, author.n == 1) %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
sole.long <- gather(sole.authors, Gender, number, Male:Female) #Make wide data long
sole.authors.t <- as.data.frame(t(sole.authors[,-1])) #Transpose
colnames(sole.authors.t) <- c("2019", "2020") #Fix column names
sole.authors.t$per.dif.1920 <- ((sole.authors.t$`2020`-sole.authors.t$`2019`)/(sole.authors.t$`2019`))*100 #Calculate percent change, 2020 over 2019

#Make figure for single-authored preprints
p2 <- ggplot(data=sole.long, aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = colours1, labels=yr.labels)+theme(legend.position = "top", legend.justification="left", legend.title = element_blank())+ggplot2::annotate("text", x=c(1, 2),  y=c(270,1350), label = paste0("+", round(sole.authors.t$per.dif.1920[1:2], 1), "%"))+theme(legend.text=element_text(size=fontsize))+labs(title="arXiv", subtitle = "sole authors")+guides(fill=guide_legend(nrow=2))
p2
```

![](README_files/figure-gfm/Sole%20authors-1.png)<!-- -->

Single-authored arXiv submissions are also up overall, but again the
number of male authors is currently growing faster than the number of
female authors, both as a percent change and in absolute terms.

The same data, but in tabular form:

``` r
#Or in tabular form
colnames(sole.long) <- c("Year", "Gender", "Number of single-authored arXiv preprints submittted, by gender")
kable(sole.long)
```

| Year | Gender | Number of single-authored arXiv preprints submittted, by gender |
| :--- | :----- | --------------------------------------------------------------: |
| 2019 | Male   |                                                            1161 |
| 2020 | Male   |                                                            1273 |
| 2019 | Female |                                                             188 |
| 2020 | Female |                                                             195 |

### Comparing arXiv preprint submissions in the months before and during COVID-19 pandemic, by gender

Next, I looked back over the months leading up to widespread
stay-at-home orders and school and childcare closures that North
Americans experienced beginning in late March or early April, 2020.
These measures were implemented to different degrees and on different
dates in different parts of the world, but I assumed their effects would
be most pronounced globally in the months of March and April,
2020.

``` r
start.date <- as.Date("2020-03-01") #Month WHO declared COVID-19 a pandemic
df.all2020$COVID <- ifelse(as.Date(df.all2020$submitted) < start.date, "January 1-February 29, 2020", "March 1-April 30, 2020") #Classify dates as COVID or not
arxiv <- as.data.frame(ungroup(subset(df.all2020, as.Date(submitted) >= "2020-01-01" & as.Date(submitted) <= "2020-04-30") %>% group_by(COVID) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE)))) #Summarize by month
arxiv.long <- gather(arxiv, gender, n, female.n:male.n) #Make wide data long
arxiv.long$gender <- as.factor(arxiv.long$gender) #Make sure gender is a factor
levels(arxiv.long$gender) <- c("Female", "Male") #Capitalize genders
arxiv.t <- as.data.frame(t(arxiv[, -1])) #Transpose
colnames(arxiv.t) <- c("January 1-February 29, 2020", "March 1-April 30, 2020") #Fix column names
arxiv.t$per <- ((arxiv.t$`March 1-April 30, 2020`-arxiv.t$`January 1-February 29, 2020`)/(arxiv.t$`January 1-February 29, 2020`))*100 #Calculate percent change
arxiv.long$per <- c(arxiv.t$per[1],arxiv.t$per[1],arxiv.t$per[2],arxiv.t$per[2]) #Add percent change to long-format dataframe
bump = 3000 #Set for figure annotatin
arxiv.long$y <- c((arxiv.long[2,3]+bump), (arxiv.long[2,3]+bump), (arxiv.long[4,3]+bump), (arxiv.long[4,3]+bump)) #Y coordinates for figure text annotation
arxiv.long$x <- c(1,1,2,2) #X coordinates for figure text annotation
m.labels=c("Jan. 1 - Feb. 29, 2020", "Mar. 1 - Apr. 30, 2020")

#Make figure
p3 <- ggplot(data=arxiv.long, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("arXiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Gender")+theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=fontsize), legend.justification="left")+geom_text(aes(x=x,y=y, label=paste0("+", round(per), "%")))+scale_fill_manual(values = colours2, labels=m.labels)+labs(title="arXiv", subtitle="all authors")+guides(fill=guide_legend(nrow=2))
p3
```

![](README_files/figure-gfm/Early%202020%20arXiv%20analysis-1.png)<!-- -->

Again, during the pandemic, the number of male authors has grown faster
than the number of female authors, both in absolute terms and as a
percent change, although the difference in percent change is only ~1%.

The same data, but in tabular form:

``` r
#Or in tabular form
colnames(arxiv.long) <- c("Dates", "Gender", "Number of arXiv preprint authors by gender")
kable(arxiv.long[,1:3])
```

| Dates                       | Gender | Number of arXiv preprint authors by gender |
| :-------------------------- | :----- | -----------------------------------------: |
| January 1-February 29, 2020 | Female |                                      11852 |
| March 1-April 30, 2020      | Female |                                      14122 |
| January 1-February 29, 2020 | Male   |                                      44993 |
| March 1-April 30, 2020      | Male   |                                      54056 |

Preprints had over 9000 more male authors during the pandemic than right
before, while female authors increased by a little more than 2200 over
the same period.

## bioRxiv submissions

Next, I scraped submission data from bioRxiv, the main preprint server
for biology. I used the rbiorxiv package, see:

Fraser, N (2020). rbiorxiv. R package,
<https://github.com/nicholasmfraser/rbiorxiv>

I scraped the same date ranges as for the arXiv analysis, above.

``` r
#Not run
#Get all submissions between Jan 1, 2020 and April 22, 2020
df.b.2020 <- biorxiv_content(from = "2020-01-01", to = "2020-04-22", limit = "*", format = "df")
#Get all submissions for March 15 to April 15, 2019
df.b.2019 <- biorxiv_content(from = "2019-03-15", to = "2019-04-15", limit = "*", format = "df")

#Update with April 22 to April 30, 2020 data
df.b.2020.update <- biorxiv_content(from = "2020-04-22", to = "2020-04-30", limit = "*", format = "df")

write.csv(df.b.2020, "Data/biorxiv_2020_data.csv")
write.csv(df.b.2019, "Data/biorxiv_2019_data.csv")
write.csv(df.b.2020.update, "Data/biorxiv_2020_update_data.csv")
```

I inferred the gender of corresponding authors of bioRxiv preprints, as
above. Note that the bioRxiv API only returns first names for
corresponding authors, and not for all authors. (Unfortunately.)

``` r
df.b.2019 <- read.csv("Data/biorxiv_2019_data.csv")
df.b.all2020 <- read.csv("Data/biorxiv_2020_data.csv")
df.b.2020.update <- read.csv("Data/biorxiv_2020_update_data.csv")

df.b.full <- rbind(df.b.2019, subset(df.b.all2020, as.Date(date) >= "2020-03-15" & as.Date(date) <= "2020-04-15")) #Make year comparison, subsetting 2020 data to just March 15 to April 15
df.b.all2020 <- rbind(df.b.all2020, df.b.2020.update) #Make early 2020 dataset

df.b.full$cor.author.first.name <- sapply(strsplit(as.character(df.b.full$author_corresponding), " "), head, 1) #Extract first names
df.b.all2020$cor.author.first.name <- sapply(strsplit(as.character(df.b.all2020$author_corresponding), " "), head, 1) #Extract first names

df.b.full$year <- as.factor(year(as.Date(df.b.full$date))) #Extract year
df.b.all2020$year <- as.factor(year(as.Date(df.b.all2020$date)))

#Predict gender for year-over-year dataset
gender <- NULL
gender <- gender(df.b.full$cor.author.first.name, method = "ssa")
gender <- unique(gender[ , c(1,2,4)])
df.b.full <- merge(df.b.full, gender, by.x = "cor.author.first.name",  by.y ="name", all = TRUE)
df.b.full <- df.b.full[!duplicated(df.b.full),] #Remove duplicated rows, if any

#Predict gender for early 2020 dataset
gender <- NULL
gender <- gender(df.b.all2020$cor.author.first.name, method = "ssa")
gender <- unique(gender[ , c(1,2,4)])
df.b.all2020 <- merge(df.b.all2020, gender, by.x = "cor.author.first.name",  by.y ="name", all = TRUE)
df.b.all2020 <- df.b.all2020[!duplicated(df.b.all2020),] #Remove duplicated rows, if any
```

Next I calculated some summary statistics for the bioRxiv dataset.

``` r
all.biorxiv <- rbind(df.b.all2020, df.b.full) #Combine datasets
total.b.preprints <- length(all.biorxiv$doi) #Total number of preprints
total.b.authors.with.gender <- length(all.biorxiv[!is.na(all.biorxiv$gender), "gender"]) #Total number of authors with gender predicted
per.b.gender <- round(total.b.authors.with.gender/total.b.preprints*100, 1) #Percent success

year.biorxiv.preprints <- length(df.b.full[, "doi"]) #Preprints for just year-over-year comparison
year.biorxiv.authors <- length(df.b.full[!is.na(df.b.full$gender), "gender"]) #Preprint authors with gender for year-over-year comparison
```

There are 24188 preprints in the full bioRxiv dataset, each with a
single corresponding author. I inferred the gender of 19378
corresponding authors, or 80.1%, with the rest omitted from subsequent
analyses.

For just the comparison of March 15-April 15, 2020 with the same dates
in 2019, there are 7818 bioRxiv preprints with 6333 corresponding
authors for whom I inferred
gender.

### Comparing bioRxiv preprint authors between Mar/Apr 2019 and Mar/Apr 2020, by gender

How many male and female corresponding authors were there on bioRxiv
preprints between Mar/Apr 2019 and 2020?

``` r
df.b.full$year <- as.factor(year(as.Date(df.b.full$date))) #Extract year
biorxiv.yr <- as.data.frame(ungroup(subset(df.b.full, !is.na(gender)) %>% group_by(year, gender) %>% summarize(n=n()))) #Summarize by year
biorxiv.yr$gender <- as.factor(biorxiv.yr$gender) #Make sure gender is a factor
levels(biorxiv.yr$gender) <- c("Female", "Male") #Capitalize genders
biorxiv.yr.wide <- spread(biorxiv.yr, year, n) #Make long data wide
biorxiv.yr.wide$per.dif.1920 <- ((biorxiv.yr.wide$`2020`-biorxiv.yr.wide$`2019`)/biorxiv.yr.wide$`2019`)*100 #Calculate percent change, 2019 to 2020

#Make figure for 2019 versus 2020 comparison
p4 <- ggplot(data=biorxiv.yr, aes(fill=year, y=n, x=as.factor(gender)))+geom_bar(position="dodge",stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+labs(fill="Date range")+scale_fill_manual(values = colours1, labels=yr.labels)+ggtitle("bioRxiv")+ggplot2::annotate("text", x=c(1,2),  y=c(1130,2680), label = paste0("+", round(biorxiv.yr.wide$per.dif.1920), "%"))+theme(legend.position="top", legend.justification="left", legend.title=element_blank(), legend.text=element_text(size=fontsize))+guides(fill = guide_legend(nrow=2))+labs(title="bioRxiv", subtitle="corresponding authors")
p4
```

![](README_files/figure-gfm/Visualize%20bioRxiv%202019%20versus%202020%20data-1.png)<!-- -->

``` r
p5 <- plot_grid(p1, p2, p4, nrow=1, align='hv', axis='lb') #Combine into part of a single figure
```

The same data in tabular
form:

``` r
colnames(biorxiv.yr) <- c("Year", "Gender", "Number of bioRxiv preprints submitted, by gender of corresponding author")
kable(biorxiv.yr)
```

| Year | Gender | Number of bioRxiv preprints submitted, by gender of corresponding author |
| :--- | :----- | -----------------------------------------------------------------------: |
| 2019 | Female |                                                                      803 |
| 2019 | Male   |                                                                     2006 |
| 2020 | Female |                                                                      997 |
| 2020 | Male   |                                                                     2527 |

The gender difference for corresponding authors of bioRxiv preprints is
more modest than in the arXiv dataset, but the number of male
corresponding authors of bioRxiv preprints has still increased more than
the number of female corresponding authors of bioRxiv preprints, again
both as a percent change and in absolute
terms.

### Comparing bioRxiv preprint submissions in the months before and during COVID-19 pandemic, by gender

As for arXiv submissions, I also compared bioRxiv submissions across
months for early
2020.

``` r
df.b.all2020$COVID <- ifelse(as.Date(df.b.all2020$date) < start.date, "January 1-February 29, 2020", "March 1-April 30, 2020") #Classify dates as COVID or not
biorxiv <- as.data.frame(ungroup(subset(df.b.all2020, as.Date(date) >= "2020-01-01" & as.Date(date) <= "2020-04-30" & !is.na(gender)) %>% group_by(COVID,gender) %>% summarize(n=n()))) #Summarize by month
biorxiv$gender <- as.factor(biorxiv$gender) #Make sure gender is a factor
levels(biorxiv$gender) <- c("Female", "Male") #Capitalize genders
biorxiv$per <- c(biorxiv[3,3]/biorxiv[1,3], biorxiv[4,3]/biorxiv[2,3],biorxiv[3,3]/biorxiv[1,3], biorxiv[4,3]/biorxiv[2,3]) #Calculate percent change
biorxiv$per <- biorxiv$per*100-100 #Make it actually a percent
bump = 300 #For figure annotation
biorxiv$y <- c((biorxiv[3,3]+bump), (biorxiv[4,3]+bump), (biorxiv[3,3]+bump), (biorxiv[4,3]+bump)) #y coordinates for figure text
biorxiv$x <- c(1,2,1,2) #x coodinates for figure text

#Make figure
p6 <- ggplot(data=biorxiv, aes(fill=COVID, y=n, x=gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("bioRxiv")+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Date range")+scale_fill_manual(values = colours2, labels=m.labels)+theme(legend.position="top", legend.justification="left", legend.title=element_blank(), legend.text=element_text(size=fontsize))+geom_text(aes(x=x,y=y, label=paste0("+", round(per), "%")))+labs(title="bioRxiv", subtitle="corresponding authors")+guides(fill=guide_legend(nrow=2))
p6
```

![](README_files/figure-gfm/Visualize%20bioRxiv%202020%20data-1.png)<!-- -->

``` r
p7 <- plot_grid(p3, p6, nrow=1, align='hv', axis='lb') #Combine into part of a single figure
p7 <- plot_grid(p7, NULL, nrow=1, rel_widths=c(2,1))
```

The same data, but in tabular form:

``` r
#Or in tabular form
colnames(biorxiv) <- c("Dates", "Gender", "Number of arXiv preprint authors by gender")
kable(biorxiv[,1:3])
```

| Dates                       | Gender | Number of arXiv preprint authors by gender |
| :-------------------------- | :----- | -----------------------------------------: |
| January 1-February 29, 2020 | Female |                                       1761 |
| January 1-February 29, 2020 | Male   |                                       4118 |
| March 1-April 30, 2020      | Female |                                       2113 |
| March 1-April 30, 2020      | Male   |                                       5053 |

Finally, I put it all together in a single figure.

``` r
p8 <- plot_grid(p5, p7, nrow=2, align = 'v', axis='l')
p8 #Omnibus figure
```

![](README_files/figure-gfm/Combine%20visualizations-1.png)<!-- -->

``` r
save_plot("figure.png", p8, base_height=8, base_width=8)
```

Throughout this analysis, effects are likely conservative because many
preprints describe research completed months ago, long before the
COVID-19 pandemic. Furthermore, it is important to note that gender is
not perfectly predictive of increased caregiving demands during the
pandemic; some male academics are primary caregivers for their children,
and many female academics do not have children at home. And, of course,
children are not the only people who may need more care than usual
during COVID-19. Furthermore, I am assuming that gender differences in
caregiving explain the patterns, but there may also be other mechanisms
at work. For example, perhaps there has been a surge in preprints from
some fields, and those fields are more male-dominated.

Nonetheless, the trends in both preprint servers are consistent with the
hypothesis that the pandemic is disproportionately hurting the
productivity of female scholars. How long this effect will persist, and
what its downstream consequences might be for journal publications and
academic careers, are open questions. This analysis could also be
extended to examine the effects of author order, field, and researcher
institution or country, or to other preprint servers, none of which I
have done.

In summary, in a ‘publish or perish’ world, it seems this pandemic could
be setting back the hard-won progress of women in STEM.

Interested in collaborating? Please get in touch with me at
m.frederickson(at)utoronto(dot)ca
