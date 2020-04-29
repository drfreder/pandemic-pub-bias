COVID-19’s gendered impact on academic productivity
================
Megan Frederickson
28/04/2020

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

## arXiv submissions

First, I scraped submission data from arXiv, a preprint server for
physics, math, computer science, statistics, and other quantitative
disciplines. I used the aRxiv package to scrape the data, see:

Karthik, R. and K. Broman (2019). aRxiv: Interface to the arXiv API. R
package version 0.5.19. <https://CRAN.R-project.org/package=aRxiv>

I began by scraping all records for March 15-April 15, 2020, during the
COVID-19 pandemic, and for the same date range in 2019 and 2018. Then, I
scraped all the data for January 1 to April 22, 2020. I scraped the data
in batches, as recommended in the aRxiv package tutorial.

``` r
#Not run
#Get all submissions between March 15, 2020 and April 15, 2020 (during the COVID-19 pandemic)
n.2020 <- arxiv_count(query = 'submittedDate:[202003150000 TO 202004152400]')
df.2020.1 <- arxiv_search(query = 'submittedDate:[202003150000 TO 202003212400]', limit=n.2020, batchsize=1000)
df.2020.2 <- arxiv_search(query = 'submittedDate:[202003220000 TO 202003282400]', limit=n.2020, batchsize=1000)
df.2020.3 <- arxiv_search(query = 'submittedDate:[202003290000 TO 202004032400]', limit=n.2020, batchsize=1000)
df.2020.4 <- arxiv_search(query = 'submittedDate:[202004040000 TO 202004092400]', limit=n.2020, batchsize=1000)
df.2020.5 <- arxiv_search(query = 'submittedDate:[202004100000 TO 202004121415]', limit=n.2020, batchsize=2000)
df.2020.6 <- arxiv_search(query = 'submittedDate:[202004121420 TO 202004152400]', limit=n.2020, batchsize=1000)
df.2020.full <- rbind(df.2020.1, df.2020.2, df.2020.3, df.2020.4, df.2020.5, df.2020.6)
n.2020-length(df.2020.full$id) #Check that the number of records matches
write.csv(df.2020.full, file="Data/arxiv_2020_data.csv")

#Get all submission between March 15, 2019 and April 15, 2019 (the same dates last year)
n.2019 <- arxiv_count(query = 'submittedDate:[201903150000 TO 201904152400]')
df.2019.1 <- arxiv_search(query = 'submittedDate:[201903150000 TO 201903222400]', limit=n.2019, batchsize=2000)
df.2019.2 <- arxiv_search(query = 'submittedDate:[201903230000 TO 201903292400]', limit=n.2019, batchsize=2000)
df.2019.3 <- arxiv_search(query = 'submittedDate:[201903300000 TO 201904052400]', limit=n.2019, batchsize=2000)
df.2019.4 <- arxiv_search(query = 'submittedDate:[201904060000 TO 201904122400]', limit=n.2019, batchsize=2000)
df.2019.5 <- arxiv_search(query = 'submittedDate:[201904130000 TO 201904152400]', limit=n.2019, batchsize=2000)
df.2019.full <- rbind(df.2019.1, df.2019.2, df.2019.3, df.2019.4, df.2019.5)
n.2019-length(df.2019.full$id)
write.csv(df.2019.full, file="Data/arxiv_2019_data.csv")

#Get all submissions between March 15, 2018 and April 15, 2018 (same dates two years ago)
n.2018 <- arxiv_count(query = 'submittedDate:[201803150000 TO 201804152400]')
df.2018.1 <- arxiv_search(query = 'submittedDate:[201803150000 TO 201803212400]', limit=n.2018, batchsize=2000)
df.2018.2 <- arxiv_search(query = 'submittedDate:[201803220000 TO 201804032400]', limit=n.2018, batchsize=2000)
df.2018.3 <- arxiv_search(query = 'submittedDate:[201804040000 TO 201804092400]', limit=n.2018, batchsize=2000)
df.2018.4 <- arxiv_search(query = 'submittedDate:[201804100000 TO 201804152400]', limit=n.2018, batchsize=2000)
df.2018.full <- rbind(df.2018.1, df.2018.2, df.2018.3, df.2018.4)
n.2018-length(df.2018.full$id)
write.csv(df.2018.full, file="Data/arxiv_2018_data.csv")

#Get all submissions between Jan. 1, 2020 and March 15, 2020 (before COVID-19 pandemic)
n.early2020 <- arxiv_count(query = 'submittedDate:[202001010000 TO 202003152400]')
df.early2020.1 <- arxiv_search(query = 'submittedDate:[202001010000 TO 202001152400]', limit=n.early2020, batchsize=2000)
df.early2020.2 <- arxiv_search(query = 'submittedDate:[202001160000 TO 202001312400]', limit=n.early2020, batchsize=2000)
df.early2020.3 <- arxiv_search(query = 'submittedDate:[202002010000 TO 202002152400]', limit=n.early2020, batchsize=2000)
df.early2020.4 <- arxiv_search(query = 'submittedDate:[202002160000 TO 202002292400]', limit=n.early2020, batchsize=2000)
df.early2020.5 <- arxiv_search(query = 'submittedDate:[202003010000 TO 202003152400]', limit=n.early2020, batchsize=2000)
df.early2020.full <- rbind(df.early2020.1, df.early2020.2, df.early2020.3, df.early2020.4, df.early2020.5)
n.early2020-length(df.early2020.full$id)
write.csv(df.early2020.full, file="Data/arxiv_early2020_data.csv")

#Get all submissions between Apr. 16, 2020 and April 22, 2020 (update analysis with most recent data)
n.update <- arxiv_count(query = 'submittedDate:[202004160000 TO 202004222400]')
df.update <- arxiv_search(query = 'submittedDate:[202004160000 TO 202004222400]', limit=n.update, batchsize=2000)
n.update - length(df.update$id)
write.csv(df.update, file="Data/arxiv_update2020_data.csv")
```

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
#First compbine data for year-by-year comparison
df.2020 <- read.csv("Data/arxiv_2020_data.csv") #Read in data
df.2019 <- read.csv("Data/arxiv_2019_data.csv")
df.2018 <- read.csv("Data/arxiv_2018_data.csv")
df.full <- rbind(df.2018, df.2019, df.2020) #Combine in one dataframe

#Next combine data for 2020 comparison
df.early2020 <- read.csv("Data/arxiv_early2020_data.csv")
df.update <- read.csv("Data/arxiv_update2020_data.csv")
df.all2020 <- rbind(df.2020, df.early2020, df.update) #Combine in one dataframe

split.names <- function(x){strsplit(as.character(x), "|", fixed=TRUE)} #Function to split strings of author names

#For the year over year dataset
df.full$split.names <- lapply(df.full$authors, split.names) #Apply function

all_first_names <- word(unlist(df.full$split.names),1) #Make a list of all author first names
gender <- gender(all_first_names, method = "ssa") #Assign gender
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
write.csv(df.full.output, "Data/arxiv_full_gender.csv")

#Same for the all 2020 dataset
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
write.csv(df.all2020.output, "Data/arxiv_all2020_gender.csv")
```

First, let’s get some summary statistics for the arXiv dataset.

``` r
df.full <- read.csv("Data/arxiv_full_gender.csv") #Read in data
df.full <- df.full[!duplicated(df.full), ] #Remove duplicates, if any
df.full$author.n <- str_count(df.full$authors, pattern = "\\|")+1 #Count author number

df.all2020 <- read.csv("Data/arxiv_all2020_gender.csv") #Read in data
df.all2020 <- df.all2020[!duplicated(df.all2020), ] #Remove duplicated rows, if any
df.all2020$author.n <- str_count(df.all2020$authors, pattern = "\\|")+1 #Count author number

all.arxiv <- rbind(df.full, df.all2020)
all.arxiv <- all.arxiv[!duplicated(all.arxiv), ]

total.preprints <- length(all.arxiv$id)
total.authors <- sum(all.arxiv$author.n)
total.authors.with.gender <- sum(all.arxiv$male.n+all.arxiv$female.n)
per.gender <- round(total.authors.with.gender/total.authors*100, 1)
```

There are 90417 preprints in the arXiv dataset, with a total of 431833
non-unique authors. I inferred the gender of 206642 authors, or 47.9%,
with the rest omitted from subsequent
analyses.

### Total numbers of arXiv preprint authors in Mar/Apr 2020 compared to Mar/Apr 2019, by gender

How many male versus female authors of preprints were there in Mar/Apr,
2020, compared to the same dates last year? Note: this is not the number
of unique authors; it includes authors who submitted multiple
preprints.

``` r
df.full$year <- as.factor(year(as.Date(df.full$submitted))) #Extract year
all <- as.data.frame(ungroup(df.full %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
all$total <- all$Female+all$Male #Total authors
all.long <- gather(all, Gender, number, Male:Female) #Make wide data long
all.t <- as.data.frame(t(all[,-1])) #Transpose
colnames(all.t) <- c("2018", "2019", "2020") #Fix column names
all.t$per.dif.1920 <- ((all.t$`2020`-all.t$`2019`)/(all.t$`2019`))*100 #Calculate percent change, 2020 over 2019

#Make figure comparing 2020 to 2019
p1 <- ggplot(data=subset(all.long, year != 2018), aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = wes_palette("Royal1"), labels=c("Mar/Apr 2019", "Mar/Apr 2020"))+ggtitle("arXiv: 2019 vs 2020, all authors")+theme(legend.position = c(0.1, 0.9), legend.title = element_blank(), plot.title = element_text(hjust = 0.5))+annotate("text", x=c(1, 2),  y=c(8700,29200), label = paste0("+", round(all.t$per.dif.1920[1:2], 1), "%"))
p1 
```

![](README_files/figure-gfm/Visualize%20data-1.png)<!-- -->

arXiv preprint submissions are up overall, but the number of male
authors is currently growing faster than the number of female authors.
Comparing preprint submissions in late March and early April 2020 to the
same dates in 2019, the number of male authors has grown more than the
number of female authors, both as a percent change and in absolute
terms.

### Numbers of single-authored arXiv preprints in Mar/Apr 2020 compared to Mar/Apr 2019, by gender

How many arXiv preprints were sole-authored by a female versus a male
academic in Mar/Apr, 2020, compared to the same dates last
year?

``` r
sole.authors <- as.data.frame(ungroup(subset(df.full, author.n == 1) %>% group_by(year) %>% summarize(Female = sum(female.n, na.rm=TRUE), Male = sum(male.n, na.rm=TRUE)))) #Summarize by year
sole.long <- gather(sole.authors, Gender, number, Male:Female) #Make wide data long
sole.authors.t <- as.data.frame(t(sole.authors[,-1])) #Transpose
colnames(sole.authors.t) <- c("2018", "2019", "2020") #Fix column names
sole.authors.t$per.dif.1920 <- ((sole.authors.t$`2020`-sole.authors.t$`2019`)/(sole.authors.t$`2019`))*100 #Calculate percent change, 2020 over 2019

#Make figure for sole-authored preprints
p2 <- ggplot(data=subset(sole.long, year != 2018), aes(fill=as.factor(year), y=number, x=Gender))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = wes_palette("Royal1"), labels=c("Mar/Apr 2019", "Mar/Apr 2020"))+ggtitle("arXiv: 2019 vs 2020, sole authors")+theme(legend.position = c(0.1, 0.9), legend.title = element_blank(), plot.title = element_text(hjust = 0.5))+annotate("text", x=c(1, 2),  y=c(260,1350), label = paste0("+", round(sole.authors.t$per.dif.1920[1:2], 1), "%"))+theme(legend.position="none")
p2
```

![](README_files/figure-gfm/Sole%20authors-1.png)<!-- -->

Single-authored arXiv submissions are also up overall, but again the
number of male authors is currently growing faster than the number of
female authors, both as a percent change and in absolute
terms.

### Numbers of arXiv preprint submissions in January to April, 2020, by gender

Next, I zoomed in on the months leading up to widespread stay-at-home
orders and school and childcare closures that North Americans
experienced beginning in late March or early April, 2020. (These
measures were implemented to different degrees and on different dates in
different parts of the
world.)

``` r
df.all2020$month <- floor_date(as.Date(df.all2020$submitted), "month") #Bin by month
arxiv.m <- as.data.frame(ungroup(df.all2020 %>% group_by(month) %>% summarize(female.n=sum(female.n, na.rm=TRUE), male.n=sum(male.n, na.rm=TRUE), n.days = length(unique(as.Date(submitted)))))) #Summarize by month
arxiv.m.long <- gather(arxiv.m, gender, n, female.n:male.n) #Make wide data long
arxiv.m.long$pubs.per.day <- arxiv.m.long$n/arxiv.m.long$n.days #Adjust for different numbers of days in each month
arxiv.m.long$gender <- as.factor(arxiv.m.long$gender) #Make sure gender is a factor
levels(arxiv.m.long$gender) <- c("Female", "Male") #Capitalize genders

#Make early 2020 figure
p3 <- ggplot(data=arxiv.m.long, aes(fill=gender, y=pubs.per.day, x=month))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("arXiv: early 2020")+xlab("Month")+ylab("Preprint authors per day (no.)")+labs(fill="Gender")+facet_grid(~gender)+theme(legend.position="none", plot.title = element_text(hjust = 0.5))
p3
```

![](README_files/figure-gfm/2020%20analysis-1.png)<!-- -->

The number of male authors of arXiv preprints has increased through
early 2020, while the number of female authors of arXiv preprints has
almost plateaued during the pandemic.

## bioRxiv submissions

Next, I scraped submission data from bioRxiv, the main preprint server
for biology. I used the rbiorxiv package, see:

Fraser, N (2020). rbiorxiv. R package,
<https://github.com/nicholasmfraser/rbiorxiv>

I scraped all records for March 15-April 15, 2020, during the COVID-19
pandemic, and for the same date range in 2019 and 2018. I then expanded
to all of 2020, up to April 22, 2020.

``` r
#Not run
#Get all submissions between Jan 1, 2020 and April 22, 2020
df.b.2020 <- biorxiv_content(from = "2020-01-01", to = "2020-04-22", limit = "*", format = "df")
#Get all submissions for March 15 to April 15, 2019
df.b.2019 <- biorxiv_content(from = "2019-03-15", to = "2019-04-15", limit = "*", format = "df")
#Get all submissions for March 15 to April 15, 2018
df.b.2018 <- biorxiv_content(from = "2018-03-15", to = "2018-04-15", limit = "*", format = "df")

write.csv(df.b.2020, "Data/biorxiv_2020_data.csv")
write.csv(df.b.2019, "Data/biorxiv_2019_data.csv")
write.csv(df.b.2018, "Data/biorxiv_2018_data.csv")
```

I then inferred the gender of corresponding authors of bioRxiv
preprints, as above. Note that the bioRxiv API only returns first names
for corresponding authors, and not for all authors. (Unfortunately.)

``` r
df.b.2018 <- read.csv("Data/biorxiv_2018_data.csv") #Read in data
df.b.2019 <- read.csv("Data/biorxiv_2019_data.csv")
df.b.all2020 <- read.csv("Data/biorxiv_2020_data.csv")

df.b.full <- rbind(df.b.2018, df.b.2019, subset(df.b.all2020, as.Date(date) >= "2020-03-15" & as.Date(date) <= "2020-04-15")) #Make year comparison, subsetting 2020 data to just March 15 to April 15

df.b.full$cor.author.first.name <- sapply(strsplit(as.character(df.b.full$author_corresponding), " "), head, 1) #Extract first names
df.b.all2020$cor.author.first.name <- sapply(strsplit(as.character(df.b.all2020$author_corresponding), " "), head, 1) #Extract first names

#Assign gender for year-over-year dataset
gender <- NULL
gender <- gender(df.b.full$cor.author.first.name, method = "ssa")
gender <- unique(gender[ , c(1,2,4)])
df.b.full <- merge(df.b.full, gender, by.x = "cor.author.first.name",  by.y ="name", all = TRUE)
df.b.full <- df.b.full[!duplicated(df.b.full),] #Remove duplicated rows, if any

#Assigng gender for early 2020 dataset
gender <- NULL
gender <- gender(df.b.all2020$cor.author.first.name, method = "ssa")
gender <- unique(gender[ , c(1,2,4)])
df.b.all2020 <- merge(df.b.all2020, gender, by.x = "cor.author.first.name",  by.y ="name", all = TRUE)
df.b.all2020 <- df.b.all2020[!duplicated(df.b.all2020),] #Remove duplicated rows, if any
```

Let’s get some summary statistics for the arXiv dataset.

``` r
all.biorxiv <- rbind(df.b.all2020, df.b.full)
all.biorxiv <- all.biorxiv[!duplicated(all.biorxiv), ]

total.b.preprints <- length(all.biorxiv$doi)
total.b.authors <- length(all.biorxiv$author_corresponding)
total.b.authors.with.gender <- length(all.biorxiv[!is.na(all.biorxiv$gender), "gender"])
per.b.gender <- round(total.b.authors.with.gender/total.b.authors*100, 1)
```

There are 20350 preprints in the bioRxiv dataset. Because the bioRxiv
analysis focuses on corresponding authors only, and each preprint has a
single corresponding author, there are also 20350 non-unique
corresponding authors. I inferred the gender of 16401 corresponding
authors, or 80.6%, with the rest omitted from subsequent
analyses.

### Total numbers of bioRxiv preprint authors in Mar/Apr 2020 compared to Mar/Apr 2019, by gender

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
p4 <- ggplot(data=subset(biorxiv.yr, year != 2018), aes(fill=year, y=n, x=as.factor(gender)))+geom_bar(position="dodge",stat="identity")+theme_cowplot()+xlab("Gender")+ylab("Authors (no.)")+labs(fill="Year")+scale_fill_manual(values = wes_palette("Royal1"))+ggtitle("bioRxiv: 2019 vs 2020")+theme(plot.title = element_text(hjust = 0.5))+annotate("text", x=c(1,2),  y=c(1150,2700), label = paste0("+", round(biorxiv.yr.wide$per.dif.1920, 1), "%"))+theme(legend.position="none")
p4
```

![](README_files/figure-gfm/Visualize%20bioRxiv%202019%20versus%202020%20data-1.png)<!-- -->

``` r
p5 <- plot_grid(p1, p2, p4, nrow=1) #Combine into part of a single figure
```

The gender difference among corresponding authors for bioRxiv preprints
is more modest than in the arXiv dataset, but the number of male
corresponding authors of bioRxiv preprints has still increased more than
the number of female corresponding authors of bioRxiv preprints, again
both as a percent change and in absolute
terms.

### Numbers of bioRiv preprint submissions in January to April, 2020, by gender

As for arXiv submissions, I also compared bioRxiv submissions across
months for early
2020.

``` r
df.b.all2020$date <- as.Date(df.b.all2020$date) #Make sure date column is in date format
df.b.all2020$month <- floor_date(df.b.all2020$date, "month") #Bin by month
biorxiv.m <- as.data.frame(ungroup(subset(df.b.all2020, !is.na(gender)) %>% group_by(month, gender) %>% summarize(n=n(), n.days = length(unique(date)), pubs.per.day = n/n.days))) #Summarize by month and gender
biorxiv.m$gender <- as.factor(biorxiv.m$gender) #Make sure gender is a factor
levels(biorxiv.m$gender) <- c("Female", "Male") #Capitalize genders

#Compare Jan/Feb 2020 (immediately before pandemic) to Mar/Apr 2020 (during pandemic)
fem.mar.apr <- (biorxiv.m[biorxiv.m$month == "2020-03-01" & biorxiv.m$gender == "Female", "n"]+
biorxiv.m[biorxiv.m$month == "2020-04-01" & biorxiv.m$gender == "Female", "n"])/(biorxiv.m[biorxiv.m$month == "2020-03-01" & biorxiv.m$gender == "Female", "n.days"]+
biorxiv.m[biorxiv.m$month == "2020-04-01" & biorxiv.m$gender == "Female", "n.days"])
fem.jan.feb <- (biorxiv.m[biorxiv.m$month == "2020-01-01" & biorxiv.m$gender == "Female", "n"]+
biorxiv.m[biorxiv.m$month == "2020-02-01" & biorxiv.m$gender == "Female", "n"])/(biorxiv.m[biorxiv.m$month == "2020-01-01" & biorxiv.m$gender == "Female", "n.days"]+
biorxiv.m[biorxiv.m$month == "2020-02-01" & biorxiv.m$gender == "Female", "n.days"])
fem.per.increase <- (fem.mar.apr-fem.jan.feb)/fem.jan.feb*100

m.mar.apr <- (biorxiv.m[biorxiv.m$month == "2020-03-01" & biorxiv.m$gender == "Male", "n"]+
biorxiv.m[biorxiv.m$month == "2020-04-01" & biorxiv.m$gender == "Male", "n"])/(biorxiv.m[biorxiv.m$month == "2020-03-01" & biorxiv.m$gender == "Male", "n.days"]+
biorxiv.m[biorxiv.m$month == "2020-04-01" & biorxiv.m$gender == "Male", "n.days"])
m.jan.feb <- (biorxiv.m[biorxiv.m$month == "2020-01-01" & biorxiv.m$gender == "Male", "n"]+
biorxiv.m[biorxiv.m$month == "2020-02-01" & biorxiv.m$gender == "Male", "n"])/(biorxiv.m[biorxiv.m$month == "2020-01-01" & biorxiv.m$gender == "Male", "n.days"]+
biorxiv.m[biorxiv.m$month == "2020-02-01" & biorxiv.m$gender == "Male", "n.days"])
fem.per.increase <- round((fem.mar.apr-fem.jan.feb)/fem.jan.feb*100, 1)
m.per.increase <- round((m.mar.apr-m.jan.feb)/m.jan.feb*100, 1)
  
#Make early 2020 figure
p6 <- ggplot(data=biorxiv.m, aes(fill=gender, y=pubs.per.day, x=month))+geom_bar(position="dodge", stat="identity")+theme_cowplot()+ggtitle("bioRxiv: early 2020")+xlab("Month")+ylab("Preprint authors per day (no.)")+facet_grid(~gender)+theme(legend.position="none", plot.title = element_text(hjust = 0.5))
p6
```

![](README_files/figure-gfm/Visualize%20bioRxiv%202020%20data-1.png)<!-- -->

``` r
p7 <- plot_grid(p3, p6, nrow=1) #Combine into part of a single figure
```

The number of male authors of bioRxiv preprints has increased steadily
through early 2020, while the number of female authors of bioRxiv
preprints has increased only slightly. To put some numbers on this,
bioRxiv preprints with female corresponding authors have increased

from March/April, 2020, compared to January/February, 2020.

Finally, I put it all together in a single figure.

``` r
p8 <- plot_grid(p5, p7, nrow=2)
p8
```

![](README_files/figure-gfm/Combine%20visualizations-1.png)<!-- -->

``` r
save_plot("figure.png", p8, base_height=12, base_width=12)
```

Throughout this analysis, effects are likely conservative because many
preprints describe research completed months ago, long before the
COVID-19 pandemic. Furthermore, it is important to note that gender is
not perfectly predictive of increased caregiving demands during the
pandemic; some male academics are primary caregivers for their children,
many female academics do not have children at home, and children are not
the only people who may need more care than usual during COVID-19.

However, the trends in both preprint servers are consistent with the
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
