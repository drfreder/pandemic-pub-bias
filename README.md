The pandemic’s impact on academic productivity, by gender
================
Megan Frederickson
18/04/2020

## How is the global COVID-19 pandemic affecting the productivity of male versus female academics?

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

``` r
df.2020 <- read.csv("arxiv_2020_data.csv")
df.2019 <- read.csv("arxiv_2019_data.csv")
```
