install.packages("tidyverse")
library("tidyverse")
tbl <- read_delim("/Users/nikolaydimov/Documents/Master i e-businessteknologi og Cybersikkerhet/BTS4210-1 23H Data Mining and Business Analytics/Practice/rpractice/tilsyn.csv", delim = ";")
spec(tbl)

is.integer(tbl$orgnummer)
is.numeric(tbl$orgnummer)

mutate(tbl, orgnummer=as.integer(orgnummer))
tbl1 <- mutate(tbl, orgnummer=as.integer(orgnummer))
is.integer(tbl1$orgnummer)

select(tbl,orgnummer,navn)
tbl

tbl2 <- summarise(tbl1, n_controls = n(), worst_rating = max(total_karakter))

select(tbl, navn, poststed, total_karakter) %>% filter(poststed =="SANDNES") %>% arrange(desc(total_karakter))


