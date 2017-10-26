library(dplyr)
library(DBI)
con <- DBI::dbConnect(RSQLite::SQLite(), path = "db.sqlite")

# 
# copy_to(con, nycflights13::flights, "flights",
#         temporary = FALSE, 
#         indexes = list(
#           c("year", "month", "day"), 
#           "carrier", 
#           "tailnum",
#           "dest"
#         )
# )
# 
# copy_to(con, nycflights13::airlines, "airlines",
#         temporary = FALSE)

dbListTables(con)


dbGetQuery(con, "SELECT * FROM USArrests")

dbGetQuery(con, "SELECT * FROM airlines")
t <- dbGetQuery(con, "SELECT * FROM airlines")
t
write.csv(t, "lala.txt")


dbGetQuery(con, "SELECT * FROM flights where dep_time > 600")
t <- dbGetQuery(con, "SELECT * FROM flights")

# magia -------------------------------------------------------------------
d <- tbl(con, "flights")
d

# WHOA! Funciono!! XD
write.csv(d, "lalad.txt")

library(ggplot2)

ggplot(t) +
  geom_point(aes(month, day))

ggplot(d) +
  geom_point(aes(month, day))

# Que es d?

d1 <- d %>% 
  filter(dep_time < 500) %>% 
  select(year, month) %>% 
  mutate(ym = year + month) %>% 
  group_by(ym) %>% 
  summarise(n = n())


d2 <-  tbl(con, "airlines")

glimpse(d)
glimpse(d2)



d %>% 
  select(-minute, -contains("time")) %>% 
  show_query()
  

d3 <- d %>% 
  group_by(carrier) %>% 
  summarise(nv = n()) %>% 
  left_join(d2) %>% 
  arrange(desc(nv))

show_query(d3)


dbGetQuery(con, "SELECT *
FROM (SELECT `TBL_LEFT`.`carrier` AS `carrier`, `TBL_LEFT`.`nv` AS `nv`, `TBL_RIGHT`.`name` AS `name`
           FROM (SELECT `carrier`, COUNT() AS `nv`
           FROM `flights`
           GROUP BY `carrier`) AS `TBL_LEFT`
           LEFT JOIN `airlines` AS `TBL_RIGHT`
           ON (`TBL_LEFT`.`carrier` = `TBL_RIGHT`.`carrier`)) ORDER BY `nv` DESC")


df <- collect(d3)
