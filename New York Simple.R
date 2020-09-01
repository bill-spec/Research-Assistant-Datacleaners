newYork <- read.csv("C:/Users/the36/Desktop/DataRA/hiphop_insyle/hiphop_nyc.csv")

#Removing right adjustment
newYork1$Column.Location <- trimws(newYork1$Column.Location, which = c("right"))


#Manual Cleaning 
newYork1$Column.Location <- sub("nj","new jersey", newYork1$Column.Location)
newYork1$Column.Location <- sub("ny","new york", newYork1$Column.Location)
newYork1$Column.Location <- sub("li","long island", newYork1$Column.Location)
newYork1$Column.Location <- sub("-","", newYork1$Column.Location)
newYork1$Column.Location <- sub("&","", newYork1$Column.Location)
newYork1$Column.Location <- sub(" ","", newYork1$Column.Location)

#Saying that if it follows a certain pattern it is added to the new calculated column

pattern3 <- c("manhattan|westchester|newjersey|westside|eastside|queens|longisland|brooklyn|nassausuffolk|connecticut|bronx|queens& long island|newyork|westchestercounty|queenslongisland|statenisland")
newYorkExport <- newYork1 %>% mutate(Coarse.Geo = ifelse(
  grepl(pattern3, Column.Location, ignore.case = TRUE), as.character(Column.Location), 1))

#Cleaning the new column

newYorkExport$Coarse.Geo <- sub("county","", newYorkExport$Coarse.Geo)
newYorkExport$Coarse.Geo <- sub(" state","", newYorkExport$Coarse.Geo)
newYorkExport$Coarse.Geo <- sub("queens long island","queens", newYorkExport$Coarse.Geo)
newYorkExport$Coarse.Geo <- sub("queenslongisland","queens", newYorkExport$Coarse.Geo)
newYorkExport$Coarse.Geo <- sub("queensand long island","queens", newYorkExport$Coarse.Geo)
newYorkExport$Coarse.Geo <- sub("brooklynlongisland","brooklyn", newYorkExport$Coarse.Geo)
newYorkExport$Coarse.Geo <- sub(" st","", newYorkExport$Coarse.Geo)
newYorkExport$Coarse.Geo <- sub("newengland","", newYorkExport$Coarse.Geo)
newYorkExport$Coarse.Geo <- sub("manhattanbronx","manhattan", newYorkExport$Coarse.Geo)
newYorkExport$Coarse.Geo <- sub("ville","", newYorkExport$Coarse.Geo)
newYorkExport$Coarse.Geo <- sub("brooklynqueens","brooklyn", newYorkExport$Coarse.Geo)

#showing the results

crossTable <- newYorkExport %>% group_by(Coarse.Geo) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

print('Total:')
sum(crossTable$count)
print('Found:')
sum(crossTable$count[1:12])
