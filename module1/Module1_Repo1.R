library(ggplot2)
library(tidyverse)

dat <- read.csv("scales.csv")

class.column <- sapply(dat,class)
view(class.column)

dimension.data <- data.frame(dim(dat))
rownames(dimension.data) <- c("N. Rows", "N. Columns")
view(dimension.data)

species.n <- dat %>% 
  group_by(species) %>%
  summarise(n.puncture = n())

species.s <- dat %>%
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

data_summary <- species.n %>%
  mutate(species.s)

view(data_summary)
#complies the code to show one table with n.puncture and n.specimen

pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

list.files(pattern=".pdf")

