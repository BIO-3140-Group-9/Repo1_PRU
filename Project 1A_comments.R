library(ggplot2)
library(tidyverse)

dat <- read.csv("scales.csv")

#CPK: We can write code that will display results in the console rather than viewing the resulting tables [-1]
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

#CPK: PDFs should have your name in file name [-1]
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: This is unneeded [-1]
list.files(pattern=".pdf")

#CPK: Really solid work. Just be sure to include only what's need, no more or less. Also, please name scripts with your name as asked.


