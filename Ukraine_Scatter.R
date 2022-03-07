library(ggplot2)
library(dplyr)

data=read.csv("Ukraine.csv") %>%
  mutate(Gender=ifelse(grepl("M", PopGroup), "M",
                       ifelse(grepl("F", PopGroup), "F", "Total"))) %>%
  mutate(agegroup=gsub("M|F", "", PopGroup)) %>%
  subset(Gender %in% c("F", "M")) 


rdf=data %>%
      group_by(PopGroup) %>%
      summarise(rsq=cor(NSO, WorldPop)^2,
                NSO=max(NSO),
                WorldPop=0) %>%
  mutate(Gender=ifelse(grepl("M", PopGroup), "M",
                       ifelse(grepl("F", PopGroup), "F", "Total"))) %>%
  mutate(agegroup=gsub("M|F", "", PopGroup),
         rsqformat=round(rsq,2))
     
  genderplot=ggplot(data,aes(x=NSO, y=WorldPop, colour=Gender)) +
    geom_point() +
    geom_text(data=subset(rdf, Gender=="M"), aes(label=paste("rsq=", rsqformat), hjust=0, vjust=1),
              x=-Inf, y=Inf)+
    geom_text(data=subset(rdf, Gender=="F"), aes(label=paste("rsq=", rsqformat),hjust=1, vjust=0),
              x=Inf, y=-Inf)+
    facet_wrap(~agegroup, scale="free") +
    theme(axis.text.x=element_text(angle=90))+
    theme(legend.position = c(0.9,0.1))
  
  ggsave("genderplot.jpg", genderplot, width=10, height=10, units="in", dpi=400)
  
