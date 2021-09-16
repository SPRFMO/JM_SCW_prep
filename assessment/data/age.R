library(tidyverse)
library(ggplot2)
library(ggthemes)

# Read in new data 
df_age <- read.csv("All_Age_Data_Chile.csv") 
names(df_age)
unique( df_age$Method)
unique( df_age$Zone)
# Read in data that is in the assessment model for same sources 
df_age_in <- read.csv("AgeDataInAssessment.csv") 

# Make a plot of currently used wt-at-age for S-C Chile
# 
names(df_age_in) <- c("Year","Fleet","type","Label",1:12)
df_age_in <- df_age_in %>% pivot_longer(cols =5:16,names_to="age")
glimpse(df_age_in)

tmp1 <- df_age %>% filter(Year>2000,grepl('Fleet 2',Zone)) %>% 
group_by(Method,Year,Age) %>% 
summarise(Catch_no=sum(Catch_no) ) %>%
group_by(Method,Year) %>% mutate(Proportion=Catch_no/sum(Catch_no)) 

tmp2 <- df_age_in %>% 
  filter(type=='catch',Year>2000,Fleet==2) %>%  mutate(Age=as.integer(age)) %>% 
  group_by(Year,Age) %>% 
  transmute(Year,Method="assessment",Age,Catch_no=sum(value)) %>%
  group_by(Year) %>% mutate(Proportion=Catch_no/sum(Catch_no)) 

rbind(tmp1,tmp2) %>% filter(Age %in% (1:8))  %>% filter(Method != "validacion") %>% mutate(Age=as.factor(Age)) %>% 
ggplot(aes(x=Age,y=Proportion,fill=Method,group=Year)) +
geom_bar(stat='identity',position="dodge2") + theme_few(base_size=8) + 
facet_grid(Year~Method) + theme(legend.position = "none") 

tmp1 %>% inner_join(tmp2,by=c("Year","Age")) %>% filter(Age %in% 1:8) %>% 
  transmute(Year,Age=as.factor(Age),diff=Proportion.x-Proportion.y,pos=diff>=0) #%>% 
ggplot(aes(x=Age,y=diff,group=Year,fill=pos)) +
geom_bar(stat='identity',position="dodge2") + theme_few(base_size=7) +
facet_grid(Year~.) + theme(legend.position = "none") 

# Average wt age
df_age %>% filter(Age<8,Year>2010,grepl('Fleet 1',Zone)) %>% group_by(Method,Year,Age) %>% 
  summarise(Age=as.factor(Age),Catch_wt=sum(Catch_wt),Catch_no=(sum(Catch_no)),awt=1e6*Catch_wt/Catch_no) %>% 
  ggplot(aes(x=Year,y=awt,color=Age,shape=Method,type=Method)) + geom_point(size=2) + geom_line() + theme_few() +
  facet_grid(Method~.)

tmp1 <- df_age %>% 
  transmute(qtr=as.numeric(quarter),fleet=as.numeric(fleet), Method,Year,Age,Catch_no) %>% 
  filter(fleet==2,Year>2000) %>% group_by(Method,Year) %>% 
  mutate(p= prop.table(Catch_no) )
  tmp1

tmp2 <- df_age_in %>% 
  filter(type=='catch',Year>2000,Fleet==2) %>%  mutate(Age=as.integer(age)) %>% 
  group_by(Year) %>% 
  mutate(Year,Method="assessment",Age,p=prop.table(value ))

rbind(tmp1,tmp2) %>% filter(Age %in% (3:6))  %>%  mutate(Age=as.factor(Age)) %>% 
  ggplot(aes(x=Year,y=p,color=Age,shape=Method,type=Method)) + geom_point(size=2) + geom_line() + theme_few() +
  facet_grid(Method~.,scales='free')
#--
rbind(tmp1,tmp2) %>% filter(Age %in% (1:10),Method=="assessment")  %>%  select(Age,Proportion,Year)  %>% 
ggplot(aes(x=Age,y=as.factor(Year),height = Proportion)) + 
        geom_density_ridges(stat = "identity",scale = .4, alpha = .2,fill="salmon",color="black") + 
        theme_few() +
         ylab("Year") + xlab("Age (years)") + scale_y_discrete(limits=rev(levels(as.factor(sdf$Year))))

ggplot(aes(x = Age, y = Proportion, height = Year,group=Method)) +
geom_density_ridges()
  ) +
  guides(y = "none") +
  theme_ridges(grid = FALSE) +
  theme_few()

rbind(tmp1,tmp2) %>% filter(Age %in% (4:5))  %>%  mutate(Age=as.factor(Age)) %>% 
  ggplot(aes(x=Year,y=p,color=Age,shape=Method,type=Method)) + geom_point(size=2) + theme_few() 
  facet_grid(Method~.,scales='free')
library(ggridges)
?ggridges

tmp1 <- df_age %>% filter(Age<8,Year>2000,grepl('Fleet 2',Zone)) %>% 
  group_by(Method,Year) %>% 
  mutate(tot=sum(Catch_no)) %>% ungroup() %>% 
  group_by(Method,Year,Age) %>% 
  summarise(Age=as.factor(Age),Catch_wt=sum(Catch_wt),Catch_no=(sum(Catch_no)/tot),awt=1e6*Catch_wt/Catch_no) %>% transmute(Year,Method,Age,Catch_no,awt)

head(df_age)
head(tmp1)
         tmp2 <- df_age_in %>% filter(as.numeric(age)<8,type=='catch',Year>2000,Fleet==2) %>%  
           transmute(Year,Method="assessment",Age=as.factor(as.numeric(age)),Catch_no=value )
         rbind(tmp1,tmp2) %>% 
           ggplot(aes(x=Year,y=Catch_no,color=Age,shape=Method,type=Method)) + geom_point(size=2) + geom_line() + theme_few() +
           facet_grid(Method~.,scales='free')
         