library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggridges)

# Read in new data 
df_age <- read.csv("All_Age_Data_Chile.csv") 
#df_age <- read_csv("ChileAge2022.csv") 
names(df_age)

names(df_age) <- c("year","zone","quarter","age","catch_no","prop","length","var","mnwt","t","wtprop","method")
#names(df_age) <- c("year","macrozone","zone","quarter","age","catch_no","prop","length","var","mnwt","t","wtprop","method")

unique( df_age$method)
unique( df_age$zone)
df_age<-df_age %>% filter(method=="validacion") %>% mutate(fleet=
  ifelse(zone=="Arica - antofagasta (Fleet 1)","N_Chile",          
  ifelse(zone=="Caldera - Guaitecas (Fleet 2)","CS_Chile_ps",
  ifelse(zone=="Caldera - Guaitecas (acoustic fleet 2)" , "Chile_AcousCS",
    "Chile_AcousN" ) ) )
)

qtr_mn <- df_age%>% filter(year>1995,fleet=="CS_Chile_ps") %>% group_by(quarter,age) %>%
 summarise(mnwt=1000*sum(t)/sum(catch_no)) %>%
  pivot_wider(names_from=age,values_from=mnwt);
  flextable::flextable(qtr_mn)

#qtr4_yr_mn <- 
df_age%>% filter(age %in% 2:17,fleet=="CS_Chile_ps") %>% mutate(age=as.factor(age)) %>% group_by(year,age,quarter) %>% 
  summarise(mnwt=sum(t)/sum(catch_no)) %>%
  ggplot(aes(x=year,y=mnwt,color=age)) + geom_line(stat='identity') + 
  ggtitle("Quarters, CS Chile mean weight-at-age") + theme_few(base_size=14) + facet_wrap(.~quarter)

glbl_mn <- df_age %>% 
  group_by(age) %>% summarise(mnwt=1e3*sum(t)/sum(catch_no)) %>%
  pivot_wider(names_from=age,values_from=mnwt)
  glbl_mn


glbl_fsh_mn <- df_age %>% filter(fleet=="N_Chile"|fleet=="CS_Chile_ps")%>% 
  group_by(fleet,age) %>% summarise(mnwt=1e3*sum(t)/sum(catch_no)) %>%
  pivot_wider(names_from=age,values_from=mnwt)

glbl_srv_mn <- df_age %>% filter(fleet=="Chile_AcousN"|fleet=="Chile_AcousCS") %>%
  group_by(fleet,age) %>% summarise(mnwt=1000*sum(t)/sum(catch_no)) %>%
  pivot_wider(names_from=age,values_from=mnwt)
  glbl_srv_mn
  glbl_fsh_mn

ndf<-df_age %>% group_by(year,fleet,age) %>% summarise(no=sum(catch_no)) %>% 
  arrange(age) %>% pivot_wider(names_from=age,values_from=no)
  ndf

wtdf<-df_age %>% group_by(year,fleet,age) %>% summarise(mnwt=1e3*sum(t)/sum(catch_no)) %>% arrange(age) %>%
  pivot_wider(names_from=age,values_from=mnwt,values_fill=0)
  dim(wtdf );wtdf
  dim(ndf )
for (i in 1:dim(wtdf[2])){
  for (j in 3:15)
    if (wtdf[i,j]==0) wtdf[i,j] <- glbl_mn[j-2]
}
wtdf
write_csv( wtdf, "ChileWtAgesForModel.csv")

nodf <- df_age %>% group_by(fleet,year,age) %>% summarise(catch_no=sum(catch_no) ) 
nodf
nodf_wd <- nodf %>% pivot_wider(names_from=age,values_from=catch_no) %>% mutate_if(is.numeric,funs(ifelse(is.na(.), 0, .))) %>% select(str_sort(names(.),numeric=TRUE))
nodf_wd %>%filter(fleet=="Chile_AcousN")
write_csv( nodf_wd, "ChileAgesForModel.csv")

# Approach to getting mean weight-at-age
# 3 levels, global over all gears, if n < l_3, by gear if l_3 < n < l_2, by year and gear if n>l_1
# Get the counts and plot
#tmp1 <- df_age %>% group_by(macrozone,year,age) %>% summarise(no=sum(catch_no),mnwt=sum(t)/no ) %>%
tmp1 <- df_age %>% group_by(macrozone,year,age) %>% summarise(no=sum(catch_no)) %>% mutate(no=ifelse(no<250,NA,no)) %>%
  pivot_wider(names_from=age,values_from=no)
  is.na(tmp1)
tmp1 <- df_age %>% group_by(macrozone,year,age) %>% summarise(no=sum(catch_no)) %>% mutate(no=ifelse(no<250,NA,no)) 
tmp1 <- df_age %>% filter(macrozone<3) %>%group_by(type=as.factor(macrozone),year,age) %>% summarise(no=sum(catch_no),mnwt=sum(t)/no ) %>% filter(no>50)
tmp1 %>% filter(year>2000) %>% ggplot(aes(x=age,y=mnwt,color=type,fill=type)) + geom_point() + geom_smooth() + facet_grid(year~type) + theme_few()

library(mgcv)
bs <- gam(mnwt~s(age)+macrozone,data=tmp1)
plot(bs)
#%>%
  pivot_wider(names_from=age,values_from=no)


tmp1 %>% ggplot(aes(x=no)) + 
  stat_ecdf(geom = "step") + xlim(c(0,500))

df_age %>% 
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile()

geom_density()
ggplot(aes(x=age,y=as.factor(year),height = no)) + 
        geom_density_ridges(stat = "identity",scale = .4, alpha = .2,fill="salmon",color="black") + 
        theme_few() + facet_grid(.~macrozone)

df_age %>% group_by(macrozone,year,age) %>% summarise(mnwt=sum(t)/sum(catch_no) ) %>%
  pivot_wider(names_from=age,values_from=mnwt)

tmpno <- df_age %>% group_by(macrozone,year,age) %>% summarise(catch_no=sum(catch_no) ) %>%
group_by(macrozone,year) %>% mutate(Proportion=catch_no/sum(catch_no)) 
tmpwt <- df_age %>% group_by(macrozone,year,age) %>% summarise(wt=sum(t)/sum(catch_no) )# %>%
tmpwt <- df_age %>% group_by(macrozone,age) %>% summarise(wt=sum(t)/sum(catch_no) )# %>%
tmpwt <- df_age %>% group_by(age) %>% summarise(wt=sum(t)/sum(catch_no) )# %>%
tmpwt
group_by(macrozone,year) 
pivot_wider(tmpwt,names_from=age,values_from=wt) %>% mutate_if(is.numeric,funs(ifelse(is.na(.), 0, .))) 
#flextable::flextable
# Printout

?pivot_wider


# Read in data that is in the assessment model for same sources 
df_age_in <- read.csv("AgeDataInAssessment.csv") 

# Make a plot of currently used wt-at-age for S-C Chile
# 
names(df_age_in) <- c("Year","Fleet","type","Label",1:12)
df_age_in <- df_age_in %>% pivot_longer(cols =5:16,names_to="age")
glimpse(df_age_in)


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
         