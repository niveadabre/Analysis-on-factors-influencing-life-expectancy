library(readr)
expect<-read_csv("C:/Users/dabre/OneDrive/Desktop/lfe.csv")
head(expect) 

library(tidyverse) # metapackage with lots of helpful functions
library(gridExtra) # grid.arrange to make quick subplots
library(reshape2)
expect <- expect %>% 
  drop_na() %>% 
  rename('lifexp'='Life expectancy',
         'percexp'='percentage expenditure',
         'totexp'='Total expenditure',
         'admort'='Adult Mortality',
         'infmort'='infant deaths',
         'u5deaths'='under-five deaths',
         'hepb'='Hepatitis B',
         'HIV'='HIV/AIDS') %>% 
  filter(!is.na(lifexp), !is.na(admort), !is.na(infmort))
head(expect)

#Descriptive statistics grouped by status of country
expect %>% 
  group_by(Status) %>% 
  summarize(count = n(),
            avg_lifexp = mean(lifexp, na.rm=TRUE),
            #avg_infmort = mean(infmort, na.rm=TRUE),
            #avg_admort = mean(admort, na.rm=TRUE),
            avg_GDP = mean(GDP, na.rm=TRUE),
            avg_Population = mean(Population, na.rm=TRUE),
            avg_totexp = mean(totexp,na.rm=TRUE))


devd <- expect %>% 
  filter(Country %in% c('Australia', 'Germany', 'Israel'))
deving <- expect %>% 
  filter(Country %in% c('Afghanistan', 'Nigeria', 'Indonesia'))

# removing 2015 data, has some NA values
devd <- devd %>% 
  filter(Year != 2015)
deving <- deving %>% 
  filter(Year != 2015)

#EDA for Developed countries against percentage expenditure and life expectancy
devdp1 <- ggplot(devd, aes(Year, percexp))+
  geom_smooth(aes(color=Country), se=FALSE, show.legend=FALSE)+
  facet_grid(Country~.)

devdp2 <- ggplot(devd, aes(Year, lifexp))+
  geom_smooth(aes(color=Country), se=FALSE, show.legend=FALSE)+
  facet_grid(Country~.)
grid.arrange(devdp1, devdp2, nrow=1)

#EDA for Developed countries against GDP and life expectancy
devdp1 <- ggplot(devd, aes(Year, GDP))+
  geom_smooth(aes(color=Country), se=FALSE, show.legend=FALSE)+
  facet_grid(Country~.)

devdp2 <- ggplot(devd, aes(Year, lifexp))+
  geom_smooth(aes(color=Country), se=FALSE, show.legend=FALSE)+
  facet_grid(Country~.)
grid.arrange(devdp1, devdp2, nrow=1)

#EDA for developing countries against percentage expenditure and life expectancy
devingp1 <- ggplot(deving, aes(Year, percexp))+
  geom_smooth(aes(color=Country), se=FALSE, show.legend=FALSE)+
  facet_grid(Country~.)

devingp2 <- ggplot(deving, aes(Year, lifexp))+
  geom_smooth(aes(color=Country), se=FALSE, show.legend=FALSE)+
  facet_grid(Country~.)
grid.arrange(devingp1, devingp2, nrow=1)

#EDA for developing countries against GDP and life expectancy
devingp1 <- ggplot(deving, aes(Year, GDP))+
  geom_smooth(aes(color=Country), se=FALSE, show.legend=FALSE)+
  facet_grid(Country~.)

devingp2 <- ggplot(deving, aes(Year, lifexp))+
  geom_smooth(aes(color=Country), se=FALSE, show.legend=FALSE)+
  facet_grid(Country~.)
grid.arrange(devingp1, devingp2, nrow=1)

#correlation between life expectancy and percentage expenditure, use cor() to check 
cor(expect$lifexp, expect$percexp)

#correlation between life expectancy and GDP
cor(expect$lifexp, expect$GDP)

#correlation between life expectancy and Alcohol
cor(expect$lifexp,expect$Alcohol)




# looking at infant deaths and under-five deaths for developed countries
ggplot(devd, show.legend=FALSE)+
  geom_smooth(aes(Year, infmort),color='blue', se=FALSE)+
  
  geom_smooth(aes(Year, u5deaths), color='red', se=FALSE)+
  facet_grid(Country~.)

# looking at infant deaths and under-five deaths for developed countries
ggplot(deving, show.legend=FALSE)+
  geom_smooth(aes(Year, infmort), color='blue', se=FALSE)+
  geom_smooth(aes(Year, u5deaths), color='red', se=FALSE)+
  facet_grid(Country~.)


# any correlation between vaccinations?
cordf <- expect %>%
  drop_na() %>% 
  select(hepb, Polio, Measles, HIV, Diphtheria)
cormat <- cor(cordf)
melted <- melt(cormat)
ggplot(melted)+
  geom_tile(aes(Var1, Var2, fill=value))

cordf <- expect %>%
  drop_na() %>% 
  select(lifexp,admort,infmort,Alcohol,percexp)
cormat <- cor(cordf)
melted <- melt(cormat)
ggplot(melted)+
  geom_tile(aes(Var1, Var2, fill=value))

# how about adult mortality and infant mortality?
expect %>%
  group_by(Year) %>% 
  summarize(Polio=mean(Polio),
            Measles=mean(Measles)) %>% 
  ggplot()+
  geom_smooth(aes(Year, Polio), color='blue', se=FALSE)+
  geom_smooth(aes(Year, Measles), color='red', se=FALSE)
  #geom_smooth(aes(Year, HIV), color='yellow', se=FALSE)+
  #geom_smooth(aes(Year, Diphtheria), color='', se=FALSE)+

expect %>%
  group_by(Year) %>% 
  summarize(BMI=mean(BMI),
            Alcohol=mean(Alcohol)) %>% 
  ggplot()+
  geom_smooth(aes(Year, BMI), color='blue', se=FALSE)+
  geom_smooth(aes(Year, Alcohol), color='red', se=FALSE)

#distribution of life expectancy by year
expect %>%
  ggplot()+
  geom_violin(aes(x=Year, y=lifexp, group=Year, fill=Year))

ggplot(expect,aes(y=Year,x=infmort))+geom_boxplot(fill='yellow')

#Converting status into categorical and plotting boxplot
expect$factor = as.factor(expect$Status)
ggplot(expect,aes(y=lifexp,x=Status))+geom_boxplot()


x<-ggplot(data=expect, mapping=aes(x=lifexp,y=Alcohol))
x+geom_col(aes(color="red"))

expect$factor = as.factor(expect$Status)
ggplot(expect,aes(y=lifexp,x=GDP))+geom_boxplot()

library(ggplot2)
e1<-ggplot(expect,aes(x=lifexp,y=Alcohol))+geom_point(color='blue')
e2<-ggplot(expect,aes(x=lifexp,y=admort))+geom_point(color='blue')
e3<-ggplot(expect,aes(x=lifexp,y=infmort))+geom_point(color='blue')

e4<-ggplot(expect,aes(x=lifexp,y=Polio))+geom_point(color='red')
e5<-ggplot(expect,aes(x=lifexp,y=Measles))+geom_point(color='red')
e6<-ggplot(expect,aes(x=lifexp,y=BMI))+geom_point(color='red')
e7<-ggplot(expect,aes(x=lifexp,y=HIV))+geom_point(color='red')
e8<-ggplot(expect,aes(x=lifexp,y=hepb))+geom_point(color='red')

e9<-ggplot(expect,aes(x=lifexp,y=GDP))+geom_point(color='green')
e10<-ggplot(expect,aes(x=lifexp,y=percexp))+geom_point(color='green')
e11<-ggplot(expect,aes(x=lifexp,y=totexp))+geom_point(color='green')

grid.arrange(e1,e2,e3,ncol=3)

grid.arrange(e5,e6,e7,e8,ncol=2,nrow=2)

grid.arrange(e9,e10,e11,ncol=3)





