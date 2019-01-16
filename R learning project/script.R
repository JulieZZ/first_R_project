read.csv("data/gapminder_data.csv")
dat <- read.csv("data/gapminder_data.csv")

install.packages(c("dplyr","ggplot2","tidyr"))
install.packages("dplyr")

#view data
str(dat)
head(dat)
tail(dat)
dim(dat)
nrow(dat)
ncol(dat)
summary(dat)

# subsetting of data
dat
dat[1,]
dat[,2]
dat[1:4,]
dat[c(1,2,3,4),] 

#seq
seq(1,10)
seq(1,10, by=2)

dat[seq(1,1704,by=10),]
dat[seq(1,nrow(dat),by=10),]

#help
help(read.csv)
?read.csv

#data manipulation with dplyr
library(dplyr)
mean(dat$gdpPercap)
mean(dat[dat$continent=="Americas", "gdpPercap"]) #subsetting

filter(dat,continent=="Americas") #filter chose the rows
filter(dat,year>2000)
dat_2 <- filter(dat,continent=="Americas",year>2000)

select(dat,continent) #select the columns
select(dat_2,continent,year)

dat %>%  filter(continent=="Amrtivsnd", year>2000) %>% select(conuntry,year,gdpPercap)

sin(log(exp(5)))
5 %>% 
  exp() %>% 
  log() %>% 
  sin()

#group_by() and summarize()
#category variables
summary_1 <- dat %>% group_by(country) %>%
summarize(ave_life_exp=mean(lifeExp))

#compute the avearage gdppercap for each country
dat %>% group_by(country) %>%
  summarize(ave_gdp=mean(gdpPercap))
#compute the average gdpPerchao for each country in year 1957
dat %>% filter(year=="1957") %>% group_by(continent) %>% 
  summarize(ave_gdp=mean(gdpPercap))
dat %>%  group_by(continent) %>% filter(year=="1957") %>%
  summarize(ave_gdp=mean(gdpPercap))

# double equal == comparison/one equal sign =

# multiples summary outputs
dat %>% group_by(continent) %>%
  summarize(ave_gdp=mean(gdpPercap), min_gdp=min(gdpPercap),max_gdp=max(gdpPercap),num_values=n())

#making new column variables
dat %>% mutate(gdp_billion=gdpPercap*10^9)

#long/wide format
dat2 <- dat%>% select(country,year,gdpPercap)
library(tidyr)
dat2_wide <- dat2%>% spread(year, gdpPercap)


dat2_long <- dat2_wide %>% gather(year, gdp, "1952":"2007")


#ggplot, make a new plot
install.packages("ggplot2")
library(ggplot2)
ggplot()
ggplot(data=dat)
ggplot(data=dat,mapping=aes(x=gdpPercap,y=lifeExp))
ggplot(data=dat,mapping=aes(x=gdpPercap,y=lifeExp)) + geom_point()
#scatterplot and line plot
ggplot(data=dat,mapping=aes(x=gdpPercap,y=lifeExp)) + geom_point()+ geom_line()

#plot lifeExp on the y-axis, and year on the x-axis
ggplot(data=dat,mapping=aes(x=year,y=lifeExp)) + geom_point()
ggplot(data=dat,mapping=aes(x=year,y=lifeExp)) + geom_line()  #sort data
ggplot(data=dat,mapping=aes(x=year,y=lifeExp,by=country)) + geom_line()  #sort data
ggplot(data=dat,mapping=aes(x=year,y=lifeExp,by=country,color=continent)) + geom_line()  #sort data

#subplots by continent
ggplot(data=dat,mapping=aes(x=year,y=lifeExp,by=country)) + geom_line()+facet_wrap(~ continent)
ggplot(data=dat,mapping=aes(x=year,y=lifeExp,by=country)) + geom_line()+
  facet_wrap(~ continent,scales="free_x")
ggplot(data=dat,mapping=aes(x=year,y=lifeExp,by=country)) + geom_line()+
  facet_wrap(~ continent,scales="free_y")

# modify axis labels
ggplot(data=dat,mapping=aes(x=year,y=lifeExp,by=country)) + geom_line()+
  facet_wrap(~ continent)+
  labs(x="Year", y="life Expenctancy",title="Figure 1")

# change overall appearance using themes
ggplot(data=dat,mapping=aes(x=year,y=lifeExp,by=country)) + geom_line()+
  facet_wrap(~ continent,scales="free_y")+
  theme_bw()
ggplot(data=dat,mapping=aes(x=year,y=lifeExp,by=country)) + geom_line()+
  facet_wrap(~ continent)+
  theme_bw()+theme(panel.grid = element_line(color=NA))

ggplot(data=dat,mapping=aes(x=year,y=lifeExp,by=country)) + geom_line()+
  facet_wrap(~ continent)+
  labs(x="Year", y="life Expenctancy",title="Figure 1")+
  theme_bw()+theme(panel.grid = element_line(color=NA))

# save into a variable
figure_1 <- ggplot(data=dat,mapping=aes(x=year,y=lifeExp,by=country)) + geom_line()+
  facet_wrap(~ continent)+
  labs(x="Year", y="life Expenctancy",title="Figure 1")+
  theme_bw()+theme(panel.grid = element_line(color=NA))
figure_1
figure_1+theme_dark()

#save plot to file
ggsave(filename="figures/life_exp.png",plot=figure_1)
ggsave(filename="figures/life_exp.png",plot=figure_1,width=6,height=4,units="in")
?ggsave

#regression line
ggplot(data=dat,mapping=aes(x=gdpPercap,y=lifeExp))+
  geom_point()+scale_x_log10()
ggplot(data=dat,mapping=aes(x=gdpPercap,y=lifeExp))+
  geom_point()+scale_x_log10()+
  geom_smooth(method="lm")

#fitting a model
#
model_1 <- lm(lifeExp~log(gdpPercap),data=dat)

df <- data.frame (gdpPercap=range(dat$gdpPercap))
df$lifeExp <- predict(model_1,newdata=df)
ggplot(data=dat,mapping=aes(x=gdpPercap,y=lifeExp))+
  geom_point()+
  scale_x_log10()+
  geom_smooth(method="lm")+
  geom_line(data=df,mapping=aes(x=gdpPercap,y=lifeExp),color="red")

#from the instructor           
model_1 <- lm(lifeExp ~ log(gdpPercap), 
              data = dat)
regression_line <- data.frame(gdpPercap = range(dat$gdpPercap))
regression_line$lifeExp <- predict(model_1, 
                                   newdata = regression_line)
ggplot(data = dat, 
       mapping = aes(x = gdpPercap, 
                     y = lifeExp)) + 
  geom_point() + 
  scale_x_log10() + 
  geom_smooth(method = "lm") + 
  geom_line(data = regression_line, 
            mapping = aes(x = gdpPercap, 
                          y = lifeExp), 
            color = "red")+
  theme_custom

#model statitstics
summary(model_1)
model_1_summary <- summary(model_1)
attributes(summary(model_1))
attriabutes(model_1_summary)
r_sq <- summary(model_1)$r.squared
summary(model_1)$adj.r.squared
my_title <- paste("r^2=",round(r_sq,2))

#
theme_custom <- theme_bw() + 
  theme(panel.grid = element_line(color = NA))

ggplot(data = dat, 
       mapping = aes(x = gdpPercap, 
                     y = lifeExp)) + 
  geom_point() + 
  scale_x_log10() + 
  geom_line(data = regression_line, 
            mapping = aes(x = gdpPercap, 
                          y = lifeExp), 
            color = "red")+theme_custom+
  labs(title=my_title)+ geom_text(x=3,y=80,label=my_title)

#
install.packages("knitr")
install.packages("rmarkdown")

install.packages("rticles")

