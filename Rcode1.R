#Connect to Database
library(RODBC)
myconn<-odbcConnect("SQLTest",uid ="",pwd = "")
myconn

#Data from Database
exp9 <- sqlQuery(myconn,"SELECT t2.Course_Name, t2.Launch_Date, statstable.Participants FROM stats statstable INNER JOIN Course t2 ON statstable.Course_ID=t2.Course_ID WHERE t2.Course_Name='Science Technology Engineering and Mathematics'")

exp11 <- sqlQuery(myconn,"SELECT t2.Course_Name, t2.Launch_Date, statstable.Participants FROM stats statstable INNER JOIN Course t2 ON statstable.Course_ID=t2.Course_ID WHERE t2.Course_Name='Computer Science")

exp12 <- sqlQuery(myconn,"SELECT t2.Course_Name, t2.Launch_Date, statstable.Participants FROM stats statstable INNER JOIN Course t2 ON statstable.Course_ID=t2.Course_ID WHERE t2.Course_Name='Government Health & Social Science")

exp13 <- sqlQuery(myconn,"SELECT t2.Course_Name, t2.Launch_Date, statstable.Participants FROM stats statstable INNER JOIN Course t2 ON statstable.Course_ID=t2.Course_ID WHERE t2.Course_Name='Humanities History Design Religion & Education' ")

#plot participants in a single graph
>library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(quantmod)
library(forecast)

combined_close_tech <- data.frame(date = exp9$`Launch Date`, 
                                    +                                   exp9, 
                                    +                                   row.names = NULL) %>% 
  +     select(date, close = Participants) %>% 
  +     mutate(ticker = 'Science,Technology & Mathematics') %>%
  +     bind_rows(., 
                  +               data.frame(date = exp11$`Launch Date`, 
                                             +                          exp11, 
                                             +                          row.names = NULL) %>% 
                    +                   select(date, close = Participants) %>% 
                    +                   mutate(ticker = 'Computer Science')) %>% 
  +     bind_rows(., 
                  +               data.frame(date = exp12$`Launch Date`, 
                                             +                          exp12, 
                                             +                          row.names = NULL) %>% 
                    +                   select(date, close = Participants) %>% 
                    +                   mutate(ticker = 'Government,Health & Social Science'))%>%
  + bind_rows(., 
              +           data.frame(date = exp13$`Launch Date`, 
                                     +                      exp13, 
                                     +                      row.names = NULL) %>% 
                +               select(date, close = Participants) %>% 
                +               mutate(ticker = 'Humanities,History,Design,Religion and Education'))


g <- ggplot(data = combined_close_tech, 
              +             aes(x = date, y = close, color = ticker)) + 
  +     geom_line() + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 months"))
g

                      

#plot participants of STEM
exp14<-exp6$Participants
recife2=ts(exp14,start = 2013,frequency = 12)
plot(recife2,ylab='Participants',xlab='Year',main='Science,Technology and Mathematics Participants')




#for auto correlation function
acf(ts(recife2,freq=1),lag.max=48,main="Autocorrelation Function for Recife2 Data",ylim=c(-1,1))

#To select arima model automatically
fit<-auto.arima(recife2, seasonal=FALSE)
#residuals for ARIMA
tsdisplay(residuals(fit), lag.max=48, main='(0,1,1) Model Residuals')

#Change q value
fit_diff_br1<-arima(recife2,order = c(0,1,7))
summary(fit_diff_br1)
fit_diff_brf1<-forecast(fit_diff_br1,h=12)
plot(forecast(fit_diff_br1,h=12),include = 48)

#To know cofidence bounds
f<-forecast(fit_diff_brf1,12)
autoplot(f)


# Note the above commands were repeated for the forecasting on dataset 1
# But the ARIMA model was changed to (0,1,7)
