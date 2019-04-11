Analysis of Particulate Pollution in Lavington, BC
================

Summary
=======

In 2015, Pinnacle Renewable Energy set up a pellet facility in Lavington, BC. The local community raised concerns about the air quality following the opening of the plant due its proximity to a local elementary school. The Ministry of Environment (MOE) set up an air quality monitoring station (BAM) at Lavington Baptist Church downwind of the pellet plant, with a smaller monitor (EBAM) being installed upwind of the plant later on. The MOE issued a report stating that the new pellet factory had no significant impact on the air quality in Lavington. Our clients contest this claim, citing video footage which shows plumes of smoke drifting across Lavington from the nearby factory.

The MOE report used aggregated data, since hourly correlation between the two instruments was too low for any comparative studies. Following a literature review we came across studies that showed that the BAM and EBAM behave quite erratically when the humidity is high. Looking at the data we found that the correlation between the two monitors changes with humidity and averaging the data fails to take this relationship into account. Our client also suggested we look at the PM2.5 levels during ivnersion conditions, an atmospheric event that results in air being trapped in the valley, to determine if the PM2.5 levels are increasing over time. We found that there was no systematic increase in PM2.5 conditions over the years, but this may be due to several confounding factors such as the production level of the facotry and traffic pollution. Compounded with the lack of data prior to the facility’s opening, we cannot conclude that the emissions produced by the facility are negatively impacting air quality in Lavington. A more robust experiment design conducted over a longer time period may be required to come to a more decisive answer.

Introduction
============

The Ministry of Environment (MOE) has been monitoring the impact of a new wood pellet manufacturing plant, situated in Lavington BC since November 2015. The MOE is primarily looking at the impact of the plant on the levels of particulate matter of less than 2.5 microns in diameter (PM2.5), since high levels of PM2.5 have been linked to adverse health outcomes <sup>1</sup>. A beta-attenuation monitor (BAM) was set up in Lavington Baptist Church, downwind of the plant, to monitor PM2.5 concentrations, with an additional smaller monitor (EBAM) being temporarily installed upwind of the factory between 2017-2018. The MOE's analysis conclude that the pellet factory had no significant impact on the Lavington community. The goal of this project is to independently review the MOE report and conduct our own investigation.

The structure of our analysis essentially followed our clients direction. We first looked at the correlation between the two monitors, comparing the hourly and averaged daily readings. Following this we came across research that suggests that humidity greatly effects the monitoring systems and investigated the impact of this further.The analysis of inversion conditions looked at how the level of PM2.5 changed over time during these inversion days and if there has been a systematic increase in PM2.5 concentrations when these inversion events occured. We finally looked at the upwind and downwind effects of the plant.

[1]: Xing, Yu-Fei et al. “The impact of PM2.5 on the human respiratory system.” Journal of thoracic disease vol. 8,1 (2016): E69-74. <doi:10.3978/j.issn.2072-1439.2016.01.19> 

Data Description
================

Extracting, Transforming and Loading the data
---------------------------------------------

This initial data consisted of readings from both the BAM and EBAM. Before we could conduct any analysis we had to clean the data due formatting issues. The script that we ran renamed all the columns and changed the respective formats for easier manipulation in R. In addition to the raw data provided we also needed humidity, temperature and ventilation index data to conduct a more informed analysis. This meteorological data was unfortunately unavailable for Lavington, so it had to be acquired from a nearby monitoring station in Vernon, which is approximately 12km away. The hourly humidity data was merged with the provided data into a new file (merged.csv). The script (CleanUpData.R) that was used is included in the appendix and was omitted from this section for clarity.

| Date.Time           |  EBAM.PM25|  WIND.SPEED|  WIND.DIR|  BAM.PM25|  Rel.Humidity|
|:--------------------|----------:|-----------:|---------:|---------:|-------------:|
| 2015-11-09 14:00:00 |         NA|         2.0|     250.5|       0.1|            67|
| 2015-11-09 15:00:00 |         NA|         1.5|     212.7|       5.1|            70|
| 2015-11-09 16:00:00 |         NA|         1.3|     235.4|       6.1|            77|
| 2015-11-09 17:00:00 |         NA|         1.4|     207.5|       9.7|            82|
| 2015-11-09 18:00:00 |         NA|         0.8|     186.2|      17.2|            86|
| 2015-11-09 19:00:00 |         NA|         0.5|      29.0|      12.1|            86|

The columns of the above table are respectively:

-   Date.Time: The date (yyyy-mm-dd) and time (hh:mm:ss) of a given reading
-   EBAM.PM25: The PM25 reading at the EBAM
-   WIND.SPEED: The windspeed measured at the BAM (m/s)
-   WIND.DIR: The wind direction measured at the BAM (degrees)
-   BAM.PM25: The PM25 reading at the BAM
-   Rel.Humidity: The relative humidity of the air in Vernon (%)

After cleaning, reformatting and merging the data we found that there were large chunks of data still missing. This was either due to the BAM/EBAM having to be temporarily shut down for maintenance or the Vernon weather station having technical issues and failing to record any humidity measurements. To see if the missing data would be problematic a “missingness map” was created (Figure 1). From the visualization it is clear that the EBAM has quite a lot of missing data (as expected), since it was only functional for a short period of time. There are also large chunks of humidity data missing, but this should not be a problem, since we are only looking at days when the EBAM and BAM are both functional and the humidity data is present for those given days.

![Missingness map](/figure-markdown_github/mismap-1.png)

Visualising the data
--------------------

Having determined that our analysis was feasible we produced some plots to visualize the data. The first plots that we created were histograms to look at the distribution of each column, shown below (Figure 2). Data on inversion days was obtained separately and is also shown included.
<img src="/figure-markdown_github/distplot-1.png" alt="Distribution of data" height="50%" />
<p class="caption">
Distribution of data
</p>

The histograms of the EBAM and BAM both have a strong right skew (i.e. the readings are clustered near the left side and have a longer tail on the right). The distribution of the humidity data almost mirrors this, showing a strong left skew, with a large number of readings having quite high humidity. The plot of the wind direction is quite interesting, since it shows two clear modes, suggesting the majority of the wind comes from two distinct directions. The inversion data data also has a pronounced skew and may need to be transformed before any further analysis can be conducted.

After looking at the distribution of the columns, we created a scatter plot array (Figure 3), to look at any relationships between the variables.

<img src="/figure-markdown_github/scatterarray-1.png" alt="Scatter plot array" height="50%" />
<p class="caption">
Scatter plot array
</p>

There is a lot that can be inferred from the scatter plot array, which can then be rigorously tested later on. The scatter plots of WIND.SPEED vs EBAM.PM25 and BAM.PM25, suggests that as windspeed increases there are fewer particulates in the air. This is essentially a hypothesis that our clients suggested we should test. Namely, that on inversions days (i.e. days when there is no wind due to certain weather systems), the pollution levels in the valley should be higher due to stagnant air. There are several other pronounced patterns in the scatter plot array. A striking one is the difference between the BAM and EBAM as relative humidity increases. The PM2.5 levels measured by the EBAM appear to increase with humidity, however for the BAM the distribution remains fairly uniform. Finally, the WIND.SPEED vs WIND.DIR suggests that strong winds only come from two directions, as depicted by the wind-rose provided by the MOE.

A time series plot of the PM2.5 data was created for both the BAM and EBAM.The below figure shows that there is quite a lot of noise in the PM2.5 recordings from both the machines. The large spikes near the end of 2017 and also 2018 correspond to the forest fires that occurred in BC. Its also clear that the EBAM was only running for a short period of time. The majority of this project will focus on that time window when both machines were functional.

![Time Series plot for BAM and EBAM](/figure-markdown_github/tsPlot-1.png)

Methods and Results
===================

Effect of humidity of EBAM and BAM
----------------------------------

Following our meeting with the clients there was quite a lot of uncertainty regarding the statistical approach used by the MOE. A major concern was the fact that the MOE decided to average the hourly data and only look at this daily aggregated average in their analysis. The MOE stated that averaging the data created far higher correlation between the two instruments and removed any noise from the data. We first computed the correaltion between the BAM and EBAM to verify the MOE's findings.

``` r
#Find first and last EBAM readings
NAidx <- which(!is.na(df.merged$EBAM.PM25))
first <- min(NAidx)
last <- max(NAidx)

first_EBAM <- df.merged$Date.Time[first]
last_EBAM <- df.merged$Date.Time[last]

# Create a subset of the data
ss <- subset(df.merged,first_EBAM < Date.Time & Date.Time < last_EBAM)

#Create averaged data
ag1 <- aggregate(ss$EBAM.PM25, by=list(ss$Date.Time), mean,na.action = na.omit);
ag2 <- aggregate(ss$BAM.PM25, by=list(ss$Date.Time), mean,na.action = na.omit);

#Comptute raw and aggregate correlations
cor(ss$EBAM.PM25,ss$BAM.PM25,use = "complete.obs"), "\n")
```

    ## Correaltion of hourly data:  0.517835

``` r
cor(ag1$x,ag2$x,use="complete.obs"))
```

    ## Correaltion of daily data:  0.9210984

After creating a subset of the data that only looks at the window when both instruments were running we computed the raw correlation between the two sets. The correlation was approximately 0.513 compared the to averaged daily correlation, which was far higher at 0.921 confirming the MOE’s findings that the daily averages have far higher correaltion between the BAM and EBAM. A scatter plot (Figure 5), was created to visually display these findings.

![Scatter plots of BAM vs EBAM readings](/figure-markdown_github/scatterCor-1.png)

After some research we came across an article <sup>2</sup> that suggested the low correlation in the readings may be due to humidity, which which increases the noise in measuring the PM2.5 concentrations. To look at the impact of humidty on the correlation of the two instruments we only looked at days when the instruments were located next to each other or the 'colocation period'.(Figure 6).

![How humidty effects correlation](/figure-markdown_github/unnamed-chunk-3-1.png)

The figure produced showed an interesting pattern. The hourly correlation between the two instruments is fairly high when the humidity level is around 20%, after which it drops steadily, with the lowest correlation seen at around 40% humidity. Following this the hourly correlation increases with humidity, levelling out at around 0.6. The daily correlation at different levels of humidity also shows a similar pattern, however as the humidity increased so did the correlation without tapering off. These plots suggest that humidity has a clear impact on the instruments and although averaging the daily readings appears to somewhat mitigate this effect it may not necessarily be the appropriate way to analyze the data. Thus, the effect of humidity on the BAM and EBAM is a topic that should be explored further and a naive averaging of the data may not be the statistically appropriate way to conduct the analysis.

[2]: Schweizer, Don et al. "A comparative analysis of temporary and permanent beta attenuation monitors: The importance of understanding data and equipment limitations when creating PM2.5 air quality health advisories." Atmospheric Pollution Research vol 7.5 (2016): E865-875. <doi:10.1016/j.apr.2016.02.003>

Data Description
================

Inversion day analysis
----------------------

An inversion day is an atmospheric phenomena that predominantly occurs during winter when a cool air system descends into a valley, followed by a warmer air system resulting in stagnant wind conditions. Due to the wind being stagnant any pollution in the valley is trapped. The client proposed an analysis looking at inversion days over time to see if there is any significant increase over successive years in the PM2.5 levels.

The distribution of the inversion data is shown in the previous section (Figure 2). Due to the strong skew, the data was log-transformed before any further analysis was conducted. Following this box-plots were made to see if there is any visual difference in the inversion days for different years (Figure 7).

![Box-plot of inversion days](/figure-markdown_github/inversionBox-1.png)

The resulting box plots show that there may be a difference in the PM2.5 concentrations over time during inversion days. To test this we conducted an ANOVA and a post hoc Tukey HSD. The aim of this was to determine if the differences in PM2.5 that are seen in the box-plots are statistically significant or due to natural variation in the data.

``` r
anova.treatment <- aov(log(lowVIdays$BAM.PM25) ~ lowVIdays$Group, data=lowVIdays)
summary(anova.treatment)
```

    ##                  Df Sum Sq Mean Sq F value   Pr(>F)    
    ## lowVIdays$Group   3   7.58  2.5250   15.33 3.76e-09 ***
    ## Residuals       238  39.20  0.1647                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![QQ and residual plots from ANOVA](/figure-markdown_github/QQandResid-1.png)

The results of the ANOVA indicate that there is a significant difference in the PM2.5 levels during the different winter seasons. This is clear from the p-value for lowVIdays$Group, which is 3.76e-0.9, implying that at least one of the seasons is different from the others. This confirms what the box-plots show, where seasons B and D appear to be different from seasons A and C. The validity of the ANOVA was confirmed via residual and QQ plots (Figure 8). These show that the ANOVA is a fairly good fit to the data.

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = log(lowVIdays$BAM.PM25) ~ lowVIdays$Group, data = lowVIdays)
    ## 
    ## $`lowVIdays$Group`
    ##            diff         lwr        upr     p adj
    ## B-A  0.41262441  0.23641831  0.5888305 0.0000000
    ## C-A  0.10320908 -0.06143872  0.2678569 0.3682950
    ## D-A  0.42603597  0.15358285  0.6984891 0.0004081
    ## C-B -0.30941533 -0.48889451 -0.1299362 0.0000742
    ## D-B  0.01341156 -0.26825239  0.2950755 0.9993302
    ## D-C  0.32282689  0.04824560  0.5974082 0.0138358

In order to determine which seasons are different we conducted a post-hoc Tukey analysis. When comparing multiple groups it is incorrect to just compare one group to the next. Doing so increases the family wise error (FWE). Essentially, if you compare multiple groups from the same family the chance of one group being statistically different from the rest by chance increases with the number of groups being compared. To account for this we used Tukeys HSD, which accounts for the FWE. The results of this showed that the PM2.5 concentrations of groups A and C (2015 and 2017 respectively) were significantly different from groups B and D (2016 and 2018 respectively). This is clear from the p adj values, where any value below 0.05 indicates a statistically significant difference between the groups. This result doesn’t allow us to conclusively say that the PM2.5 concentration has steadily increased year by year.

Effect of Wind on readings
--------------------------

The scatter array clearly shows that majority of the strong winds are within 10-80 degrees (i.e. North-Easterly winds). Since the EBAM was located NE of the pellet factory, whereas the BAM was SE, we believed that the wind would drag pollutants from the factory to the BAM but not past the EBAM. Essentially, if the pellet factory produced a significant amount of pollution the strong NE wind would bring the pollutants to the BAM but not the EBAM, due to their relative locations. Therefore one would expect BAM readings to be higher than EBAM on days when there was a NE wind. To test this hypothesis we created a subset of the data, only focusing on days with a NE wind. We then conducted a simple t-test to compare the difference between BAM and EBAM (BAM - EBAM) on days when there is NE wind against days when there isn’t a NE wind. The results of this t-test are below.

``` r
dat.wind <- subset(df.merged, df.merged$WIND.DIR < 80 & df.merged$WIND.DIR > 10)
dat.wind.diff <- dat.wind$BAM.PM25 - dat.wind$EBAM.PM25

dat.nowind <- subset(df.merged,!(df.merged$WIND.DIR < 80 & df.merged$WIND.DIR > 10))
dat.nowind.diff <- dat.nowind$BAM.PM25 - dat.nowind$EBAM.PM25
tt1 <- t.test(dat.wind.diff, dat.nowind.diff, alternative = c("greater"))
tt1
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  dat.wind.diff and dat.nowind.diff
    ## t = 2.6744, df = 3461.8, p-value = 0.003761
    ## alternative hypothesis: true difference in means is greater than 0
    ## 95 percent confidence interval:
    ##  0.2653289       Inf
    ## sample estimates:
    ##  mean of x  mean of y 
    ##  0.4400840 -0.2494585

The null hypothesis of the t-test is that the (BAM - EBAM) readings are the same on days when there is NE wind compared with days when there is no NE wind. The t-test we conducted gave a significant p-value of 0.003761, thus we reject the null hypothesis in favour of the alternative. This suggests that on days when there is a NE wind the pollutants are being dragged from the factory into Lavington. It is interesting to note the difference in means. The mean of x (BAM - EBAM on days when there is a NE wind) is 0.44, the mean of y (BAM - EBAM on days when there isn’t a NE wind) is -0.25. Thus, on days when there is a NE wind the BAM readings are higher compared to the EBAM and on days when there is no NE the opposite is true. It should be noted that this does not directly imply that the pellet factory is causing the pollution. There may be several confounding variables that could be affecting the PM2.5 readings. An important one being humidity. It was noted in a previous section that humidity changes the correlation between the BAM and the EBAM and it could be the case that certain humidity conditions render comparisons between the two instruments meaningless.

Concluding discussion
=====================

The goal of this report was to assess the impact of the pellet factory on pollution in Lavington, BC. Due to the large scope of the project and multiple questions from the client we broke the problem down into three separate areas. The analysis of humidity showed that there is a clear trend in the correlation between the two instruments with differing humidity. Therefore, averaging the hourly data to get a better correlation may not necessarily be the statistically correct approach, as it fails to account for the underlying trend. It was suggested that differencing the BAM and EBAM may produce more stable readings. This is done below for both hourly and averaged daily readings.

``` r
diff.df <- ss$EBAM.PM25 - ss$BAM.PM25
fit.dif <- lmRob(diff.df ~ ss$Rel.Humidity)
plot(diff.df ~ ss$Rel.Humidity,ylab="(EBAM - BAM) Difference in PM25 concentration",
     xlab="Relative Humidty (%)",,ylim=c(-50,100))
abline(fit.dif$coefficients[1],fit.dif$coefficients[2],col="red")
```

![Difference plot Hourly](/figure-markdown_github/difFig-1.png)

The above plot shows the difference in readings between the BAM and EBAM and how this change with humidity. A robust linear regression was fit and the red line shows this fit. Clearly from the above plot, when the humidity is low the difference is fairly well centred around 0, however with increasing humidity there is greater variance in the difference. Nonetheless, the differenced data appears to be centred around zero. Therefore, we can naively assume that neither the BAM or EBAM are getting systematically higher readings as humidity changes.

``` r
aggregate.df <- aggregate(ss$EBAM.PM25-ss$BAM.PM25, by=list(ss$Date.Time),
                          mean,na.action=na.omit)
humidty.agg <- aggregate(ss$Rel.Humidity, by=list(ss$Date.Time), mean,na.action = na.omit)
fit.agg <- lmRob(aggregate.df$x ~ humidty.agg$x)
plot(aggregate.df$x ~ humidty.agg$x,ylab="(EBAM - BAM) Daily difference in
     PM25 concentration",xlab="Relative Humidty (%)",ylim=c(-50,100))
abline(fit.agg$coefficients[1],fit.agg$coefficients[2],col="red")
```

![Difference plot Daily](/figure-markdown_github/diffFigDaily-1.png)

The same was done for averaged daily data. This plot is not noticibly different from the hourly values.

The analysis of inversion conditions showed statistically significant differences between the years, however there was no real trend in PM2.5 values. The level of pollutant increased but then subsequently decreased. This means we cannot say that the pellet factory is increasing PM2.5 levels in Lavington. The PM2.5 levels in the area should continue to be monitored over time to see if the trend we found is cyclical or caused by pellet factory production. Another important confounding factor that we hope to look at is the amount of pellet factory production. It may have been the case the the PM2.5 levels dropped in certain years due to the factory being less active.

Finally, we looked at the maximum level of PM2.5 overtime and found that with the exception of two wildfire spikes, the concentration of PM2.5 otherwise remains well under 100 micrograms per cubic meter, up to which the air can still be considered healthy.

``` r
max.df <- df.merged %>% 
        group_by(Date = as.Date(df.merged$Date.Time)) %>% 
        summarise(y = max(BAM.PM25,na.rm=TRUE))
colnames(max.df)[2]<- "dailyMax"

maxplot <- ggplot(max.df,aes(x=Date, y=dailyMax))+ geom_line(color = "black", size = 0.5)+ 
            geom_hline(yintercept=55, linetype="dashed", color = "red")+
            ylab("PM2.5 Reading (BAM)") + xlab("Year")
print(maxplot)
```

![Time Series of Daily Maximum PM2.5 Concentration (BAM)](/figure-markdown_github/maxtime-1.png)

Appendix
========

CleanUpData.R

``` r
#Script to clean and merge data

#Get packages
library(data.table)
library(dplyr)

#Load raw data
data <- read.csv('Data/Lavington_2015-2018.csv', header = T, stringsAsFactors = FALSE)

#Change column type
data$BAM.PM25 <- as.numeric(data$PM25_BAM..ug.m3.)
data$WIND.DIR <- as.numeric(data$WDIR_VECT_BAM..deg.)
data$WIND.SPEED <- as.numeric(data$WSPD_SCLR_BAM..m.s.)
data$EBAM.PM25 <- as.numeric(data$PM25_EBAM..ug.m3.)
colnames(data)[1] <- 'Date.Time'

#create a new df
df <- data[,c('Date.Time','EBAM.PM25','WIND.SPEED','WIND.DIR','BAM.PM25')]
df <- df[1:26928,] #last few rows had summary info
df$Date.Time <- as.POSIXct(df$Date.Time, format= "%Y-%m-%d %H:%M") #make column datetime object


#### Cleaning Date.Time column ####
na.idx <- which(is.na(df$Date.Time)) #anything with NA had formatting issue

for( i in (1:length(na.idx))){
  idx <- na.idx[i]
  date.before <- df$Date.Time[idx-1]
  date.after <- df$Date.Time[idx+1]
  if(is.na(date.after) | is.na(date.before)){
    print("Error both rows before and after are NA")
    print(i)

  }
  else if(date.before + 2*60*60 != date.after ){
    print('Error, this rows time cannot be determined')
    print(i)
  }else{
    date.missing <- date.before+ 1*60*60
    df$Date.Time[idx] <- date.missing
  }
}


#get first and last dates of BAM recording
start.idx <- min(which(!is.na(df$BAM.PM25)))
start.idx

last.idx <- max(which(!is.na(df$BAM.PM25)))
last.idx

start.date <- df$Date.Time[start.idx]
end.date <- df$Date.Time[last.idx]


#### Getting humidity Data ####
weather.data.files <- list.files("Data/Vernon_Data/",pattern= '.csv', full.names = TRUE)
temp.files <- lapply(weather.data.files,fread,skip=14)
weather.data <- rbindlist(temp.files, fill = TRUE)
humdity.data <- weather.data[,c('Date/Time','Rel Hum (%)')]
#head(humdity.data)


#Add date/time column
names(humdity.data)[1] <- 'Date.Time'
humdity.data$Date.Time <- as.POSIXct(humdity.data$Date.Time, format= "%Y-%m-%d %H:%M")
#head(humdity.data)

#sort data
humdity.data <- humdity.data[order(Date.Time),]



#filter humidity data
humdity.data <- humdity.data %>%
  filter(Date.Time >= start.date ) %>% filter(Date.Time <= end.date)

df <- df %>%
  filter(Date.Time >= start.date) %>% filter(Date.Time <= end.date)

#head(df)
#head(humdity.data)

#tail(df)
#tail(humdity.data)


# Get the range of dates covered
# Clearly some dates are missing. Not an issue right now.
df$Date.Time[!df$Date.Time %in% humdity.data$`Date/Time`]

#Left-Join both dataframes
df.merged <- left_join(df, humdity.data, by=c("Date.Time"))
head(df.merged)

#write.csv(df.merged,'Data/mergedData.csv')
```

Function used to generate scatter plot of Correlation vs Humidty:

``` r
Extra time series plots for the subset of data
```

``` r
#Plotting time range where the data for EBAM is present
NAidx <- which(!is.na(df.merged$EBAM.PM25))
first <- min(NAidx)
last <- max(NAidx)

first_EBAM <- df.merged$Date.Time[first]
last_EBAM <- df.merged$Date.Time[last]


# Plot a subset of the data
ss <- subset(df.merged,first_EBAM < Date.Time & Date.Time < last_EBAM)


bamPlot <- ggplot(ss,aes(x=Date.Time, y=BAM.PM25))+ geom_line(color = "#FC4E07", size = 0.5)+ylim(c(0,100))
ebamPlot <- ggplot(ss,aes(x=Date.Time, y=EBAM.PM25))+ geom_line(color = "blue", size = 0.5)+ylim(c(0,100))
plot_grid(bamPlot, ebamPlot, labels = "AUTO")
```

![](/figure-markdown_github/unnamed-chunk-9-1.png)

Plotting averaged time series data from BAM and EBAM

``` r
ggplot()+
    geom_line(data=aggregateBAM,aes(x=Group.1, y=x,color= "BAM"))+
    geom_line(data=aggregateEBAM,aes(x=Group.1, y=x,color= "EBAM"))+
     xlab("Time") + ylab("PM25 concentration") + labs()
```

![](/figure-markdown_github/unnamed-chunk-10-1.png)

Even after averaging still quite erratic

Plot of differenced time series

``` r
ggplot()+
    geom_line(data=ss,aes(x=Date.Time, y=BAM.PM25 - EBAM.PM25,color= "BAM - EBAM"))+
     xlab("Time") + ylab("PM25 concentration") + ggtitle('Plot of differenced time series') +labs()
```

![](/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
dat <- df.merged
dat <- na.omit(dat)

row.names(dat) <- 1:nrow(dat)
start <- rownames(dat[dat$Date.Time == "2017/11/23 00:00",])
end <- rownames(dat[dat$Date.Time == "2018/4/9 23:00",])

dat.comp <- dat[c(start:end),]
```

    ## Warning in start:end: numerical expression has 24 elements: only the first
    ## used

    ## Warning in start:end: numerical expression has 23 elements: only the first
    ## used

``` r
dat.wind.ne <- subset(dat.comp, dat.comp$WIND.DIR < 80 & 
                        dat.comp$WIND.DIR > 10)
dat.wne.strong <- subset(dat.wind.ne, dat.wind.ne$WIND.SPEED >= 3)
dat.wne.weak <- subset(dat.wind.ne, dat.wind.ne$WIND.SPEED < 3)

tt1 <- t.test(dat.wind.ne$BAM.PM25, dat.wind.ne$EBAM.PM25, alternative = c("greater"))
tt2 <- t.test(dat.wne.strong$BAM.PM25, dat.wne.strong$EBAM.PM25, alternative = c("greater"))
tt3 <- t.test(dat.wne.weak$BAM.PM25, dat.wne.weak$EBAM.PM25, alternative = c("greater"))

dat.wind.sw <- subset(dat.comp, dat.comp$WIND.DIR < 260 & 
                        dat.comp$WIND.DIR > 190)
dat.wsw.strong <- subset(dat.wind.sw, dat.wind.sw$WIND.SPEED >= 3)
dat.wsw.weak <- subset(dat.wind.sw, dat.wind.sw$WIND.SPEED < 3)

tt4 <- t.test(dat.wind.sw$BAM.PM25, dat.wind.sw$EBAM.PM25, alternative = c("less"))
tt5 <- t.test(dat.wsw.strong$BAM.PM25, dat.wsw.strong$EBAM.PM25, alternative = c("less"))
tt6 <- t.test(dat.wsw.weak$BAM.PM25, dat.wsw.weak$EBAM.PM25, alternative = c("less"))

temptable <- matrix(, nrow = 6, ncol = 2)
ttestname <- c("All NE Winds", "Strong NE Winds", "Weak NE Winds", "All SW Winds", 
            "Strong SW Winds", "Weak SW Winds")
pvalue <- c(0.031, 0.290, 0.035, 0.002, 0.000, 0.020)
temptable[,1] <- ttestname
temptable[,2] <- pvalue

regtable <- matrix(, nrow = 4, ncol = 4)
cov <- c("Wind Direction (Binary)", "Wind Speed (Scalar)", "Relative Humidity (%)", "Wind Direction * Wind Speed")
est <- c(-1.641, -1.156, 0.034, 1.464)
pval <- c(0.022, 0.000, 0.002, 0.001)
sig <- c(0.05, 0.001, 0.01, 0.001)
regtable[,1] <- cov
regtable[,2] <- est
regtable[,3] <- pval
regtable[,4] <- sig
kable(regtable, digits = 3, align = "c", caption = "\\label{tab:regress}Multilinear Model Estimated Coefficients", col.names = c("Covariate", "Estimated Coefficient", "P-Value", "Significance Level"))
```

|           Covariate          | Estimated Coefficient | P-Value | Significance Level |
|:----------------------------:|:---------------------:|:-------:|:------------------:|
|    Wind Direction (Binary)   |         -1.641        |  0.022  |        0.05        |
|      Wind Speed (Scalar)     |         -1.156        |    0    |        0.001       |
|     Relative Humidity (%)    |         0.034         |  0.002  |        0.01        |
| Wind Direction \* Wind Speed |         1.464         |  0.001  |        0.001       |




