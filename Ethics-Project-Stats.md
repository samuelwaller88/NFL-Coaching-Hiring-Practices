Ethics Project
================
Sam Waller
3/2/2020

# Introduction

I studied whether or not the addition of the Rooney Rule has helped to
solve the lack of diversity among NFL Head Coaches. The Rooney Rule was
an intiative created in 2003 and named after long time Pittsburgh
Steelers executive Dan Rooney who was a chairman on the league’s
diversity committee. The rule requires that one minority candidate be
interview for an open head coaching position. Over the years many have
question the effectiveness of the rule, which led to a change in 2018
that required teams to interview a minority candidate that is not
employed within their organization. In 2019, TIDES, The Institute for
Diverisity and Ethics in Sports, published a report and gave the NFL a
D+ for diversity among head coaches due to a decrease in minority head
coaches to its lowest figure (4 coaches) since 2003 when the rule was
first introduced. In this study, I will identify one of the biggest
factor that causes the lack of minority coaches, and I will give my
opinion on how the NFL may fix their problem. In order to look at the
data and create visializations, I downloaded the packages tidyverse and
readr.

## Load Tidyverse and Readr

``` r
library(tidyverse)
library(readr)
```

## Load Data

``` r
Ethics_Project_Stat_Sheet_Programming_Coaches_Data<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Programming%20Coaches%20Data.csv"))


Ethics_Project_Stat_Sheet_Programming_Coaches_Data_1<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Programming%20Coaches%20Data%20(1).csv"))


Ethics_Project_Stat_Sheet_Programming_Coaches_Data_2<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Programming%20Coaches%20Data%202.csv"))


Ethics_Project_Stat_Sheet_Programming_Coaches_Data_3<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Programming%20Coaches%20Data%203.csv"))


Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Programming%20Overall%20Player%20Data.csv"))


Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_2<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Programming%20Overall%20Player%20Data%202.csv"))


Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_3<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Programming%20Overall%20Player%20Data%203.csv"))


Ethics_Project_Stat_Sheet_Programming_Player_Data_by_Position<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Programming%20Player%20Data%20by%20Position.csv"))


Ethics_Project_Stat_Sheet_Programming_Captian_Data<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Programming%20Captian%20Data.csv"))


Ethics_Project_Stat_Sheet_Coaching_Hiring_Practices<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Coaching%20Hiring%20Practices.csv"))


Ethics_Project_Stat_Sheet_Offense_Defense_Coordinator_Numbers<-read.csv(url("https://raw.githubusercontent.com/samuelwaller88/NFL-Coaching-Hiring-Practices/master/Ethics%20Project%20Stat%20Sheet%20-%20Offense_Defense%20Coordinator%20Numbers.csv"))
```

## Coaching Hiring Data

``` r
minority_white_color <- c("Minority Coaches" = "red", "White Coaches" = "blue","Rooney Rule Instituted in 2003"="gray4")

Number_Of_Coaches_By_Year<- Ethics_Project_Stat_Sheet_Programming_Coaches_Data_3 %>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y=Total_Minority_Coaches, color="Minority Coaches"),linetype = "solid")+
  geom_line(aes(y=Total_White_Coaches, color="White Coaches"), linetype = "solid")+
  geom_vline(aes(xintercept =2003, color="Rooney Rule Instituted in 2003"))+
     scale_x_continuous(limits = c(1990,2020),
        breaks = seq(1990, 2020, 3)) +
     scale_y_continuous(limits = c(0,30),
        breaks = seq(0, 30, 3)) +
  scale_color_manual(values=minority_white_color)+
    labs( x="Year", y="Number of Minority Coaches", title="Number of Minority Coaches by Year", subtitle="The Number of Minority Coaches Has Increased Since 1990", color="Legend" )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
Number_Of_Coaches_By_Year
```

![](Ethics-Project-Stats_files/figure-gfm/Coaching%20Hiring%20Data-1.png)<!-- -->

``` r
Number_Of_Minority_Coaches_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Coaches_Data%>%
   summarize(avg = mean(Total_Minority_Coaches), med = median(Total_Minority_Coaches), 
                    standard_dev = sd(Total_Minority_Coaches), 
                    iqr = IQR(Total_Minority_Coaches))
Number_Of_Minority_Coaches_By_Year_ss
```

    ##   avg med standard_dev iqr
    ## 1 4.4   4     2.110728   3

``` r
Number_Of_White_Coaches_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Coaches_Data_3%>%
   summarize(avg = mean(Total_White_Coaches), med = median(Total_White_Coaches), 
                    standard_dev = sd(Total_White_Coaches), 
                    iqr = IQR(Total_White_Coaches))
Number_Of_White_Coaches_By_Year_ss
```

    ##        avg  med standard_dev  iqr
    ## 1 26.56667 26.5     1.568732 1.75

``` r
Turnover_Rate_hist<-Ethics_Project_Stat_Sheet_Programming_Coaches_Data_2 %>%
            ggplot(aes(x = Turnover_Rate)) +
            geom_density(fill="navyblue", color="red") +
            labs(x = "NFL Turnover Rate", y = "", 
                 title = "Distribution of the NFL Turnover Rate since 1990", subtitle = "On Average 22% of NFL Teams Are Looking for a New Head Coach Every Year")  +
        scale_x_continuous(limits = c(0,0.5),
          breaks = seq(0, 0.5, .1)) +
        geom_vline(aes(xintercept = median(Turnover_Rate)), color = "black")  +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
Turnover_Rate_hist
```

![](Ethics-Project-Stats_files/figure-gfm/Coaching%20Hiring%20Data-2.png)<!-- -->

``` r
Turnover_Rate_by_Year<-Ethics_Project_Stat_Sheet_Programming_Coaches_Data_3%>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y = Turnover_Rate), linetype="solid")+
  scale_x_continuous(limits = c(1990,2020),
        breaks = seq(1990, 2020, 3)) +
     scale_y_continuous(limits = c(0,1),
        breaks = seq(0, 1, .1)) +
    labs( x="Year", y="Turnover Rate", title="Turnover Rate by Year", subtitle=" 1999-2019")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
Turnover_Rate_by_Year
```

![](Ethics-Project-Stats_files/figure-gfm/Coaching%20Hiring%20Data-3.png)<!-- -->

``` r
Turnover_Rate_ss<-Ethics_Project_Stat_Sheet_Programming_Coaches_Data_2%>%
   summarize(avg = mean(Turnover_Rate), med = median(Turnover_Rate), 
                    standard_dev = sd(Turnover_Rate), 
                    iqr = IQR(Turnover_Rate))
Turnover_Rate_ss
```

    ##     avg  med standard_dev   iqr
    ## 1 0.221 0.22   0.05622308 0.055

``` r
Minority_Hiring_Rate_hist<-Ethics_Project_Stat_Sheet_Programming_Coaches_Data_2 %>%
            ggplot(aes(x = Percent_of_Total_Hiring)) +
      geom_density(fill="navyblue", color="red") +
            labs(x = "NFL Hiring Rate of Minoritiy Head Coaches", y = "", 
                 title = "Distribution of the NFL Hiring of Minority Head Coaches", subtitle = "On Average Minority Head Coaches Make Up 14% of the Head Coaching Hirings")  +
        scale_x_continuous(limits = c(0,0.5),
          breaks = seq(0, 0.5, .1)) +
        geom_vline(aes(xintercept = median(Percent_of_Total_Hiring)), color = "black")  +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
Minority_Hiring_Rate_hist
```

![](Ethics-Project-Stats_files/figure-gfm/Coaching%20Hiring%20Data-4.png)<!-- -->

``` r
Minority_Percent_of_Total_Hiring_by_Year<-Ethics_Project_Stat_Sheet_Programming_Coaches_Data_3%>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y = Percent_of_Total_Hiring), linetype="solid")+
  scale_x_continuous(limits = c(1990,2020),
        breaks = seq(1990, 2020, 3)) +
     scale_y_continuous(limits = c(0,1),
        breaks = seq(0, 1, .1)) +
    labs( x="Year", y="Percent of Hired Coaches That Were a Minority", title="Percent of Hired Coaches That Were a Minority by Year", subtitle=" 1999-2019")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
Minority_Percent_of_Total_Hiring_by_Year
```

![](Ethics-Project-Stats_files/figure-gfm/Coaching%20Hiring%20Data-5.png)<!-- -->

``` r
Minority_Hiring_Rate_ss<-Ethics_Project_Stat_Sheet_Programming_Coaches_Data_2%>%
   summarize(avg = mean(Percent_of_Total_Hiring), med = median(Percent_of_Total_Hiring), 
                    standard_dev = sd(Percent_of_Total_Hiring), 
                    iqr = IQR(Percent_of_Total_Hiring))
Minority_Hiring_Rate_ss
```

    ##         avg  med standard_dev    iqr
    ## 1 0.1463333 0.14    0.1169581 0.1725

I first looked at the amount of minority coaches before and after the
Rooney Rule was implemented. On average from 1990 to 2019, there were 4
minority coaches and 27 white coaches. The distribution of minority head
coaches was skewed right by the all time high of 9 minority coaches in
2011, while the distribution of white coaches was nearly evenly
distributed. After 2003 the amount of minority coaches increased, and
the amount hasn’t been below 3 minority head coaches since the rule was
first implemented. However, the high of 9 coaches in 2019 has not been
sustainable. On average, the turnover rate of NFL head coaches was 0.22
(22%) and the distribution was normally distributed and somewhat spread
out with a standard deviation of 0.0562. This means that in any given
year, one could expect that roughly seven NFL teams are looking for a
new head coach. The minimum number of head coaching vacancies was 3 in
2005 followed by a maximum of 10 head coaching vacancies in 2006. Other
than those two years, most years the amount of head coaching vacancies
hovers close to the median because of the normal distribution. In order
to determine why the amount of minority heads coaches have dropped, I
had to find the rate minority head coaches were being hired by NFL
teams. Since 1990, about 14% of the NFL head coaching hirings were
minority candidates. This distribution was slightly skewed right but
there was a high standard deviation of 0.117. This suggests that while
only one minority candidate is hired every year given the average of 7
head coaching vacancies, there are instances where zero minority head
coaches are hired (1991, 1994, 1995, 1997, 1998, 2000, 2010, 2013) and
instances where two minorities were hired (1992, 2004, 2006, 2009, 2013,
2017) or even three minority heads coaches are hired (2011). This proves
a problem when multiple minority head coaches are fired. If half the
time teams are hiring one minority head coaches per year then if several
are fired, then there isn’t a big enough applicant pool to replace them.
For example, the dip in minority head coaches in 2019 was caused by five
minority head coaches (Steve Wilks, Marvin Lewis, Hue Jackson, Vance
Joseph, and Todd Bowles) being fired during and after the 2018 season
and threre was one minority candidate (Brian Flores) hired.

## Overall Player Data

``` r
colors <- c("Minority Players" = "red", "White Players" = "blue")

ggplot(Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_3, aes(x=Year)) +
   scale_color_manual(values = colors)+
  geom_line(aes(y = Number_of_White_Players, color = "White Players"), linetype="solid") + 
  geom_line(aes(y = Number_of_Minority_Players,color="Minority Players"), linetype="solid")+
  scale_color_manual(values=colors)+
  scale_x_continuous(limits = c(1999,2017),
        breaks = seq(1999, 2017, 3)) +
     scale_y_continuous(limits = c(0,1800),
        breaks = seq(0, 1800, 300)) +
    labs( x="Year", y="Number of NFL Players", title="Number of NFL Players by Year", subtitle=" 1999-2016", color = "Legend")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
```

    ## Scale for 'colour' is already present. Adding another scale for 'colour',
    ## which will replace the existing scale.

![](Ethics-Project-Stats_files/figure-gfm/Overall%20Player%20Data-1.png)<!-- -->

``` r
Minority_Players_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_3%>%
   summarize(avg = mean(Number_of_Minority_Players), med = median(Number_of_Minority_Players), 
                    standard_dev = sd(Number_of_Minority_Players), 
                    iqr = IQR(Number_of_Minority_Players))
Minority_Players_By_Year_ss
```

    ##        avg  med standard_dev iqr
    ## 1 1530.533 1524     26.65619  15

``` r
White_Players_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_3%>%
   summarize(avg = mean(Number_of_White_Players), med = median(Number_of_White_Players),
                    standard_dev = sd(Number_of_White_Players), 
                    iqr = IQR(Number_of_White_Players))
White_Players_By_Year_ss
```

    ##        avg med standard_dev iqr
    ## 1 560.4667 567     26.65619  15

``` r
ggplot(Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_3, aes(x=Year)) +
   scale_color_manual(values = colors)+
  geom_line(aes(y = White_Players_on_Offense, color = "White Players"), linetype="solid") +
  geom_line(aes(y = Minortity_Players_on_Offense,color="Minority Players"), linetype="solid")+
  scale_x_continuous(limits = c(1999,2017),
        breaks = seq(1999, 2017, 3)) +
     scale_y_continuous(limits = c(0,1000),
        breaks = seq(0, 1000, 100)) +
    labs( x="Year", y="Number of Offensive NFL Players", title="Number of Offensive NFL Players by Year", subtitle=" 1999-2016", color = "Legend")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
```

![](Ethics-Project-Stats_files/figure-gfm/Overall%20Player%20Data-2.png)<!-- -->

``` r
Minority_Offensive_Players_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_3%>%
   summarize(avg = mean(Minortity_Players_on_Offense), med = median(Minortity_Players_on_Offense), 
                    standard_dev = sd(Minortity_Players_on_Offense), 
                    iqr = IQR(Minortity_Players_on_Offense))
Minority_Offensive_Players_By_Year_ss
```

    ##        avg med standard_dev  iqr
    ## 1 633.7333 635     16.34654 21.5

``` r
White_Offensive_Players_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_3%>%
   summarize(avg = mean(White_Players_on_Offense), med = median(White_Players_on_Offense),
                    standard_dev = sd(White_Players_on_Offense), 
                    iqr = IQR(White_Players_on_Offense))
White_Offensive_Players_By_Year_ss
```

    ##        avg med standard_dev  iqr
    ## 1 398.2667 397     16.34654 21.5

``` r
ggplot(Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_3, aes(x=Year)) +
   scale_color_manual(values = colors)+
  geom_line(aes(y = White_Players_on_Defense, color = "White Players"), linetype="solid") +
  geom_line(aes(y = Minority_Players_on_Defense,color="Minority Players"), linetype="solid")+
  scale_x_continuous(limits = c(1999,2017),
        breaks = seq(1999, 2017, 3)) +
     scale_y_continuous(limits = c(0,1000),
        breaks = seq(0, 1000, 100)) +
    labs( x="Year", y="Number of Defensive NFL Players", title="Number of Defensive NFL Players by Year", subtitle=" 1999-2016", color = "Legend")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
```

![](Ethics-Project-Stats_files/figure-gfm/Overall%20Player%20Data-3.png)<!-- -->

``` r
Minority_Defensive_Players_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_3%>%
   summarize(avg = mean(Minority_Players_on_Defense), med = median(Minority_Players_on_Defense), 
                    standard_dev = sd(Minority_Players_on_Defense), 
                    iqr = IQR(Minority_Players_on_Defense))
Minority_Defensive_Players_By_Year_ss
```

    ##        avg med standard_dev iqr
    ## 1 896.7333 893     20.13336  13

``` r
White_Defensive_Players_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Overall_Player_Data_3%>%
   summarize(avg = mean(White_Players_on_Defense), med = median(White_Players_on_Defense),
                    standard_dev = sd(White_Players_on_Defense), 
                    iqr = IQR(White_Players_on_Defense))
White_Defensive_Players_By_Year_ss
```

    ##        avg med standard_dev iqr
    ## 1 162.2667 166     20.13336  13

To get a bigger picture of overall racial trends in the NFL, I looked at
the NFL player data from 1999-2016 collect over varius censuses. The
data ends in 2016 because the NFL has not publicaly published another
player census since 2016. I also assumed that there were a constant
amount of players in the league each year. Between 1999 and 2016, there
were an average of 1524 minority players(72.9%) and 567 white players
(27.1%). The standard deviation were equal but that was probably due the
constant amount of players and the relatively flat lines. Starting in
2013, the amount of minority players has increased which causes the
distribution of minority players to skew right. To look at this trend
further, I broke the players up between offense and defense. On offense,
there was an average of 635 minority players and 397 whites while on
defense there was average of 893 minority players and 166 white players.
The difference in the numbers comes from the overall player data being
more skewed than the individual offensive and defensive player data.
While both the amount offensive and defensive minority players has
increased since 2013, it is hard to tell which has seen a greater
increase without looking a the data of individual
positions.

## Individual Player Data

``` r
colors1 <- c("Quaterback" = "royalblue", "Center" = "red3", "Tight End"="snow4","Cornerback"="black", "Linebacker"="khaki")
ggplot(Ethics_Project_Stat_Sheet_Programming_Player_Data_by_Position, aes(x=Year)) +
   scale_color_manual(values = colors1)+
  geom_line(aes(y = QB, color = "Quaterback"), linetype="solid") + 
  geom_line(aes(y = C,color="Center"), linetype="solid")+
  geom_line(aes(y = TE, color = "Tight End"), linetype="solid") + 
  geom_line(aes(y = CB,color="Cornerback"), linetype="solid")+
  geom_line(aes(y = LB, color = "Linebacker"), linetype="solid") + 
  scale_x_continuous(limits = c(1999,2017),
        breaks = seq(1999, 2017, 3)) +
     scale_y_continuous(limits = c(0,1),
        breaks = seq(0, 1, 0.1)) +
    labs( x="Year", y="Players by Positions that are Minority (%)", title="Number of Minority NFL Players by Position by Year", subtitle=" 1999-2016", color = "Legend")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
```

![](Ethics-Project-Stats_files/figure-gfm/Individual%20Player%20Data-1.png)<!-- -->

``` r
Number_Of_Minority_QB_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Player_Data_by_Position%>%
   summarize(avg = mean(QB), med = median(QB), 
                    standard_dev = sd(QB), 
                    iqr = IQR(QB))
Number_Of_Minority_QB_By_Year_ss
```

    ##         avg med standard_dev  iqr
    ## 1 0.2026667 0.2   0.02344192 0.04

``` r
Number_Of_Minority_C_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Player_Data_by_Position%>%
   summarize(avg = mean(C), med = median(C), 
                    standard_dev = sd(C), 
                    iqr = IQR(C))
Number_Of_Minority_C_By_Year_ss
```

    ##         avg  med standard_dev  iqr
    ## 1 0.2213333 0.22   0.05730453 0.08

``` r
Number_Of_Minority_TE_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Player_Data_by_Position%>%
   summarize(avg = mean(TE), med = median(TE), 
                    standard_dev = sd(TE), 
                    iqr = IQR(TE))
Number_Of_Minority_TE_By_Year_ss
```

    ##     avg  med standard_dev  iqr
    ## 1 0.442 0.44   0.02305273 0.02

``` r
Number_Of_Minority_CB_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Player_Data_by_Position%>%
   summarize(avg = mean(CB), med = median(CB), 
                    standard_dev = sd(CB), 
                    iqr = IQR(CB))
Number_Of_Minority_CB_By_Year_ss
```

    ##         avg  med standard_dev  iqr
    ## 1 0.9746667 0.98    0.0176743 0.01

``` r
Number_Of_Minority_LB_By_Year_ss<-Ethics_Project_Stat_Sheet_Programming_Player_Data_by_Position%>%
   summarize(avg = mean(LB), med = median(LB), 
                    standard_dev = sd(LB), 
                    iqr = IQR(LB))
Number_Of_Minority_LB_By_Year_ss
```

    ##         avg  med standard_dev  iqr
    ## 1 0.7686667 0.76   0.03020564 0.03

I looked at the proportion of minority player over 5 positions between
1999 and 2016. All of the distributions were roughly evenly
distributated and with minimal standard deviation. On offense, the
quarterback remained near constant around the average of 0.2 players
were a minority. Likewise, center remained around the median of 0.22.
The first position of were there was a change in the proportion of
minorities was the tight end position. In 1999, 45% of tights end were
minority. However in 2016, that percentage increased to 51%.
Additionally, this increase began in 2013, so this one position that
helped to increase the amount of minority players between 2013 and 2016.
On defense, the position that most helped the trend of increasing
minority players in the NFL was linebacker position. From 2012 to 2016,
the number of minority players increased from 73% of players to 82% of
the players. Lastly, I looked at minority cornerbacks who by 2016 made
up 100% of the cornerback group. With this data, we can see that there
are position where minority are well represented (Cornerback,
Linebacker, Tight End) while there other position of offense where
minorities are poorly represented (Quarterback and
Center).

## 2019 NFL Captain Data

``` r
Captain_Prop_Table_Setup<- Ethics_Project_Stat_Sheet_Programming_Captian_Data %>%
            group_by(Offense_Defense, Minority) %>%
            summarize(number = n()) %>%
            spread(Offense_Defense, number)
Captain_Prop_Table_Setup

prop.table(table(Ethics_Project_Stat_Sheet_Programming_Captian_Data$Offense_Defense, Ethics_Project_Stat_Sheet_Programming_Captian_Data$Minority)) %>% 
  round(4)
```

``` r
Captain_100_Percent_Stacked <- Ethics_Project_Stat_Sheet_Programming_Captian_Data %>%
            ggplot(aes(Offense_Defense, fill = Minority)) +
            geom_bar(position = "fill") +
            labs(x = "Offense or Defense", y = "Proportion", fill = "Minority", title = "Distribution of 2019 NFL Captains")+
          scale_fill_manual(values=c("Blue", "Red"))+
          scale_y_continuous(limits = c(0,1),
                    breaks = seq(0, 1, 0.1)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
Captain_100_Percent_Stacked
```

![](Ethics-Project-Stats_files/figure-gfm/NFL%202019%20Captain%20Data-1.png)<!-- -->

The last piece of player data I analized the was proportion of
minorities players that were voted team captain. In 2019 there were 128
offensive and defensive captains over 29 teams as the Bears, Broncos,
and Ravens were the only teams that didn’t have official captains during
the 2019 season. There were roughly the same amount of offensive and
defensive captains with offense having two more captains. Of the 63
defensive captains, 59 of the captains were minorities while only four
were white players. This reflects the player data as there are far more
minority defensive players than white defensive players especially at
the cornerback and linebacker. On offense it more even with 26 of the 65
captains being minority. This also reflect the individual position data
as quarterbacks, a predominantly white position, are usually captains.
However, the 40/60 split is still good as shows that more minority
players are playing on offense and are gaining leadership positions
within the team.

NFL Coaching Hiring
Practices

``` r
Coaching_Prop_Table_Setup<- Ethics_Project_Stat_Sheet_Coaching_Hiring_Practices %>%
            group_by(Year, Position_Before_Hiring) %>%
            summarize(number = n()) %>%
            spread(Year, number)
Coaching_Prop_Table_Setup

prop.table(table(Ethics_Project_Stat_Sheet_Coaching_Hiring_Practices$Year, Ethics_Project_Stat_Sheet_Coaching_Hiring_Practices$Position_Before_Hiring)) %>% 
  round(4)
```

``` r
Position_Ordered<-c("CFL Head Coach", "College Offensive Coordinator", "NFL Defensive Position Coach", "NFL Offensive Position Coach", "College Head Coach", "NFL Defensive Coordinator","NFL Offensive Coordinator", "Former NFL Head Coach")


Coach_Hiring_100_Percent_Stacked <- Ethics_Project_Stat_Sheet_Coaching_Hiring_Practices %>%
  mutate(Position_Before_Hiring=ordered(Position_Before_Hiring, levels=Position_Ordered))%>%    arrange(Position_Before_Hiring)%>%      
  ggplot(aes(Year, fill = Position_Before_Hiring)) +
            geom_bar(position = "fill",width = .5, color = "darkgray") +
            labs(x = "Year", y = "Proportion", fill = "Position", title = "Distribution of Hired Head Coaching Candidates By Year", subtitle = "2010-2019")+
          scale_x_continuous(limits = c(2009,2020),
                    breaks = seq(2010, 2019, 1)) +
          scale_y_continuous(limits = c(0,1),
                    breaks = seq(0, 1, 0.1)) +
        scale_fill_manual(values=c("gray87","gray87","gray87","gray87","gray87","blue","blue3","blue4"))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
  theme(axis.text.x = element_text(face = "bold", 
                           size = 10, angle = 0)) 
Coach_Hiring_100_Percent_Stacked
```

![](Ethics-Project-Stats_files/figure-gfm/Coaching%20Hiring%20Practices-1.png)<!-- -->

In the past 10 years, the NFL has hired 67 new head coaches. 83.6% of
the coaching hires where either Coordinators or former head coaches at
the NFL Level. Teams hired 21 NFL offensive coordinators 20 former head
coaches, and 15 NFL defensive coordinators over that span. The other 11
coaches were a mixture of experience levels ranging from a former CFL
head coach to College head coaches to NFL position coaches. Lastly, from
2010-2019, 41 of the coaches had a offensive background while 26 of the
coaches were of a defensive background. Because the league is heading
toward offensive coaches, I examined the data of NFL coordinators from
the last 10 years to see if minorities are represented in those coaching
positions that they are more likely to be hired for NFL Head Coaching
Jobs.

## NFL Offense and Defense Coordinators Data

``` r
Defense_Coordinator<- Ethics_Project_Stat_Sheet_Offense_Defense_Coordinator_Numbers %>%
         filter(Offense_Coordinator_Defense_Coordinator == "Defense")
Defense_Coordinator

Offense_Coordinator<- Ethics_Project_Stat_Sheet_Offense_Defense_Coordinator_Numbers %>%
         filter(Offense_Coordinator_Defense_Coordinator == "Offense")
Offense_Coordinator
```

``` r
Total_Coordinator_Prop_Table_Setup<-Ethics_Project_Stat_Sheet_Offense_Defense_Coordinator_Numbers %>%
            group_by(Year, Minority) %>%
            summarize(number = n()) %>%
            spread(Year, number)
Total_Coordinator_Prop_Table_Setup

prop.table(table(Ethics_Project_Stat_Sheet_Offense_Defense_Coordinator_Numbers$Year, Ethics_Project_Stat_Sheet_Offense_Defense_Coordinator_Numbers$Minority)) %>% 
  round(4)

Defense_Coordinator_Prop_Table_Setup<- Defense_Coordinator %>%
            group_by(Year, Minority) %>%
            summarize(number = n()) %>%
            spread(Year, number)
Defense_Coordinator_Prop_Table_Setup

prop.table(table(Defense_Coordinator$Year, Defense_Coordinator$Minority)) %>% 
  round(4)

Offense_Coordinator_Prop_Table_Setup<- Offense_Coordinator %>%
            group_by(Year, Minority) %>%
            summarize(number = n()) %>%
            spread(Year, number)
Offense_Coordinator_Prop_Table_Setup

prop.table(table(Offense_Coordinator$Year, Offense_Coordinator$Minority)) %>% 
  round(4)
```

``` r
Total_Bar<-Ethics_Project_Stat_Sheet_Offense_Defense_Coordinator_Numbers%>%
ggplot(aes(x=Year, fill = Minority)) +
            geom_bar(position="fill", width = .5, color = "darkgray") +
            labs(x = "Year", y = "Offensive and Defensive Coordinators", fill = "Minority", title = "White vs. Minority Coordinators by Year", subtitle = "2010-2019")+
  scale_fill_manual(values=c("blue","red"))+
        scale_x_continuous(limits = c(2009,2020),
                    breaks = seq(2010, 2019, 1)) +
         scale_y_continuous(limits = c(0,1),
                    breaks = seq(0, 1, .2)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
  theme(axis.text.x = element_text(face = "bold", 
                           size = 10, angle = 0))
Total_Bar
```

![](Ethics-Project-Stats_files/figure-gfm/NFL%20Offense%20and%20Defense%20Coordinator-1.png)<!-- -->

``` r
Defense_Coordinator_Bar <- Defense_Coordinator%>%
  ggplot(aes(x=Year, fill = Minority)) +
            geom_bar(position="fill",width = .5, color = "darkgray") +
            labs(x = "Year", y = "Defensive Coordinators", fill = "Minority", title = "White vs. Minority Defensive Coordinators by Year", subtitle = "2010-2019")+
  scale_fill_manual(values=c("blue","red"))+
        scale_x_continuous(limits = c(2009,2020),
                    breaks = seq(2010, 2019, 1)) +
         scale_y_continuous(limits = c(0,1),
                    breaks = seq(0, 1, .2)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
  theme(axis.text.x = element_text(face = "bold", 
                           size = 10, angle = 0)) 
Defense_Coordinator_Bar 
```

![](Ethics-Project-Stats_files/figure-gfm/NFL%20Offense%20and%20Defense%20Coordinator-2.png)<!-- -->

``` r
Offense_Coordinator_Bar <- Offense_Coordinator%>%
  ggplot(aes(x=Year, fill = Minority)) +
            geom_bar(position="fill", width = .5, color = "darkgray") +
            labs(x = "Year", y = "Proportion of Offensive Coordinators", fill = "Minority", title = "White vs. Minority Offensive Coordinators by Year", subtitle = "2010-2019")+
  scale_fill_manual(values=c("blue","red"))+
        scale_x_continuous(limits = c(2009,2020),
                    breaks = seq(2010, 2019, 1)) +
        scale_y_continuous(limits = c(0,1),
                    breaks = seq(0, 1, .2)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
  theme(axis.text.x = element_text(face = "bold", 
                           size = 10, angle = 0)) 
Offense_Coordinator_Bar 
```

![](Ethics-Project-Stats_files/figure-gfm/NFL%20Offense%20and%20Defense%20Coordinator-3.png)<!-- -->

In the last ten years, the diversity among NFL coordinators has roughly
been the same. The number of minority NFL coordinators breifly fell to
15.25% between 2011 and 2012 because of the increased hiring of minority
head coaches. Between 2014 and the 2016, the number of minority
coordinators increased as some of the minority head coaches were fired
by their teams and a few new minority coaches were promoted to
coordinator positions. From 2017 to 2019 the number of minority coaches
hovered around 20%. Minority Defensive Coordinators decreased in the
first half of the decade as 4 out of the six minority head coaching
hires between 2010 and 2014 were defensive coordinators. Starting in
2014, more new minority coaches were promoted to defensive coordinator
until a maximum of minority defensive coordinators in 2018 when 11 of
the 32 defensive coordinator were a minority. Last year, one third of
the league defensive coordinators were a minority. While minorities
caoches are making headway to becoming defensive coordinators, the same
can not be said for offensive coordinators. The number of minority
offensive coordinators was greatest in 2016 when 5 out of the 35 (some
offenisve coordinators were replaced mid year) offensive coordinators
were minority. This has since regressed to two minority offensive
coordinators in 2019 out of 30 total offensive coordinators.

### Conclusion

While the NFL was able to succeed at first when implementing the Rooney
Rule in 2003 when in 2011 there were 9 minority head coaches, the number
of minority head coaches has dropped off in recent years due to a lack
of new coaching candidates specifically on offense. As the league has
shifted to younger offensive minded coaches like Zac Taylor of the
Cincinnati Bengals and Sean McVay of the Los Angles Rams, there hasn’t
been enough minority coordinators within the NFL in order to sustain the
hiring of minorities. One way that the NFL can build out the base of
offensive minded minority coaches that fits the hiring trend in the last
ten years is by incentiving former minority players to get involved in
coaching. Byron Leftwich and Eric Bieniemy, the only two minority
offensive coordinators in the last two years are both former players. If
NFL teams focused on hiring more former players rather than hiring
assistant and position coaches in their mid twenties coming out of
college than the number of minorities coaches would increase because the
low level entry coaching positions would not always reflect the overall
population of the United States. The percentage of minorities in the US
is roughly 28% and on average since 2003 minority head coaches make up
roughly 18% of NFL head coaches. The average percentage of minority head
coaches since 2003 is more representitive of the full US population than
the NFL minority percentage of 72%. One way to solve the hiring problem
would be to extend the Rooney Rule to the hiring of all coaching
posistions in an effort to attract former players and give equal
opportunity to minorities that are coming out of college. If I was to
continue doing research, I would take a better look into the hiring
practices of teams for entry level coaching positions as well as looking
at the diversity of coaches in college football.

### Bibliography

Fittipaldo, Ray. “Why Do NFL Coaches Rarely Rise from the Ranks of
Players?” Pittsburgh Post-Gazette, Pittsburgh Post-Gazette, 22
Aug. 2014,
www.post-gazette.com/sports/around-the-league-nfl/2014/08/24/Why-NFL-coaches-rarely-rise-from-ranks-of-players/stories/201408240108.

Lapchick, Richard. The 2019 Racial and Gender Report Card: National
Football League. TIDES, 30 Oct. 2019,
43530132-36e9-4f52-811a-182c7a91933b.filesusr.com/ugd/3844fb\_1478b405e58e42608f1ed2223437d398.pdf.

“National Football League Team Captains.” Wikipedia, Wikimedia
Foundation, 17 Jan. 2020,
en.wikipedia.org/wiki/National\_Football\_League\_team\_captains.

“NFL Head Coaching Hires: 2008-2012.” FOX Sports, Fox Sports, 17
Jan. 2013,
www.foxsports.com/other/story/nfl-head-coaching-hires-2008-2012-011713.

“Pro Football Statistics and History.” Pro Football Reference, Sports
Reference LLC, www.pro-football-reference.com/.

Rose, Donovan. NFL Head Coaches Hired During The Last 10 Years: What the
Data Says. The Cauldron, 12 Jan. 2016,
the-cauldron.com/nfl-head-coaches-hired-during-the-last-ten-years-a-look-at-the-data-a41e2e68a1b6.

Wyche, Steve. “Coaching Class of 2006 Still Trying to Find Its Way.”
NFL.com, National Football League, 3 Aug. 2012,
www.nfl.com/news/story/09000d5d80b21b0e/article/coaching-class-of-2006-still-trying-to-find-its-way.
