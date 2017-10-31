Effects of Health Insurance Expansion in the United States
============================================================

Group Members: 
--------------------
Jiangshan Ni (jn2587)        
Joseph G Krongold (jgk2131)      
Rui Chen (rc3054)        
Youyang Cao (yc3232)        


Section 1: Features of Health Insurance in different states.
------------------------------------------------------------
##### Figure 1: US uninsured rate over time in Medicaid expansion states

![](images/joe1.jpg)

In 2014 a number of provisions of the affordable care act went into
effect including the expansion of Medicaid in some states to include all
residents up to 138% of the federal poverty threshold. Figure 1 shows
the time trend in the uninsured rate in states that expanded Medicaid
and those that did not. While Medicaid expansion states had lower
uninsured rates prior to the ACA, perhaps do to more state level
progressive policies on health coverage, the also experienced a
sharper drop after Medicaid expansion, presumably driven by the adoption
of this policy in addition to the ones applicable to both groups.

##### Figure 2: State level changes in uninsurance with Medicaid expansion

![](images/joe2.jpg)

This is a messy plot but does give a sense of the distribution
comprising the trends in Figure 1. It can also be used as a foundation
for a user input that highlights the trend of a state of interest.

##### Figure 3: State level changes in uninsurance with Medicaid expansion

![](images/joe3.jpg) 
Redux of figure 2 with factets. It's cleaner, but
also doesn't lend itself to comparison as well.

##### Figure 4: State level changes in uninsurance with Medicaid expansion

![](images/joe4.jpg)

Here I visualized the same data as figure 2 and 3 but as bar plots,
grouped by state for the three years of interest. I don't love the
plot, but the faceting is kind of interesting. It's also apparent that
there are bigger drop-offs in 2014 for the expansion states, but that
the effect is not uniform.

##### Figure 5: State level uninsured rate over time

![](images/joe5.jpg)

Here I show an example of a state level plot. This could also be
activated by a user. I might try out some treemaps as well to
incorporate the public and private insurance rates in states of
interest. Alternatively that can be included as hover text or something
in the interactive component.

##### Figure 6-8: State level changes in uninsurance rate over time (map)

![](images/un_2013.jpg) 
![](images/un_2014.jpg) 
![](images/un_2015.jpg)

Here the data is presented on a map at the state level. I used
grid.arrange to put the three together and included each of them
separately to work with. As per above, clicking a state could open the
info on type of insurance (from NHIS\_state\_ins\_rate.csv) This shows
the general trend of a decrease over time, the heterogeneity in coverage
across the country, and the fact that some states really don't move
much, presumably these are non-expansion states.

##### Figure 9-13: Changes in uninsured rate in Medicaid expansion states and not

![](images/joe9.jpg) 
![](images/jeo10.jpg) 
![](images/un_2013_non_exp.jpg)
![](images/change_map.jpg) 
![](images/joe13.jpg)

Here I broke out the map to only show expansion states or non-expansion
states This makes the point from above slightly clearer and lets the
viewer see the impact of the policy. There could be a button that toggle
between the full map and just the expansion or not expansion plots.
Overall we can see that while insurance coverage increased in most
states it increased more in those that expanded Medicaid.

Section 2:Time Trend
--------------------

### 1. Uninsured Rate by Region of the Country

![](images/jn0.jpg) Discription: The variable region means that
respondent's living places. 1=Mideast,2=Northwest,3=South,4=West.From
the graph, we could see that the respondents who live in south region
have the highest uninsured rate.And the respondents who live in
northwest region have the lowest uninsured rate.

### 2. Then we could see the time trend for medicaid coverage.

![](images/jn1.jpg) Discription: Over time eligibility for Medicaid has
expanded to provide a base of coverage for the low-income population,
While eligibility has increased over time, eligibility levels vary
significantly across states and eligibility groups. Over time, the gap
between the male with medicaid coverage and femae with low medicaid
coverage.

### 3.Trends over time.

![](images/jn2.jpg) Description:In 2015, the people who have medicaid are
more likely to have higher income. One of the most important provisions
of the Affordable Care Act is the expansion of health coverage to
low-income families through the Medicaid program.

### 4.Boxplot: The time trend on annual final income and sex.

![](images/jn3.jpg) Desiciption:The results shows that the increasing
trend of medicade coverage to the respondents' final annual income.

#### 5.Dotplot: Comparing medicaid level to final annual income on time trend.

![](images/jn4.jpg)

Section 3: Text Mining of Health Insurance
----------------------------------------

In the text mining, we conduct sentiment analysis on the New York Times
articles about United States health insurance, and extract the
high-frequency words in the academic literature which focus on health
care.

Firstly, the positive words and negatives words in the New York Times
articles are identified. The positive words are: tough, ideal, happy,
optimism better, protection, happier, friendly, boom, improve, free,
wins, good, boost, support. From these positives words, we may deduce
that the health insurance does be beneficial to some people.

The negative words extracted are: challenging, dropouts, crisis, worst,
paralyzed, opposition, object, demise. It seems that the health
insurance system could not satisfy every type group, many people still
maintain that it is not helpful, maybe the expensive premium keeps
people worrying.

The overall score of tone estimated in these articles is 0.3043478, not
even be neutral. It could be concluded that even though the health
insurance system in the United States is one of the most mature medicare
systems around the world, but it still has space to be improved.

Next, the Regressive Imagery Dictionary are employed to estimation
several types of emotions highlighted in the New York articles. The
emotions are classified into two type: positive emotions and negative
emotions. The positive emotions involve the sense of affection and
glory, negative emotions contain the feelings of anxiety and sadness.

In the figure below, the trend of two types of positive emotions over
the time could be observed. There exists an increasing trend of both
affection emotion and glory emotion from 2014 to 2017, but big waves
still appear in the patterns, suggesting a fluctuation in the trend of
people's attitudes. The score of affection emotion reaches the highest
around 2016, and the score of glory emotion presents the highest around
2015.

![](images/good_emo(NYtimes).jpg)

In terms of the change of negative emotions, the figure below plots two
pattern of anxiety and sadness respectively. The two panels present a
similar pattern, namely, both emotions of anxiety and sadness reach the
lowest point around 2015 and keep soaring after 2015, which conforms the
decreasing trends of positive emotions after 2015. From these two
figures of emotions toward United States health insurance, it could be
concluded that from 2015-2016, citizens may get more disappointed about
the health care system.

![](images/bad_emo(NYtimes).jpg)

In the last part of text mining, we focus on scholars' attitudes
towards health care. We process the abstracts of health care literatures
from 2010 to 2017, and calculate the frequency of words appearing in
these abstracts. A word cloud of high-frequency word is plotted. The
word could shows that the words researchers always used in their study
of health insurance are: affordable, coverage, percent, etc. These words
suggest to us that scholars may care more about the price issue and the
coverage problem of health insurance, which are indeed severe problems
of the health care system in the States.

![](images/wordcloud(abstract).png)

Process Book
============

Section 1: Features of Health Insurance in different states.
------------------------------------------------------------

    states <- read.csv("data/NHIS_state_ins_rate.csv")
    states$Year <- as.factor(states$Year)
    colnames(states) <- tolower(colnames(states))

    expansion <- read.csv("data/NHIS_expansion_ins_rate.csv")
    expansion$Medicaid.expansion <- as.factor(expansion$Medicaid.expansion)

    #NHIS_2011 <- load("data/personsx2011.rda")
    #NHIS_2012 <- load("data/personsx2012.rda")

    non_exp_states <- c("Alabama", "Florida", "Georgia", "Idaho", "Kansas", "Maine", "Mississippi",
                        "Missouri", "Nebraska", "North Carolina", "Oklahoma", "South Carolina",
                        "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", "Wisconsin", "Wyoming")

    states$exp_ever <- ifelse(states$state %in% non_exp_states,"no medicaid expansion", " medicaid expansion")

    str(expansion)

    ## 'data.frame':    12 obs. of  5 variables:
    ##  $ X...Uninsured     : num  16.4 15.3 15 14.9 10.9 8.2 20.3 19.6 19.2 18.4 ...
    ##  $ Public.Insurance  : num  21.8 23.1 23.1 24.1 25.6 26.7 22.1 22.7 24 23.4 ...
    ##  $ Private.Insurance : num  63.1 62.9 63.3 62.3 64.9 66.4 59 59.1 58.3 59.6 ...
    ##  $ Year              : int  2010 2011 2012 2013 2014 2015 2010 2011 2012 2013 ...
    ##  $ Medicaid.expansion: Factor w/ 2 levels "0","1": 2 2 2 2 2 2 1 1 1 1 ...

    str(states)

    ## 'data.frame':    153 obs. of  11 variables:
    ##  $ x...state     : Factor w/ 51 levels "Alabama","Alaska",..: 1 2 3 4 5 6 7 9 8 10 ...
    ##  $ uninsured     : num  10.8 17.1 13.2 14 9.1 6.7 5.7 6.3 4 15.3 ...
    ##  $ public        : num  30.2 22.5 31.6 27.6 30.2 21.8 25.5 25.2 24.9 25.1 ...
    ##  $ private       : num  61.5 63.7 55.6 60.5 61.5 72 69.9 70.7 71.7 60.8 ...
    ##  $ year          : Factor w/ 3 levels "2013","2014",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ expansion_year: Factor w/ 3 levels "2014","2015",..: 3 3 1 1 1 1 1 1 1 3 ...
    ##  $ expansion     : int  0 0 1 1 1 1 1 1 1 0 ...
    ##  $ other_source  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ x             : logi  NA NA NA NA NA NA ...
    ##  $ x.1           : logi  NA NA NA NA NA NA ...
    ##  $ exp_ever      : chr  "no medicaid expansion" " medicaid expansion" " medicaid expansion" " medicaid expansion" ...

    suppressPackageStartupMessages(library(ggplot2))
    suppressPackageStartupMessages(library(ggthemes))
    suppressPackageStartupMessages(library(dplyr))

    #overall uninsured rate
    ggplot(data = expansion, aes(x = Year, y = Uninsured, group = Medicaid.expansion)) + geom_line(aes(color = Medicaid.expansion))

![](images/joe1.jpg)

    #view for each state. Could do this in Shiny where the user selects a state of interest that gets a color change or shown individually

    ggplot(data = states, aes(x = year, y = uninsured, group = state)) +
      geom_line(aes(color = exp_ever))

![](images/joe2.jpg)

    #with facet for medicaid expansion

    ggplot(data = states, aes(x = year, y = uninsured, group = state)) +
      geom_line(aes(color = exp_ever)) + facet_grid(exp_ever ~ .)

![](images/joe3.jpg)

    #same idea but with bar graphs for each state and year for the uninsurance rate
     ggplot(data = states, aes(x = state, y = uninsured, fill = year)) +
      geom_bar(stat="Identity", position=position_dodge()) + facet_grid(exp_ever ~ .) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())

![](images/joe4.jpg)

    #example of a state level view, state of interest (Utah here) could be a user input
    ggplot(data = states[states$state == "Utah", ], aes(x = state, y = uninsured, fill = year)) +
      geom_bar(stat="Identity", position=position_dodge()) + xlab("state")

![](images/joe5.jpg)

setup code:

    suppressPackageStartupMessages(library(maps))
    suppressPackageStartupMessages(library(tidyverse))

    ## Conflicts with tidy packages ----------------------------------------------

    suppressPackageStartupMessages(library(stringr))
    suppressPackageStartupMessages(library(gridExtra))

    us.states <- map_data("state")
    us.states <- as_data_frame(us.states)
    us.states <- dplyr::rename(us.states, state=region)
    us.states$subregion = NULL
    us.states$state <- str_to_title(us.states$state)
    # Add State Abbreviations and Centers
    statenames <- as_data_frame(cbind(state=state.name, 
                    state.abb = state.abb, state.center.x = state.center$x, 
                    state.center.y = state.center$y))
    statenames <- statenames %>% mutate_each_(funs(as.numeric), 
                    vars=c("state.center.x","state.center.y"))
    us.states <- left_join(us.states,statenames)

    ## Joining, by = "state"

and the maps:

    merge_2015 <- left_join(states[states$year == 2015,], us.states)

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## character vector and factor, coercing into character vector

    merge_2014 <- left_join(states[states$year == 2014,], us.states)

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## character vector and factor, coercing into character vector

    merge_2013 <- left_join(states[states$year == 2013,], us.states)

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## character vector and factor, coercing into character vector

    un_2015 <- ggplot(merge_2015,aes(x = long, y = lat, group=group)) + 
      geom_polygon(aes(fill = uninsured), color="white") + 
      scale_fill_gradientn(limits = c(0,25), colours=c("white","blue")) +
      geom_text(data=statenames, inherit.aes = FALSE, 
                aes(label=state.abb, x=state.center.x, y=state.center.y), 
                colour="white", size=2) +
      theme_tufte() + coord_map(projection = "mercator") +
      ggtitle(label = "2015 Uninsurance Rate")

    un_2014 <- ggplot(merge_2014,aes(x = long, y = lat, group=group)) + 
      geom_polygon(aes(fill = uninsured), color="white") + 
      scale_fill_gradientn(limits = c(0,25), colours=c("white","blue")) +
      geom_text(data=statenames, inherit.aes = FALSE, 
                aes(label=state.abb, x=state.center.x, y=state.center.y), 
                colour="white", size=2) +
      theme_tufte() + coord_map(projection = "mercator") +
      ggtitle(label = "2014 Uninsurance Rate")

    un_2013 <- ggplot(merge_2013,aes(x = long, y = lat, group=group)) + 
      geom_polygon(aes(fill = uninsured), color="white") + 
      scale_fill_gradientn(limits = c(0,25), colours=c("white","blue")) +
      geom_text(data=statenames, inherit.aes = FALSE, 
                aes(label=state.abb, x=state.center.x, y=state.center.y), 
                colour="white", size=2) +
      theme_tufte() + coord_map(projection = "mercator") +
      ggtitle(label = "2013 Uninsurance Rate")
    grid.arrange(un_2013, un_2014, un_2015, ncol = 1, nrow = 3)

![](Combined_Project_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    un_2013; un_2014; un_2015

![](images/un_2013.jpg)
![](images/un_2014.jpg)
![](images/un_2015.jpg)

Breakouts of maps by expansion and not for each year

    merge_2015_exp <- left_join(states[states$year == 2015 & states$expansion == 1,], us.states)

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## character vector and factor, coercing into character vector

    merge_2015_non_exp <- left_join(states[states$year == 2015 & states$expansion == 0,], us.states)

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## character vector and factor, coercing into character vector

    un_2015_exp <- ggplot(merge_2015_exp,aes(x = long, y = lat, group=group)) + 
      geom_polygon(aes(fill = uninsured), color="white") + 
      scale_fill_gradientn(limits = c(0,25), colours=c("white","blue")) +
      geom_text(data=statenames, inherit.aes = FALSE, 
                aes(label=state.abb, x=state.center.x, y=state.center.y), 
                colour="white", size=2) +
      theme_tufte() + coord_map(projection = "mercator") +
      ggtitle(label = "2015 Uninsurance Rate in expansion states")

    un_2015_non_exp <- ggplot(merge_2015_non_exp,aes(x = long, y = lat, group=group)) + 
      geom_polygon(aes(fill = uninsured), color="white") + 
      scale_fill_gradientn(limits = c(0,25), colours=c("white","blue")) +
      geom_text(data=statenames, inherit.aes = FALSE, 
                aes(label=state.abb, x=state.center.x, y=state.center.y), 
                colour="white", size=2) +
      theme_tufte() + coord_map(projection = "mercator") +
      ggtitle(label = "2015 Uninsurance Rate in non-expansion states")

     grid.arrange(un_2015_exp, un_2015_non_exp, nrow=2,ncol=1)

![](images/joe9.jpg)

    merge_2014_exp <- left_join(states[states$year == 2014 & states$expansion == 1,], us.states)

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## character vector and factor, coercing into character vector

    merge_2014_non_exp <- left_join(states[states$year == 2014 & states$expansion == 0,], us.states)

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## character vector and factor, coercing into character vector

    un_2014_exp <- ggplot(merge_2014_exp,aes(x = long, y = lat, group=group)) + 
      geom_polygon(aes(fill = uninsured), color="white") + 
      scale_fill_gradientn(limits = c(0,25), colours=c("white","blue")) +
      geom_text(data=statenames, inherit.aes = FALSE, 
                aes(label=state.abb, x=state.center.x, y=state.center.y), 
                colour="white", size=2) +
      theme_tufte() + coord_map(projection = "mercator") +
      ggtitle(label = "2014 Uninsurance Rate in expansion states")

    un_2014_non_exp <- ggplot(merge_2014_non_exp,aes(x = long, y = lat, group=group)) + 
      geom_polygon(aes(fill = uninsured), color="white") + 
      scale_fill_gradientn(limits = c(0,25), colours=c("white","blue")) +
      geom_text(data=statenames, inherit.aes = FALSE, 
                aes(label=state.abb, x=state.center.x, y=state.center.y), 
                colour="white", size=2) +
      theme_tufte() + coord_map(projection = "mercator") +
      ggtitle(label = "2014 Uninsurance Rate in non-expansion states")

    grid.arrange(un_2014_exp, un_2014_non_exp, nrow=2,ncol=1)

![](images/jeo10.jpg)

    merge_2013_non_exp <- left_join(states[states$year == 2013 & states$expansion == 0,], us.states)

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## character vector and factor, coercing into character vector

    un_2013_non_exp <- ggplot(merge_2013_non_exp,aes(x = long, y = lat, group=group)) + 
      geom_polygon(aes(fill = uninsured), color="white") + 
      scale_fill_gradientn(limits = c(0,25), colours=c("white","blue")) +
      geom_text(data=statenames, inherit.aes = FALSE, 
                aes(label=state.abb, x=state.center.x, y=state.center.y), 
                colour="white", size=2) +
      theme_tufte() + coord_map(projection = "mercator") +
      ggtitle(label = "2013 Uninsurance Rate in non-expansion states (all)")
![](images/un_2013_non_exp.jpg)
Change in uninsured rate, public insurance rate, and private insurance
from 2013 to 2015 in each state

    tmp <- cbind.data.frame(state = c(as.character(unique(states$state))),
                 un_2013 = c(states$uninsured[states$year == 2013]),
                 un_2015 = c(states$uninsured[states$year == 2015]))
    tmp$change <- tmp$un_2015 - tmp$un_2013
    tmp$medicaid_expansion <- ifelse(tmp$state %in% non_exp_states,"no expansion","expanded Medicaid")
    str(tmp)

    ## 'data.frame':    51 obs. of  5 variables:
    ##  $ state             : Factor w/ 51 levels "Alabama","Alaska",..: 1 2 3 4 5 6 7 9 8 10 ...
    ##  $ un_2013           : num  13 18.5 21.1 21.2 19.1 14.7 10.5 10.7 3.8 24.7 ...
    ##  $ un_2015           : num  10.8 17.1 13.2 14 9.1 6.7 5.7 6.3 4 15.3 ...
    ##  $ change            : num  -2.2 -1.4 -7.9 -7.2 -10 ...
    ##  $ medicaid_expansion: chr  "no expansion" "expanded Medicaid" "expanded Medicaid" "expanded Medicaid" ...

    merge_change <- left_join(tmp, us.states)

    ## Joining, by = "state"

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## character vector and factor, coercing into character vector

    change_map <- ggplot(merge_change,aes(x = long, y = lat, group=group)) + 
      geom_polygon(aes(fill = change), color="white") + 
      scale_fill_gradientn(colours=c("white","blue")) +
      geom_text(data=statenames, inherit.aes = FALSE, 
                aes(label=state.abb, x=state.center.x, y=state.center.y), 
                colour="white", size=2) +
      theme_tufte() + coord_map(projection = "mercator") +
      ggtitle(label = "change in unisurance rate from 2013 to 2015")



    change_bar <- ggplot(tmp, aes(x = reorder(state,-change), y = change, fill = medicaid_expansion)) +
      geom_bar(stat="identity") + coord_flip() + labs(x = "state", y = "change in uninsured rate")

     change_bar + scale_x_discrete(position = "top")

![](images/change_map.jpg)
![](images/joe13.jpg)

Section 2:Time Trend
--------------------

### 1. Then we could see the time trend for medicaid coverage.

Process: I aggregate the colums and rows to comebine the data to the
dataframe.Because variable medicaid has 5 level.The medicaid.1 indicaits
that the respondents have medicaid coverage. The variable medicaid.2
indicaits the respondents have medicaid coverage but no information.The
variable medicaid.3 indicaits the respondents don't have medicaid
coverage.The variable medicaid.7 indicaits the respondents refused to
answer this question.

    dt<-read.csv("data/dt.csv",sep=',',header=T)
    dt<-data.frame(dt)
    dim(dt)

    ## [1] 530368      6

    summary(dt)

    ##        X               year         medicaid         region     
    ##  Min.   :     1   Min.   :2011   Min.   :1.000   Min.   :1.000  
    ##  1st Qu.:132593   1st Qu.:2012   1st Qu.:3.000   1st Qu.:2.000  
    ##  Median :265184   Median :2013   Median :3.000   Median :3.000  
    ##  Mean   :265184   Mean   :2013   Mean   :2.719   Mean   :2.766  
    ##  3rd Qu.:397776   3rd Qu.:2014   3rd Qu.:3.000   3rd Qu.:4.000  
    ##  Max.   :530368   Max.   :2015   Max.   :9.000   Max.   :4.000  
    ##       wtfa            sex       
    ##  Min.   :    0   Min.   :1.000  
    ##  1st Qu.: 1585   1st Qu.:1.000  
    ##  Median : 2616   Median :2.000  
    ##  Mean   : 2931   Mean   :1.517  
    ##  3rd Qu.: 4096   3rd Qu.:2.000  
    ##  Max.   :32717   Max.   :2.000

    data1<-load("personsx2011.rda")
    data2<-load("personsx2012.rda")
    data3<-load("personsx2013.rda")
    data4<-load("personsx2014.rda")
    data5<-load("personsx2015.rda")

    dt1<-data.frame(cbind(NHIS.11.personsx.df$srvy_yr,NHIS.11.personsx.df$medicaid,NHIS.11.personsx.df$sex))
    dt2<-data.frame(cbind(NHIS.12.personsx.df$srvy_yr,NHIS.12.personsx.df$medicaid,NHIS.12.personsx.df$sex))
    dt3<-data.frame(cbind(NHIS.13.personsx.df$srvy_yr,NHIS.13.personsx.df$medicaid,NHIS.13.personsx.df$sex))
    dt4<-data.frame(cbind(NHIS.14.personsx.df$srvy_yr,NHIS.14.personsx.df$medicaid,NHIS.14.personsx.df$sex))
    dt5<-data.frame(cbind(NHIS.15.personsx.df$srvy_yr,NHIS.15.personsx.df$medicaid,NHIS.15.personsx.df$sex))
    dt1m<-dt1[dt1$X3==1,]
    dt1f<-dt1[dt1$X3==2,]
    dt2m<-dt1[dt2$X3==1,]
    dt2f<-dt1[dt2$X3==2,]
    dt3m<-dt1[dt3$X3==1,]
    dt3f<-dt1[dt3$X3==2,]
    dt4m<-dt1[dt4$X3==1,]
    dt4f<-dt1[dt4$X3==2,]
    dt5m<-dt1[dt5$X3==1,]
    dt5f<-dt1[dt5$X3==2,]
    m1<-prop.table(table(dt1m$X2))
    f1<-prop.table(table(dt1f$X2))
    m2<-prop.table(table(dt2m$X2))
    f2<-prop.table(table(dt2f$X2))
    m3<-prop.table(table(dt3m$X2))
    f3<-prop.table(table(dt3f$X2))
    m4<-prop.table(table(dt4m$X2))
    f4<-prop.table(table(dt4f$X2))
    m5<-prop.table(table(dt5m$X2))
    f5<-prop.table(table(dt5f$X2))
    dataset<-rbind(m1,f1,m2,f2,m3,f3,m4,f4,m5,f5)
    dataset

    ##            1           2         3           7           9
    ## m1 0.1408491 0.002071907 0.8479586 0.002254723 0.006865732
    ## f1 0.1670244 0.002602336 0.8214265 0.002032482 0.006914237
    ## m2 0.1534764 0.002311453 0.8354164 0.002229632 0.006566163
    ## f2 0.1552050 0.002377897 0.8331698 0.002057070 0.007190307
    ## m3 0.1549307 0.002356670 0.8339564 0.001930031 0.006826216
    ## f3 0.1538564 0.002336049 0.8345204 0.002336049 0.006951171
    ## m4 0.1544801 0.002618651 0.8337055 0.002111161 0.007084568
    ## f4 0.1542775 0.002090738 0.8347557 0.002166765 0.006709368
    ## m5 0.1537790 0.002455108 0.8346556 0.002069595 0.007040682
    ## f5 0.1549344 0.002243773 0.8338658 0.002205743 0.006750333

    library(ggplot2)
    library(ggthemes)
    library(devtools)
    library(RCurl)

    ## Loading required package: bitops

    ## 
    ## Attaching package: 'RCurl'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     complete

    library(plyr)

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

    ## The following object is masked from 'package:maps':
    ## 
    ##     ozone

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    dd<-read.csv("data/plot1.csv",sep=',',header=T)
    dd<-data.frame(dd)
    dim(dd)

    ## [1] 10  7

    dd

    ##    srvy_yr medicaid.1  medicaid.2 medicaid.3  medicaid.7  medicaid.9 sex
    ## 1     2011  0.1408491 0.002071907  0.8479586 0.002254723 0.006865732   1
    ## 2     2011  0.1670244 0.002602336  0.8214265 0.002032482 0.006914237   2
    ## 3     2012  0.1534764 0.002311453  0.8354164 0.002229632 0.006566163   1
    ## 4     2012  0.1552050 0.002377897  0.8331698 0.002057070 0.007190307   2
    ## 5     2013  0.1549307 0.002356670  0.8339564 0.001930031 0.006826216   1
    ## 6     2013  0.1538564 0.002336049  0.8345204 0.002336049 0.006951171   2
    ## 7     2014  0.1544801 0.002618651  0.8337055 0.002111161 0.007084568   1
    ## 8     2014  0.1542775 0.002090738  0.8347557 0.002166765 0.006709368   2
    ## 9     2015  0.1537790 0.002455108  0.8346556 0.002069595 0.007040682   1
    ## 10    2015  0.1549344 0.002243773  0.8338658 0.002205743 0.006750333   2

    library(ggplot2)
    transmission<-factor(dd$sex,levels = c(1,2),labels=c("Male","Female"))
    #medicaid.1
    qplot(dd$srvy_yr,dd$medicaid.1,data = dd,alpha=0.3,
          color=transmission,geom=c("point","smooth"),
          method="lm",formula=y~x,xlab="Year of National Health Interview Survey", ylab="Medicaid coverage ")+geom_point(alpha=0.3)+geom_smooth(method = lm, se = TRUE,alpha=0.3)+ theme_tufte()+theme(legend.position="none")+ggtitle("Medicaid Coverage of Female and Male on Time Trend")

    ## Warning: Ignoring unknown parameters: method, formula

![](images/jn1.jpg)

    ggsave("jn2.jpg")

    ## Saving 7 x 5 in image

    library(ggplot2)
    dt1<-cbind(NHIS.11.personsx.df$srvy_yr,NHIS.11.personsx.df$medicaid,NHIS.11.personsx.df$region,NHIS.11.personsx.df$wtfa,NHIS.11.personsx.df$sex)
    dt2<-cbind(NHIS.12.personsx.df$srvy_yr,NHIS.12.personsx.df$medicaid,NHIS.12.personsx.df$region,NHIS.12.personsx.df$wtfa,NHIS.12.personsx.df$sex)
    dt3<-cbind(NHIS.13.personsx.df$srvy_yr,NHIS.13.personsx.df$medicaid,NHIS.13.personsx.df$region,NHIS.13.personsx.df$wtfa,NHIS.13.personsx.df$sex)
    dt4<-cbind(NHIS.14.personsx.df$srvy_yr,NHIS.14.personsx.df$medicaid,NHIS.14.personsx.df$region,NHIS.14.personsx.df$wtfa,NHIS.14.personsx.df$sex)
    dt5<-cbind(NHIS.15.personsx.df$srvy_yr,NHIS.15.personsx.df$medicaid,NHIS.15.personsx.df$region,NHIS.15.personsx.df$wtfa,NHIS.15.personsx.df$sex)
    dt<-data.frame(rbind(dt1,dt2,dt3,dt4,dt5))
    dt<-data.frame(rbind(dt1,dt2,dt3,dt4,dt5))
    dim(dt)

    ## [1] 530368      5

    names(dt)<-c("year","medicaid","region","wtfa","sex")
    write.csv(dt,'dt.csv')
    


    by.year <-dt %>% group_by(year,medicaid,sex,region) %>% summarize(wtfa=mean(wtfa,na.rm=TRUE))
    by.year

    ##       wtfa
    ## 1 2930.749

### 2.Trends over time.

Process: I average data by group to do data manipulation.I use ggplot by
year to find the relationship between repondents' weighted final annual
income(wtfa) and medicaid. From the graph, we could see that the
respondents who have medicaid coverage are more poor than the
respondnets who don't have medicaid coverage. It indicates that more
respondents have medicaid coverage as a year-on year increase.

![](images/jn2.jpg)

### 3.Boxplot: The time trend on annual final income and sex.

Process: My first thought would be to have a general idea as to what the
relationship between medicaid coverage and respondents' final annual
income. I use ggplot to plot the graph. I would identify and visualize
the issue with wtfa and medicaid by using gram\_boxplot to have a
clearly visualized graph.

    ggplot(dt, alpha=0.6,aes(x=reorder(medicaid, wtfa, na.rm=TRUE),y=wtfa)) + geom_boxplot(aes(fill = sex),alpha=0.6) + theme_tufte()+ ylab("Final Annual Income") + xlab("Meidicaid Coverage") + labs(title = "The effect of medicaid on  final annual income and sex ") + theme(legend.position="none")+scale_y_continuous(limits = c(0,10000))

![](images/jn3.jpg)

### $.Dotplot: Comparing medicaid level to final annual income on time trend.

    ggplot(dt, aes(x = year, y = wtfa, color = medicaid)) +
    geom_point(alpha=0.3, size=3) +
    facet_grid( . ~ medicaid) +
    geom_smooth(color="black", lwd=1, se=TRUE, method = lm) +
    ylab("Final Annual Income") +
    theme_tufte() +
    theme(legend.position="none",axis.text.x = element_text(size = 8)) +
    ggtitle("Comparing medicaid level to final annual income on time trend.")

![](images/jn4.jpg)

Section 3
---------

### 1.Sentiment Analysis of New York Times Articles

In this part, we are going to pick out the positive words and negative
words in the New York Times, estimate the tone of articles, and plot the
change of authors' emotions about the health insurance over the time.
The texts being analyzed are New York Times articles about United States
Health Insurance.

    # load the csv file contianing texts and the dates
    raw.text <- read.csv("data/rawtext.csv", header = TRUE)
    raw.text <- raw.text[1:34,c(3:4)]

    # make the texts as character
    raw.text$text <- as.character(raw.text$text)

    # make the date in R-friendly format
    raw.text$date <- as.Date(raw.text$date, "%m/%d/%y")
    row.names(raw.text) <- 1:34

    # make the corpus and attach meta data
    raw <- Corpus(VectorSource(raw.text$text))

    # Cleaning the text
    raw <- tm::tm_map(raw, content_transformer(replace_symbol))
    raw <- tm::tm_map(raw, content_transformer(tolower))

    # make two customized cleaning functions
    removeSpecialChars1 <- function(x) gsub("<>%","",x)
    removeSpecialChars2 <- function(x) gsub("^","",x)
    raw <- tm::tm_map(raw, removeSpecialChars1)
    raw <- tm::tm_map(raw, removeSpecialChars2)

    corpus <- tm::tm_map(raw, removeNumbers)
    corpus <- tm::tm_map(corpus, removePunctuation)
    corpus <- tm::tm_map(corpus, removeWords, c(stopwords("english")))  
    corpus <- tm::tm_map(corpus, stripWhitespace)
    corpus <- tm::tm_map(corpus, PlainTextDocument)

    # attach the date as meta data
    meta(corpus, type="local", tag = "date") <- raw.text$date

    # make a quandeta corpus
    corp <- corpus(raw.text,text_field = "text")

Extract postive words and negative words, measure the score of tones.

    # load two dictionaries
    pos <- read.table("data/positive-words.txt", as.is=T)
    neg <- read.table("data/negative-words.txt", as.is=T)

    # make dictionary
    myDict <- dictionary(list(positive = pos, negative = neg))

    # Hu & Liu sentiment analysis function
    sentiment <- function(words=c("really great good stuff bad")){
      require(quanteda)
      tok <- quanteda::tokenize(words)
      pos.count <- sum(tok[[1]]%in%pos[,1])
      cat("\n positive words:",tok[[1]][which(tok[[1]]%in%pos[,1])],"\n")
      neg.count <- sum(tok[[1]]%in%neg[,1])
      cat("\n negative words:",tok[[1]][which(tok[[1]]%in%neg[,1])],"\n")
      out <- (pos.count - neg.count)/(pos.count+neg.count)
      cat("\n Tone of Document:",out)
    }

    # sentiment analysis of nytimes articles using Hu & Liu
    sent <- sentiment(corp$documents$texts)

    ## 
    ##  positive words: tough ideal happy optimism better protection happier friendly boom improve free wins good boost support 
    ## 
    ##  negative words: challenging dropouts crisis worst paralyzed opposition object demise 
    ## 
    ##  Tone of Document: 0.3043478

    sent

    ## NULL

Measuring Emotions by RID Dictionary

    # loading RID dictionary
    RID_dictionary <- dictionary(file="data/RID.cat",
                                 format = "wordstat")

    # make dfm based of the dictionary
    dtm_rid <- dfm(corp, dictionary = RID_dictionary)

    # add date info to the dtm
    RIDdf <- melt(as.matrix(dtm_rid))
    RIDdf$date <- as.Date(raw.text$date)
    RIDdf1 <- as_data_frame(RIDdf)

    # select emociton features
    posi.emotions <- filter(RIDdf1, features == c("EMOTIONS.AFFECTION._", "EMOTIONS.POSITIVE_AFFECT._ ",   "EMOTIONS.GLORY._"))

    ## Warning in is.na(e1) | is.na(e2): longer object length is not a multiple of
    ## shorter object length

    ## Warning in `==.default`(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, :
    ## longer object length is not a multiple of shorter object length

    pass.emotions <- filter(RIDdf1, features == c("EMOTIONS.ANXIETY._",  "EMOTIONS.SADNESS._"))

    # plot positive emotions reflected in New York Times Articles
    library(ggrepel)

    good_emo <- ggplot(posi.emotions, aes(x=date, y=value, color = features)) + 
                     scale_colour_manual(values=c("orange", "red"))+
                      ylab("Emotion Score") + 
                      xlab("Time") + 
                      theme_minimal() +
                      geom_smooth(se = FALSE ) +
                      facet_wrap(~features)+
                     scale_color_discrete(name = "Positive Emotions",
                                          breaks = c( c("EMOTIONS.AFFECTION._", "EMOTIONS.GLORY._")),
                                          labels=c("Affection", "Glory")) +
                      ggtitle("Positive Emotions about Health Insurance Reflected in the New Yore Times Articles") +
                      theme(plot.title = element_text(hjust = 0.5))

    ## Scale for 'colour' is already present. Adding another scale for
    ## 'colour', which will replace the existing scale.

    good_emo

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](images/good_emo(NYtimes).jpg)

    # plot negative emotions reflected in New York Times Articles
    bad_emo <- ggplot(pass.emotions, aes(x=date, y=value, color = features)) + 
                     scale_colour_manual(values=c("bisque4", "steelblue"))+
                      ylab("Emotion Score") + 
                      xlab("Time") + 
                      theme_minimal() +
                      geom_smooth(se = FALSE ) +
                      facet_wrap(~features)+
                      guides(fill=guide_legend(title="Emotions")) +
                      scale_color_discrete(name = "Negative Emotions",
                                          breaks = c("EMOTIONS.ANXIETY._",  "EMOTIONS.SADNESS._"),
                                          labels=c("Anxiety", "Sadness")) +
                      ggtitle("Negative Emotions about Health Insurance Reflected in the New Yore Times Articles") +
                      theme(plot.title = element_text(hjust = 0.5))

    ## Scale for 'colour' is already present. Adding another scale for
    ## 'colour', which will replace the existing scale.

    bad_emo

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](images/bad_emo(NYtimes).jpg)

### 2. Plot the high-frequency words appearing in the abstract of academic literature about health insurance

In this part, in order to explore more about people's attitude towards
health insurance in academy, a wordcloud of high-frequency words in the
abstract of academic literature will be plotted.

    # load csv.documents of abstracts
    sc_20102011 <- read.csv("data/scopus_2010_2011.csv")
    sc_2012 <- read.csv("data/scopus2012.csv")
    sc_2013 <- read.csv("data/scopus_2013.csv")
    sc_2014 <- read.csv("data/scopus_2014.csv")
    sc_2015 <- read.csv("data/scopus_2015.csv")
    sc_20162017 <- read.csv("data/scopus_2016_2017.csv")

    # bind dataframes together
    sc <- rbind(sc_20102011[,c(1,3)], sc_2012[,c(1,3)],sc_2013[,c(1,3)],sc_2014[,c(1,3)],sc_2015[,c(1,3)], sc_20162017[,c(1,3)])
    sc <- na.omit(sc)

    # change column names
    colnames(sc) <- c("year", "text")
    sc$text <- as.character(sc$text)

    # make the corpus of abstract texts
    raw_sc <- Corpus(VectorSource(sc$text))

    # Cleaning the raw corpus
    raw_sc <- tm::tm_map(raw_sc, content_transformer(replace_symbol))
    raw_sc <- tm::tm_map(raw_sc, content_transformer(tolower))

    removeSpecialChars1 <- function(x) gsub("<>%","",x)
    removeSpecialChars2 <- function(x) gsub("^","",x)
    raw_sc <- tm::tm_map(raw_sc, removeSpecialChars1)
    raw_sc <- tm::tm_map(raw_sc, removeSpecialChars2)

    corpus_sc <- tm::tm_map(raw_sc, removeNumbers)
    corpus_sc <- tm::tm_map(corpus_sc, removePunctuation)
    corpus_sc <- tm::tm_map(corpus_sc, removeWords, c(stopwords("english")))  
    corpus_sc <- tm::tm_map(corpus_sc, removeWords, c("abstract", "available", "health", "care"))
    corpus_sc <- tm::tm_map(corpus_sc, stripWhitespace)
    corpus_sc <- tm::tm_map(corpus_sc, PlainTextDocument)

    # make document-term matrix
    dtmsc <- DocumentTermMatrix(corpus_sc)
    tdtmsc <- tidy(dtmsc)

    # count the word frequency
    ee <- group_by(tdtmsc ,term) %>%
            summarise(n = sum(count))
