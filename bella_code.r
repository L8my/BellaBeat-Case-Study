##Create some dataframes for the data
daily_activity <- read.csv("dailyActivity_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
daily_calories <- read.csv("dailyCalories_merged.csv")
hourly_calories <- read.csv("hourlyCalories_merged.csv")
daily_intensities <- read.csv("dailyIntensities_merged.csv")
hourly_intensities <- read.csv("hourlyIntensities_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")

##Lets check to make sure everything imported correctly

head(daily_activity)
head(daily_calories)
head(hourly_intensities)
head(hourly_calories)

colnames(daily_activity)
colnames(hourly_calories)

##Looks like we need to separate out time and date into two separate columns for ease of sight

#hourly intensities
hourly_intensities$ActivityHour = as.POSIXct(hourly_intensities$ActivityHour, format ="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_intensities$Time <- format(hourly_intensities$ActivityHour, format = "%H:%M:%S")

hourly_intensities$Date <- format(hourly_intensities$ActivityHour, format = "%m/%d/%y")

head(hourly_intensities)

##for activites 

daily_activity$ActivityDate=as.POSIXct(daily_activity$ActivityDate, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
daily_activity$Time <- format(daily_activity$ActivityDate, format = "%H:%M:%S")
daily_activity$Date <- format(daily_activity$ActivityDate, format = "%m/%d/%y")

head(daily_activity)

##for calories
hourly_calories$ActivityHour = as.POSIXct(hourly_calories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_calories$Time <- format(hourly_calories$ActivityHour, format = "%H:%M:%S")
hourly_calories$Date <- format(hourly_calories$ActivityHour, format = "%m/%d/%y")

head(hourly_calories)

##for sleep
sleep_day$SleepDay=as.POSIXct(sleep_day$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep_day$Date <- format(sleep_day$SleepDay, format = "%m/%d/%y")

head(sleep_day)

hourly_steps$ActivityHour = as.POSIXct(hourly_steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_steps$Time <- format(hourly_calories$ActivityHour, format = "%H:%M:%S")
hourly_steps$Date <- format(hourly_calories$ActivityHour, format = "%m/%d/%y")
head(hourly_steps)

##Lets clean up the columns a bit
sleep_day2<- sleep_day [, -2]
head(sleep_day2)

hourly_calories2 <- hourly_calories[, -2]
head(hourly_calories2)

hourly_steps2 <- hourly_steps[,-2]
head(hourly_steps2)

##These 2 codes do the same thing as above, just a different technique
daily_activity2 <- subset(daily_activity, select = -c(ActivityDate, Time, Date))
head(daily_activity2)

hourly_intensities2 <- subset(hourly_intensities, select = -c(ActivityHour))
head(hourly_intensities2)

##Lets make sure there are the same number of participants in each data set

n_distinct(sleep_day2$Id)
n_distinct(hourly_calories2$Id)
n_distinct(hourly_intensities$Id)
n_distinct(hourly_steps$Id)
n_distinct(daily_activity2$Id)
n_distinct(hourly_steps$Id)

##Looks like there's less participants for the sleep data, but there should be enough to make some basic recommendations

##Lets now look at some statistics for the data and make some dataframes

##for activity
daily_activity2 %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()

##for sleep activity
sleep_day2 %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

## for daily intensities aka how active they are in a day
daily_intensities %>%
  select(SedentaryMinutes,
         LightlyActiveMinutes,
         FairlyActiveMinutes,
         VeryActiveMinutes) %>%
  summary()

## Lets look at the average hourly steps
hourly_steps3 <- hourly_steps2 %>%
  group_by(Time) %>%
  drop_na() %>%
  summarise(mean_hour_steps = mean(StepTotal))

##Lets look at average daily steps for each participant

daily_act <- daily_activity2 %>%
  group_by(Id) %>%
  drop_na() %>%
  summarise(mean_steps = mean(TotalSteps))

##Looking at this data, it seems like the average sedentary time is 991 minutes, or 16.5 hours
##For sleep data, the average time spent asleep is about 7 hours, while the time spent in bed
##is about 7.6 hours, meaning that most of the time spent in bed is sleeping.
##Average amount of steps is about 7638, which is less than the CDC recommended amount of 8000 steps/day


##Before we plot, we need to merge some data to get a look at the big picture

combined_data <- merge(sleep_day2, daily_activity2, by= "Id")
head(combined_data)

##Lets look plot a chart looking at sedentary minutes vs very active minutes

ggplot(data = daily_activity2, aes(x=VeryActiveMinutes, y=SedentaryMinutes)) +
  geom_point() +
  labs(title = "Sedentary Minutes vs. Very Active Minutes")

## It looks like there are a lot of Sedentary Minutes compared to Very Active Minutes. 
## Maybe a reminder on a BellaBeat app to move more often?

##Lets now plot a chart looking at Calories vs Active Minutes (assuming calories burned)

ggplot(data = combined_data, aes(x= Calories, y= VeryActiveMinutes)) +
 geom_point() +
  geom_smooth() +
  labs(title = "Active Minutes vs. Calories")

##It looks like there is a positive correlation between calories burned and Very Active Minutes, 
##Which does make sense, the more you move, the more calories are burned.
##This is assuming the caloric data is calories burned and not calories consumed.

## Lets now plot for step total per day

ggplot(data = hourly_steps2, aes(x=Date, y = StepTotal))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Activity Date")

## Looks like most days, most of the participants stayed under 5000 steps.

## I'm going to now plot the average number of daily steps per participant

ggplot(data = daily_act, aes(x = Id, y = mean_steps))+
  geom_point()+
  geom_smooth()+
  labs(title = "Average Daily Steps per Participant", y = "Average Steps")

## And now the average number of steps per hour in a day

ggplot(data = hourly_steps3, aes(x = Time, y = mean_hour_steps))+
  geom_histogram( stat = 'identity', fill = 'darkgreen') +
  labs(title = "Average Hourly Steps", y = "Average Number of Steps")+
  theme(axis.text.x = element_text(angle = 90))

##Looks like the average number of steps are taken mid-afternoon and mid-evening, averaging between 400 and 600 steps an hour

##Lets plot some data looking at the hourly intensities over time

hourly_intensities3 <- hourly_intensities2 %>%
  group_by(Time) %>%
  drop_na() %>%
  summarise(mean_intensities = mean(TotalIntensity))

glimpse(hourly_intensities3)

ggplot(data = hourly_intensities3, aes(x=Time, y = mean_intensities))+
  geom_bar(fill = 'darkblue', stat = 'identity')+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Intensities by the Hour", y = "Mean Intensity Minutes")

##It looks like people are more active after work, in the evenings, perhaps they go to the gym after work?  

