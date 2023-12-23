
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

# define new column then add to df
city <- c('nyc')
ny <- cbind(ny, city)
head(ny)

# define new column then add to df
city <- c('washington')
wash <- cbind(wash, city)
head(wash)

# define new column then add to df
city <- c('chicago')
chi <- cbind(chi, city)
head(chi)


# bind together the dfs, put na if a df has missing columns
combined <- bind_rows(ny, chi, wash)

head(combined)

# find the avg trip duration by city
dur_mean <- aggregate(
    # Specify data column
    x = combined$Trip.Duration,
    # Specify group indicator
    by = list(combined$city),      
    # Specify function, exclude missing values
    FUN = mean, na.rm = TRUE, na.action = na.pass
)

# rename columns
names(dur_mean)[names(dur_mean) == 'x'] <- 'avg_duration'
names(dur_mean)[names(dur_mean) == 'Group.1'] <- 'city'
#print(dur_mean)

# convert seconds to minutes
dur_mean$avg_duration_min <- (dur_mean$avg_duration / 60)
print(dur_mean)

# create a bar chart to compare avg duration by city
barplot(dur_mean$avg_duration_min, names.arg = dur_mean$city,
        main = "Average Ride Duration", col="darkblue",
        xlab = "City", ylab = "Duration (Minutes)")

# find count of rides by city and user type
subscr_count <- aggregate(
    combined$X ~ combined$city + combined$User.Type, 
    data = combined, 
    FUN = length 
)

# rename columns
names(subscr_count)[names(subscr_count) == 'combined$city'] <- 'city'
names(subscr_count)[names(subscr_count) == 'combined$User.Type'] <- 'user_type'
names(subscr_count)[names(subscr_count) == 'combined$X'] <- 'count'
#print(subscr_count)

# convert user_type from factor to character
subscr_count$user_type <- as.character(subscr_count$user_type) 
 
# replace null user_type with unknown
subscr_count['user_type'][subscr_count['user_type'] == ""] <- "Unknown"
print(subscr_count)

# create a grouped bar chart to compare counts by type per city
ggplot(subscr_count, aes(fill=user_type, y=count, x=city)) + 
    geom_bar(position='dodge', stat='identity') +
    labs(x='City', y='Count', title='Rides by City and User Type') +
    theme(plot.title = element_text(hjust=0.5)) 

gender_count <- aggregate(
    # Specify data column
    x = combined$X,
    # Specify group indicator
    by = list(combined$Gender),      
    # Specify function 
    FUN = length
)

# convert group.1 from factor to character
gender_count$Group.1 <- as.character(gender_count$Group.1) 
 
# replace null group with unknown
gender_count$Group.1[gender_count$Group.1 == ""] <- "Unknown"
#print(gender_count)

# rename columns
names(gender_count)[names(gender_count) == 'x'] <- 'count'
names(gender_count)[names(gender_count) == 'Group.1'] <- 'gender'
print(gender_count)

# create a stacked bar chart to compare counts by gender
ggplot(gender_count, aes(fill=gender, y=count, x='ny-wash-chi')) + 
  geom_bar(position='stack', stat='identity') +
  labs(x='City', y='Count', title='Rides by Gender') +
  theme(plot.title = element_text(hjust=0.5))

system('python -m nbconvert Explore_bikeshare_data.ipynb')
