
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

filename="datasciencejobs_2024.csv"
mydata=read.csv(filename)


# see data ----------------------------------------------------------

head(mydata)


# see data types ----------------------------------------------------------

str(mydata)

# deliverable 1 ----------------------------------------------------------

library(ggplot2)

mydata$Work_Setting <- factor(mydata$remote_ratio, levels = c("In person", "Remote", "Hybrid"))

#plot

base= ggplot(data=mydata) 
deliverable1= base + geom_bar(aes(x=Work_Setting))+
  theme_minimal()+
  labs(title= "Where are new job listings located post-pandemic?",
         subtitle = "Number of Available Jobs in Data Science by Work Location",       
       x = "Work Setting",
       y = "Number of jobs",
        caption = "Source: “The Global AI, ML, Data Science Salary Index for 2024.” AI-Jobs, 2024, ai-jobs.net/salaries/2024/." )
print(deliverable1)

# save del1 ----------------------------------------------------------
saveRDS(deliverable1, file = "deliverable1.rds")


# deliverable 2 ----------------------------------------------------------

mydata_filtered <- subset(mydata, salary_in_usd <= 400000)
mean_salary <- mean(mydata_filtered$salary_in_usd)
median_salary <- median(mydata_filtered$salary_in_usd)
base2= ggplot(data=mydata_filtered)
deliverable2 <- base2 + 
  geom_histogram(aes(x = salary_in_usd)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE), breaks = seq(0, 400000, by = 50000)) +
  scale_y_continuous(breaks = seq(0, 1600, by = 200)) +
  labs(title="What do data scientists get paid?",
    subtitle = "Salaries in USD for Data Science Jobs 2020-2024",       
    x = "Salary in USD",
    y = "Number of Jobs",
    caption = "Source: “The Global AI, ML, Data Science Salary Index for 2024.” AI-Jobs, 2024, ai-jobs.net/salaries/2024/"
  ) +
  annotate("text", x = 350000, y = 1400, label = paste("Mean: $", format(mean_salary)), color = "black", size = 4) +
  annotate("text", x = 350000, y = 1300, label = paste("Median: $", format(median_salary)), color = "black", size = 4)


deliverable2


# save del2 ----------------------------------------------------------
saveRDS(deliverable2, file = "deliverable2.rds")


# deliverable 3 ----------------------------------------------------------

library(dplyr)
library(ggplot2)

#transforming data so that percents add to 100

mydata2 <- mydata %>%
  group_by(work_year, remote_ratio) %>%
  summarise(Count = n()) %>%
  group_by(work_year) %>%
  mutate(percent = Count / sum(Count))

#plot

deliverable3 <- ggplot(mydata2, aes(x = work_year, y = percent, fill = remote_ratio)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "How has remote work offering changed?",
       subtitle = "Location of Jobs by Year, 2020-2024",
       x="Year of Job Posting",
       y="Share of Jobs",
       fill="Work Location",
       caption = "Source: “The Global AI, ML, Data Science Salary Index for 2024.” AI-Jobs, 2024, ai-jobs.net/salaries/2024/")+
  theme(plot.caption = element_text(size = 6))

print(deliverable3)

# save del3 ----------------------------------------------------------
saveRDS(deliverable3, file = "deliverable3.rds")



# deliverable 4  ----------------------------------------------------------

#importing datasets and geojson

library(sf)
library(ggplot2)
world_map=sf::read_sf("world.geojson")
head(world_map)
head(mydata)

# merge data into map ----------------------------------------------------------
mydataWorld=aggregate(data=mydata,salary_in_usd~employee_residence,FUN = mean)
myMapWorldDataJob=merge(world_map,mydataWorld,by.x="ISO_A2","employee_residence")

head(myMapWorldDataJob)

#creating breaks and labels for my map intervals

breaks <- c(0, 50000, 75000, 100000, 125000, 150000, 200000, Inf)

labels <- c("Under 50k USD", "50k to 75k USD", "75k to 100k USD", "100k to 125k USD", "125k to 150k USD", "150k to 200k USD", "Over 200k USD")

# Cut the data into same intervals 
myMapWorldDataJob$SalaryRange <- cut(myMapWorldDataJob$salary_in_usd, breaks = breaks, labels = labels, right = FALSE)

# Prepare plot
base <- ggplot(myMapWorldDataJob)

# Create choropleth map with custom designated breaks
deliverable4 <- base + 
  geom_sf(aes(fill = SalaryRange)) +
  scale_fill_manual(
    values = c("Under 50k USD" = "red", "50k to 75k USD" = "orange", 
               "75k to 100k USD" = "yellow", "100k to 125k USD" = "green", 
               "125k to 150k USD" = "blue", "150k to 200k USD" = "purple", "Over 200k USD" = "lavender" )) +
  labs(title="What is the average data science job salary range around the world?",
     subtitle = "Average Data Science Job Salary by Range on World Map (USD)",
     caption = "Source: “The Global AI, ML, Data Science Salary Index for 2024.” AI-Jobs, 2024, ai-jobs.net/salaries/2024/",
     fill= "Salary Range in USD")+
  theme(plot.caption = element_text(size = 6), plot.title = element_text(size = 14))

# Display the plot
deliverable4


# save del4Draft ----------------------------------------------------------
saveRDS(deliverable4, file = "deliverable4.rds")

