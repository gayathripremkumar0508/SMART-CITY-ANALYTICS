# Step 1: Load the complaint dataset

complaints <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE
)

# Step 2: Display the first six rows

head(complaints)

# Step 3: Display the column names

names(complaints)

# Step 4: Display the dataset dimensions

dim(complaints)

# Step 5: Check the structure of the dataset

str(complaints) 

# Step 6: Count missing values in each column

colSums(is.na(complaints)) 

# Step 7: Check the range of latitude and longitude

summary(complaints$latitude)
summary(complaints$longitude)  

# Step 8: Count complaints in each category

sort(
  table(complaints$category_title),
  decreasing = TRUE
)  

# Step 9: Count complaints by status

table(complaints$complaint_status_title)  

# Step 10: View sample complaint dates

head(complaints$created_at, 10)
tail(complaints$created_at, 10)  

# Step 11: Convert created_at into proper date-time format

complaints$date_time <- as.POSIXct(
  complaints$created_at,
  format = "%m-%d-%Y %H:%M",
  tz = "Asia/Kolkata"
)

# Try parsing dates that were not converted
missing_dates <- is.na(complaints$date_time)

complaints$date_time[missing_dates] <- as.POSIXct(
  complaints$created_at[missing_dates],
  format = "%m/%d/%Y %H:%M",
  tz = "Asia/Kolkata"
)

# Check the conversion
sum(is.na(complaints$date_time))
head(complaints[, c("created_at", "date_time")])  

# Step 12: Keep rows with valid coordinates

clean_data <- complaints[
  !is.na(complaints$latitude) &
    !is.na(complaints$longitude) &
    is.finite(complaints$latitude) &
    is.finite(complaints$longitude) &
    complaints$latitude >= 12.5 &
    complaints$latitude <= 13.5 &
    complaints$longitude >= 77.0 &
    complaints$longitude <= 78.0,
]

# Check the new dataset size
dim(clean_data)


# Step 13: Replace text NULL with NA in selected columns

clean_data$category_title[
  trimws(clean_data$category_title) == "NULL"
] <- NA

clean_data$civic_agency_title[
  trimws(clean_data$civic_agency_title) == "NULL"
] <- NA

clean_data$complaint_status_title[
  trimws(clean_data$complaint_status_title) == "NULL"
] <- NA

# Check category and status values again
sum(is.na(clean_data$category_title))
table(clean_data$complaint_status_title, useNA = "ifany")  


# Step 14: Final data checks

dim(clean_data)

summary(clean_data$latitude)
summary(clean_data$longitude)

sum(is.na(clean_data$date_time))

head(clean_data[, c(
  "date_time",
  "latitude",
  "longitude",
  "category_title",
  "ward_title",
  "complaint_status_title"
)]) 

# Step 15: Prepare coordinates for K-Means

coordinates <- clean_data[, c("latitude", "longitude")]

# Check the coordinate data
head(coordinates)

# Check for missing values
sum(is.na(coordinates)) 

# Step 16: Elbow method

set.seed(123)

wss <- numeric(10)

for (k in 1:10) {
  model <- kmeans(
    coordinates,
    centers = k,
    nstart = 10
  )
  
  wss[k] <- model$tot.withinss
}

plot(
  1:10,
  wss,
  type = "b",
  pch = 19,
  xlab = "Number of Clusters (K)",
  ylab = "Within-Cluster Sum of Squares",
  main = "Elbow Method for K-Means"
)  

windows(width = 8, height = 6)

plot(
  1:10,
  wss,
  type = "b",
  pch = 19,
  xlab = "Number of Clusters (K)",
  ylab = "Within-Cluster Sum of Squares",
  main = "Elbow Method for K-Means"
)  


# Standardize the coordinates

scaled_coordinates <- scale(coordinates)

# Recalculate the elbow values

set.seed(123)

wss <- numeric(10)

for (k in 1:10) {
  model <- kmeans(
    scaled_coordinates,
    centers = k,
    nstart = 5,
    iter.max = 100
  )
  
  wss[k] <- model$tot.withinss
}  

plot(
  1:10,
  wss,
  type = "b",
  pch = 19,
  xlab = "Number of Clusters (K)",
  ylab = "Within-Cluster Sum of Squares",
  main = "Elbow Method for K-Means"
)   


# Step 17: Build the final K-Means model

set.seed(123)

kmeans_model <- kmeans(
  scaled_coordinates,
  centers = 4,
  nstart = 25,
  iter.max = 100
)

# Add the cluster number to each complaint
clean_data$cluster <- factor(kmeans_model$cluster)

# Display model results
print(kmeans_model)

# Check the number of complaints in each cluster
table(clean_data$cluster)    

# Step 18: Visualize geographical complaint clusters

library(ggplot2)

ggplot(
  clean_data,
  aes(
    x = longitude,
    y = latitude,
    color = cluster
  )
) +
  geom_point(alpha = 0.6, size = 1.5) +
  labs(
    title = "Bengaluru Civic Complaint Hotspots",
    subtitle = "K-Means Clustering with Four Clusters",
    x = "Longitude",
    y = "Latitude",
    color = "Cluster"
  ) +
  theme_minimal()  


# Step 19: Summarize complaints by cluster and category

category_summary <- as.data.frame(
  table(
    clean_data$cluster,
    clean_data$category_title,
    useNA = "no"
  )
)

names(category_summary) <- c(
  "Cluster",
  "Category",
  "Complaint_Count"
)

# Sort by cluster and complaint count
category_summary <- category_summary[
  order(
    category_summary$Cluster,
    -category_summary$Complaint_Count
  ),
]

# Display the results
head(category_summary, 15)



# Step 20: Top 5 categories in every cluster

top_categories <- do.call(
  rbind,
  lapply(
    split(category_summary, category_summary$Cluster),
    function(x) {
      head(
        x[order(-x$Complaint_Count), ],
        5
      )
    }
  )
)

print(top_categories, row.names = FALSE)


# Step 21: Visualize complaint counts

library(ggplot2)

ggplot(
  clean_data,
  aes(x = cluster, fill = cluster)
) +
  geom_bar() +
  labs(
    title = "Number of Civic Complaints by Cluster",
    x = "Geographical Cluster",
    y = "Number of Complaints"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Step 22: Find the top 5 complaint categories in each cluster

top_categories <- do.call(
  rbind,
  lapply(
    split(category_summary, category_summary$Cluster),
    function(x) {
      head(
        x[order(-x$Complaint_Count), ],
        5
      )
    }
  )
)

print(top_categories, row.names = FALSE)



# Step 23: Top 10 complaint categories across Bengaluru

library(dplyr)
library(ggplot2)

top10_categories <- clean_data %>%
  filter(!is.na(category_title)) %>%
  count(category_title, sort = TRUE) %>%
  slice_head(n = 10)

ggplot(
  top10_categories,
  aes(
    x = reorder(category_title, n),
    y = n
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Civic Complaint Categories",
    x = "Complaint Category",
    y = "Number of Complaints"
  ) +
  theme_minimal()


# Step 24: Extract the month from the complaint date

library(dplyr)
library(ggplot2)

clean_data$month <- format(
  clean_data$date_time,
  "%Y-%m"
)

# Count complaints per month
monthly_complaints <- clean_data %>%
  count(month, name = "total_complaints") %>%
  arrange(month)

# Display the first few months
head(monthly_complaints, 10)

# Create the monthly trend chart
ggplot(
  monthly_complaints,
  aes(
    x = month,
    y = total_complaints,
    group = 1
  )
) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Monthly Civic Complaint Trends in Bengaluru",
    x = "Month",
    y = "Number of Complaints"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 7
    )
  )


# Step 25: Count complaints by status

status_summary <- clean_data %>%
  filter(!is.na(complaint_status_title)) %>%
  count(
    complaint_status_title,
    sort = TRUE
  )

print(status_summary)

# Create status chart
ggplot(
  status_summary,
  aes(
    x = reorder(complaint_status_title, n),
    y = n
  )
) +
  geom_col(fill = "darkseagreen4") +
  coord_flip() +
  labs(
    title = "Civic Complaint Status Distribution",
    x = "Complaint Status",
    y = "Number of Complaints"
  ) +
  theme_minimal()



# Step 26: Count complaints by ward

ward_summary <- clean_data %>%
  filter(
    !is.na(ward_title),
    trimws(ward_title) != "",
    ward_title != "NULL"
  ) %>%
  count(
    ward_title,
    sort = TRUE,
    name = "total_complaints"
  )

# Display the top 10 wards
top10_wards <- head(ward_summary, 10)

print(top10_wards)

# Create ward ranking chart
ggplot(
  top10_wards,
  aes(
    x = reorder(ward_title, total_complaints),
    y = total_complaints
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Wards by Complaint Count",
    x = "Ward",
    y = "Number of Complaints"
  ) +
  theme_minimal()

install.packages("shiny")

library(shiny)  

# Step 28: Save the processed dataset and model

saveRDS(
  clean_data,
  "clean_data.rds"
)

saveRDS(
  kmeans_model,
  "kmeans_model.rds"
)

write.csv(
  top10_wards,
  "top10_wards.csv",
  row.names = FALSE
)

write.csv(
  top_categories,
  "top_categories.csv",
  row.names = FALSE
)

getwd()