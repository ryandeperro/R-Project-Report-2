library(readr)
yelp_user <- read_csv("Data Science Fundamentals/OA 11.6 - yelp_academic_dataset_user.json (1).csv")
View(yelp_user)

print(colnames(yelp_user))

# Calculates the Pearson correlation matrix, then prints the correlation matrix
correlation_matrix <- cor(yelp_user[, c("cool_votes", "funny_votes", "useful_votes")], method = "pearson")

print(correlation_matrix)


# creates a linear regression of funny votes and useful votes
linear_model <- lm(yelp_user$useful_votes~yelp_user$funny_votes)
print(linear_model)

coefs <- coef(linear_model)
lm_intercept <- coefs[1]
lm_slope <- coefs[2]

cat("Slope:", lm_slope, "Intercept:", lm_intercept)

library(ggplot2)

equation <- paste("y =", (lm_slope), "*x +",(lm_intercept))
print(equation)

ggplot(yelp_user) + geom_point(aes(x=funny_votes, y=useful_votes)) +
  geom_smooth(aes(x=funny_votes, y=useful_votes), method="lm", se=F) +
  labs(x="Funny Votes", y="Useful Votes") + 
  annotate("text", x = max(yelp_user$funny_votes) * 0.3, y = max(yelp_user$useful_votes) * 0.9,label = equation, color = "darkorange", size = 5, hjust = 0)

# The above created a line of best fit and plotted the linear regression and the
# best fit line as well as wrote the equation with it



fans_linear_model <- lm(yelp_user$fans~yelp_user$review_count)
print(linear_model)

coefs <- coef(linear_model)
lm_intercept <- coefs[1]
lm_slope <- coefs[2]

cat("Slope:", lm_slope, "Intercept:", lm_intercept)


ggplot(yelp_user) + geom_point(aes(x=review_count, y=fans)) +
  geom_smooth(aes(x=review_count, y=fans), method="lm", se=F) +
  labs(x="Number of Reviews Written", y="Number of Fans")

# The above created a linear regression showing the relationship between
# Number of Views written and the number of fans one has. There does seem to
# be a relationship because there is a positive slope, however,
# those could be outliers since there is so few people who have written a ton of reviews



cool_linear_model <- lm(yelp_user$fans~yelp_user$cool_votes)
print(linear_model)

coefs <- coef(linear_model)
lm_intercept <- coefs[1]
lm_slope <- coefs[2]

cat("Slope:", lm_slope, "Intercept:", lm_intercept)


ggplot(yelp_user) + geom_point(aes(x=cool_votes, y=fans)) +
  geom_smooth(aes(x=cool_votes, y=fans), method="lm", se=F) +
  labs(x="Number of Cool Votes", y="Number of Fans")


# I made another linear regression that shows that the number of cool votes
# does result in an increase in fans as shown in the line of best fit. 





# A cluster for the review count vs fans

cluster_data <- yelp_user[, c("review_count", "fans")]

scaled_data <- scale(cluster_data)

kmeans_result <- kmeans(scaled_data, centers = 4) 

yelp_user$cluster <- kmeans_result$cluster

print(kmeans_result$centers)

ggplot(yelp_user, aes(x = review_count, y = fans, color = factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering of Review Counts and Fans",
       x = "Review Count",
       y = "Fans",
       color = "Cluster")





# A cluster for the cool votes vs fans

cluster_cool_data <- yelp_user[, c("cool_votes", "fans")]

scaled_data <- scale(cluster_cool_data)

kmeans_result <- kmeans(scaled_data, centers = 2) 

yelp_user$cluster <- kmeans_result$cluster

print(kmeans_result$centers)

ggplot(yelp_user, aes(x = cool_votes, y = fans, color = factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering of Cool Votes and Fans",
       x = "Cool Vote Count",
       y = "Fans",
       color = "Cluster")











