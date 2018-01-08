install.packages("recommenderlab")
library(datasets)
# Load required library
library(recommenderlab) # package being evaluated
library(ggplot2) # For plots
# Load the data we are going to work with
data(MovieLense)
MovieLense
# 943 x 1664 rating matrix of class ‘realRatingMatrix’ with 99392 ratings.

# Visualizing a sample of this (grafik 1)
image(sample(MovieLense, 500), main = "Raw ratings")

# Visualizing ratings (grafik 2)
qplot(getRatings(MovieLense), binwidth = 1,
main = "Histogram of ratings", xlab = "Rating")

summary(getRatings(MovieLense)) # Skewed to the right
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.00    3.00    4.00    3.53    4.00    5.00

# How many movies did people rate on average (grafik 3)
qplot(rowCounts(MovieLense), binwidth = 10,
main = "Movies Rated on average",
xlab = "# of users",
ylab = "# of movies rated")

# Seems people get tired of rating movies at a logarithmic pace. But most rate some.
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
# We have a few options
# Let’s check some algorithms against each other
scheme <- evaluationScheme(MovieLense, method = "split", train = .9,
k = 1, given = 10, goodRating = 4)
scheme
algorithms <- list(
"random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
"popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
"user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
method="Cosine",
nn=50, minRating=3))
)

# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
plot(results, annotate = 1:4, legend="topleft")
# See precision / recall
plot(results, "prec/rec", annotate=3)