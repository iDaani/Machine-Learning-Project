setwd("./")

library(ggplot2)

# Installing moments library for skewness() function

library(moments)

# Installing robustbase for colMedians() function

library(robustbase)

# Installing GMCM for colSds() function

library(GMCM)

# Installing data.table for histogram  of  feature 
#   values for the groups

library(data.table)

# Installing library for gginference - used to plot t.test values

library(gginference)

# Installing library for ggstatsplot - to plot correlations (linear association)

library(ggstatsplot)

# Installing library for ggstatsplot - this library is needed for ggstatsplot to work

library(ggcorrplot)



# Reading from the CSV file

data <- read.csv("./40280938_features.csv",
         header = TRUE)
data


# 3.1

# nr_pix histogram

nr_pix <- as.numeric(data$nr_pix)
nr_pix
hist(nr_pix)

ggplot(data, aes(x=nr_pix)) + geom_histogram()

# rows_with_1 histogram

rows_with_1 <- as.numeric(data$rows_with_1)
rows_with_1
hist(rows_with_1)
ggplot(data, aes(x=rows_with_1)) + geom_histogram()

# cols_with_1 histogram

cols_with_1 <- as.numeric(data$cols_with_1)
cols_with_1
hist(cols_with_1)
ggplot(data, aes(x=cols_with_1)) + geom_histogram()

# rows_with_3p histogram

rows_with_3p <- as.numeric(data$rows_with_3p)
rows_with_3p
hist(rows_with_3p)
ggplot(data, aes(x=rows_with_3p)) + geom_histogram()


# cols_with_3p histogram

cols_with_3p <- as.numeric(data$cols_with_3p)
cols_with_3p
hist(cols_with_3p)
ggplot(data, aes(x=cols_with_3p)) + geom_histogram()

# aspect_ratio histogram

aspect_ratio <- as.numeric(data$aspect_ratio)
aspect_ratio
hist(aspect_ratio)
ggplot(data, aes(x=aspect_ratio)) + geom_histogram()


# 3.2

nrow(data)


only_letters <- data[1:80, ]

only_non_letters <- data[81:140, ]



# Finding Mean for full set of letters

mean_of_letters <- colMeans(only_letters[, 2:18])

mean_of_letters


# Finding mean for full set of non-letters

mean_of_non_letters <- colMeans(only_non_letters[, 2:18])

mean_of_non_letters


# Finding Median for full set of letters
# Used data.matrix as colMedians only takes vector or matrix as input

median_of_letters <- colMedians(data.matrix(only_letters[, 2:18]))

median_of_letters


# Finding Median for full set of non-letters
# Used data.matrix as colMedians only takes vector or matrix as input

median_of_non_letters <- colMedians(data.matrix(only_non_letters[, 2:18]))

median_of_non_letters


# Finding Standard Deviation for full set of letters
# Used data.matrix as colSds only takes vector or matrix as input

sd_of_letters <- sd(data.matrix(only_letters[, 2:18]))

sd_of_letters


# Finding Standard Deviation for full set of non-letters
# Used data.matrix as colSds only takes vector or matrix as input

sd_of_non_letters <- sd(data.matrix(only_non_letters[, 2:18]))

sd_of_non_letters



# Finding skewness of features (1 to 13) of letters

only_letter_nr_pix_skew = skewness(only_letters$nr_pix)
only_letter_nr_pix_skew


only_letter_rows_with_1_skew = skewness(only_letters$rows_with_1)
only_letter_rows_with_1_skew

only_letter_cols_with_1_skew = skewness(only_letters$cols_with_1)
only_letter_cols_with_1_skew

only_letter_rows_with_3p_skew = skewness(only_letters$rows_with_3p)
only_letter_rows_with_3p_skew

only_letter_cols_with_3p_skew = skewness(only_letters$cols_with_3p)
only_letter_cols_with_3p_skew

only_letter_aspect_ratio_skew = skewness(only_letters$aspect_ratio)
only_letter_aspect_ratio_skew

only_letter_neigh_1_skew = skewness(only_letters$neigh_1)
only_letter_neigh_1_skew

only_letter_no_neigh_above_skew = skewness(only_letters$no_neigh_above)
only_letter_no_neigh_above_skew

only_letter_no_neigh_below_skew = skewness(only_letters$no_neigh_below)
only_letter_no_neigh_below_skew

only_letter_no_neigh_left_skew = skewness(only_letters$no_neigh_left)
only_letter_no_neigh_left_skew

only_letter_no_neigh_right_skew = skewness(only_letters$no_neigh_right)
only_letter_no_neigh_right_skew

only_letter_no_neigh_horiz_skew = skewness(only_letters$no_neigh_horiz)
only_letter_no_neigh_horiz_skew

only_letter_no_neigh_vert_skew = skewness(only_letters$no_neigh_vert)
only_letter_no_neigh_vert_skew

only_letter_custom_skew = skewness(only_letters$custom)
only_letter_custom_skew


# Finding skewness of features (1 to 13) of non-letters

only_non_letter_nr_pix_skew_skew = skewness(only_non_letters$nr_pix)
only_non_letter_nr_pix_skew_skew


only_non_letter_rows_with_1_skew = skewness(only_non_letters$rows_with_1)
only_non_letter_rows_with_1_skew

only_non_letter_cols_with_1_skew = skewness(only_non_letters$cols_with_1)
only_non_letter_cols_with_1_skew

only_non_letter_rows_with_3p_skew = skewness(only_non_letters$rows_with_3p)
only_non_letter_rows_with_3p_skew

only_non_letter_cols_with_3p_skew = skewness(only_non_letters$cols_with_3p)
only_non_letter_cols_with_3p_skew

only_non_letter_aspect_ratio_skew = skewness(only_non_letters$aspect_ratio)
only_non_letter_aspect_ratio_skew

only_non_letter_neigh_1_skew = skewness(only_non_letters$neigh_1)
only_non_letter_neigh_1_skew

only_non_letter_no_neigh_above_skew = skewness(only_non_letters$no_neigh_above)
only_non_letter_no_neigh_above_skew

only_non_letter_no_neigh_below_skew = skewness(only_non_letters$no_neigh_below)
only_non_letter_no_neigh_below_skew

only_non_letter_no_neigh_left_skew = skewness(only_non_letters$no_neigh_left)
only_non_letter_no_neigh_left_skew

only_non_letter_no_neigh_right_skew = skewness(only_non_letters$no_neigh_right)
only_non_letter_no_neigh_right_skew

only_non_letter_no_neigh_horiz_skew = skewness(only_non_letters$no_neigh_horiz)
only_non_letter_no_neigh_horiz_skew

only_non_letter_no_neigh_vert_skew = skewness(only_non_letters$no_neigh_vert)
only_non_letter_no_neigh_vert_skew

only_non_letter_custom_skew = skewness(only_non_letters$custom)
only_non_letter_custom_skew


#
# histogram  of  feature 
# values for the groups

df = data.frame(y = c(data$rows_with_3p, data$no_neigh_left, data$no_neigh_right),
                x = rep(c(0,100), each = 210))
#full hist
fullhist = hist(df$y, breaks = 20, main = "Histogram of feature values of groups - Number of rows with 3 Black pixels, number of rows with no neighbouring black pixel in left, number of rows with no neighbouring black pixel in right") #specify more breaks than probably necessary
#create histograms for the different features using breaks from full histogram
zerohist = with(subset(df, y == data$rows_with_3p), hist(y, breaks = fullhist$breaks))
oneshist = with(subset(df, y == data$no_neigh_left), hist(y, breaks = fullhist$breaks))
morehist = with(subset(df, y == data$no_neigh_right), hist(y, breaks = fullhist$breaks))
#combine the hists
combhist = fullhist
combhist$counts = zerohist$counts - oneshist$counts - morehist$counts
plot(combhist, main="Histogram  of  feature 
values for the groups", xlab= "rows_with_3p, no_neigh_left, no_neigh_right")



# 3.3



# Calculating p-values values for every feature other than:

t.test(only_letters$nr_pix, only_non_letters$nr_pix)

t.test(only_letters$rows_with_1, only_non_letters$rows_with_1)

t.test(only_letters$cols_with_1, only_non_letters$cols_with_1)

t.test(only_letters$rows_with_3p, only_non_letters$rows_with_3p)

t.test(only_letters$cols_with_3p, only_non_letters$cols_with_3p)

t.test(only_letters$aspect_ratio, only_non_letters$aspect_ratio)

t.test(only_letters$neigh_1, only_non_letters$neigh_1)

t.test(only_letters$no_neigh_above, only_non_letters$no_neigh_above)

t.test(only_letters$no_neigh_below, only_non_letters$no_neigh_below)

t.test(only_letters$no_neigh_left, only_non_letters$no_neigh_left)

t.test(only_letters$no_neigh_right, only_non_letters$no_neigh_right)

t.test(only_letters$no_neigh_horiz, only_non_letters$no_neigh_horiz)

t.test(only_letters$no_neigh_vert, only_non_letters$no_neigh_vert)

t.test(only_letters$custom, only_non_letters$custom)




# Performing visualisations to show difference


# Plotting cols_with_3p t-test

ggttest(t.test(only_letters$cols_with_3p, only_non_letters$cols_with_3p))

# Plotting neigh_1 t-test

ggttest(t.test(only_letters$neigh_1, only_non_letters$neigh_1))

# Plotting no_neigh_vert t-test

ggttest(t.test(only_letters$no_neigh_vert, only_non_letters$no_neigh_vert))

# Plotting custom t-test

ggttest(t.test(only_letters$custom, only_non_letters$custom))


# 3.4


# Calculating degree of linear association between features 

cor(data[, 3:15])

correlation_data <- data

ggstatsplot::ggcorrmat(
  data = correlation_data[, 3:15],
  type = "parametric",
  colors = c("darkred", "white", "steelblue")
)


# Plotting degree of linear association
