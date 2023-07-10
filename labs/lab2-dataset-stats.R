super_data <- read.csv("data/movies_updated.csv")

## I'll be using the score to analyze
score <- super_data$score

mean <- mean(score)
median <- median(score)
range <- max(score) - min(score)

variance <- var(score)
standardDeviation <- sd(score)
interquartileRange <- IQR(score)

upperCutoff <- quantile(score, 0.75) + interquartileRange * 1.5
lowerCutoff <- quantile(score, 0.25) - interquartileRange * 1.5

score[score >= upperCutoff] # yes, there are outliers
score[score <= lowerCutoff]

score = score[score < upperCutoff & score > lowerCutoff]
