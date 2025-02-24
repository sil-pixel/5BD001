library(biostat3)
library(knitr)
library(survival)
library(collett)


# Load the data
melanoma = biostat3::melanoma |>
  subset(stage=="Localised") |> 
  transform(death_cancer = ifelse( status == "Dead: cancer", 1, 0),
            death_all = ifelse( status == "Dead: cancer" |
                                  status == "Dead: other", 1, 0))

#(a) Plots of survival

# Prepare the data for survival analysis
melanoma$calendar_period <- ifelse(melanoma$year8594 == "Diagnosed 75-84", "1975–1984", "1985–1994")

# Create a survival object
surv_object <- Surv(time = melanoma$surv_mm, event = melanoma$death_cancer)

# Kaplan-Meier estimates by calendar period
km_fit <- survfit(surv_object ~ calendar_period, data = melanoma)

# Plot Kaplan-Meier curves 
plot(km_fit,
     col = c("blue", "red"),
     lwd = 2,
     conf.int = TRUE,
     main = "Kaplan-Meier Survival Curves by Calendar Period",
     xlab = "Time in months",
     ylab = "Survival Probability")
legend("topright", legend = levels(melanoma$calendar_period), 
       col = c("blue", "red"), lwd = 2)