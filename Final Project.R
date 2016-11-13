# Jelani Denis, Andrew Hartnett, Mark Goldstein
# Final Project 

contributions <- read.csv("contributions.csv")

# get the candidates, remove illegitimate entries
candidates <- na.omit(unique(contributions$cand_nm))
candidates <- candidates[candidates != ""]
candidates

# list ideology scores according to placement in candidates vector
ideologies = c(6.8, 2.2, -3.2, 8.0, 6.0, 5.8, 4.8, -6.8, 8.2, -8.5, 4.8, -4.5, 7.2, 2.0, 4.0, 6.5, 4.5, 6.8, 2.2, 5.2, NA)
names(ideologies) <- candidates

#sort candidate vector according to ideology score
ideologies <- sort(ideologies, decreasing = TRUE, na.last = TRUE)
candidates <- names(ideologies)
plot(ideologies)


# medical variable where for doctors medical==1 and for non doctors medical==0
contributions$medical <- ifelse(contributions$contbr_occupation == "PHYSICIAN"
                              | contributions$contbr_occupation == "OPTHALMOLOGIST"
                              | contributions$contbr_occupation == "DOCTOR"
                              | contributions$contbr_occupation == "RADIOLOGIST"
                              | contributions$contbr_occupation == "MEDICAL DOCTOR"
                              | contributions$contbr_occupation == "ANESTHESIOLOGIST"
                              | contributions$contbr_occupation == "M.D."
                              | contributions$contbr_occupation == "SURGEON"
                              | contributions$contbr_occupation == "SEMI-RETIRED PHYSICIAN"
                              | contributions$contbr_occupation == "PEDIATRICIAN"
                              | contributions$contbr_occupation == "PHYSICIAN/SURGEON"
                              | contributions$contbr_occupation == "OPTOMETRIST"
                              | contributions$contbr_occupation == "PSYCHIATRIST"
                              | contributions$contbr_occupation == "DIAGNOSTIC RADIOLOGY"
                              | contributions$contbr_occupation == "GASTROENTEROLOGIST"
                              | contributions$contbr_occupation == "INTERNAL MEDICINE SPECIALIST"
                              | contributions$contbr_occupation == "NEUROSURGEON"
                              | contributions$contbr_occupation == "MD"
                              | contributions$contbr_occupation == "ORTHOPEDIC SURGEON"
                                ,1 ,0)

# subset for only positive contributions
contributions <- contributions[contributions$contb_receipt_amt > 0, ]

# total contribution amount per candidate
x <- length(candidates)
sum.contributions.norm <- rep(0, x)
sum.contributions.med <- rep(0, x)

#get contribution amount according to candidate
for (i in 1:x) 
{
  money.norm <- contributions$contb_receipt_amt[contributions$cand_nm == candidates[i] &
                                                  contributions$medical != 1]
  money.med <- contributions$contb_receipt_amt[contributions$cand_nm == candidates[i] &
                                                 contributions$medical == 1]
  money.norm <- na.omit(money.norm)  
  money.med <- na.omit(money.med)
  
  sum.contributions.norm[i] <- sum(money.norm)
  sum.contributions.med[i] <- sum(money.med)
  
}
names(sum.contributions.norm) <- candidates
names(sum.contributions.med) <- candidates

# number of contributions per candidate
num.contributions.norm <- rep(0, x)
num.contributions.med <- rep(0, x)
for (i in 1:x) 
{
  candid.norm <- contributions$contb_receipt_amt[contributions$cand_nm == candidates[i] & 
                                                   contributions$medical != 1]
  
  candid.med <- contributions$contb_receipt_amt[contributions$cand_nm == candidates[i] & 
                                                  contributions$medical == 1]
  num.contributions.norm[i] <- length(candid.norm)
  num.contributions.med[i] <- length(candid.med)
}

names(num.contributions.norm) <- candidates
names(num.contributions.med) <- candidates

# add appropriate polity score to each contribution
cand_nm <- candidates
combined <- data.frame(cand_nm, ideologies)
new <- merge(contributions, combined, by = "cand_nm", sort = TRUE)
new <- new[ , 1:10]
View(new)

# remove any duplicate contributors in order to calculate number of unique contributors per candidate
new.unique <- new[!duplicated(new$contbr_nm), ]
View(new.unique)

# number of unique contributors per candidate
contributors.med <- rep(NA, x)
contributors.norm <- rep(NA, x)
for(i in 1:x)
{
  contributors.med[i] = nrow(new.unique[new.unique$cand_nm == candidates[i] & new.unique$medical == 1 , ])
  contributors.norm[i] = nrow(new.unique[new.unique$cand_nm == candidates[i] & new.unique$medical == 0 , ])
}
names(contributors.med) <- candidates
names(contributors.norm) <- candidates

# Combine candidate names, ideologies, and contributions into one data frame
cand_nm <- candidates
combined <- data.frame(cand_nm, ideologies, sum.contributions.med, sum.contributions.norm, 
                       num.contributions.med, num.contributions.norm, contributors.med,
                       contributors.norm)
View(combined) 

##### Bar-Plots for Write-Up #####

# change margins for horizontal bar graphs
op <- par(mar = c(4,11,4,2) + 0.1)

# change colors for Ben Carson
colors <- rep("gray80", length(candidates))
colors[8] = "dodgerblue3"

#Percentage of All Non-Medical Contributors
barplot(combined$contributors.norm / sum(combined$contributors.norm), 
        names.arg = candidates, 
        horiz = TRUE, las = 2, 
        main = "Percentage Share of All\n Non-Medical Contributors",
        xlab = "Percentage of Non-Medical Contributors",
        col = colors)

#Percentage of All Medical Contributors
barplot(combined$contributors.med / sum(combined$contributors.med), 
        names.arg = candidates, 
        horiz = TRUE, las = 2, 
        main= "Percentage Share of All\n Medical Contributors",
        col = colors)

#Difference in Contributor Percentage Share
barplot(combined$contributors.med / sum(combined$contributors.med) - 
          combined$contributors.norm / sum(combined$contributors.norm), 
        names.arg = candidates, 
        horiz = TRUE, las = 2, 
        main = "Difference in Contributor Percentage Share\n Medical - NonMedical",
        xlab = "Difference in %ge (more Medical ???)",
        col = colors)

abline(v=0, lwd=1)

#reset margins in preparation for histograms
par(op)
  
##### QQ Plot for Write-Up #####

#QQPLOT to compare Medical versus Non Medical Ideology Preference
qqplot(new.unique$ideologies[new.unique$medical == 0], 
       new.unique$ideologies[new.unique$medical == 1],
       main = "Medical versus Non Medical\n Ideology Distribution",
       xlab = "Non Medical Contributors",
       ylab = "Medical Contributors")
abline(0,1)
abline(v = 5.8, col = "red", lty="dashed")
abline(h = 5.8, col="red", lty="dashed")

text("Carson Ideology", x = -2, y = 6.3, col="red")
text("Ideology", x = 0, y = -11, xpd = TRUE, col = "gray36")
text("??? Liberal", x = -5, y = -11, xpd = TRUE, col = "royalblue1")
text("Conservative ???", x = 5, y = -11, xpd = TRUE, col = "indianred1")
text("Ideology", y = 0, x = -11, srt = 90, xpd = TRUE, col = "gray36")
text("??? Liberal", y = -5, x = -11, srt = 90, xpd = TRUE, col = "royalblue1")
text("Conservative ???", y = 5, x = -11, srt = 90, xpd = TRUE, col = "indianred1")
##### Hypothesis Test  ######

#split into four groups based on whether donors contributed to Carson and whether they are doctors
total.med <- nrow(new.unique[new.unique$medical == 1, ])
carson.med <- nrow(new.unique[new.unique$medical == 1 & new.unique$cand_nm == "Carson, Benjamin S.", ])
total.non <- nrow(new.unique[new.unique$medical == 0, ])
carson.non <- nrow(new.unique[new.unique$medical == 0 & new.unique$cand_nm == "Carson, Benjamin S.", ])

#proportion of unique medical contributors that donated to Carson
carson.med/total.med
#0.1987193

#proportion of unique non medical contributors that donated to Carson
carson.non/total.non
#0.1495674

#prop.test to validate p-value
success <- c(carson.med, carson.non)
trials <- c(total.med, total.non)
prop.test(success, trials, alternative = "two.sided")
#X-squared = 85.3892
#p-value < 2.2e-16
#95 percent confidence interval: ( 0.03748202 0.06082191)

