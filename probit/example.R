set.seed(1234)
match.it <- matchit(Group ~ Age + Sex, data = mydata, method="nearest", ratio=1)
a <- summary(match.it)

# Table 2: Sample sizes
kable(a$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Sample sizes')

# Table 3: Summary of balance for matched data
kable(a$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

plot(match.it, type = 'jitter', interactive = FALSE)

# Saving the matched samples
df.match <- match.data(match.it)[1:ncol(mydata)]
rm(df.patients, df.population)

#  check the differences in  both samples are still significant.
pacman::p_load(tableone)
table4 <- CreateTableOne(vars = c('Age', 'Sex', 'Distress'), 
                         data = df.match, 
                         factorVars = 'Sex', 
                         strata = 'Sample')
table4 <- print(table4, 
                printToggle = FALSE, 
                noSpaces = TRUE)
kable(table4[,1:3],  
      align = 'c', 
      caption = 'Table 4: Comparison of matched samples')

pacman::p_load(knitr, wakefield, MatchIt, tableone, captioner)
