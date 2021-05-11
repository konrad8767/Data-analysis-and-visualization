Zadanie 1

Zamienić poniższe wykresy generowane za pomocą  pakietu graphics na wykresy generowane pakietem lattice.

# Program 1
par(mai = c(1, 1, 1, 1), omi = c(0, 0, 0, 0))
xx <- c(9.20, 6.00, 6.00, 11.25, 11.00, 7.25, 9.7, 13.25, 14.00, 8.00)
hist(xx, breaks = c(6, 8, 10, 12, 14), right = F)

#Program 2

par(mai = c(1, 1, 1, 1), omi = c(0, 0, 0, 0))
  set.seed(591)
  xx1 <- rnorm(20, mean = 3, sd = 3.6)
  xx2 <- rpois(40, lambda = 3.5)
  xx3 <- rchisq(31, df = 5, ncp = 0)
  box1 <- boxplot(xx1, xx2, xx3, names = c("Group-1", "Group-2",
   "Group-3"), cex = 0.7)

Zadanie 2 

Zmienić wizualizację do zadania realizowanego w "Visualization of multivariate data" na wykorzystującą wykresy z pakietu lattice.