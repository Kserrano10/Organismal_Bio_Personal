### my comments preceded by `CPK:`
## Question 1.

my_vec <- runif(10, min = 0, max = 10)
my_vec_6 <- my_vec[6]
print(my_vec_6)

divided <- my_vec / 2
my_vec_mean <- mean(my_vec)

## Question 2.

virginica_data <- iris[iris$Species == "virginica", ]
min_petal_length <-  min(virginica_data$Petal.Length)
print(min_petal_length)

species <-  levels(iris$Species)
for (s in species) {
  print(mean(iris$Petal.Length[iris$Species == s]))
}




###CPK: Spot on, Kayla. Well done!



#Total points: 10

