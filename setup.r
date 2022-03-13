#GSoC 2022
#GeomScale
#Author: Agilan S
#Date: 13/03/2022
#Name: Exclude Lpsolve from R Interface of VolEsti Tests

#TEST 1

library(volesti)

polytope <- gen_simplex(5, 'H')
#A simplex is the simplest possible polytope in a given dimesnsion

print(volume(polytope))

#----------------------------------------------------------------------------#

#TEST 2

#CVXR is an R package that can be used to compute the maximum ball of a polytope.
#https://cvxr.rbind.io/cvxr_examples/cvxr_2d_ball/
library(CVXR)
library(ggplot2)
library(ggforce)

x1 <- matrix(c(2, 1))
x2 <- matrix(c(2, -1))
x3 <- matrix(c(-1, 2))
x4 <- matrix(c(-1, -2))
b <- c(1, 1, 1, 1)

r <- Variable(name = "radius")
c <- Variable(2, name = "center")
obj <- Maximize(r)
constraints <- list(
  t(x1) %*% c + p_norm(x1, 2) * r <= b[1],
  # t(<matrix>) is the transpose of <matrix>
  # p_norm(<matrix>, 2) gives the largest singular value of the matrix
  t(x2) %*% c + p_norm(x1, 2) * r <= b[2],
  t(x3) %*% c + p_norm(x1, 2) * r <= b[3],
  t(x4) %*% c + p_norm(x1, 2) * r <= b[4]
)
P <- Problem(obj, constraints)
result <- solve(P)
radius <- result$getValue(r)
center <- result$getValue(c)
cat(sprintf("The radius is %0.5f for an area %0.5f\n", radius, pi * radius ^ 2))

ggplot() + 
  geom_abline(slope = -x1[1] / x1[2], intercept = b[1] / x1[2]) + 
  geom_abline(slope = -x2[1] / x2[2], intercept = b[2] / x2[2]) + 
  geom_abline(slope = -x3[1] / x3[2], intercept = b[3] / x3[2]) + 
  geom_abline(slope = -x4[1] / x4[2], intercept = b[4] / x4[2]) + 
  geom_circle(mapping = aes(x0 = center[1], y0 = center[2], r = radius), color = "blue") + 
  geom_point(mapping = aes(x = center[1], y = center[2]), color = "red", size = 2) + 
  geom_line(mapping = aes(x = c(center[1], center[1] - radius), y = c(center[2], center[2])),
            arrow = arrow(length = unit(0.03, "npc"), ends = "first", type = "closed"),
            color = "brown") + 
  annotate("text", x = -0.2, y = 0.04, label = sprintf("r = %0.5f", radius)) + 
  labs(x = "X", y = "Y") + 
  xlim(-1, 1) + ylim(-1, 1)

#----------------------------------------------------------------------------#

#TEST 3

library(nloptr)