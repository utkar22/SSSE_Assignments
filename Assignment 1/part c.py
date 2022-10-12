import matplotlib.pyplot as plt
import math

R = list(range(1,101))
R_mean = []

lam = 100 #plugging in

for r in R:
    curr = (1/lam) - (r*math.exp(-1*lam*r))/(1-math.exp(-1*lam*r))
    R_mean.append(curr)

plt.plot(R,R_mean)
plt.xlabel("R")
plt.ylabel("R mean")
plt.show()
