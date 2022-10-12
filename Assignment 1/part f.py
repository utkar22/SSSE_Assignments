import matplotlib.pyplot as plt
import math

R = list(range(1,101))
H_R = []

lam = 100 #plugging in

for r in R:
    curr = 1 - math.log(lam) + math.log(1-math.exp(-1*lam*r)) - (lam*r*math.exp(-1*lam*r))/(1-math.exp(-1*lam*r))
    H_R.append(curr)

plt.plot(R,H_R)
plt.xlabel("R")
plt.ylabel("H_R")
plt.title(f"Lambda = {lam}")
plt.show()
