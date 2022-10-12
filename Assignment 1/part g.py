import matplotlib.pyplot as plt
import math

R = list(range(1,101))
H_diff = []

lam = 100 #plugging in

for r in R:
    curr = - math.log(1-math.exp(-1*lam*r)) + (lam*r*math.exp(-1*lam*r))/(1-math.exp(-1*lam*r))
    H_diff.append(curr)

plt.plot(R,H_diff)
plt.xlabel("R")
plt.ylabel("H_diff")
plt.title(f"Lambda = {lam}")
plt.show()
