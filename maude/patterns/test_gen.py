import sys
import random
n=int(sys.argv[1])
nums=[0,1,2,3]
print("rew in NUM-LABEL : < I ; ",end="")
for i in range(n):
  print(random.choice(nums),end=" + ")
print(" 0 > .")
