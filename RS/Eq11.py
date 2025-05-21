# Playing with ways to generate Eq. 11 from the ICN paper.
# No way is very fluid. First one creates nesting, second is long,
# and the third is horribly code-golfed.

print([(i, i*i) for i in range(1,5)])

l = []
for i in range(1,5):
    l.append(i)
    l.append(i*i)
print(l)
    
l = []
[[l.append(j) for n, j in enumerate([i, i*i])] for i in range(1,5)]
print(l)