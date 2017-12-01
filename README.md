---
output:
  pdf_document: default
  html_document: default
---
# kombinops

The goal of kombinops is to allow the user to execute basic operations from enumerative combinatorics: 
permutations (with and without repetition), combinations (with and without repetition) and permutations 
of multisets.

## Example

Consider the following task: Each of 6 students randomly with the same probability choose 
one of 4 rooms to come in. What is the probability that there will be no empty rooms left?

From the perspective of combinatorics the solution is given by formula:
$$\frac{\bar{A}_4^6-C_4^3\cdot\bar{A}_3^6+C_4^2\cdot\bar{A}_2^6-C_4^1\cdot\bar{A}_1^6}{\bar{A}_4^6}=
\frac{195}{512}\approx0.38.$$

```r
## Solution with kombinops
res <- (Ank_(4,6)-
       Cnk(4,3)*Ank_(3,6)+
       Cnk(4,2)*Ank_(2,6)-
       Cnk(4,1)*Ank_(1,6))/
       Ank_(4,6)
print(res)
```
