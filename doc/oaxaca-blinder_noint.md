## OAXACA BLINDER DECOMPOSITION

Sure, let's add a second characteristic, which is race (binary: white, non-white).

Assume we have this new dataset:

| Group | Mean Wage | % Male | Male Wage Contribution | % White | White Wage Contribution |
|-------|-----------|--------|------------------------|---------|-------------------------|
| H     | 60        | 70%    | 30                     | 60%     | 15                      |
| N     | 50        | 50%    | 20                     | 50%     | 10                      |

We would now calculate the endowment, coefficients and interaction effects for each characteristic separately, and then add them up to get the total for each effect.

1. **Calculate the wage differential.**

   ΔW = WH - WN = 60 - 50 = 10

2. **Calculate the 'explained' part.** This is the sum of the explained parts for the male and white characteristics.

   Explained_Male = (Pr_Male_H - Pr_Male_N) * Wage_Male_N = (0.7 - 0.5) * 20 = 4

   Explained_White = (Pr_White_H - Pr_White_N) * Wage_White_N = (0.6 - 0.5) * 10 = 1

   Explained_Total = Explained_Male + Explained_White = 4 + 1 = 5

3. **Calculate the 'coefficients' part.** This is the sum of the coefficients parts for the male and white characteristics.

   Coefficients_Male = Pr_Male_H * (Wage_Male_H - Wage_Male_N) = 0.7 * (30 - 20) = 7

   Coefficients_White = Pr_White_H * (Wage_White_H - Wage_White_N) = 0.6 * (15 - 10) = 3

   Coefficients_Total = Coefficients_Male + Coefficients_White = 7 + 3 = 10

4. **Calculate the 'interaction' effect.** In this case, the interaction effect is zero since we have no overlapping characteristics (e.g., male and white).

So, we would say that 5 units of the wage differential are 'explained' by differences in the Pr of male and white workers (endowments effect), 10 units are due to differences in how being male and being white are rewarded in the two groups (coefficients or structure effect). In this case, there's a negative unexplained part, suggesting that we might have overestimated the effects in our simple model, or that there may be other characteristics which have opposing effects not captured here.

This example introduces multiple characteristics, but it's still a simplification of actual Oaxaca-Blinder decomposition analyses, which would use regression models to accurately estimate the coefficients.