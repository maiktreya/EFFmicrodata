## OAXACA BLINDER DECOMPOSITION

Of course! Let's upgrade the example to include an overlapping characteristic for race and gender. To do this, we'll need an additional piece of information, the mean wage contribution for being a white male.

We'll make an assumption for this value as follows:

| Group | Mean Wage | % Male | Male Wage Contribution | % White | White Wage Contribution | % White Males | White Male Wage Contribution |
|-------|-----------|--------|------------------------|---------|-------------------------|---------------|-----------------------------|
| H     | 60        | 70%    | 30                     | 60%     | 15                      | 50%           | 35                          |
| N     | 50        | 50%    | 20                     | 50%     | 10                      | 40%           | 25                          |

Here's how we'd update the Oaxaca-Blinder decomposition:

1. **Calculate the wage differential.**

   ΔW = WH - WN = 60 - 50 = 10

2. **Calculate the 'endowments' effect.** This includes the contribution of being male, being white, and being a white male.

   Endowments_Male = (Pr_Male_H - Pr_Male_N) * Wage_Male_N = (0.7 - 0.5) * 20 = 4

   Endowments_White = (Pr_White_H - Pr_White_N) * Wage_White_N = (0.6 - 0.5) * 10 = 1

   Endowments_White_Male = (Pr_White_Male_H - Pr_White_Male_N) * Wage_White_Male_N = (0.5 - 0.4) * 25 = 2.5

   Endowments_Total = Endowments_Male + Endowments_White + Endowments_White_Male = 4 + 1 + 2.5 = 7.5

3. **Calculate the 'coefficients' effect.** This includes the additional wage contribution of being male, being white, and being a white male in the homeowner group.

   Coefficients_Male = Pr_Male_H * (Wage_Male_H - Wage_Male_N) = 0.7 * (30 - 20) = 7

   Coefficients_White = Pr_White_H * (Wage_White_H - Wage_White_N) = 0.6 * (15 - 10) = 3

   Coefficients_White_Male = Pr_White_Male_H * (Wage_White_Male_H - Wage_White_Male_N) = 0.5 * (35 - 25) = 5

   Coefficients_Total = Coefficients_Male + Coefficients_White + Coefficients_White_Male = 7 + 3 + 5 = 15

4. **Calculate the 'interaction' effect.** With interaction terms, the interaction effect is calculated as the difference between the total wage differential and the sums of endowments and coefficients effects.

   Interaction = ΔW - Endowments_Total - Coefficients_Total = 10 - 7.5 - 15 = -2.5

In this case, the interaction effect is negative. This suggests that the interaction of being white and male doesn't contribute to the wage differential as much as the individual characteristics of being white and male. This could be due to a number of reasons, such as a smaller than expected number of white males in the homeowners group or a larger number of white males in the non-homeowners group.

Keep in mind, this example is still a simplification. Real world applications would require running a regression to estimate the wage contributions rather than assuming them.

## We need two elements per characteristic: CONTRIBUTION AND PROPORTION

### CONTRIBUTION IS TAKEN AS THE REGRESSION COEFFICIENT
### PROPORTION: reflects the share of people meeting the feature between the groups.

# HOMEOWNERSHIP
r$> svytable(~homeowner, sv_eff) %>% prop.table
homeowner
Homeowner Non-Owner
0.2606545 0.7393455

r$> svytable(~homeowner, sv_eff_w) %>% prop.table
homeowner
Homeowner Non-Owner
0.2693581 0.7306419

r$> svytable(~homeowner, sv_eff_h) %>% prop.table
homeowner
Homeowner Non-Owner
0.1413172 0.8586828

# SEX
r$> svytable(~sex, sv_eff) %>% prop.table
sex
      Man     Women
0.5415986 0.4584014

r$> svytable(~sex, sv_eff_w) %>% prop.table
sex
      Man     Women
0.5304086 0.4695914

r$> svytable(~sex, sv_eff_h) %>% prop.table
sex
      Man     Women
0.6950266 0.3049734