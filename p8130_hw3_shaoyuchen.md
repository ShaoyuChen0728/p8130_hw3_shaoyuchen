sc5351_hw3
================
Shaoyu Chen
2023-10-29

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
df = birthwt
```

\##Problem 1

Some medical professionals claim that the average weight of American
women is 171 pounds. The column lwt holds the mother’s weight (in
pounds) at last menstrual period, i.e. her pre-pregnancy weight. Use
this column for the following questions.

Problem 1a

Construct a 95% confidence interval of true mean weight of American
women.

n = 189

$\bar{X}$ (sample mean) = 129.8148148

$s$ (sample standard deviation) = 30.5793804

``` r
n = length(pull(df, lwt))
x = mean(pull(df, lwt))
s = sd(pull(df, lwt))
```

``` r
lower_lwt = x - qt(0.975, df = n-1)*(s/sqrt(n))
upper_lwt = x + qt(0.975, df = n-1)*(s/sqrt(n))
```

95% confidence interval is (125.43, 134.2.

Problem 1b

Interpret the confidence interval.

We are 95% confident that the true population mean of women’s weight
lies between (125.43, 134.2.

Problem 1c

Comment on the validity of the statement above (“Some medical
professionals claim that the average weight of American women is 171
pounds”). In other words, what can we say about this statement given our
confidence interval from part a?

This statement is not valid, because the average weight of American
women is 171 pounds is not included in the 95% confidence interval.

\##Problem 2

In this data set, we have a variable (smoke) indicating the smoking
status of the mothers during pregnancy. Some doctors believe that
smoking status is related to weight. Using the columns smoke and lwt,
test this claim. (Note: a value of 1 indicates the mother is in the
“smoking” group.)

Problem 2a

1)  Test for the equality of variances between the two groups. (Use a 5%
    significance level.)

Test for Equality of Variances

Sample 1: $s_1, ~ n_1, ~ \bar{x}_1$

Sample 2: $s_2, ~ n_2, ~ \bar{x}_2$

Testing the hypotheses:
$$  H_0 : \sigma_1^2 = \sigma_2^2 \quad vs \quad H_1 : \sigma_1^2 \ne \sigma_2^2$$

With significance level α pre-specified, compute the test statistic:
$$ F = {\frac{s_1^2}{s_2^2}} \sim F_{n_1-1, n_2-1},under H_0$$

``` r
var_smoke = 
  birthwt |> 
  filter(smoke == 1) |> 
  pull(lwt) |>
  var()

var_nonsmoke = 
  birthwt |> 
  filter(smoke == 0) |> 
  pull(lwt) |>
  var()

# calculate F statistic
F_test = var_smoke/var_nonsmoke

n_smoke = 
  birthwt |> 
  filter(smoke == 1) |> 
  nrow()
  
n_nonsmoke = 
  birthwt |> 
  filter(smoke == 0) |> 
  nrow()

# calculate critical value  
F_crit = qf(0.975, df1 = 73, df2 = 114)

# decision
ifelse(F_test > F_crit, "reject", "fail to reject")
```

    ## [1] "fail to reject"

So, it fail to reject

Problem 2b

Given your answer from part a, what kind of hypothesis test will you
perform?

I will perform Two-Sample Independent t-test.

Problem 2c

Conduct your chosen hypothesis test from part b at the 10% significance
level. What is your decision regarding the null? Interpret this result
in the context of the problem.

Assume the hypotheses:
$$H_0 : \mu_1 = \mu_2 \quad vs \quad H_1 : \mu_1 \ne \mu_2$$

With significance level α pre-specified, compute the test statistic:
$$t = {\frac{\overline{X_1}-\overline{X_2}}{s\sqrt{\frac{1}{n_1}+\frac{1}{n_2}}}} \sim t_{n_1+n_2-2}$$

Where s is called the pooled sample standard deviation

$$s^2 = \frac{(n_1-1)s_1^2+(n_2-1)s_2^2}{n_1+n_2-2}$$

``` r
# calculate mean X1,X2
mean_nonsmoke = 
  birthwt |> 
  filter(smoke == 0) |> 
  pull(lwt) |>
  mean()

mean_smoke = 
  birthwt |> 
  filter(smoke == 1) |> 
  pull(lwt) |>
  mean()

# calculate s1,s2
sd_nonsmoke = 
  birthwt |> 
  filter(smoke == 0) |> 
  pull(lwt) |>
  sd()

sd_smoke = 
  birthwt |> 
  filter(smoke == 1) |> 
  pull(lwt) |>
  sd()

# calculate s
s_pool = sqrt(((n_nonsmoke-1)*sd_nonsmoke^2+(n_smoke-1)*sd_smoke^2)/(n_nonsmoke + n_smoke -2))

# calculate T statistic
T_test = (mean_nonsmoke - mean_smoke)/(s_pool*sqrt((1/n_nonsmoke)+(1/n_smoke)))
T_test
```

    ## [1] 0.6047303

``` r
# calculate critical value  
T_crit = qt(0.95, n_smoke + n_nonsmoke - 2)
T_crit
```

    ## [1] 1.653043

``` r
# decision
ifelse(T_test > T_crit, "reject", "fail to reject")
```

    ## [1] "fail to reject"

Based on this result, it fail to reject the null hypothesis, since
T_test = 0.6047303 \< T_crit = 1.653043.

\##Problem 3

According to the CDC, approximately 20% of pregnant American women
suffer from hypertension. Do our data support this claim? (Use column
ht - a value of 1 means the mother is suffering from hypertension.)

Problem 3a

Conduct a 99% confidence interval and interpret the results. What can we
conclude about the CDC’s claim from this interval?

It is Interval Estimation: One-Sample Proportion

a 100 (1 − 𝛼) % confidence interval for one population proportion is
given by:
$$(\hat{p}-z_{1-\alpha/2}\sqrt{\frac{\hat{p}(1-\hat{p})}{n}},\hat{p}+z_{1-\alpha/2}\sqrt{\frac{\hat{p}(1-\hat{p})}{n}})$$

``` r
# calculate number of mother is suffering from hypertension
n_ht = 
  birthwt|>
  nrow()

ht_0 = 
  birthwt|>
  filter(ht == 0) |>
  nrow()

ht_1 =
  birthwt|>
  filter(ht == 1) |>
  nrow()

# calculate the value of proportion
ht_prop = ht_1/(ht_1 + ht_0)

# calculate the N(0,1) with a = 0.01
z_ht = qnorm(0.995, 0, 1)

# calculate the lower and upper of CI
lower_ht = ht_prop - z_ht * sqrt(ht_prop*(1 - ht_prop)/n_ht)
upper_ht = ht_prop + z_ht * sqrt(ht_prop*(1 - ht_prop)/n_ht)
```

99% confidence interval is (0.0178041, 0.10918).

We are 99% confident that the population proportion of pregnant American
women suffer from hypertension lies between (0.0178041, 0.10918). Based
on this result, the claim that CDC has approximately 20% of pregnant
American women suffer from hypertension is not correct, because 0.2 does
not include in 99% confidence interval.

Problem 3b

Conduct a one-sided hypothesis test at the 𝛼 = 0.1 level. In this test,
we want to see if the true proportion is indeed less than the claimed
20%. What can we conclude about the CDC’s claim?

I will use One-Sample Test for Binomial Proportion,tests for
One-Population Proportion, Normal Theory Methods

Assume the hypothesis: $$H_0 : p = p_0 \quad vs \quad H_1 : p < p_0$$

With significance level α pre-specified, compute the test statistic:
$$ z = {\frac{\hat{p} - p_0}{\sqrt{p_0(1 - p_0)/n}}}\sim N(0,1)$$

``` r
# calculate critical value  
Z_crit = qnorm(0.95, 0, 1)
Z_crit
```

    ## [1] 1.644854

``` r
# calculate Z statistic
Z_test = (ht_prop-0.2)/sqrt(0.2*(1-0.2)/n_ht)
Z_test
```

    ## [1] -4.691685

``` r
# decision
ifelse(abs(Z_test) > Z_crit, "reject", "fail to reject")
```

    ## [1] "reject"

Based on this result, \|Z_test\| = 4.691685 \> Z_crit = 1.644854, so it
reject the null hypothesis, which means the true proportion is indeed
less than the claimed 20%. for CDC’s claim, approximately 20% of
pregnant American women suffer from hypertension is correct.

\##Problem 4

Is there a difference between uterine irritability in the group of
pregnant women who smoke vs the group of pregnant women that don’t
smoke? (Use columns ui and smoke.) Conduct a hypothesis test at the 𝛼 =
0.01 level. What can we conclude about the proportions of women with
uterine irritability between the smoking groups?

First I will use Tests for Two-Population Proportions, Normal Theory
Methods:

Assume the hypothesis:
$$H_0 : p_1 = p_2 \quad vs \quad H_1 : p_1 \ne p_2$$

The test statistic is given by:
$$z = {\frac{\hat{p_1} - \hat{p_2}}{\sqrt{\hat{p}\hat{q}(\frac{1}{n_1}+\frac{1}{n_2})}}}\sim N(0,1)$$
and where $$\hat{p} = \frac{n_1\hat{p_1}+n_2\hat{p_2}}{n_1+n_2}$$

``` r
# calculate p1, p2 and p
smoke_ui = 
  birthwt|>
  filter(smoke == 1) |> 
  group_by(ui) |>
  filter(ui == 1) |>
  nrow()

nonsmoke_ui = 
  birthwt|>
  filter(smoke == 0) |> 
  group_by(ui) |>
  filter(ui == 1) |>
  nrow()

# number of nonsmoke = n1, number of smoke = n2
n1 = 
  birthwt |>
  filter(smoke == 0) |>
  nrow()

n2 = 
  birthwt |>
  filter(smoke == 1) |>
  nrow()

# calculate p1 and p2 
p1 = nonsmoke_ui / n1
p2 = smoke_ui / n2

# calculate p
p = (n1*p1+n2*p2) / (n1 + n2)

# calculate Z statistic
Z_test2 = (p1 - p2) / sqrt(p*(1-p)*((1/n1)+(1/n2)))
Z_test2
```

    ## [1] -0.8545449

``` r
# calculate critical value 
Z_crit2 = qnorm(0.995, 0, 1)
Z_crit2
```

    ## [1] 2.575829

``` r
# decision
ifelse(abs(Z_test2) > Z_crit2, "reject", "fail to reject")
```

    ## [1] "fail to reject"

According to this result, At 0.01 significance level, we can’t reject
the null hypothesis, because \|Z_test2\| = 0.8545449 \< Z_crit2 =
2.575829. we can say that there is not a significant difference between
proportions of women with uterine irritability between the smoking
groups.

\##Problem 5

Is race related to birth weight? (Use columns race and bwt.)

``` r
race_df =
  birthwt |> 
  group_by(race) |> 
  count(race)
race_df
```

    ## # A tibble: 3 × 2
    ## # Groups:   race [3]
    ##    race     n
    ##   <int> <int>
    ## 1     1    96
    ## 2     2    26
    ## 3     3    67

Problem 5a:

What test would be most appropriate to answer this question?

ANOVA test.

Problem 5b

What assumptions are we making by using this test? Are all assumptions
met?

1.  There are k populations of interest (k \> 2),
2.  The samples are drawn independently from the underlying populations;
3.  Homoscedasticity: the variances of the k populations are equal;
4.  Normality: the distributions of the error terms are normal (could be
    relaxed when the sample size is large) They are all met!!! k = 3,
    the variances of the k populations are equal, the distributions of
    the error terms are normal, and the samples are drawn independently
    from the underlying populations.

Problem 5c

Conduct the test at the 5% significance level and interpret your
results. Be sure to write the hypotheses you are testing.

Assume the hypothesis:

$$H_0 : \mu_1 = \mu_2 = \mu_3 \quad vs \quad H_1 : at least two means are not equal$$

Compute the test statistic:

$$F = {\frac{BetweenSS/(k-1)}{WithinSS/(n-k)}}\sim F_{k-1,n-k}$$

distribution under $H_0$

Rejection rules: Reject $H_0$: if F \> $F_{k-1,n-k,1-\alpha}$

Fail to reject $H_0$: if F $\leqslant F_{k-1,n-k,1-\alpha}$

Problem 5d

Perform multiple comparisons - which races are significantly different?
Interpret your results

``` r
pairwise.t.test(birthwt$bwt,birthwt$race,p.adj="bonferroni")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  birthwt$bwt and birthwt$race 
    ## 
    ##   1     2    
    ## 2 0.049 -    
    ## 3 0.029 1.000
    ## 
    ## P value adjustment method: bonferroni

Based on this result, race 1 is different.
