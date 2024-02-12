## Chi-Square testing and ANOVA

### Section 11-1

6.Blood Types A medical researcher wishes to see if hospital patients in a large hospital have thesame blood type distribution as those in the general population. The distribution for the generalpopulation is as follows: type A, 20%; type B, 28%; type O, 36%; and type AB = 16%. He selectsa random sample of 50 patients and finds the following: 12 have type A blood, 8 have type B, 24have type O, and 6 have type AB blood.
At α = 0.10, can it be concluded that the distribution is the same as that of the general population?

8.On-Time Performance by Airlines According to the Bureau of Transportation Statistics, on-time performance by the airlines is described as follows:
 
| Action                        | % of Time |
|-------------------------------|-----------|
| On time                       | 70.8      |
| National Aviation System-delay| 8.2       |
| Aircraft arriving late        | 9.0       |
| Other                         | 12.0      |

Records of 200 randomly selected flights for a major airline company showed that 125 planes
were on time; 40 were delayed because of weather, 10 because of a National Aviation System
delay, and the rest because of arriving late. At α = 0.05, do these results differ from the
government’s statistics?

### Section 11-2

8. Ethnicity and Movie Admissions Are movie admissions related to ethnicity? A 2014 study
indicated the following numbers of admissions (in thousands) for two different years. At the 0.05
level of significance, can it be concluded that movie attendance by year was dependent upon
ethnicity?

|          | Caucasian | Hispanic | African American  | Other |
|----------|-----------|----------|-------------------|-------|
| **2013** | 724       | 335      | 174               | 107   |
| **2014** | 370       | 292      | 152               | 14    |

10. Women in the Military This table lists the numbers of officers and enlisted personnel for
women in the military. At α = 0.05, is there sufficient evidence to conclude that a relationship
exists between rank and branch of the Armed Forces?
| Branch        | Officers | Enlisted |
|---------------|----------|----------|
| Army          | 10,791   | 62,491   |
| Navy          | 7,816    | 42,750   |
| Marine Corps  | 932      | 9,525    |
| Air Force     | 11,819   | 54,344   |

### Section 12-1

8.Sodium Contents of Foods The amount of sodium (in milligrams) in one serving for a randomsample of three different kinds of foods is listed. At the 0.05 level of significance, is theresufficient evidence to conclude that a difference in mean sodium amounts exists amongcondiments, cereals, and desserts?

| Condiments | Cereals | Desserts |
|------------|---------|----------|
| 270        | 260     | 100      |
| 130        | 220     | 180      |
| 230        | 290     | 250      |
| 180        | 290     | 250      |
| 80         | 200     | 300      |
| 70         | 320     | 360      |
| 200        | 140     | 300      |
|            |         | 160      |

12.Per-Pupil Expenditures The expenditures (in dollars) per pupil for states in three sections ofthe country are listed. Using α = 0.05, can you conclude that there is a difference in means?

| Region          | Data 1 | Data 2 | Data 3 | Data 4 | Data 5 | Data 6 | Data 7 | Data 8 | Data 9 | Data 10 | Data 11 | Data 12 |
|-----------------|--------|--------|--------|--------|--------|--------|--------|--------|--------|---------|---------|---------|
| Eastern third   | 4946   | 6149   | 5282   | 5953   | 7451   | 8605   | 6202   | 6000   | 6528   | 7243    | 6479    | 6911    |
| Middle third    | -      | -      | -      | -      | -      | -      | -      | -      | -      | -       | -       | -       |
| Western third   | -      | -      | -      | -      | -      | -      | -      | -      | -      | -       | -       | -       |

### Section 12-3

10.Increasing Plant Growth A gardening company is testing new ways to improve plant growth.Twelve plants are randomly selected and exposed to a combination of two factors, a “Grow-light”in two different strengths and a plant food supplement with different mineral supplements. After anumber of days, the plants are measured for growth, and the results (in inches) are put into theappropriate boxes.

|             | Grow-light 1      | Grow-light 2      |
|-------------|-------------------|-------------------|
| Plant food A| 9.2, 9.4, 8.9     | 8.5, 9.2, 8.9     |
| Plant food B| 7.1, 7.2, 8.5     | 5.5, 5.8, 7.6     |

Can an interaction between the two factors be concluded? Is there a difference in mean growth with respect to light? With respect to plant food? Use α = 0.05.

Use R to complete the following steps. Be sure to include all code in an appendix at the end of your submission. Assume the expected frequencies are equal and  = 0.05.
    1.Download the file ‘baseball.csv’ from the course resources and import the file into R.
    2.Perform EDA on the imported data set. Write a paragraph or two to describe the data setusing descriptive statistics and plots. Are there any trends or anything of interest to discuss?
    3.Assuming the expected frequencies are equal, perform a Chi-Square Goodness-of-Fit test todetermine if there is a difference in the number of wins by decade. Be sure to include thefollowing:
        a.State the hypotheses and identify the claim.
        b.Find the critical value ( = 0.05) (From table in the book).
        c.Compute the test value.
        d.Make the decision. Clearly state if the null hypothesis should or should not berejected and why.
        e.Does comparing the critical value with the test value provide the same result ascomparing the p-value from R with the significance level?

    4. Use the file ‘crop_data.csv’ from the course resources and import the file into R.
    5. Perform a Two-way ANOVA test using yield as the dependent variable and fertilizer anddensity as the independent variables
    Explain the results of the test. Is there reason to believethat fertilizer and density have an impact on yield?