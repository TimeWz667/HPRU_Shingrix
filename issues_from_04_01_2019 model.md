
### Issues 


- Discounting in the QALY loss due to death was not fully applied
  - Discounting function did not properly written
  - Correct: sum of q_a * survival_a / (1 + d) ^ (a - a0) from a0 to 100, where a for age, a0 for age of death
  - Current version: sum of q_a * survival_a / (1 + d) ^ 2 from a0 to 100
  - Impact: QOL loss due to death over-estimated
- Overall HZ incidence and GP-only HZ incidence are simulated separately.
  - As the GP-only HZ incidence (r_gp) is not far from the Overall HZ incidence (r), it is possible that the former > the later.
  - Impact: Internal inconsistency, uncertainty over-estimated
  - Suggestions: moodelling the GP-only HZ incidence by an overall rate and a ratio of who only found in GP records.
- Survival did not consider HZ-specific mortality
  - Survival probability (prob. of arriving next age group) only consider back-ground mortality
  - Preventing HZ cases will not increase the population size in the next age group
  - Impact: impacts on disease prevention over-estimated
