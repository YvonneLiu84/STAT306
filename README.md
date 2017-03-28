Requirments:

(a) variable selection and creation of explanatory variables

(b) residual plots to decide on transforms, quadratic terms and interactions (see Section 4.4 on transforms and nonlinearity)

(c) cross-validation comparisons of a few "good" models. 


Reports:

Section 1 summary/abstract: this section includes the goals of your project and the main conclusions.

Section 2: describe your variables and include summary statistics and frequency tables (the latter for categorical variables), and some scatterplots.

Next sections: outline of analyses to support your main conclusions. This should not be a history of what you did; approaches that did not work should not be discussed.

Contributions Section (before Appendix): 1. Mention how your team formed: a common idea for a project, a team of friends/acquaintances, or something else. 2. Explain the ordering of names of authors (usually alphabetical by surname, or ordered from most contributed to the project to least contributed). 3. Summarize how teamwork evolved, and the major contributions (e.g., topic, statistical analysis, coding, criticism, writing, organizer of discussion meetings) of each team member; mention the team leader(s) if all members agree that someone had a leadership role.

Appendix: R code and secondary plots (only most important plots are needed in the main part of the report) -- the Appendix is for your own reference when you need to do a future project.


TODO:

1.Transformation

    a. each explainatory versus responsable variable and try to see what kinda of transformation of each explainatory variables.

    b. Each var summary

    c. Correlation table (highly cor variables might not include all of them)

    d. Cause "Mo" && "Ex" are very close -> consider combining them

2. Linear model
    *(No baseline chosen)
    
    * explain why do we delete O3 (By exhautive test and no monotonic increasing) (If we treate AQI cate)
    a. whether add quatratic terms on explainatory var.

3. Print residual VS each variable

4. Cross-valifdation and out of sample comparison
    train and hold outa -> calc cross-validation

Questions:

    1. Course pack: "ex" is between Step 3 and 4, but we choose to put it in Step 1. Can we?


    