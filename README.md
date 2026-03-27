---
editor_options: 
  markdown: 
    wrap: 72
---

# Monitoring Scholar Success: A Data-Driven Evaluation of an African Youth Scholarship Program

> ⚠️ **Disclaimer:** The TechRise Scholar Program is entirely fictional
> and created for portfolio demonstration purposes only. All data used
> in this project is simulated and does not represent any real
> organisation, program or individual. The analysis, results and
> recommendations are based solely on this simulated data.

## About the Program

TechRise Scholar Program is a fictional skills-driven scholarship
designed to equip economically disadvantaged African youth, including
refugees and displaced individuals, with in-demand digital skills. Over
a 4-month intensive training period, scholars choose a track in:

-   Data Analysis
-   Product Design
-   Cybersecurity
-   Front-End Development
-   Digital Marketing

The program runs three cohorts per year, with each cohort serving 100
scholars selected through a competitive application process. Beyond
technical training, TechRise provides mentorship, career support and a
structured Give Back commitment where graduates are encouraged to mentor
the next generation of scholars.

## The Problem

Many African youth scholarship programs collect data on their scholars
but struggle to turn that data into clear, actionable insights. Without
a structured monitoring system, it becomes difficult to:

-   Track whether scholars are truly thriving
-   Identify scholars who need support before it's too late
-   Show funding partners clear evidence of what the program is actually
    achieving

This project builds an end-to-end Monitoring & Evaluation (M&E) system
from survey design through statistical analysis to an interactive
dashboard to show how data can drive better decisions in scholarship
programs. Access survey
[here](https://qualtricsxmzqbmsl9fv.qualtrics.com/jfe/form/SV_3CTWm58SOocjUp0)

## Research Questions

This project set out to answer four key questions:

1.  How are scholars experiencing the program across the four pillars:
    Access & Thrive, Quality Learning, Dignified Work and Leadership &
    Give Back?
2.  What factors most strongly predict whether a scholar will complete
    the program and secure employment after graduation?
3.  Is the program improving : Are scholars in later cohorts achieving
    better outcomes than earlier ones?
4.  Are refugee and displaced scholars experiencing the program
    differently from their peers and what does the data say about their
    support needs?

## Tools & Methods

| Stage | Tool | Purpose |
|-----------------------|--------------------|-----------------------------|
| Survey Design | Qualtrics | 26-question endline survey. |
| Data Simulation | R (`dplyr`) | 300 simulated scholar records across 3 cohorts |
| Survey Reliability | SPSS | Cronbach's alpha, item-level descriptive statistics |
| Statistical Analysis | R (`ggplot2`, `tidyr`) | Descriptive stats, regression, ANOVA, t-tests |
| Visualisation | R (`ggplot2`) | 5 publication-ready charts |
| Dashboard & Reporting | Tableau | Interactive KPI dashboard for donor reporting |

## Project Structure

```         
techrise
│
├── src/
│   ├── 01_simulate_data.R        # Simulates 300 scholar records
│   └── 02_analysis.R             # Full M&E analysis (RQ1–RQ4)
│
├── data/
│   ├── techrise_scholar_survey_data.csv   # Simulated raw survey data (300 rows)
│   └── techrise_cohort_summary.csv        # Aggregated cohort summary for Tableau
│
├── outputs/
│   └── plots/
│       ├── plot1_overall_pillar_scores.png
│       ├── plot2_employment_predictors.png
│       ├── plot3_cohort_comparison.png
│       ├── plot4_outcome_trends.png
│       └── plot5_refugee_comparison.png
|
├── requirements.R                # Package installer
├── .gitignore
├──  README.md
└── report.pdf                    # Analysis results & recommendations
```

## How to Reproduce This Project

### Step 1: Clone the repository

``` bash
git clone https://github.com/[your-username]/techrise-me-portfolio.git
cd techrise-me-portfolio
```

### Step 2: Install R packages

Open R or RStudio and run:

``` r
source("requirements.R")
```

This installs `dplyr`, `ggplot2` and `tidyr` if they are not already
present.

### Step 3: Simulate the data (optional)

The dataset is already included in `data/`. To re-generate it from
scratch:

``` r
source("src/01_simulate_data.R")
```

This will create `techrise_scholar_survey_data.csv` in your working
directory.

### Step 4: Run the analysis

``` r
source("src/02_analysis.R")
```

This produces all 5 plots and `techrise_cohort_summary.csv` in your
working directory.

### Step 5: Tableau dashboard

Load `data/techrise_cohort_summary.csv` into Tableau to build the KPI
dashboard (see [Dashboard](#dashboard) section below).

**R version used:** R 4.5.1\
**RStudio recommended** for best experience

## Survey Design

The Scholar Experience & Outcomes Assessment is a 26-question endline
survey administered to all scholars at the end of their 4-month program.
It covers six sections:

| Section | Questions | Focus |
|---------------------|-----------|-------------------------------------|
| About You | Q1–Q8 | Demographics, training track, refugee status |
| Access & Thrive | Q9–Q11 | Device access, financial stability, sense of belonging |
| Quality & Relevant Learning | Q12–Q14 | Content relevance, practical skills, training satisfaction |
| Dignified & Fulfilling Work | Q15–Q18 | Employment confidence, career planning, job status |
| Leadership & Give Back | Q19–Q22 | Leadership readiness, Give Back commitment, mentorship |
| Overall Experience | Q23–Q26 | Satisfaction rating, NPS score, expectations |

All Likert-scale items use a 1- 5 scale (1 = Strongly Disagree, 5 =
Strongly Agree).

## Data

-   300 scholar records across 3 cohorts (100 per cohort)
-   32 variables including demographics, Likert responses, categorical
    outcomes and derived pillar scores
-   Simulated with realistic patterns built in later cohorts perform
    better, refugee scholars face access gaps, Give Back participation
    grows over time

## Dashboard {#dashboard}

The **TechRise Scholar KPI Dashboard** was built in Tableau using
`data/techrise_cohort_summary.csv`.

### Dashboard Preview

**[watch the dashboard walkthrough video]**

### Dashboard Pages

| Page | What it shows |
|-----------------------|-------------------------------------------------|
| **Program Overview** | KPI cards (completion, employment, Give Back), pillar score summary, outcome trends by cohort |
| **Pillar Scores** | All four pillar scores side by side across cohorts, filtered by cohort and refugee status |
| **Employment Outcomes** | Employment rate by cohort, employment relevance breakdown, work confidence scores |
| **Leadership & Give Back** | Give Back participation trends, mentorship intention, cohort-by-cohort growth |

## Author

Omowunmi Obadero

Built as a portfolio project demonstrating end-to-end M&E skills: survey
design, statistical analysis in R and SPSS and interactive dashboard
development in Tableau.

**Tools used:** R · SPSS · Tableau · Qualtrics · GitHub

*This project is for portfolio and demonstration purposes only.*
