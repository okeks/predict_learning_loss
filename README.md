# Covid-19 Learning Loss Analysis

## 📌 Project Overview
This project explores the **impact of Covid-19 on learning outcomes across multiple countries**, using datasets from the **World Bank** and other relevant sources.  
The goal is to understand how prolonged school closures and disruptions affected educational attainment and learning outcomes, and to highlight global disparities in recovery.

The analysis was conducted in **R**, with a focus on data cleaning, visualization, and statistical comparison across regions.

---

## 📂 Data Sources
- **World Bank EdStats**: [World Bank Education Data](https://databank.worldbank.org/source/education-statistics-%5e-all-indicators)  
- **UNESCO/UNICEF reports** (optional references)  
- Country-level socio-economic indicators (GDP per capita, internet penetration, etc.)  

---

## ⚙️ Methods & Tools
The workflow was implemented in **R** using the following key packages:
- `tidyverse` – data cleaning & wrangling  
- `ggplot2` – visualizations  
- `dplyr` – transformations and grouping  
- `readr` – reading data  
- `countrycode` – harmonizing country codes  
- `ggthemes` – visualization themes  

---

## 📊 Analysis Approach
1. **Data Preparation**
   - Import World Bank dataset
   - Clean missing values and harmonize country codes
   - Select key education indicators

2. **Exploratory Data Analysis**
   - Regional breakdown (Africa, Asia, Europe, Americas, etc.)
   - Correlation with socio-economic factors (e.g., GDP, digital access)

3. **Visualization**
   - Scatter plots linking economic resilience to education recovery

4. **Findings**
   - Many low-income countries experienced **larger and more persistent learning losses**  
   - Digital divide strongly influenced recovery speed  
   - Some high-income countries managed partial recovery by 2022  


git clone https://github.com/your-username/covid19-learning-loss.git
cd covid19-learning-loss
