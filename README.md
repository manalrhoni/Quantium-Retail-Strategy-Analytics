# 🛒 Retail Strategy & Analytics | Quantium Virtual Experience

## 📄 Project Overview
This project simulates a real-world data analytics engagement for a major supermarket chain. The goal was to analyze customer transaction data to drive category growth and evaluate the impact of a new store layout trial.

**Role:** Data Analyst (Virtual Intern)  
**Tools:** R (data.table, ggplot2), Statistical Analysis, A/B Testing

---

## 🎯 Key Objectives
1.  **Customer Analytics:** Analyze transaction data to identify high-value customer segments.
2.  **Experimentation (Uplift Testing):** Evaluate the performance of a new store layout using control store methodology.
3.  **Commercial Strategy:** Deliver data-driven recommendations to the Category Manager.

---

## 📊 Key Findings (Task 1)
* **Top Segment:** "Mainstream - Young Singles/Couples" are the highest value opportunity.
* **Insight:** Despite smaller basket sizes, this segment is willing to pay a **premium price per unit** compared to budget families.
* **Proof:** T-test results confirmed a statistically significant difference in unit price (p < 0.05).

![Sales by Segment](Visualizations/your_sales_graph_name.png)
*(Replace with your actual graph filename)*

---

## 🧪 Experimentation Results (Task 2)
To evaluate the store layout trial, I selected control stores based on **sales correlation** and **magnitude distance**.

* **Trial Store 77:** ✅ SUCCESS (Significant uplift vs Control Store 233)
* **Trial Store 86:** ✅ SUCCESS (Significant uplift vs Control Store 155)
* **Trial Store 88:** ❌ No Impact

![Trial Performance](Visualizations/your_trial_graph_name.png)
*(Replace with your actual graph filename)*

---

## 🚀 Recommendations
1.  **Target Mainstream Young Singles:** Launch marketing campaigns featuring **Tyrrells** chips and **270g packs**, as affinity analysis shows strong preference.
2.  **Rollout Strategy:** Expand the new store layout to all locations matching the profile of Store 77 and 86.
3.  **Investigate:** Pause rollout in Store 88-like locations until further root cause analysis is done.

---

## 💻 How to Run the Code
The analysis was performed in R.
1. Clone this repo.
2. Load the datasets (not included due to size constraints).
3. Run `Task1_Data_Prep.R` for customer analysis.
4. Run `Task2_Experimentation.R` for the control store analysis.
