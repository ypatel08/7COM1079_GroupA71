library(ggplot2)
library(dplyr)

dept_data <- read.csv("department_store_dataset.csv", stringsAsFactors = FALSE)

dept_data$Department <- as.factor(dept_data$Department)

# 1. Difference in Means (t-test)
# Compare Revenue between two departments: Eletrônicos vs Vestuário

data_two_dept <- subset(dept_data, Department %in% c("Eletrônicos", "Vestuário"))

# Boxplot
ggplot(data_two_dept, aes(x = Department, y = Revenue)) +
  geom_boxplot(fill = c("lightblue", "lightgreen")) +
  labs(
    title = "Daily Revenue Comparison by Department",
    x = "Department",
    y = "Revenue (R$)",
    caption = "Source: department_store_dataset.csv"
  )
ggsave("boxplot_revenue_by_department.png", width = 6, height = 4, dpi = 300)

# Histogram
ggplot(data_two_dept, aes(x = Revenue, fill = Department)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(
    title = "Histogram of Revenue by Department",
    x = "Revenue (R$)",
    y = "Frequency",
    caption = "Source: department_store_dataset.csv"
  ) +
  theme_minimal()
ggsave("histogram_revenue.png", width = 6, height = 4, dpi = 300)

# T-test
mean_test <- t.test(Revenue ~ Department, data = data_two_dept)
capture.output(mean_test, file = "t_test_summary.txt")

# 2. Difference in Proportions (prop.test)

if ("Revenue.Goal" %in% names(dept_data)) {
  dept_data$MetGoal <- as.numeric(dept_data$Revenue >= dept_data$Revenue.Goal)
  successes <- c(
    sum(dept_data$MetGoal[dept_data$Department == "Eletrônicos"]),
    sum(dept_data$MetGoal[dept_data$Department == "Vestuário"])
  )
  totals <- c(
    sum(dept_data$Department == "Eletrônicos"),
    sum(dept_data$Department == "Vestuário")
  )
  prop_test <- prop.test(successes, totals, correct = FALSE)
  capture.output(prop_test, file = "prop_test_summary.txt")
} else {
  write("Column 'Revenue.Goal' not found in dataset", file = "prop_test_summary.txt")
}


# 3. Correlation Analysis

if ("Sales.Quantity" %in% names(dept_data)) {
  
  cor_res <- cor.test(dept_data$Revenue, dept_data$Sales.Quantity)
  capture.output(cor_res, file = "correlation_summary.txt")
  
  # Scatter + regression line
  p_scatter <- ggplot(dept_data, aes(Sales.Quantity, Revenue)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", color = "darkred", se = FALSE) +
    labs(
      title = "Correlation between Revenue and Sales Quantity",
      x = "Sales Quantity", y = "Revenue (R$)"
    ) +
    theme_minimal()
  
  save_plot(p_scatter, "scatter_revenue_vs_quantity.png")
  
} else {
  write("Column 'Sales.Quantity' not found in dataset", "correlation_summary.txt")
}
