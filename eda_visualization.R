# ============================================================
# 📊 Sales Data EDA & Visualization in R
# Author: Muhammad Mubashir
# Description: Complete exploratory data analysis with
#              ggplot2, dplyr, and advanced visualizations
# ============================================================

# ── Install & Load Libraries ─────────────────────────────────
packages <- c("ggplot2", "dplyr", "tidyr", "readr", "scales",
              "ggthemes", "gridExtra", "corrplot", "RColorBrewer",
              "lubridate", "viridis", "ggcorrplot")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, quiet = TRUE)
  library(pkg, character.only = TRUE)
}
invisible(sapply(packages, install_if_missing))
cat("✅ All libraries loaded!\n")

# ── Load Data ────────────────────────────────────────────────
df <- read_csv("sales_data.csv", show_col_types = FALSE)
df$date     <- as.Date(df$date)
df$month    <- format(df$date, "%Y-%m")
df$month_name <- format(df$date, "%b")
df$year     <- format(df$date, "%Y")
df$profit_margin <- round(df$profit / df$sales * 100, 2)

cat("✅ Data loaded!\n")
cat(sprintf("   Shape  : %d rows × %d columns\n", nrow(df), ncol(df)))
cat(sprintf("   Sales  : $%s — $%s\n", 
    format(min(df$sales), big.mark=","), 
    format(max(df$sales), big.mark=",")))
cat(sprintf("   Period : %s to %s\n", min(df$date), max(df$date)))
print(head(df, 5))

# ── Summary Statistics ───────────────────────────────────────
cat("\n📊 Summary Statistics:\n")
print(summary(df[, c("sales","profit","quantity","discount","rating")]))

# ── Set Theme ────────────────────────────────────────────────
theme_mubashir <- theme_dark() +
  theme(
    plot.background    = element_rect(fill = "#0e1117", color = NA),
    panel.background   = element_rect(fill = "#1e2130", color = NA),
    panel.grid.major   = element_line(color = "#2d3348", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(color = "white", size = 14, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(color = "#aaaaaa", size = 10, hjust = 0.5),
    axis.text          = element_text(color = "#cccccc", size = 9),
    axis.title         = element_text(color = "white", size = 10),
    legend.background  = element_rect(fill = "#1e2130"),
    legend.text        = element_text(color = "white"),
    legend.title       = element_text(color = "white"),
    strip.background   = element_rect(fill = "#2d3348"),
    strip.text         = element_text(color = "white", face = "bold")
  )

# ── Plot 1: Sales by Category ────────────────────────────────
cat("\n📈 Creating Plot 1: Sales by Category...\n")
p1 <- df %>%
  group_by(category) %>%
  summarise(total_sales = sum(sales), avg_profit = mean(profit), .groups = "drop") %>%
  arrange(desc(total_sales)) %>%
  ggplot(aes(x = reorder(category, total_sales), y = total_sales / 1000, fill = category)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0("$", round(total_sales/1000), "K")),
            hjust = -0.1, color = "white", size = 3.5) +
  coord_flip() +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Total Sales by Category",
       subtitle = "Which category generates the most revenue?",
       x = NULL, y = "Total Sales ($K)") +
  theme_mubashir

# ── Plot 2: Monthly Sales Trend ──────────────────────────────
cat("📈 Creating Plot 2: Monthly Sales Trend...\n")
p2 <- df %>%
  group_by(month) %>%
  summarise(monthly_sales  = sum(sales),
            monthly_profit = sum(profit), .groups = "drop") %>%
  pivot_longer(cols = c(monthly_sales, monthly_profit),
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = month, y = value / 1000, color = metric, group = metric)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("monthly_sales" = "#00d4ff", "monthly_profit" = "#ff6b6b"),
                     labels = c("Monthly Profit", "Monthly Sales")) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Monthly Sales & Profit Trend",
       subtitle = "Revenue and profit over time",
       x = "Month", y = "Amount ($K)", color = "Metric") +
  theme_mubashir +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ── Plot 3: Region Performance ───────────────────────────────
cat("📈 Creating Plot 3: Region Performance...\n")
p3 <- df %>%
  group_by(region, category) %>%
  summarise(total_sales = sum(sales), .groups = "drop") %>%
  ggplot(aes(x = region, y = total_sales / 1000, fill = category)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_viridis_d(option = "turbo") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Sales by Region & Category",
       subtitle = "Regional breakdown per product category",
       x = "Region", y = "Sales ($K)", fill = "Category") +
  theme_mubashir

# ── Plot 4: Discount vs Profit ───────────────────────────────
cat("📈 Creating Plot 4: Discount vs Profit...\n")
p4 <- df %>%
  ggplot(aes(x = discount * 100, y = profit_margin, color = category)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, color = "#ffd700") +
  scale_color_viridis_d(option = "plasma") +
  labs(title = "Discount vs Profit Margin",
       subtitle = "Does higher discount reduce profitability?",
       x = "Discount (%)", y = "Profit Margin (%)", color = "Category") +
  theme_mubashir

# ── Plot 5: Rating Distribution ──────────────────────────────
cat("📈 Creating Plot 5: Rating Distribution...\n")
p5 <- df %>%
  ggplot(aes(x = rating, fill = region)) +
  geom_histogram(binwidth = 0.2, color = "white", alpha = 0.8, position = "identity") +
  facet_wrap(~ region, nrow = 2) +
  scale_fill_viridis_d(option = "mako") +
  labs(title = "Customer Rating Distribution by Region",
       x = "Rating (1-5)", y = "Count", fill = "Region") +
  theme_mubashir

# ── Plot 6: Sales Heatmap ─────────────────────────────────────
cat("📈 Creating Plot 6: Sales Heatmap...\n")
p6 <- df %>%
  group_by(region, category) %>%
  summarise(avg_sales = mean(sales), .groups = "drop") %>%
  ggplot(aes(x = category, y = region, fill = avg_sales)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0("$", round(avg_sales/1000, 1), "K")),
            color = "white", size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "#1a1a4e", mid = "#7c3aed", high = "#00d4ff",
                       midpoint = median(df$sales),
                       labels = label_comma()) +
  labs(title = "Avg Sales Heatmap: Region × Category",
       x = "Category", y = "Region", fill = "Avg Sales ($)") +
  theme_mubashir +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ── Save All Plots ────────────────────────────────────────────
cat("\n💾 Saving plots...\n")
ggsave("01_sales_by_category.png",   p1, width=10, height=6, dpi=150, bg="#0e1117")
ggsave("02_monthly_trend.png",       p2, width=12, height=6, dpi=150, bg="#0e1117")
ggsave("03_region_performance.png",  p3, width=12, height=6, dpi=150, bg="#0e1117")
ggsave("04_discount_vs_profit.png",  p4, width=10, height=6, dpi=150, bg="#0e1117")
ggsave("05_rating_distribution.png", p5, width=12, height=7, dpi=150, bg="#0e1117")
ggsave("06_sales_heatmap.png",       p6, width=10, height=6, dpi=150, bg="#0e1117")

# ── Business Insights ─────────────────────────────────────────
cat("\n🔍 Key Business Insights:\n")
cat("="  %>% strrep(50), "\n")

top_cat    <- df %>% group_by(category) %>% summarise(s=sum(sales)) %>% slice_max(s,n=1) %>% pull(category)
top_region <- df %>% group_by(region)   %>% summarise(s=sum(sales)) %>% slice_max(s,n=1) %>% pull(region)
avg_margin <- mean(df$profit_margin, na.rm=TRUE)
best_rated <- df %>% group_by(category) %>% summarise(r=mean(rating)) %>% slice_max(r,n=1) %>% pull(category)

cat(sprintf("  Top Category   : %s\n", top_cat))
cat(sprintf("  Top Region     : %s\n", top_region))
cat(sprintf("  Avg Margin     : %.1f%%\n", avg_margin))
cat(sprintf("  Best Rated     : %s\n", best_rated))
cat(sprintf("  Total Revenue  : $%s\n", format(sum(df$sales), big.mark=",")))
cat(sprintf("  Total Profit   : $%s\n", format(sum(df$profit), big.mark=",")))
cat("="  %>% strrep(50), "\n")
cat("\n✅ All plots saved! Check the project folder.\n")
