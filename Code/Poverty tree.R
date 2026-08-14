tree_vars <- c("subnatid1","age","male","educat4","educy" ,
               "literacy","lstatus","empstat","industrycat4","urban",
              "whours","unitwage" )

tree_vars <- c("subnatid1","age","male","educat4","educy" ,
               "literacy","lstatus","empstat","urban",
              "whours","unitwage" )

df$poor = ifelse(df$welfare<4.2,1,0)

#tree_vars <- c("industrycat4","urban")
# Redo match_data
tree_data <- df |>
  filter(industrycat4 == "Agriculture") |>
  select(all_of(c("poor", tree_vars))) |>
  filter(complete.cases(across(all_of(tree_vars)))) |>
  mutate(
    across(where(haven::is.labelled), haven::zap_labels),
    across(where(is.character), as.factor),
    poor = factor(poor)
  )
 
#Classification tree
# CLASSIFICATION TREE: predict gems from tree_vars ---------------------------
library(rpart)
library(rpart.plot)

# Fit tree (classification)
tree_formula <- as.formula(paste("poor ~", paste(tree_vars, collapse = " + ")))

tree_fit <- rpart(
 tree_formula,
 data = tree_data,
 method = "class",
 parms = list(prior = c(0.5, 0.5)),
 control = rpart.control(cp = 0.002, minsplit = 10, minbucket = 10)
)

# Cross-validated error plot (1-SE rule)
par(mar = c(4, 4, 2, 1))
plotcp(tree_fit, main = "Cross-validated Error by Tree Size")

# Prune to 1-SE optimal cp
cp_opt  <- tree_fit$cptable[which.min(tree_fit$cptable[, "xerror"]), "CP"]
tree_pruned <- prune(tree_fit, cp = cp_opt)

# Main tree plot
rpart.plot(
  tree_pruned,
  type    = 4,        # label all nodes
  extra   = 104,      # show % obs + predicted class
  box.palette = list("Non-poor" = "#d9e6f2", "Poor" = "#f4a582"),
  shadow.col  = "gray80",
  nn          = TRUE,
  cex = 0.6,
  main        = "Classification Tree: Predicting Poverty - Agriculture",
  cex.main    = 2
)

# Confusion matrix + classification error
pred_class <- predict(tree_pruned, tree_data, type = "class")
conf_mat   <- table(Predicted = pred_class, Actual = factor(tree_data$poor, labels = c("Non-poor", "Poor")))

cat("\nConfusion Matrix:\n")
print(conf_mat)
cat(sprintf(
  "\nOverall accuracy : %.1f%%\nClassification error: %.1f%%\n",
  100 * sum(diag(conf_mat)) / sum(conf_mat),
  100 * (1 - sum(diag(conf_mat)) / sum(conf_mat))
))

# Variable importance plot
var_imp <- tibble(
  variable   = names(tree_pruned$variable.importance),
  importance = tree_pruned$variable.importance
) |> arrange(desc(importance))

ggplot(var_imp, aes(x = importance, y = reorder(variable, importance))) +
  geom_col(fill = "#4292c6", width = 0.7) +
  labs(
    title = "Variable Importance — Classification Tree",
    x     = "Importance (improvement in Gini)",
    y     = NULL
  ) +
  theme_bw(base_size = 11)

