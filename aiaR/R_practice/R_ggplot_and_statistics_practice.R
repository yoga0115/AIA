# Practice 1 
# 1) please show percent of shops that have avg.cost larger than 100 (and how's the subcategory)
df.breakfast1 <- df.allshop.info[avg.cost > 100]
View(df.breakfast1)

ggplot(df.breakfast1, aes(x = cate.minor)) +
  geom_bar(fill = "royalblue2", 
           aes(y = (..count..)/sum(..count..))) + 
  theme_bw() + 
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(x = "minor category", y = "percentage") +
  ggtitle("Percentage of Sub-categories in breakfast where avg.cost > 100") +
  theme(text = element_text(family = "STHeiti"))

# 2) please show the ECDF of n.view of each sub-category
ggplot(df.allshop.info, aes(x = avg.cost)) +
  stat_ecdf(aes(color = factor(cate.minor)), geom = "line") +
  theme_bw() + 
  labs(y = "cumulative n") +
  facet_wrap(~cate.minor, scales = "free") +
  theme(text = element_text(family = "STHeiti"))

# 3) please show the median avg.cost of each sub-category
ggplot(df.breakfast, aes(x = cate.minor, y = med.avg.cost)) +
  geom_bar(stat = "identity", fill = "royalblue2") + 
  geom_text(aes(label = round(med.avg.cost,2) )) +
  theme_bw() +
  theme(text = element_text(family = "STHeiti"))

# 4) please remove shops that avg.cost equal to 0 and observe hist/ecdf again
df.allshop.info <- df.allshop.info[avg.cost != 0]

