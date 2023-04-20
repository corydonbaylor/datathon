dx = df%>%
  group_by(dx1)%>%
  summarise(count = n())%>%
  arrange(desc(count))%>%
  slice(1:5)

# no rows without dx1
df%>%
  filter(is.na(dx1))
