dx = df%>%
  group_by(dx1)%>%
  summarise(count = n())%>%
  arrange(desc(count))%>%
  slice(1:5)
