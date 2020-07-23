attention_index=d$check1_correct3==3&d$check2_correct1==1&d$check3_correct5==5

d_no=d[!attention_index,]
nrow(d_no)
a1 = 18:29
a2 = 30:44
a3 = 45:59
a4 = 60:79
ages=list(a1, a2, a3, a4)
#lapply(ages, function(l))

x=sapply(d_no$age, function(a){
  lapply(ages, function(l)a%in%l)
})

d_no$age_cat =apply(x, 2, function(y)which(y==TRUE))
d_no$quota = ifelse(d_no$female==1, d_no$age_cat+4, d_no$age_cat)
cbind(d_no$age, d_no$female, d_no$quota)
aggregate(data=d_no, age~quota, length)
