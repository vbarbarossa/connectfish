
calc_ci <- function(occ, alpha = 0.55){
  
  L <- occ$SUB_AREA**alpha
  
  # diadromous
  sa_cur = sum(L[occ$group_cur == min(occ$group_cur)])
  dci <- sa_cur/sum(L)*100
  
  # non-diadromous
  sa_cur = 0
  for(i in unique(occ$group_cur)) sa_cur = sa_cur + sum(L[occ$group_cur == i])**2
  nci <- sa_cur/(sum(L)**2)*100
  
  return(data.frame(diadromous = dci, nondiadromous = nci))
  
}

calc_ci(data.frame(SUB_AREA = c(154.6,240.3,125.5,140.6), group_cur = c(1,1,1,1)))
calc_ci(data.frame(SUB_AREA = c(154.6,240.3,125.5,140.6), group_cur = c(1,2,2,2)))
calc_ci(data.frame(SUB_AREA = c(154.6,240.3,125.5,140.6), group_cur = c(1,2,3,4)))


