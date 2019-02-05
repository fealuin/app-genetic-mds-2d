fnc_statistic_filter = function(algorithms,algorithms2)
{
  algorithms3=algorithms
  for (a in 1:length(algorithms$fit))
  { for (b in 1:length(algorithms2$fit))
    { 
      if (sum(algorithms$fit[[a]]-algorithms2$fit[[b]])!=0)
      { #T test application
        test=t.test(algorithms$fit[[a]],algorithms2$fit[[b]],paired = F)
        test=test$p.value
        if (!is.na(test))
        { #Restriction to paired NA
        if ((test<0.05) & ((algorithms$fitmean[[a]]<algorithms2$fitmean[[b]])))
          {#Replace the worst solution
           algorithms3$fit[[a]]=algorithms2$fit[[b]]
           algorithms3$gen[a]=algorithms2$gen[b]
           algorithms3$pop_size[a]=algorithms2$pop_size[b]
           algorithms3$met_ini[a]=algorithms2$met_ini[b]
           algorithms3$cross_rate[a]=algorithms2$cross_rate[b]
           algorithms3$mut_rate[a]=algorithms2$mut_rate[b]
           algorithms3$fitmean[a]=algorithms2$fitmean[b]
          }
        }#end if NA
      } #end if Sum -
    } 
  }
  return(algorithms3) 
}