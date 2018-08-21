CREATE PROCEDURE `normalize_results_final` ()
BEGIN

declare cur cursor for select dataset from parameters group by dataset;

open cur;
read_loop:LOOP
fetch cur into id_dataset;
if done then 
leave read_loop;
end if;

set @xmax=(select max(x) from results_final where dataset=id_dataset);
set @ymax =(select max(y) from results_final where dataset=id_dataset);
  
set @xmin=(select min(x) from results_final where dataset=id_dataset);
set @ymin =(select min(y) from results_final where dataset=id_dataset);
  
  
UPDATE results_final 
SET 
    xnorm = (x - @xmin) / (@xmax - @xmin),
    ynorm = (y - @ymin) / (@ymax - @ymin)
WHERE
    dataset = id_dataset;
end loop;
close cur;
END
