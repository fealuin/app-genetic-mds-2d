CREATE PROCEDURE `normalize_results_lr` ()
BEGIN

declare cur cursor for select id from parameters_lr group by id;

open cur;
read_loop:LOOP
fetch cur into id_dataset;
if done then
leave read_loop;
end if;

set @xmax=(select max(x) from results_lr where parameters_id=id_dataset);
set @ymax =(select max(y) from results_lr where parameters_id=id_dataset);

set @xmin=(select min(x) from results_lr where parameters_id=id_dataset);
set @ymin =(select min(y) from results_lr where parameters_id=id_dataset);


UPDATE results_lr
SET
    xnorm = (x - @xmin) / (@xmax - @xmin),
    ynorm = (y - @ymin) / (@ymax - @ymin)
WHERE
    parameters_id= id_dataset;
end loop;
close cur;
END
