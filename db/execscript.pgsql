DROP FUNCTION IF EXISTS npt_get_workorders_table_executive (date, date, bigint[]);


CREATE OR REPLACE FUNCTION npt_get_workorders_table_executive
  ( xStartDate date
  , xEndDate date
  , xGroups bigint[]
  ) 

RETURNS TABLE ( project_id   bigint
              , book_total   integer
              , book_current integer
              , log_total    integer
              , workorder_id bigint
              , budget       integer
              , extra_budget integer
              , non_billable integer
              , group_id     bigint
              , book_minutes integer
              , log_minutes  integer
              )
AS $$

SELECT p.project_id, coalesce (blt.book_total, 0), coalesce (blt.book_current, 0), coalesce (blt.log_total, 0),
       w.workorder_id, w.budget, w.extra_budget, w.non_billable, 
       rg.group_id, coalesce (sum(blt.book_minutes), 0) :: integer, coalesce (sum(blt.log_minutes), 0) :: integer

FROM projects p 
LEFT JOIN workorders w ON p.project_id = w.project_id AND NOT (w.start_date > xEndDate OR w.end_date < xStartDate)
LEFT JOIN 
(
  SELECT itb.entry_id, itb.project_id, itb, resource_id, 
         cast (sum (itb.book_minutes) OVER wnd as integer) as book_total,
         cast (sum (CASE WHEN itb.entry_date < current_date THEN itb.book_minutes ELSE 0 END) OVER wnd as integer) as book_current,
         cast (sum (itb.log_minutes) OVER wnd as integer) as log_total, 
         itb.log_minutes, 
         itb.book_minutes 
  FROM
  ( 
    (
      SELECT bok.booking_id as entry_id, bok.project_id, bok.resource_id, bok.book_date as entry_date, cast (NULL as integer) as log_minutes, bok.amount as book_minutes
      FROM bookings bok
    )
    UNION
    (
      SELECT lgt.log_id,                 lgt.project_id, lgt.resource_id, lgt.log_date,                lgt.log_minutes,                       NULL
      FROM loggedtime lgt
    )
  ) itb
  WHERE itb.entry_date >= xStartDate AND itb.entry_date <= xEndDate
  WINDOW wnd AS (PARTITION BY project_id)
) blt
ON p.project_id = blt.project_id 

LEFT JOIN resources_groups rg ON blt.resource_id = rg.resource_id

WHERE (w.workorder_id NOTNULL OR blt.entry_id NOTNULL) AND (rg.group_id ISNULL OR array_position (xGroups, rg.group_id) NOTNULL)
GROUP BY p.project_id, w.workorder_id, rg.group_id, blt.book_total, blt.book_current, blt.log_total 
ORDER BY p.project_id

$$
LANGUAGE SQL
STABLE;



--------------------------

