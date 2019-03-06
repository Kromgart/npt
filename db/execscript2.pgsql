
DROP FUNCTION IF EXISTS npt_get_workorders_table (date, date);

CREATE OR REPLACE FUNCTION npt_get_workorders_table
  ( xStartDate date
  , xEndDate date
  ) 

RETURNS TABLE ( workorder_id bigint
              , project_id   bigint
              , budget       integer
              , extra_budget integer
              , non_billable integer
              , start_date   date
              , end_date     date
              , is_approved  bool
              , book_total   integer
              , book_current integer
              , log_total    integer
              )
AS $$

SELECT w.workorder_id, w.project_id, w.budget, w.extra_budget, w.non_billable, w.start_date, w.end_date, w.is_approved,
       coalesce (bs.booked_total, 0)   :: integer, 
       coalesce (bs.booked_current, 0) :: integer, 
       coalesce (lg.logged_total, 0)   :: integer

FROM workorders w

LEFT JOIN 
(SELECT b.project_id, 
        sum (b.amount) as booked_total, 
        sum (CASE WHEN b.book_date < current_date THEN b.amount ELSE 0 END) as booked_current
 FROM bookings b WHERE b.book_date >= xStartDate AND b.book_date <= xEndDate
 GROUP BY b.project_id
) bs ON w.project_id = bs.project_id

LEFT JOIN 
(SELECT l.project_id, sum (l.log_minutes) as logged_total
 FROM loggedtime l WHERE l.log_date >= xStartDate AND l.log_date <= xEndDate
 GROUP BY l.project_id
) lg ON w.project_id = lg.project_id

WHERE NOT (w.start_date > xEndDate OR w.end_date < xStartDate)

ORDER BY w.project_id

$$
LANGUAGE SQL
STABLE;







