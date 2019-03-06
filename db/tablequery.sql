--EXPLAIN (ANALYZE, VERBOSE)

SELECT rs.resource_id, rs.start_date, rs.end_date, 
       sh.schedule_id, sh.start_date, sh.cycle_def, 
       tb.booking_id, tb.book_date, tb.project_id, tb.amount,
       tb.timeoff_id, tb.timeoff_start, tb.timeoff_end

FROM resources rs 
INNER JOIN schedules sh ON (rs.schedule_id = sh.schedule_id)
LEFT JOIN
  ( 
    (
      SELECT bok.resource_id,  
             cast (NULL as bigint) timeoff_id, cast (NULL as date) timeoff_start, cast (NULL as date) timeoff_end,
             bok.booking_id, bok.book_date, bok.project_id, bok.amount
      FROM bookings bok
      WHERE (bok.book_date >= '2017-09-01' AND bok.book_date <= '2017-09-10')
    ) 
    UNION 
    (
      SELECT tof.resource_id, 
             tof.timeoff_id, tof.start_date, tof.end_date,
             NULL, NULL, NULL, NULL
      FROM timeoffs tof
      WHERE NOT (tof.start_date > '2017-09-10' OR tof.end_date < '2017-09-01')
    )
  ) tb ON (rs.resource_id = tb.resource_id)


WHERE NOT (     rs.start_date > '2017-09-10'
            OR (rs.end_date NOTNULL AND rs.end_date < '2017-09-01') 
          ) 

ORDER BY rs.resource_id, tb.project_id

;

/*
SELECT rs.resource_id, rs.start_date, rs.end_date, 
       tb.schedule_id, tb.start_date, tb.cycle_def, 
       tb.timeoff_id, tb.timeoff_start, tb.timeoff_end,
       tb.booking_id, tb.book_date, tb.project_id, tb.amount

FROM resources rs INNER JOIN
                  ( (
                     SELECT cast (NULL as bigint) resource_id, 
                            sh.schedule_id, sh.start_date, sh.cycle_def, 
                            cast (NULL as bigint) timeoff_id, cast (NULL as date) timeoff_start, cast (NULL as date) timeoff_end,
                            cast (NULL as bigint) booking_id, cast (NULL as date) book_date, cast (NULL as bigint) project_id, cast (NULL as smallint) amount

                            FROM schedules sh
                    ) UNION (
                    SELECT tof.resource_id, 
                           NULL, NULL, NULL,
                           tof.timeoff_id, tof.start_date, tof.end_date,
                           NULL, NULL, NULL, NULL

                           FROM timeoffs tof
                           WHERE NOT (tof.start_date > '2017-09-10' OR tof.end_date < '2017-09-01')
                    ) UNION ( 
                    SELECT bok.resource_id,  
                           NULL, NULL, NULL, 
                           NULL, NULL, NULL,
                           bok.booking_id,  bok.book_date,  bok.project_id,  bok.amount

                           FROM bookings bok
                           WHERE (bok.book_date >= '2017-09-01' AND bok.book_date <= '2017-09-10')
                    )

                  ) tb ON (rs.schedule_id = tb.schedule_id OR rs.resource_id = tb.resource_id)


WHERE NOT (     rs.start_date > '2017-09-10'
            OR (rs.end_date NOTNULL AND rs.end_date < '2017-09-01') 
          ) 

ORDER BY rs.resource_id , tb.schedule_id, tb.project_id, tb.book_date ASC 

;

*/
