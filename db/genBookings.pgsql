

INSERT INTO workorders (project_id, budget, extra_budget, non_billable, start_date,    end_date, is_approved)
            VALUES     (1,          600,    300,          60,           '2017-11-01', '2017-11-30', FALSE   )
                     , (2,          48000,  0,            600,          '2017-11-01', '2017-11-30', TRUE    )
                     , (3,          18000,  0,            0,            '2017-11-01', '2017-11-30', FALSE   )
                     ;


INSERT INTO bookings   (resource_id, book_date,    project_id, amount,  last_updated)
            VALUES   
                       (1,           '2017-11-04', 1,          480,     current_timestamp)
                     , (1,           '2017-11-05', 1,          420,     current_timestamp)
                     , (1,           '2017-11-06', 1,          420,     current_timestamp)
                     , (1,           '2017-11-07', 1,          420,     current_timestamp)
                     , (1,           '2017-11-08', 1,          420,     current_timestamp)

                     , (2,           '2017-11-04', 2,          360,     current_timestamp)
                     , (2,           '2017-11-05', 2,          360,     current_timestamp)
                     , (2,           '2017-11-06', 2,          360,     current_timestamp)
                     , (2,           '2017-11-07', 2,          360,     current_timestamp)
                     , (2,           '2017-11-08', 2,          360,     current_timestamp)

                     , (49,           '2017-11-04', 1,          180,     current_timestamp)
                     , (49,           '2017-11-05', 2,          180,     current_timestamp)
                     , (49,           '2017-11-06', 1,          180,     current_timestamp)
                     , (49,           '2017-11-07', 2,          180,     current_timestamp)
                     , (49,           '2017-11-08', 1,          180,     current_timestamp)

                     , (52,           '2017-11-04', 3,          240,     current_timestamp)
                     , (52,           '2017-11-05', 2,          240,     current_timestamp)
                     , (52,           '2017-11-06', 3,          240,     current_timestamp)
                     , (52,           '2017-11-07', 2,          240,     current_timestamp)
                     , (52,           '2017-11-08', 3,          240,     current_timestamp)


                     , (65,           '2017-11-04', 1,          300,     current_timestamp)
                     , (65,           '2017-11-05', 3,          300,     current_timestamp)
                     , (65,           '2017-11-06', 1,          300,     current_timestamp)
                     , (65,           '2017-11-07', 3,          300,     current_timestamp)
                     , (65,           '2017-11-08', 1,          300,     current_timestamp)
 

                       ;


UPDATE projects SET project_mgr = 11 WHERE display_name LIKE 'G8S%';


