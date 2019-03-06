
DROP FUNCTION IF EXISTS npt_get_workorders_table_executive_old (date, date, bigint[]);

DROP FUNCTION IF EXISTS npt_get_workorders_table_executive (date, date, bigint[]);
DROP FUNCTION IF EXISTS npt_get_workorders_table (date, date);

DROP TABLE IF EXISTS workorders;
DROP TABLE IF EXISTS loggedtime;
DROP TABLE IF EXISTS bookings;
DROP TABLE IF EXISTS timeoffs;

DROP TABLE IF EXISTS projects;
DROP TABLE IF EXISTS project_types;

DROP TABLE IF EXISTS resources_groups;
DROP TABLE IF EXISTS resources;

DROP TABLE IF EXISTS schedules;
DROP TABLE IF EXISTS groups;
DROP TABLE IF EXISTS group_types;


---------------------------------------------------------------


CREATE TABLE schedules ( schedule_id  bigserial  PRIMARY KEY
                       , display_name text       NOT NULL
                       , start_date   date       NOT NULL
                       , cycle_def    smallint[] NOT NULL
                       , CONSTRAINT schedule_nonempty CHECK (cardinality (cycle_def) > 0 AND (array_position (cycle_def, NULL)) IS NULL)
                       );

ALTER TABLE schedules OWNER TO developer;                      

INSERT INTO schedules (display_name,        start_date,   cycle_def)
            VALUES    ('Standard 40h week', '2017-01-02', '{480,480,480,480,480,0,0}') ;




---------------------------------------------------------------




CREATE TABLE group_types ( group_type_id bigserial PRIMARY KEY
                         , display_name  text      NOT NULL
                         , is_locked     boolean   NOT NULL
                         );
 
ALTER TABLE group_types OWNER TO developer;

INSERT INTO group_types (group_type_id, display_name, is_locked)
            VALUES      (1,             'Location',   TRUE);





---------------------------------------------------------------



CREATE TABLE groups ( group_id      bigserial PRIMARY KEY
                    , group_type_id bigint    NOT NULL REFERENCES group_types (group_type_id) ON DELETE SET NULL ON UPDATE CASCADE
                    , display_name  text      NOT NULL
                    , CONSTRAINT "unique (group type, display name)" UNIQUE (group_type_id, display_name)
                    );

ALTER TABLE groups OWNER TO developer;



---------------------------------------------------------------



CREATE TABLE project_types ( project_type_id bigserial PRIMARY KEY
                           , display_name    text      NOT NULL
                           );
 
ALTER TABLE project_types OWNER TO developer;



---------------------------------------------------------------



CREATE TABLE resources ( resource_id   bigserial PRIMARY KEY
                       , external_id   bigint        NULL UNIQUE
                       , supervisor_id bigint        NULL REFERENCES resources (resource_id) ON DELETE SET NULL ON UPDATE CASCADE
                       , display_name  text      NOT NULL
                       , schedule_id   bigint    NOT NULL REFERENCES schedules (schedule_id) ON DELETE RESTRICT ON UPDATE CASCADE
                       , start_date    date      NOT NULL
                       , end_date      date          NULL
                       , title         text      NOT NULL
                       , email         text          NULL UNIQUE
                       , phone         text          NULL
                       , CONSTRAINT resources_check_dates CHECK (end_date IS NULL OR end_date > start_date)
                       );


ALTER TABLE resources OWNER TO developer;                      



-------------------------------------



CREATE TABLE resources_groups ( group_id    bigint NOT NULL REFERENCES groups (group_id) ON DELETE RESTRICT ON UPDATE CASCADE
                              , resource_id bigint NOT NULL REFERENCES resources (resource_id) ON DELETE RESTRICT ON UPDATE CASCADE
                              , CONSTRAINT "unique (group, resource)" UNIQUE (group_id, resource_id)
                              );

ALTER TABLE resources_groups OWNER TO developer;


---------------------------------------------------------------



CREATE TABLE timeoffs ( timeoff_id   bigserial   PRIMARY KEY
                      , external_id  bigint          NULL UNIQUE
                      , resource_id  bigint      NOT NULL REFERENCES resources (resource_id) ON DELETE RESTRICT ON UPDATE CASCADE
                      , start_date   date        NOT NULL
                      , end_date     date        NOT NULL
                      , last_updated timestamptz NOT NULL
                      , timeoff_type smallint    NOT NULL DEFAULT 0
                      , status       smallint    NOT NULL DEFAULT 0
                      , CONSTRAINT timeoffs_check_dates CHECK (start_date <= end_date)
                      );

ALTER TABLE timeoffs OWNER TO developer;                      



---------------------------------------------------------------



CREATE TABLE projects ( project_id   bigserial    PRIMARY KEY
                      , external_id  bigint       NULL UNIQUE
                      , display_name text     NOT NULL
                      , full_name    text     NOT NULL
                      , color        text     NOT NULL
                      , project_mgr  bigint       NULL REFERENCES resources (resource_id) ON DELETE SET NULL ON UPDATE CASCADE
                      , account_mgr  bigint       NULL REFERENCES resources (resource_id) ON DELETE SET NULL ON UPDATE CASCADE
                      , project_type bigint   NOT NULL REFERENCES project_types (project_type_id) ON DELETE RESTRICT ON UPDATE CASCADE
                      );

ALTER TABLE projects OWNER TO developer;                      




---------------------------------------------------------------


CREATE TABLE workorders ( workorder_id bigserial   PRIMARY KEY
                        , project_id   bigint  NOT NULL REFERENCES projects (project_id) ON DELETE RESTRICT ON UPDATE CASCADE
                        , budget       integer NOT NULL
                        , extra_budget integer NOT NULL
                        , non_billable integer NOT NULL
                        , start_date   date    NOT NULL
                        , end_date     date    NOT NULL
                        , is_approved  boolean NOT NULL
                        , CONSTRAINT workorder_positive_amount CHECK (budget > 0 AND extra_budget >= 0 AND non_billable >= 0)
                        , CONSTRAINT workorder_dates CHECK (start_date <= end_date)
                        );

ALTER TABLE workorders OWNER TO developer;                      



---------------------------------------------------------------


CREATE TABLE bookings ( booking_id   bigserial   PRIMARY KEY
                      , resource_id  bigint      NOT NULL REFERENCES resources (resource_id) ON DELETE RESTRICT ON UPDATE CASCADE
                      , book_date    date        NOT NULL
                      , project_id   bigint      NOT NULL REFERENCES projects (project_id)   ON DELETE RESTRICT ON UPDATE CASCADE
                      , amount       smallint    NOT NULL
                      , last_updated timestamptz NOT NULL DEFAULT now()
                      , CONSTRAINT "unique (resource, date, project)" UNIQUE (resource_id, book_date, project_id)
                      , CONSTRAINT bookings_positive_amount CHECK (amount > 0)
                      );



ALTER TABLE bookings OWNER TO developer;                      


---------------------------------------------------------------


CREATE TABLE loggedtime ( log_id       bigserial PRIMARY KEY
                        , external_id  bigint        NULL UNIQUE
                        , resource_id  bigint    NOT NULL REFERENCES resources (resource_id) ON DELETE RESTRICT ON UPDATE CASCADE
                        , project_id   bigint    NOT NULL REFERENCES projects (project_id)   ON DELETE RESTRICT ON UPDATE CASCADE
                        , log_date     date      NOT NULL
                        , log_minutes  smallint  NOT NULL
                        , last_updated timestamptz NOT NULL DEFAULT now()
                        , CONSTRAINT loggedhours_positive CHECK (log_minutes > 0)
                        );


ALTER TABLE loggedtime OWNER TO developer;                      



---------------------------------------------------------------

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


---------------------------------------------------------------



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



---------------------------------------------------------------





GRANT ALL ON ALL TABLES IN SCHEMA public TO npt;
GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO npt;
GRANT ALL ON ALL FUNCTIONS IN SCHEMA public TO npt;


