use [YOUR GEMINI DATABASE]

IF (SELECT [attribute_value] FROM [dbo].[gemini_install] WHERE [attribute_key] = 'VERSION' ) != '6.8.1'
BEGIN
	RAISERROR('GEMINI VERSION IS NOT 6.8.1', 20, -1) with log
END

ALTER TABLE [gemini_timetracking] ADD updated datetime NULL DEFAULT GETUTCDATE()

GO
UPDATE [gemini_timetracking] SET [updated] = [created] WHERE [updated] IS NULL

GO
CREATE TRIGGER [npt_track_updated_time] ON [dbo].[gemini_timetracking]
AFTER UPDATE
AS
BEGIN
	IF UPDATE (tstamp) 
    BEGIN
	UPDATE t SET [updated] = GETUTCDATE() FROM [gemini_timetracking] AS t
	WHERE EXISTS (SELECT * FROM inserted WHERE entryid = t.entryid)
	END
END
