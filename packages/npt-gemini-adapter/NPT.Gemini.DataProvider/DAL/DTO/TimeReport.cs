using Newtonsoft.Json;
using System;

namespace NPT.Gemini.DataProvider.DAL.DTO
{
    public class TimeReport
    {
        public int reportId { get; set; }

        public int loggedTime { get; set; }

        public string emailAddress { get; set; }

        [JsonIgnore]
        public DateTime timeEntryDate { get; set; }

        [JsonIgnore]
        public int projectId { get; set; }

        public string entryDate
        {
            get { return timeEntryDate.ToString("yyyy-MM-dd"); }
        }

        public int project
        {
            get { return projectId; }
        }
    }
}