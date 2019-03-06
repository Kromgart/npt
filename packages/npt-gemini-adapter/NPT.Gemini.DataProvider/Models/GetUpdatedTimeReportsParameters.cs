using System;
using System.ComponentModel.DataAnnotations;

namespace NPT.Gemini.DataProvider.Models
{
    public class GetUpdatedTimeReportsParameters
    {
        public DateTime timeStamp { get; set; }

        [Required]
        public string[] emails { get; set; }
    }
}