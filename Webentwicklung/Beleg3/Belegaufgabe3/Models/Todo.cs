using System.ComponentModel.DataAnnotations;

namespace Belegaufgabe3.Models
{
    public class Todo
    { 
        public int ID { get; set; }

        [Required]
        public string titel { get; set; }

        [Required]
        public string beschreibung { get; set; }
    }
}

