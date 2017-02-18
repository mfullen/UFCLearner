using CsvHelper.Configuration;

namespace UFCDataCreator.Model
{
    public class Fighter
    {
        public string Url { get; set; }
        public string Fid { get; set; }
        public string Name { get; set; }
        public string Nick { get; set; }
        public string Birth_Date { get; set; }
        public string Height { get; set; }
        public string Weight { get; set; }
        public string Association { get; set; }
        public string Class { get; set; }
        public string Locality { get; set; }
        public string Country { get; set; }
    }

    sealed class FighterDefinitionMap : CsvClassMap<Fighter>
    {
        public FighterDefinitionMap()
        {
            Map(m => m.Url).Name("url");
            Map(m => m.Fid).Name("fid");
            Map(m => m.Name).Name("name");
            Map(m => m.Nick).Name("nick");
            Map(m => m.Birth_Date).Name("birth_date");
            Map(m => m.Height).Name("height");
            Map(m => m.Weight).Name("weight");
            Map(m => m.Association).Name("association");
            Map(m => m.Class).Name("class");
            Map(m => m.Locality).Name("locality");
            Map(m => m.Country).Name("country");
        }
    }
}