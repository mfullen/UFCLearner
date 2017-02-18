using System;

namespace UFCDataCreator.Model
{
    public class FightEvent
    {
        public string pageUrl { get; set; }
        public int eid  { get; set; }
        public int mid { get; set; }
        public string event_name { get; set; }
        public string event_org { get; set; }
        public string event_date { get; set; }
        public string event_place { get; set; }
        public string f1pageurl { get; set; }
        public string f2pageurl { get; set; }
        public string f1name { get; set; }
        public string f2name { get; set; }
        public string f1result { get; set; }
        public string f2result { get; set; }
        public int f1fid { get; set; }
        public int f2fid { get; set; }
        public string method { get; set; }
        public string method_d { get; set; }
        public string reff { get; set; }
        public int round { get; set; }
        public string time { get; set; }
    }
}