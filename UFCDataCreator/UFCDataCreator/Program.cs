using System;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using CsvHelper;
using UFCDataCreator.Model;

namespace UFCDataCreator
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            var csvReader = new CsvReader(File.OpenText("/home/mfullen/Downloads/ufcfights.csv"));
            var fightEvents = new List<FightEvent>();

            var parser = new CsvParser(File.OpenText("/home/mfullen/Downloads/ufcfights.csv"));
            var index = 0;
            while (true)
            {
                var row = parser.Read();
                if (row == null)
                {
                    break;
                }

                if (index > 0)
                {
                    var fe = new FightEvent()
                    {
                        pageUrl = row[0],
                        eid = int.Parse(row[1]),
                        mid = int.Parse(row[2]),
                        event_name = row[3],
                        event_org = row[4],
                        event_date = row[5],
                        event_place = row[6],
                        f1pageurl = row[7],
                        f2pageurl =row[8],
                        f1name = row[9],
                        f2name = row[10],
                        f1result = row[11],
                        f2result = row[12],
                        f1fid = int.Parse(row[13]),
                        f2fid = int.Parse(row[14]),
                        method = row[15],
                        method_d = row[16],
                        reff = row[17]
                    };
                    fightEvents.Add(fe);
                }

                index++;
            }

            var fighters = new List<Fighter>();

            using (TextReader reader = File.OpenText("/home/mfullen/Downloads/ufcfighters.csv")) {
                var csv = new CsvReader(reader);
                csv.Configuration.RegisterClassMap<FighterDefinitionMap>();
                while (csv.Read()) {
                    var record = csv.GetRecord<Fighter>();
                    fighters.Add(record);
                }
            }

            var trainingData = new List<FighterData>();
            var random = new Random();
            foreach (var fightEvent in fightEvents)
            {
                var f1 = fighters.FirstOrDefault(f => f.Fid == fightEvent.f1fid.ToString());
                var f2 = fighters.FirstOrDefault(f => f.Fid == fightEvent.f2fid.ToString());

                if(f1 == null || f2 == null) continue;
                string result = null;
                switch (fightEvent.f1result)
                {
                    case "win":
                        result = "f1";
                        break;
                    case "loss":
                        result = "f2";
                        break;
                    default:
                        result = "tie";
                        break;
                }
                var td = new FighterData()
                {
                    EventDate = fightEvent.event_date,
                    Fighter1BirthDate = f1.Birth_Date,
                    Fighter1Country = f1.Country,
                    Fighter1Height = f1.Height,
                    Fighter1Id = f1.Fid,
                    Fighter1Name = f1.Name,
                    Figther1Weight = f1.Weight,

                    Fighter2BirthDate = f2.Birth_Date,
                    Fighter2Country = f2.Country,
                    Fighter2Height = f2.Height,
                    Fighter2Id = f2.Fid,
                    Fighter2Name = f2.Name,
                    Figther2Weight = f2.Weight,
                    Ref = fightEvent.reff,
                    Result = result
                };

                var shuffleWinner = random.NextDouble();

                if (shuffleWinner >= 0.5)
                {
                    if (result == "f1")
                    {
                        result = "f2";
                    }else if (result == "f2")
                    {
                        result = "f1";
                    }
                    td = new FighterData()
                    {
                        EventDate = fightEvent.event_date,
                        Fighter2BirthDate = f1.Birth_Date,
                        Fighter2Country = f1.Country,
                        Fighter2Height = f1.Height,
                        Fighter2Id = f1.Fid,
                        Fighter2Name = f1.Name,
                        Figther2Weight = f1.Weight,

                        Fighter1BirthDate = f2.Birth_Date,
                        Fighter1Country = f2.Country,
                        Fighter1Height = f2.Height,
                        Fighter1Id = f2.Fid,
                        Fighter1Name = f2.Name,
                        Figther1Weight = f2.Weight,
                        Ref = fightEvent.reff,
                        Result = result
                    };
                }

                trainingData.Add(td);
            }

            var csvw = new CsvWriter( File.CreateText("training.csv"));
            csvw.WriteRecords( trainingData );

            Console.ReadKey();
        }
    }
}