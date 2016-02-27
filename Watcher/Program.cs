using Interaction;
using System;
using System.Drawing;
using System.Linq;
using Recognition;

namespace Watcher
{
    class Program
    {
        static void Main(string[] args)
        {
            //var image = new Bitmap(@"C:\Users\kiwo_000\Downloads\Pkr\870727716_2ea9.bmp");
            //var result2 = ScreenRecognition.recognizeScreen(image);

            //var screenSize = new Size(650, 490);
            //var targetSize = new Size(433, 328);
            var screenSize = new Size(650, 490);
            var targetSize = new Size(650, 490);
            Console.Write("Press any key to get the list of open tables...");
            Console.ReadKey();
            while (true)
            {
                var windows = InteractionFacade.GetWindowList(screenSize, targetSize, "Heads Up ").ToArray();
                foreach (WindowInfo window in windows)
                {
                    var parts = window.Title.Split('-').Select(s => s.Trim()).ToArray();
                    if (parts.Length >= 3)
                    {
                        var tableNumber = parts[2];
                        Console.WriteLine($"\n{tableNumber} ({window.Size.Width}x{window.Size.Height})");

                        try
                        {
                            var result = ScreenRecognition.recognizeScreen(window.Bitmap);
                            foreach (var s in ScreenRecognition.print(result))
                                Console.WriteLine(s);
                            //Console.WriteLine($"Total pot: {result.TotalPot}");
                            //Console.WriteLine($"Stacks: {result.HeroStack}/{result.VillainStack}");
                            //Console.WriteLine($"Bets: {result.HeroBet}/{result.VillainBet}");
                            //Console.WriteLine($"Hand: {result.HeroHand}({result.Button} IP)");
                            //Console.WriteLine($"Actions: {result.Actions}");
                        }
                        catch (Exception ex)
                        {
                            Console.WriteLine(ex.Message);
                            Console.WriteLine(ex.StackTrace);
                            Dumper.SaveBitmap(window.Bitmap, tableNumber);
                        }
                    }
                }
                Console.Write("\n\n");
                Console.Write("Press S to save images or any key to get the list of open tables...");
                var key = Console.ReadKey();
                if (key.KeyChar == 'S' || key.KeyChar == 's')
                {
                    foreach (WindowInfo window in windows)
                    {
                        var parts = window.Title.Split('-').Select(s => s.Trim()).ToArray();
                        if (parts.Length >= 3)
                            Dumper.SaveBitmap(window.Bitmap, parts[2]);
                    }
                }
            }
        }
    }
}
