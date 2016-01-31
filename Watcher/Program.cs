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
            while (true)
            {
                Console.Write("Press any key to get the list of open tables...");
                Console.ReadKey();
                var windows = InteractionFacade.GetWindowList(screenSize, "Heads Up ").ToArray();
                Console.Write($"\n{windows.Length} tables found\n");
                foreach (WindowInfo window in windows)
                {
                    var parts = window.Title.Split('-').Select(s => s.Trim()).ToArray();
                    if (parts.Length >= 3)
                    {
                        var blinds = parts[1];
                        var tableNumber = parts[2];
                        Console.WriteLine($"{tableNumber} ({window.Size.Width}x{window.Size.Height})");
                        Console.WriteLine($"Blinds: {blinds}");

                        var resized = InteractionFacade.EnsureWindowSize(window, targetSize);

                        if (!resized)
                        {
                            try
                            {
                                var result = ScreenRecognition.recognizeScreen(window.Bitmap);
                                if (result.HeroStack > 0 && result.VillainStack > 0)
                                {
                                    Console.WriteLine($"Total pot: {result.TotalPot}");
                                    Console.WriteLine($"Stacks: {result.HeroStack}/{result.VillainStack}");
                                }
                                else
                                    SaveBitmap(window.Bitmap, tableNumber);

                                string heroBet = result.HeroBet > 0 ? result.HeroBet.ToString() : "-";
                                string villainBet = result.VillainBet > 0 ? result.VillainBet.ToString() : "-";
                                Console.WriteLine($"Bets: {heroBet}/{villainBet}");
                            }
                            catch
                            {
                                SaveBitmap(window.Bitmap, tableNumber);
                            }
                        }
                        else
                        {
                            Console.WriteLine($"Resizing...");
                        }
                    }
                    else
                    {
                        Console.WriteLine($"Error: strange table title: {window.Title}");
                    }
                }
                Console.Write("\n\n");
            }
        }

        private static void SaveBitmap(Bitmap bitmap, string tableName)
        {
            for (int i = 50; i < 150; i++)
                for (int j = 1; j < 30; j++)
                    bitmap.SetPixel(i, j, Color.White);
            for (int i = 77; i < 161; i++)
                for (int j = 328; j < 338; j++)
                    bitmap.SetPixel(i, j, Color.White);
            bitmap.Save($"{tableName}_{Guid.NewGuid().ToString().Substring(0, 4)}.bmp");
        }
    }
}
