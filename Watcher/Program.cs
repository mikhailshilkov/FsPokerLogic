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
            var screenSize = new Size(816, 577);
            var targetSize = new Size(816, 577);
            Console.Write("Press any key to get the list of open tables...");
            Console.ReadKey();

            var party = new WindowExtractor("Party", title =>
            {
                var startIndex = title.IndexOf("Heads Up Hyper Turbo (") + 22;
                var endIndex = title.IndexOf(") Table");
                return startIndex > 0 && endIndex > startIndex ? title.Substring(startIndex, endIndex - startIndex) : null;
            });

            while (true)
            {
                var windows = InteractionFacade.GetWindowList(screenSize, targetSize, party).ToArray();
                foreach (WindowInfo window in windows)
                {
                    var tableNumber = window.TableName;
                    Console.WriteLine($"\n{tableNumber} ({window.Size.Width}x{window.Size.Height})");

                    //try
                    //{
                    //    var result = WinamaxRecognition.recognizeScreenWinamax(window.Bitmap, window.Title);
                    //    foreach (var s in ScreenRecognition.print(result))
                    //        Console.WriteLine(s);
                    //    var result2 = WinamaxRecognition.recognizeBetSizeWinamax(window.Bitmap);
                    //    Console.WriteLine($"Hero entered bet size: {result2}");
                    //}
                    //catch (Exception ex)
                    //{
                    //    Console.WriteLine(ex.Message);
                    //    Console.WriteLine(ex.StackTrace);
                    //    Dumper.SaveBitmap(window.Bitmap, tableNumber, false);
                    //}
                }
                Console.Write("\n\n");
                Console.Write("Press S to save images or any key to get the list of open tables...");
                var key = Console.ReadKey();
                if (key.KeyChar == 'S' || key.KeyChar == 's')
                {
                    foreach (WindowInfo window in windows)
                    {
                        Dumper.SaveBitmap(window.Bitmap, window.TableName + Guid.NewGuid().ToString().Substring(0, 6), false);
                    }
                }
            }
        }
    }
}
