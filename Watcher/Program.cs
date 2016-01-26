using Interaction;
using System;
using System.Drawing;
using System.Linq;

namespace Watcher
{
    class Program
    {
        static void Main(string[] args)
        {
            var targetSize = new Size(650, 490);
            while (true)
            {
                Console.Write("Press any key to get the list of open tables...");
                Console.ReadKey();
                var windows = InteractionFacade.GetWindowList("Heads Up ").ToArray();
                Console.Write($"\n{windows.Length} tables found\n");
                foreach (WindowInfo window in windows)
                {
                    Console.WriteLine($"{window.Title} ({window.Size.Width}x{window.Size.Height})");
                    var resized = InteractionFacade.EnsureWindowSize(window, targetSize);

                    if (!resized)
                    {
                        window.Bitmap.Save(Guid.NewGuid().ToString().Substring(0, 6) + ".bmp");
                    }
                }
                Console.Write("\n\n");
            }
        }
    }
}
