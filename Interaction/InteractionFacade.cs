using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using WindowScrape.Types;

namespace Interaction
{
    public class WindowInfo
    {
        public string Title { get; set; }

        public Size Size { get; set; }

        public Bitmap Bitmap { get; set; }
    }

    public static class InteractionFacade
    {
        public static IEnumerable<WindowInfo> GetWindowList(Size size, params string[] searchStrings)
        {
            foreach (string searchString in searchStrings)
            {
                foreach (HwndObject window in HwndObject.GetWindows().Where(w => w.Title.StartsWith(searchString)))
                {
                    // And make a screenshot
                    /*Bitmap bitmap = new Bitmap(window.Size.Width, window.Size.Height);
                    Graphics graphics = Graphics.FromImage(bitmap as Image);
                    graphics.CopyFromScreen(window.Location.X, window.Location.Y, 0, 0, bitmap.Size);*/

                    Bitmap bitmap = new Bitmap(size.Width, size.Height);
                    Graphics memoryGraphics = Graphics.FromImage(bitmap);
                    IntPtr dc = memoryGraphics.GetHdc();
                    bool success = Win32.PrintWindow(window.Hwnd, dc, 0);
                    memoryGraphics.ReleaseHdc(dc);

                    //yield return new WindowInfo { Bitmap = bitmap, Title = window.Title, Location = window.Location, Size = window.Size };
                    yield return new WindowInfo { Title = window.Title, Size = window.Size, Bitmap = bitmap };
                }
            }
        }

        public static bool EnsureWindowSize(WindowInfo window, Size targetSize)
        {
            if (targetSize != window.Size)
            {
                HwndObject.GetWindowByTitle(window.Title).Size = targetSize;
                return true;
            }

            return false;
        }
    }
}

