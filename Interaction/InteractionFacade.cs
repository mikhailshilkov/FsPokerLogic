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
        public static IEnumerable<WindowInfo> GetWindowList(Size screenSize, Size targetSize, params string[] searchStrings)
        {
            foreach (string searchString in searchStrings)
            {
                foreach (HwndObject window in HwndObject.GetWindows().Where(w => w.Title.StartsWith(searchString)))
                {
                    if (targetSize != window.Size)
                    {
                        HwndObject.GetWindowByTitle(window.Title).Size = targetSize;
                        continue;
                    }

                    Bitmap bitmap = new Bitmap(screenSize.Width, screenSize.Height);
                    Graphics memoryGraphics = Graphics.FromImage(bitmap);
                    IntPtr dc = memoryGraphics.GetHdc();
                    bool success = Win32.PrintWindow(window.Hwnd, dc, 0);
                    memoryGraphics.ReleaseHdc(dc);

                    yield return new WindowInfo
                    {
                        Title = window.Title,
                        Size = window.Size,
                        Bitmap = bitmap
                    };
                }
            }
        }
    }
}

