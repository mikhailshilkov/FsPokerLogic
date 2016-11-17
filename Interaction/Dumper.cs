using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interaction
{
    public static class Dumper
    {
        public static void SaveBitmap(Bitmap bitmap, string fileName, bool maskIpoker)
        {
            if (maskIpoker)
            {
                for (int i = 50; i < 150; i++)
                    for (int j = 1; j < 30; j++)
                        bitmap.SetPixel(i, j, Color.White);
                for (int i = 77; i < 161; i++)
                    for (int j = 328; j < 338; j++)
                        bitmap.SetPixel(i, j, Color.White);
            }
            bitmap.Save($"{fileName}.bmp");
        }
    }
}
