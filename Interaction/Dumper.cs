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
        public static void SaveBitmap(Bitmap bitmap, string tableName)
        {
            for (int i = 50; i < 150; i++)
                for (int j = 1; j < 30; j++)
                    bitmap.SetPixel(i, j, Color.White);
            for (int i = 77; i < 161; i++)
                for (int j = 328; j < 338; j++)
                    bitmap.SetPixel(i, j, Color.White);
            for (int i = 495; i < 579; i++)
                for (int j = 328; j < 338; j++)
                    bitmap.SetPixel(i, j, Color.White);
            bitmap.Save($"{tableName}_{Guid.NewGuid().ToString().Substring(0, 4)}.bmp");
        }
    }
}
