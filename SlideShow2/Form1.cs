using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace SlideShow2
{
    public partial class Form1 : Form
    {
        private int index = 0;

        public Form1()
        {
            InitializeComponent();
            this.Text = $"Heads Up - 10/20 - {Guid.NewGuid().ToString().Substring(0, 6)}";
            this.Load += (s, ea) =>
            {
                var r = new Random();
                this.Location = new Point(r.Next(500), r.Next(200));
            };
        }

        private void Form1_ResizeEnd(object sender, EventArgs e)
        {
            pictureBox1.Width = this.Width;
            pictureBox1.Height = this.Height;
        }

        private void pictureBox1_Click(object sender, EventArgs e)
        {
            NextImage();
        }

        private void NextImage()
        {
            index += 1;
            string path = $@"C:\Work\FsPokerLogic\SlideShow2\bin\Debug\{index + 1}.png";
            if (File.Exists(path))
            {
                pictureBox1.ImageLocation = path;
            }
            else
            {
                path = $@"C:\Work\FsPokerLogic\SlideShow2\bin\Debug\{index + 1}.bmp";
                if (File.Exists(path))
                {
                    pictureBox1.ImageLocation = path;
                }
                else
                {
                    index = -1;
                    NextImage();
                }
            }
        }
    }
}
