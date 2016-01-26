using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace PokerEvaluators
{
    public class Razz : APokerEvaluationBaseClass, IPokerEvaluator
    {
        #region Private declarations
        /// <summary>
        /// The helper class
        /// </summary>
        private EvaluationHelper m_evaluationhelper = EvaluationHelper.Instance;
        #endregion

        #region IPokerEvaluator Members

        public bool IsHighLow { get { return false; } set { return; } }

        public void Evaluate(ref ulong HiResult, ref short LowResult, ulong Hand, ulong OpenCards)
        {
            LowResult = 256;
            HiResult = STRAIGHTFLUSH;
            Hand |= OpenCards;
            if ((Hand & 0x8004002001000) > 0)
            {
                ulong aces = Hand & 0x8004002001000;
                Hand ^= aces;
                Hand <<= 1;
                aces >>= 12;
                Hand |= aces;
            }
            else
                Hand <<= 1;
            ulong clubs = Hand & 8191;
            ulong hearts = (Hand >> 13) & 8191;
            ulong spades = (Hand >> 26) & 8191;
            ulong diamonds = (Hand >> 39) & 8191;
            ulong ranks = clubs | spades | diamonds | hearts;
            // Do we have five lowcards?
            if (m_evaluationhelper.bitcount_table[ranks] >= 5)
            {
                RemoveExcessCards(ref ranks);
                HiResult = ranks;
                return;
            }
            else if (m_evaluationhelper.bitcount_table[ranks] == 4)
            {
                ulong cardvalue = hearts & diamonds & clubs & spades;
                if (cardvalue > 0)
                {
                    // 4 + 1 + 1 + 1
                    HiResult = ONEPAIR | cardvalue << 13 | (ranks ^ cardvalue);
                    return;
                }
                cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
                if (m_evaluationhelper.bitcount_table[cardvalue] == 3)
                {
                    // 2 + 2 + 2 + 1
                    HiResult = ONEPAIR | (ulong) m_evaluationhelper.lsb_table[cardvalue] << 13 | (ranks ^ m_evaluationhelper.lsb_table[cardvalue]);
                }
                // One pair and trips
                else if (m_evaluationhelper.bitcount_table[cardvalue] == 1)
                {
                    // 2 + 3 + 1 + 1
                    cardvalue |= ((clubs & diamonds) | (hearts & spades)) & ((clubs & hearts) | (diamonds & spades));
                    HiResult = ONEPAIR | (ulong) m_evaluationhelper.lsb_table[cardvalue] << 13 | (ranks ^ m_evaluationhelper.lsb_table[cardvalue]);
                }
                return;
            }
            else if (m_evaluationhelper.bitcount_table[ranks] == 3)
            {
                ulong cardvalue = hearts & diamonds & clubs & spades;
                if (cardvalue > 0)
                {
                    // 4 + 2 + 1
                    cardvalue |= (ranks ^ (clubs ^ diamonds ^ hearts ^ spades));
                    HiResult = TWOPAIR | cardvalue << 13 | m_evaluationhelper.lsb_table[ranks ^ cardvalue];
                    return;
                }
                cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
                if (cardvalue != 0)
                {
                    // 2 + 3 + 2
                    cardvalue |= (ranks ^ cardvalue);
                    HiResult = TWOPAIR | (cardvalue ^ m_evaluationhelper.lsb_table[cardvalue]) << 13 | m_evaluationhelper.msb_table[cardvalue];
                }
                else
                {
                    // 3 + 3 + 1
                    cardvalue = ((clubs & diamonds) | (hearts & spades)) & ((clubs & hearts) | (diamonds & spades));
                    HiResult = TWOPAIR | cardvalue << 13 | (ranks ^ cardvalue);
                }
                return;
            }
            // 4 + 3
            ranks = (hearts & diamonds & clubs & spades) | (((clubs & diamonds) | (hearts & spades)) & ((clubs & hearts) | (diamonds & spades)));
            HiResult = FULLHOUSE | (ulong)m_evaluationhelper.lsb_table[ranks] << 13 | m_evaluationhelper.msb_table[ranks];
            return;
        }
        

        private void RemoveExcessCards(ref ulong cards)
        {
            while (m_evaluationhelper.bitcount_table[cards] > 5)
                cards ^= m_evaluationhelper.msb_table[cards];
        }
        #endregion
    }
}
