using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace PokerEvaluators
{
    public class Badugi : APokerEvaluationBaseClass, IPokerEvaluator
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

            // Check for pairs
            if (m_evaluationhelper.bitcount_table[ranks] == 4)
            {
                // No pairs, remove excess flushed cards
                RemoveExcessFlushCards(ref clubs);
                RemoveExcessFlushCards(ref hearts);
                RemoveExcessFlushCards(ref spades);
                RemoveExcessFlushCards(ref diamonds);

                ranks = clubs | spades | diamonds | hearts;
                
                if (m_evaluationhelper.bitcount_table[ranks] == 1)
                    HiResult = ONECARDHAND | ranks;
                else if (m_evaluationhelper.bitcount_table[ranks] == 2)
                    HiResult = TWOCARDHAND | ranks;
                else if (m_evaluationhelper.bitcount_table[ranks] == 3)
                    HiResult = THREECARDHAND | ranks;
                else if (m_evaluationhelper.bitcount_table[ranks] == 4)
                    HiResult = FOURCARDHAND | ranks;
                return;
            }
            // Remove excess paired cards
            RemovePairs(ref clubs, ref hearts);
            RemovePairs(ref clubs, ref spades);
            RemovePairs(ref clubs, ref diamonds);
            RemovePairs(ref hearts, ref spades);
            RemovePairs(ref hearts, ref diamonds);
            RemovePairs(ref spades, ref diamonds);
            // Remove excess paired cards if we happens to have two pairs dubbelsuited
            RemovePairs(ref clubs, ref hearts);
            RemovePairs(ref clubs, ref spades);
            RemovePairs(ref clubs, ref diamonds);
            RemovePairs(ref hearts, ref spades);
            RemovePairs(ref hearts, ref diamonds);
            RemovePairs(ref spades, ref diamonds);

            RemoveExcessFlushCards(ref clubs);
            RemoveExcessFlushCards(ref hearts);
            RemoveExcessFlushCards(ref spades);
            RemoveExcessFlushCards(ref diamonds);

            ranks = clubs | spades | diamonds | hearts;
            
            if (m_evaluationhelper.bitcount_table[ranks] == 1)
                HiResult = ONECARDHAND | ranks;
            else if (m_evaluationhelper.bitcount_table[ranks] == 2)
                HiResult = TWOCARDHAND | ranks;
            else if (m_evaluationhelper.bitcount_table[ranks] == 3)
                HiResult = THREECARDHAND | ranks;
            else if (m_evaluationhelper.bitcount_table[ranks] == 4)
                HiResult = FOURCARDHAND | ranks;
            return;
        }

        private void RemoveExcessFlushCards(ref ulong flush)
        {
            while (m_evaluationhelper.bitcount_table[flush] > 1)
                flush ^= m_evaluationhelper.msb_table[flush];
        }

        private void RemovePairs(ref ulong flush_1, ref ulong flush_2)
        {
            if ((flush_1 & flush_2) > 0)
            {
                if (m_evaluationhelper.bitcount_table[flush_1] > 1)
                    flush_1 ^= m_evaluationhelper.msb_table[flush_1 & flush_2];
                else if (m_evaluationhelper.bitcount_table[flush_2] > 1)
                    flush_2 ^= m_evaluationhelper.msb_table[flush_1 & flush_2];
                else
                    flush_1 = 0;
            }
        }
        #endregion
    }
}
