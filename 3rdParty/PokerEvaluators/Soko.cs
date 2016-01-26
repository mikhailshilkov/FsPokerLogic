using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace PokerEvaluators
{
    public class Soko : APokerEvaluationBaseClass, IPokerEvaluator
    {
		#region Private declarations
		/// <summary>
		/// The helper class
		/// </summary>
		private EvaluationHelper m_evaluationhelper = EvaluationHelper.Instance;
		/// <summary>
		/// Pre-evaluated results for highcard, straight, flush and straightflush
		/// </summary>
        private readonly ulong[] m_evaluatedresults = new ulong[8192];
        private readonly ulong[] m_sokoevaluatedresults = new ulong[8192];
        /// <summary>
		/// Placeholder for if this is a low-game
		/// </summary>
		bool m_ishighlow = false;
		#endregion

        /// <summary>
        /// Constructor, init all tables with data required for this class
        /// </summary>
        public Soko()
        {
            ulong[] straighttable = { 0x100f, 0x1f, 0x3e, 0x7c, 0xf8, 0x1f0, 0x3e0, 0x7c0, 0xf80, 0x1f00 };
            ulong[] fourstraighttable = { 0x1007, 0x0f, 0x1e, 0x3c, 0x78, 0xf0, 0x1e0, 0x3c0, 0x780, 0xf00, 0x1e00 };
			for (short i = 0; i < 8192; i++)
			{
				m_evaluatedresults[i] = 0;
                // Precalc our lookup table if i equals five bits it should contain FLUSH, STRAIGHTFLUSH, HIGHCARD or STRAIGHT
                if (m_evaluationhelper.bitcount_table[i] == 5)
				{
                    // Assume it isnt a straight, then it only can be FLUSH or HIGHCARD
                    m_evaluatedresults[i] = FLUSH | HIGHCARD | (ulong)(ushort)i;
                    for (int k = (straighttable.Length - 1); k >= 0; k--)
                        // Check each entry in the straight table for matches
                        if ((ulong)((ulong)i & straighttable[k]) == straighttable[k])
                        {
                            // We had a match then this five bit combo is either a straight or straightflush (depending of where we fetch it)
                            m_evaluatedresults[i] = STRAIGHT | STRAIGHTFLUSH | (k == 0 ? 0x0f : straighttable[k]);
                            break;
                        }
                }
                if (m_evaluationhelper.bitcount_table[i] >= 4)
                {
                    m_sokoevaluatedresults[i] = 0;
                    for (int k = (fourstraighttable.Length - 1); k >= 0; k--)
                        if ((ulong)((ulong)i & fourstraighttable[k]) == fourstraighttable[k])
                        {
                            m_sokoevaluatedresults[i] = FOURSTRAIGHT | ((k == 0 ? 0x07 : fourstraighttable[k]) << 13);
                            break;
                        }

                }
			}        
        }


        #region IPokerGame Members
        public bool IsHighLow { get { return m_ishighlow; } set { m_ishighlow = value; } }

		public void Evaluate(ref ulong HiResult, ref short LowResult, ulong Hand, ulong OpenCards)
		{
			LowResult = 256;
			HiResult = 0;

			Hand |= OpenCards;

            ulong clubs = Hand & 8191; 
            ulong hearts = (Hand >> 13) & 8191; 
            ulong spades = (Hand >> 26) & 8191; 
            ulong diamonds = (Hand >> 39) & 8191; 
			ulong ranks = clubs | spades | diamonds | hearts; 

			if (m_ishighlow)
				LowResult = EvaluateLowestHand(ranks);

			int bitcount = m_evaluationhelper.bitcount_table[ranks];

            if (bitcount == 5)
			{
                if (m_evaluationhelper.bitcount_table[clubs] == 5)
                    HiResult = m_evaluatedresults[clubs];
                else if (m_evaluationhelper.bitcount_table[diamonds] == 5)
                    HiResult = m_evaluatedresults[diamonds];
                else if (m_evaluationhelper.bitcount_table[hearts] == 5)
                    HiResult = m_evaluatedresults[hearts];
                else if (m_evaluationhelper.bitcount_table[spades] == 5)
                    HiResult = m_evaluatedresults[spades];
                else if ((m_evaluatedresults[ranks] & STRAIGHT) == STRAIGHT)
                    HiResult = m_evaluatedresults[ranks] & STRAIGHTHIGHCARDKICKERS;
                else if (m_evaluationhelper.bitcount_table[clubs] == 4)
                    HiResult = FOURFLUSH | clubs << 13 | hearts | spades | diamonds;
                else if (m_evaluationhelper.bitcount_table[hearts] == 4)
                    HiResult = FOURFLUSH | hearts << 13 | clubs | spades | diamonds;
                else if (m_evaluationhelper.bitcount_table[spades] == 4)
                    HiResult = FOURFLUSH | spades << 13 | clubs | hearts | diamonds;
                else if (m_evaluationhelper.bitcount_table[diamonds] == 4)
                    HiResult = FOURFLUSH | diamonds << 13 | clubs | hearts | spades;
                else if (m_sokoevaluatedresults[ranks] > 0)
                {
                    if ((ranks & 4097) == 4097)
                        HiResult = m_sokoevaluatedresults[ranks] | (ranks ^ 0x1007);
                    else
                        HiResult = m_sokoevaluatedresults[ranks] | (ranks ^ ((m_sokoevaluatedresults[ranks] & 67100672) >> 13));
                }
                else
                    HiResult = m_evaluatedresults[ranks] & STRAIGHTHIGHCARDKICKERS;
                return;
			}
            else if (bitcount == 4)
            {
                if (m_evaluationhelper.bitcount_table[clubs] == 4)
                    HiResult = FOURFLUSH | clubs << 13 | hearts | spades | diamonds;
                else if (m_evaluationhelper.bitcount_table[hearts] == 4)
                    HiResult = FOURFLUSH | hearts << 13 | clubs | spades | diamonds;
                else if (m_evaluationhelper.bitcount_table[spades] == 4)
                    HiResult = FOURFLUSH | spades << 13 | clubs | hearts | diamonds;
                else if (m_evaluationhelper.bitcount_table[diamonds] == 4)
                    HiResult = FOURFLUSH | diamonds << 13 | clubs | hearts | spades;
                else if(m_sokoevaluatedresults[ranks] > 0)
                    HiResult = m_sokoevaluatedresults[ranks] | (ranks ^ (clubs ^ diamonds ^ hearts ^ spades));
                if (HiResult != 0)
                    return;
            }

            ulong cardvalue;
            			
            if (bitcount == 4)
			{
                cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
				HiResult = ONEPAIR | cardvalue << 13 | (ranks ^ cardvalue);
				return;
			}
			else if (bitcount == 3)
			{
				cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
				if (cardvalue != 0)
                    HiResult = TWOPAIR | cardvalue << 13 | (ranks ^ cardvalue);
				else
				{
                    cardvalue = ((clubs & diamonds) | (hearts & spades)) & ((clubs & hearts) | (diamonds & spades));
					HiResult = THREEOFAKIND | cardvalue << 13 | (ranks ^ cardvalue);
				}
				return;
			}
			cardvalue = hearts & diamonds & clubs & spades;
			if (cardvalue > 0)
			{
				HiResult = FOUROFAKIND | cardvalue << 13 | (ranks ^ cardvalue);
				return;
			}
            cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
            HiResult = FULLHOUSE | (ranks ^ cardvalue) << 13 | cardvalue;
			return;
		}
		#endregion
    }
}
