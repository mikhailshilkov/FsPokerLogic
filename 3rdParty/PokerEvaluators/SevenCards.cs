using System;

namespace PokerEvaluators
{
	/// <summary>
	/// Inherits from APokerEvaluationBaseClass and implements IPokerGame, used for evaluation of Holdem, Seven card stud and Seven card stud Hi / Lo
	/// </summary>
	public class SevenCards : APokerEvaluationBaseClass, IPokerEvaluator
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
		/// <summary>
		/// Pre-calculated results for highcard, straight, flush and straightflush
		/// </summary>
		private readonly ulong[] m_topthree = new ulong[8192];
		/// <summary>
		/// Placeholder for if this is a low-game
		/// </summary>
		bool m_ishighlow = false;
		#endregion
        /// <summary>
        /// Constructor, init all tables with data required for this class
        /// </summary>
        public SevenCards()
        {
            ulong[] straighttable = { 0x100f, 0x1f, 0x3e, 0x7c, 0xf8, 0x1f0, 0x3e0, 0x7c0, 0xf80, 0x1f00 };
            for (short i = 0; i < 8192; i++)
            {
                ulong j = 0;
                m_evaluatedresults[i] = 0;
                if (m_evaluationhelper.bitcount_table[i] >= 5)
                {
                    j = (ulong)i;
                    while (m_evaluationhelper.bitcount_table[j] > 5)
                        j = j ^ (ulong)m_evaluationhelper.lsb_table[j];
                    m_evaluatedresults[i] = FLUSH | HIGHCARD | j;
                    for (int k = (straighttable.Length - 1); k >= 0; k--)
                        if ((ulong)((ulong)i & straighttable[k]) == straighttable[k])
                        {
                            m_evaluatedresults[i] = STRAIGHT | STRAIGHTFLUSH | (k == 0 ? straighttable[k] & 15 : straighttable[k]);
                            break;
                        }
                }
                j = (ulong)i;
                while (m_evaluationhelper.bitcount_table[j] > 3)
                    j ^= (ulong)m_evaluationhelper.lsb_table[j];
                if (m_evaluationhelper.bitcount_table[j] == 3)
                    m_topthree[i] = j;
            }
        }
		#region IPokerGame Members
		public bool IsHighLow { get { return m_ishighlow; } set { m_ishighlow = value; } }

		/// <summary>
		/// Evaluates a poker hand consisting of seven cards, return Hi / Lo result, used for gametypes like:
		/// Holdem, Seven card stud, Seven card stud Hi / Lo
		/// </summary>
		/// <param name="HiResult">
		/// The high result of the hand, higher result is better
		/// </param>
		/// <param name="LowResult">
		/// The low result of the hand, lower result is better, 256 means no low result
		/// </param>
		/// <param name="Hand">
		/// The cards hidden from other players
		/// </param>
		/// <param name="OpenCards">
		/// The open cards, eg. Flop, Turn, River and so on
		/// </param>
		public void Evaluate(ref ulong HiResult, ref short LowResult, ulong Hand, ulong OpenCards)
		{
			// Make sure that the we dont have any unwanted results
			LowResult = 256;
			HiResult = 0;

			// Construct a hand of the table and hole-cards
			Hand |= OpenCards;

			// Extract ranks and possible flushes
			ulong clubs = Hand & 8191;
			ulong hearts = (Hand >> 13) & 8191;
			ulong spades = (Hand >> 26) & 8191;
			ulong diamonds = (Hand >> 39) & 8191;
			ulong ranks = clubs | spades | diamonds | hearts;

			// Do we want to extract a 8-high hand-result?
			if (m_ishighlow)
				LowResult = EvaluateLowestHand(ranks);

			// Extract bitcount to be able to determine which hand is delt
			int bitcount = m_evaluationhelper.bitcount_table[ranks];
			// Do we have a flush, straight or highcard?
			if (bitcount >= 5)
			{
				// Yes, is it a flush or straight flush?
				if (m_evaluationhelper.bitcount_table[clubs] > 4)
					HiResult = m_evaluatedresults[clubs];
				else if (m_evaluationhelper.bitcount_table[diamonds] > 4)
					HiResult = m_evaluatedresults[diamonds];
				else if (m_evaluationhelper.bitcount_table[hearts] > 4)
					HiResult = m_evaluatedresults[hearts];
				else if (m_evaluationhelper.bitcount_table[spades] > 4)
					HiResult = m_evaluatedresults[spades];
				// Nope, do we have a straight or highcard only?
				else if (bitcount == 7 || (m_evaluatedresults[ranks] & STRAIGHT) == STRAIGHT)
					// Remove FLUSH, STRAIGHTFLUSH markers since we dont have any of them, the bits that are left represents either a highcard hand or a straight
					HiResult = m_evaluatedresults[ranks] & STRAIGHTHIGHCARDKICKERS; 
				// Did we evaluate the hand?
				if (HiResult > 0) 
					// Yes, return to the caller
					return;
			}
			// Nope, time to check for what it could be
			// Create a temporary variable for dynamic creation of the result
			ulong cardvalue;

			// Do we have a pair?
			if (bitcount == 6) 
			{
				// We have no flush, no straight and we beat highcard only, must be a pair
				// Extract the pair
				cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
				// Set ONEPAIR, which pair and all the kickers
				HiResult = ONEPAIR | cardvalue << 13 | m_topthree[ranks ^ cardvalue];
				return;
			}
			// This could be either two pairs or three of a kind
			else if (bitcount == 5) 
			{
				// Mask out two pairs, a bit faster than masking out three of a kind
				cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
				// Do we have a twopair?
				if (cardvalue != 0)
					// Yes, set TWOPAIR, which two pairs and add the kicker
					HiResult = TWOPAIR | cardvalue << 13 | (ulong)m_evaluationhelper.msb_table[ranks ^ cardvalue];
				else
				{
					// No, must be three of a kind, mask them out
					cardvalue = ((clubs & diamonds) | (hearts & spades)) & ((clubs & hearts) | (diamonds & spades));
					// Set THREEOFAKIND, which three of a kind and the kickers
					HiResult = THREEOFAKIND | cardvalue << 13 | (m_topthree[ranks ^ cardvalue] ^ (ulong)m_evaluationhelper.lsb_table[m_topthree[ranks ^ cardvalue]]);
				}
				return;
			}

			// Now it could be either four of a kind, full house, two three of a kind (which boils down to a full house) or three pairs (which boils down to two pairs with kicker)
			
			// Mask out four of a kind
			cardvalue = hearts & diamonds & clubs & spades;
			// Is it four of kind?
			if (cardvalue > 0)
			{
				// Yes, set FOUROFAKIND, which four of a kind and the kicker
				HiResult = FOUROFAKIND | cardvalue << 13 | (ulong)m_evaluationhelper.msb_table[ranks ^ cardvalue];
				return;
			}

			// Not four of a kind, mask out pairs for full house or three pairs
			cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
			bitcount = m_evaluationhelper.bitcount_table[cardvalue];
			// Is it three pairs?
			if (bitcount == 3)
			{
				// Yes, mask away the smallest pair
				cardvalue ^= (ulong) m_evaluationhelper.lsb_table[cardvalue];
				// Yes, set TWOPAIR, which two pairs and add the kicker
				HiResult = TWOPAIR | cardvalue << 13 | (ulong)m_evaluationhelper.msb_table[ranks ^ cardvalue];
				return;
			}
            // Do we have two pairs and one three of a kind
            else if (bitcount == 2)
			{
				// One three of a kind, mask out the best pair
				// Yes, set FULLHOUSE, mask out the three of a kind and the best pair
                HiResult = FULLHOUSE | (((clubs & diamonds) | (hearts & spades)) & ((clubs & hearts) | (diamonds & spades))) << 13 | (ulong)m_evaluationhelper.msb_table[cardvalue];
                return;
            }
            // Do we have one pair and one three of a kind
            else if (bitcount == 1)
            {
                HiResult = FULLHOUSE | (((clubs & diamonds) | (hearts & spades)) & ((clubs & hearts) | (diamonds & spades))) << 13 | cardvalue;
                return;
            }
			// We have two three of a kind and a kicker, mask out the better three of a kind and use the other as pair only
			cardvalue = ((clubs & diamonds) | (hearts & spades)) & ((clubs & hearts) | (diamonds & spades));
			ranks = m_evaluationhelper.lsb_table[cardvalue];
			// Set FULLHOUSE and join in the best three of a kind and the use the worst as pair only
			HiResult = FULLHOUSE | (cardvalue ^ ranks) << 13 | ranks;
			return;
		}
		#endregion
	}
}
