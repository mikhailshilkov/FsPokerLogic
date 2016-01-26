using System;

namespace PokerEvaluators
{
	public class FiveCards:APokerEvaluationBaseClass, IPokerEvaluator
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
		/// Placeholder for if this is a low-game
		/// </summary>
		bool m_ishighlow = false;
		#endregion

        /// <summary>
        /// Constructor, init all tables with data required for this class
        /// </summary>
        public FiveCards()
        {
			ulong[] straighttable = { 0x100f, 0x1f, 0x3e, 0x7c, 0xf8, 0x1f0, 0x3e0, 0x7c0, 0xf80, 0x1f00 };
			for (short i = 0; i < 8192; i++)
			{
				ulong j = 0;
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
							m_evaluatedresults[i] = STRAIGHT | STRAIGHTFLUSH | (k == 0 ? straighttable[j] & 15 : straighttable[k]);
							break;
						}
				}
			}        
        }


        #region IPokerGame Members
        public bool IsHighLow { get { return m_ishighlow; } set { m_ishighlow = value; } }

		public void Evaluate(ref ulong HiResult, ref short LowResult, ulong Hand, ulong OpenCards)
		{
			// Make sure that the we dont have any unwanted results
			LowResult = 256;
			HiResult = 0;

			// Construct a hand of the table and hole-cards
			Hand |= OpenCards;

			// Extract ranks and possible flushes
            ulong clubs = Hand & 8191; // Get the bits 0 - 12
            ulong hearts = (Hand >> 13) & 8191; // Get the bits 13 - 25
            ulong spades = (Hand >> 26) & 8191; // Get the bits 26 - 38
            ulong diamonds = (Hand >> 39) & 8191; // Get the bits 39 - 51
			ulong ranks = clubs | spades | diamonds | hearts; // Or all bits together so you have all unique ranks within bit 0 - 12

			// Do we want to extract a 8-high hand-result?
			if (m_ishighlow)
				LowResult = EvaluateLowestHand(ranks);

			// Extract bitcount to be able to determine which hand is delt
			int bitcount = m_evaluationhelper.bitcount_table[ranks];
			// Do we have a flush, straight or highcard?
			if (bitcount == 5)
			{
				// Yes, is it a flush or straight flush?
                // If we have 5 bits set within clubs then we know we have a flush or a straight flush
				if (m_evaluationhelper.bitcount_table[clubs] == 5)
                    // Then we fetch the value from our precalculated lookup table where straightflush and flush already are set for each five bit combo
					HiResult = m_evaluatedresults[clubs];
				else if (m_evaluationhelper.bitcount_table[diamonds] == 5)
					HiResult = m_evaluatedresults[diamonds];
				else if (m_evaluationhelper.bitcount_table[hearts] == 5)
					HiResult = m_evaluatedresults[hearts];
				else if (m_evaluationhelper.bitcount_table[spades] == 5)
					HiResult = m_evaluatedresults[spades];
				// Nope, then we have a straight or highcard only!
                // Since it has five unique bits set and isnt a flush or a straight flush it must be a straight or highcard
				else
					// Remove the STRAIGHTFLUSH and FLUSH markers since we dont have any of them, the bits that are left represents either a highcard hand or a straight
					HiResult = m_evaluatedresults[ranks] & STRAIGHTHIGHCARDKICKERS;
				// Here we have evaluated the hand?
				return;
			}
			// Nope, time to check for what it could be
			// Create a temporary variable for dynamic creation of the result
			ulong cardvalue;

			// Do we have a pair?
            // If we have four ranks it have to be one pair and three kickers inside our hand
			if (bitcount == 4)
			{
				// We have no flush, no straight and we beat highcard only, must be a pair
				// Extract the pair
                // ^ is XOR, 1^1=0, 1^0=1, 0^0=0
                // Use the ^ gate to extract the pair, Example: we have 2c, 2d, 3c, 4c and 5c
                // rank will then be (in binary) %01111, clubs will %01111 and diamonds will be %00001
                // (clubs ^ diamonds ^ hearts ^ spades) = %01111 ^ %00000 ^ %00000 ^ %00001 = %01110
                // ranks ^ %01110 = %01111 ^ %01110 = %00001 (which effectivly mean 2c and 2d)
                cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
                // Find out the kickers:
                // ranks ^ %00001 = %01111 ^ %00001 =  ^ %01110
                // Shift the pair to the left by 13 steps to make the kickers count
                // Set ONEPAIR, which pair and all the kickers
				HiResult = ONEPAIR | cardvalue << 13 | (ranks ^ cardvalue);
				return;
			}
			// This could be either two pairs or three of a kind
            // Three bits in ranks means one or two stray cards and two pairs or three of a kind
			else if (bitcount == 3)
			{
				// Mask out two pairs, a bit faster than masking out three of a kind
                // Two pairs work in the same way as one pair except that you get two bits left if it is two pairs
                // is it a three of a kind all bits will be cleared
				cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
				// Do we have a twopair?
				if (cardvalue != 0)
					// Yes, set TWOPAIR, which two pairs and add the kicker
                    // Kickers are foun exactly like in one pair
                    HiResult = TWOPAIR | cardvalue << 13 | (ranks ^ cardvalue);
				else
				{
					// No, must be three of a kind, mask them out
                    // To find three of a kind (2c, 2d, 2h, 3c, 4c for example) we use | (or) and & (and)
                    // 1|1=1, 1|0=1, 0|0=0
                    // 1&1=1, 0&0=0, 1&0=0
                    // (clubs & diamonds) = %0111 & %0001 = %0001
                    // (hearts & spades) = %0001 & %0000 = %0000
                    // ((clubs & diamonds) | (hearts & spades)) = %0001 | %0000 = %0001
                    // (clubs & hearts) = %0111 & %0001 = %0001
                    // (diamonds & spades) = %0001 & %0000 = %0000
                    // ((clubs & hearts) | (diamonds & spades)) = %0001 | %0000 = %0001
                    // ((clubs & diamonds) | (hearts & spades)) & ((clubs & hearts) | (diamonds & spades)) = %0001 & %0001 = %0001
                    // %0001 equals to three dueces
                    cardvalue = ((clubs & diamonds) | (hearts & spades)) & ((clubs & hearts) | (diamonds & spades));
					// Set THREEOFAKIND, which three of a kind and the kickers
                    // Kickers are foun exactly like in one pair
					HiResult = THREEOFAKIND | cardvalue << 13 | (ranks ^ cardvalue);
				}
				return;
			}

			// Now it could be either four of a kind or a full house

			// Mask out four of a kind
            // 2c 3c 2d 2h 2s = %0011 & %0001 & %0001 & %0001 = %0001, four dueces
            // 2c 3c 3d 2h 2s = %0001 & %0010 & %0001 & %0001 = %0000, three dueces and two threes
            cardvalue = hearts & diamonds & clubs & spades;
			// Is it four of kind?
			if (cardvalue > 0)
			{
				// Yes, set FOUROFAKIND, which four of a kind and the kicker
				HiResult = FOUROFAKIND | cardvalue << 13 | (ranks ^ cardvalue);
				return;
			}
			// Not four of a kind, mask out the pair for full house
            // Here we just repeat the three of a kind magic and the pair magic and join them together
            cardvalue = ranks ^ (clubs ^ diamonds ^ hearts ^ spades);
			// Set FULLHOUSE, mask out the trips and add the pair as a kicker
            // We cheat a bit since we have five cards only and if one ranks is not a pair it must be trips
            HiResult = FULLHOUSE | (ranks ^ cardvalue) << 13 | cardvalue;
			return;
		}
		#endregion
	}
}
