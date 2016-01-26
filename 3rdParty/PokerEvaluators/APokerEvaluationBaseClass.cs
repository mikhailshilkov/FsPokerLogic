namespace PokerEvaluators
{
	/// <summary>
	/// Abstract class containing functions for low-evaluation, constants for hand-evaluation and functions for interpreting cards and evaluated hands
	/// </summary>
	public abstract class APokerEvaluationBaseClass
	{
		// Constants used to determine the value of a hand
		public const ulong STRAIGHTFLUSH = (ulong)(1UL << 63);
		public const ulong FOUROFAKIND = (ulong)(1UL << 62);
		public const ulong FULLHOUSE = (ulong)(1UL << 61);
		public const ulong FLUSH = (ulong)(1UL << 60);
		public const ulong STRAIGHT = (ulong)(1UL << 59);
		public const ulong THREEOFAKIND = (ulong)(1UL << 58);
		public const ulong TWOPAIR = (ulong)(1UL << 57);
        public const ulong FOURFLUSH = (ulong)(1UL << 56);          // Sökö
        public const ulong FOURSTRAIGHT = (ulong)(1UL << 55);       // Sökö
		public const ulong ONEPAIR = (ulong)(1UL << 54);
        public const ulong HIGHCARD = (ulong)(1UL << 53);
		public const ulong STRAIGHTHIGHCARDKICKERS = HIGHCARD | STRAIGHT | 8191;

        public const ulong FOURCARDHAND = (ulong)(1UL << 53);       // Badugi
        public const ulong THREECARDHAND = (ulong)(1UL << 54);      // Badugi
        public const ulong TWOCARDHAND = (ulong)(1UL << 55);        // Badugi
        public const ulong ONECARDHAND = (ulong)(1UL << 56);        // Badugi

		/// <summary>
		/// Array containing the lowest cards for each combination of cards below 8
		/// </summary>
		private readonly short[] m_lowcards = new short[256];

		/// <summary>
		/// The helper class
		/// </summary>
		private EvaluationHelper m_evaluationhelper = EvaluationHelper.Instance;

		/// <summary>
		/// Initialize functions and arrays held by the baseclass
		/// </summary>
		public APokerEvaluationBaseClass()
		{
			for (short i = 0; i < 256; i++)
			{
				m_lowcards[i] = 256;
				short j = i;
				while (m_evaluationhelper.bitcount_table[j] > 5)
					j ^= (short)m_evaluationhelper.msb_table[j];
				if (m_evaluationhelper.bitcount_table[j] == 5)
					m_lowcards[i] = j;
			}
		}

		/// <summary>
		/// Evaluates a low hand (8 high)
		/// </summary>
		/// <param name="Cards">
		/// The card array needed to be evaluated, seven, six or five cards large
		/// </param>
		/// <returns>
		/// The lowhand, the lower the value the better, 256 means no possible low hand
		/// </returns>
		protected short EvaluateLowestHand(ulong ranks)
		{
			// Do we have an ace on hand?
			if ((ranks & 4096) == 4096)
				// Yes, shift all left and add the ace as low-card and then return the lowest possible hand
				return m_lowcards[((ranks & 127) << 1) | 1];
			else
				// Yes, shift all left and then return the lowest possible hand
				return m_lowcards[(ranks & 127) << 1];
		}
/*
		/// <summary>
		/// 
		/// </summary>
		/// <param name="value"></param>
		/// <returns></returns>
		public string EvaluatedHighValueToString(ulong value)
		{
			string result = "";
            string cards = "AKQJT98765432";
            if ((value & STRAIGHTFLUSH) == STRAIGHTFLUSH)
            {
                if ((value & 8191) == 15)
                    return "Straightflush (5, 4, 3, 2, A)";
                else
                    result = "Straightflush (";
            }
            else if ((value & FOUROFAKIND) == FOUROFAKIND)
                result = "Four of a kind (";
            else if ((value & FULLHOUSE) == FULLHOUSE)
                result = "Full house (";
            else if ((value & FLUSH) == FLUSH)
                result = "Flush (";
            else if ((value & STRAIGHT) == STRAIGHT)
            {
                if ((value & 8191) == 15)
                    return "Straight (5, 4, 3, 2, A)";
                else
                    result = "Straight (";
            }                
            else if ((value & THREEOFAKIND) == THREEOFAKIND)
                result = "Three of a kind (";
            else if ((value & TWOPAIR) == TWOPAIR)
                result = "Two pairs (";
            else if ((value & FOURFLUSH) == FOURFLUSH)
                result = "Four flush (";
            else if ((value & FOURSTRAIGHT) == FOURSTRAIGHT)
                result = "Four straight (";
            else if ((value & ONEPAIR) == ONEPAIR)
                result = "One Pair (";
            else if ((value & HIGHCARD) == HIGHCARD)
                result = "High card (";

            return result;
		}*/
	}
}