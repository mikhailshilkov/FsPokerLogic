using System;

namespace PokerEvaluators
{
	public class Omaha : IPokerEvaluator
	{

		#region Private declarations
		/// <summary>
		/// The helper class
		/// </summary>
		private EvaluationHelper m_evaluationhelper = EvaluationHelper.Instance;
		/// <summary>
		/// The class that actually will do the evaluation for us
		/// </summary>
		private FiveCards m_fivecardevaluator = new FiveCards();
		#endregion

		#region IPokerGame Members
		public bool IsHighLow { get { return m_fivecardevaluator.IsHighLow; } set { m_fivecardevaluator.IsHighLow = value; } }

		public void Evaluate(ref ulong HiResult, ref short LowResult, ulong Hand, ulong OpenCards)
		{
			ulong[] hands = { 0, 0, 0, 0, 0, 0 };
			ulong[] tables = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
			HiResult = 0;
			LowResult = 256;

			switch (m_evaluationhelper.BitCount(Hand))
			{
				case 2:
					hands[0] = Hand;
					break;
				case 4:
					hands[0] = m_evaluationhelper.MostSignificantBit(Hand);
					hands[1] = hands[0];
					hands[2] = hands[0];
					Hand ^= hands[0];
					hands[3] = m_evaluationhelper.LeastSignificantBit(Hand);
					hands[0] |= hands[3];
					hands[4] = hands[3];
					Hand ^= hands[3];
					hands[5] = m_evaluationhelper.MostSignificantBit(Hand);
					hands[1] |= hands[5];
					hands[3] |= hands[5];
					Hand ^= hands[5];
					hands[2] |= Hand;
					hands[4] |= Hand;
					hands[5] |= Hand;
					break;
				default:
					return;
			}
			ulong c4;
			ulong c3;
			ulong c5;
			switch (m_evaluationhelper.BitCount(OpenCards))
			{
				case 3:
					tables[0] = OpenCards;
					break;
				case 4:
					tables[0] = m_evaluationhelper.MostSignificantBit(OpenCards);
					c4 = m_evaluationhelper.LeastSignificantBit(OpenCards);
					OpenCards ^= (tables[0] | c4);
					tables[1] = tables[0] | c4 | m_evaluationhelper.MostSignificantBit(OpenCards);
					tables[2] = tables[0] | c4 | m_evaluationhelper.LeastSignificantBit(OpenCards);
					tables[3] = c4;
					tables[0] |= OpenCards;
					tables[3] |= OpenCards;
					break;
				case 5:
					tables[0] = m_evaluationhelper.MostSignificantBit(OpenCards);
					tables[1] = tables[0];
					tables[2] = tables[0];
					tables[3] = tables[0];
					tables[4] = tables[0];
					tables[5] = tables[0];
					OpenCards ^= tables[0];
					tables[6] = m_evaluationhelper.LeastSignificantBit(OpenCards);
					tables[0] |= tables[6];
					tables[1] |= tables[6];
					tables[2] |= tables[6];
					tables[7] = tables[6];
					tables[8] = tables[6];
					OpenCards ^= tables[6];
					tables[9] = OpenCards;
					c3 = m_evaluationhelper.MostSignificantBit(OpenCards);
					c5 = m_evaluationhelper.LeastSignificantBit(OpenCards);
					OpenCards ^= (c3 | c5);
					tables[0] |= c3;
					tables[1] |= c5;
					tables[2] |= OpenCards;
					tables[3] |= (c3 | OpenCards);
					tables[4] |= (c3 | c5);
					tables[5] |= (OpenCards | c5);
					tables[6] |= (c3 | OpenCards);
					tables[7] |= (c3 | c5);
					tables[8] |= (OpenCards | c5);
					break;
				default:
					return;
			}
			if (m_fivecardevaluator.IsHighLow)
			{
				for (int i = 0; i < 6 && hands[i] > 0; i++)
					for (int j = 0; j < 10 && tables[j] > 0; j++)
					{
						ulong innerresult = 0;
						short innerlowresult = 0;
						m_fivecardevaluator.Evaluate(ref innerresult, ref innerlowresult, hands[i], tables[j]);
						if (HiResult < innerresult)
							HiResult = innerresult;
						if (LowResult > innerlowresult)
							LowResult = innerlowresult;
					}
			}
			else
			{
				for (int i = 0; i < 6 && hands[i] > 0; i++)
					for (int j = 0; j < 10 && tables[j] > 0; j++)
					{
						ulong innerresult = 0;
						m_fivecardevaluator.Evaluate(ref innerresult, ref LowResult, hands[i], tables[j]);
						if (HiResult < innerresult)
							HiResult = innerresult;
					}
			}
			return;
		}
		#endregion
	}
}
