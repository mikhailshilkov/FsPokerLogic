using System;

namespace PokerEvaluators
{
	public interface IPokerEvaluator
	{
		/// <summary>
		/// Get / Set if we are in a Hi / Lo, high-only game
		/// </summary>
		bool IsHighLow { get; set; }
		/// <summary>
		/// Evaluates a poker hand consisting of a set of cards, return Hi / Lo result
		/// </summary>
		/// <param name="HiResult">
		/// The high result of the hand, higher result is better
		/// </param>
		/// <param name="LowResult">
		/// The low result of the hand, lower result is better
		/// </param>
		/// <param name="Hand">
		/// The cards hidden from other players
		/// </param>
		/// <param name="OpenCards">
		/// The open cards, eg. Flop, Turn, River and so on
		/// </param>
		void Evaluate(ref ulong HiResult, ref short LowResult, ulong Hand, ulong OpenCards);
	}
}
