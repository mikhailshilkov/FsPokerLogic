using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace PokerEvaluators
{
	/// <summary>
	/// Helperclass containing common functions for different needs when evaluating a poker hand
	/// </summary>
	public class EvaluationHelper
	{
		#region Private declarations
		/// <summary>
		/// The helper class singleton placeholder
		/// </summary>
		private static EvaluationHelper m_evaluationhelper = new EvaluationHelper();

		/// <summary>
		/// Array containing the number of bits for each value up to 8191
		/// </summary>
		private readonly ushort[] m_bitcount = new ushort[8192];
		/// <summary>
		/// Array containing the least significant bit for each value up to 8191
		/// </summary>
		private readonly ushort[] m_lsbtable = new ushort[8192];
		/// <summary>
		/// Array containing the most significant bit for each value up to 8191
		/// </summary>
		private readonly ushort[] m_msbtable = new ushort[8192];
		#endregion

		#region Public properties
		/// <summary>
		/// Get the singleton instance of the EvaluationHelper-class
		/// </summary>
		public static EvaluationHelper Instance { get { return EvaluationHelper.m_evaluationhelper; } }
		/// <summary>
		/// Array containing the number of bits for each value up to 8191
		/// </summary>
		public ushort[] bitcount_table
		{
			get { return m_bitcount; }
		}
		/// <summary>
		/// Array containing the least significant bit for each value up to 8191
		/// </summary>
		public ushort[] lsb_table
		{
			get { return m_lsbtable; }
		}
		/// <summary>
		/// Array containing the most significant bit for each value up to 8191
		/// </summary>
		public ushort[] msb_table
		{
			get { return m_msbtable; }
		}
		#endregion
		#region Bit operations
		/// <summary>
		/// Counts the number of bits set to 1 within an ulong
		/// </summary>
		/// <param name="value">
		/// The value to extract the number of bits from
		/// </param>
		/// <returns>
		/// The number of bits within the ulong
		/// </returns>
		public int BitCount(ulong value)
		{
			return m_bitcount[(int)(value & 8191)] + m_bitcount[(int)((value >> 13) & 8191)] + m_bitcount[(int)((value >> 26) & 8191)] + m_bitcount[(int)((value >> 39))];
		}

		/// <summary>
		/// Find the MSB of an ulong
		/// </summary>
		/// <param name="value">
		/// The value to extract the MSB from
		/// </param>
		/// <returns>
		/// The MSB of the value
		/// </returns>
		public ulong MostSignificantBit(ulong value)
		{
			if ((value & 0xFFF8000000000UL) > 0)
				return (ulong)(m_msbtable[(value >> 39) & 0x1FFFUL]) << 39;
			else if ((value & 0x7FFC000000UL) > 0)
				return (ulong)(m_msbtable[(value >> 26) & 0x1FFFUL]) << 26;
			else if ((value & 0x3FFE000UL) > 0)
				return (ulong)(m_msbtable[(value >> 13) & 0x1FFFUL]) << 13;
			return (ulong)(m_msbtable[value & 0x1FFFUL]);
		}

		/// <summary>
		/// Find the LSB of an ulong
		/// </summary>
		/// <param name="value">
		/// The value to extract the LSB from
		/// </param>
		/// <returns>
		/// The LSB of the value
		/// </returns>
		public ulong LeastSignificantBit(ulong value)
		{
			if ((value & 0x1FFFUL) > 0)
				return (ulong)(m_lsbtable[(value & 0x1FFFUL)]);
			else if ((value & 0x3FFE000UL) > 0)
				return (ulong)(m_lsbtable[(value >> 13) & 0x1FFFUL]) << 13;
			else if ((value & 0x7FFC000000) > 0)
				return (ulong)(m_lsbtable[(value >> 26) & 0x1FFFUL]) << 26;
			return (ulong)(m_lsbtable[(value >> 39) & 0x1FFFUL]) << 39;
		}
		#endregion
		
		/// <summary>
		/// Init the EvaluationHelper-class and all of the arrays published by it
		/// </summary>
		private EvaluationHelper() 
		{
			for (short i = 0; i < 8192; i++)
			{
				m_msbtable[i] = m_lsbtable[i] = m_bitcount[i] = 0;
				for (ushort j = 1; j < 8192; j <<= 1)
					if ((i & j) > 0)
						m_bitcount[i]++;
				for (ushort j = 1; j < 8192; j <<= 1)
					if ((i & j) > 0)
					{
						m_lsbtable[i] = j;
						break;
					}
				for (ushort j = 4096; j >= 1; j >>= 1)
					if ((i & j) > 0)
					{
						m_msbtable[i] = j;
						break;
					}
			}
		}
	}
}
