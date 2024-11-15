namespace System.Globalization;

public sealed class NumberFormatInfo
{
    public static NumberFormatInfo CurrentInfo => CultureInfo.CurrentCulture.NumInfo;

    public string NegativeSign => "-";

}