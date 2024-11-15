namespace System.Globalization;

public class CultureInfo
{

    internal NumberFormatInfo NumInfo = new();

    public static CultureInfo CurrentCulture { get; } = new();
}