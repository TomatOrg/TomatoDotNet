#include "converter.h"
#include "native.h"
#include "util/except.h"

#include "Locale.h"
#include "LocaleCanonicalizer.h"
#include "LocaleDisplayNamesFormatter.h"
#include "Decimal.h"

#define NAMESPACE
#define CLASS       Globalization

#define LocalizedDisplayName 0x00000002
#define EnglishDisplayName 0x00000072
#define NativeDisplayName 0x00000073
#define LocalizedLanguageName 0x0000006f
#define EnglishLanguageName 0x00001001
#define NativeLanguageName 0x00000004
#define LocalizedCountryName 0x00000006
#define EnglishCountryName 0x00001002
#define NativeCountryName 0x00000008
#define AbbreviatedWindowsLanguageName 0x00000003
#define ListSeparator 0x0000000C
#define DecimalSeparator 0x0000000E
#define ThousandSeparator 0x0000000F
#define Digits 0x00000013
#define MonetarySymbol 0x00000014
#define CurrencyEnglishName 0x00001007
#define CurrencyNativeName 0x00001008
#define Iso4217MonetarySymbol 0x00000015
#define MonetaryDecimalSeparator 0x00000016
#define MonetaryThousandSeparator 0x00000017
#define AMDesignator 0x00000028
#define PMDesignator 0x00000029
#define PositiveSign 0x00000050
#define NegativeSign 0x00000051
#define Iso639LanguageTwoLetterName 0x00000059
#define Iso639LanguageThreeLetterName 0x00000067
#define Iso639LanguageName 0x00000059
#define Iso3166CountryName 0x0000005A
#define Iso3166CountryName2 0x00000068
#define NaNSymbol 0x00000069
#define PositiveInfinitySymbol 0x0000006a
#define NegativeInfinitySymbol 0x0000006b
#define ParentName 0x0000006d
#define ConsoleFallbackName 0x0000006e
#define PercentSymbol 0x00000076
#define PerMilleSymbol 0x00000077

static const char* m_icu_locale_parse_error[] = {
    [LocaleParseError_Unknown] = "Unknown",
    [LocaleParseError_Language] = "Language",
    [LocaleParseError_Subtag] = "Subtag",
    [LocaleParseError_Extension] = "Extension",
};

static const char* m_icu_data_error[] = {
    [DataError_Unknown] = "Unknown",
    [DataError_MarkerNotFound] = "Marker Not Found",
    [DataError_IdentifierNotFound] = "Identifier Not Found",
    [DataError_InvalidRequest] = "Invalid Request",
    [DataError_InconsistentData] = "Inconsistent Data",
    [DataError_Downcast] = "Downcast",
    [DataError_Deserialize] = "Deserialize",
    [DataError_Custom] = "Custom",
    [DataError_Io] = "Io",
};

static tdn_err_t icu_get_locale_by_name(String localeName, Locale** locale) {
    tdn_err_t err = TDN_NO_ERROR;

    LocaleCanonicalizer* canonicalizer = icu4x_LocaleCanonicalizer_create_extended_mv1();
    CHECK(canonicalizer != NULL);

    char buffer[14] = {0};
    CHECK(utf16_to_utf8(localeName->Chars, localeName->Length, (utf8_t*)buffer, sizeof(buffer)) < sizeof(buffer));

    DiplomatStringView view = {
        .data = (char*)buffer,
        .len = strlen(buffer)
    };
    icu4x_Locale_from_string_mv1_result result = icu4x_Locale_from_string_mv1(view);
    CHECK(result.is_ok, "Locale parse error: %s", m_icu_locale_parse_error[result.err]);

    (void)icu4x_LocaleCanonicalizer_canonicalize_mv1(canonicalizer, result.ok);

    *locale = result.ok;

cleanup:
    if (canonicalizer) icu4x_LocaleCanonicalizer_destroy_mv1(canonicalizer);

    return err;
}

static tdn_err_t icu_get_english_locale(Locale** locale) {
    tdn_err_t err = TDN_NO_ERROR;

    DiplomatStringView view = {
        .data = "en",
        .len = 2
    };
    icu4x_Locale_from_string_mv1_result result = icu4x_Locale_from_string_mv1(view);
    CHECK(result.is_ok, "Locale parse error: %s", m_icu_locale_parse_error[result.err]);

    *locale = result.ok;

cleanup:
    return err;
}

static bool Globalization_GetLocaleName(String localeName, Char* value, int valueLength) {
    tdn_err_t err = TDN_NO_ERROR;
    Locale* locale = NULL;
    DiplomatWrite* write = NULL;

    CHECK_AND_RETHROW(icu_get_locale_by_name(localeName, &locale));
    CHECK(locale != NULL);

    write = diplomat_buffer_write_create(valueLength);
    CHECK(write != NULL);
    icu4x_Locale_to_string_mv1(locale, write);
    CHECK(!write->grow_failed);

    CHECK(utf8_to_utf16((utf8_t*)write->buf, write->len, value, valueLength) < valueLength);

cleanup:
    if (write != NULL) diplomat_buffer_write_destroy(write);
    if (locale != NULL) icu4x_Locale_destroy_mv1(locale);

    return !IS_ERROR(err);
}
NATIVE_FUNC(GetLocaleName, OBJ_REF, BY_REF, INT32);

static bool Globalization_GetLocaleInfoString(String localeName, uint32_t localeStringData, Char* value, int valueLength, String uiLocaleName) {
    tdn_err_t err = TDN_NO_ERROR;
    LocaleDisplayNamesFormatter* formatter = NULL;
    DiplomatWrite* write = NULL;
    Locale* otherLocale = NULL;
    Locale* locale = NULL;

    CHECK_AND_RETHROW(icu_get_locale_by_name(localeName, &locale));

    write = diplomat_buffer_write_create(valueLength);
    CHECK(write != NULL);

    switch (localeStringData) {
        case EnglishDisplayName:
        case NativeDisplayName:
        case LocalizedDisplayName: {
            if (localeStringData == EnglishDisplayName) {
                CHECK_AND_RETHROW(icu_get_english_locale(&otherLocale));
            } else if (localeStringData == LocalizedDisplayName) {
                CHECK_AND_RETHROW(icu_get_locale_by_name(uiLocaleName, &otherLocale));
            }

            DisplayNamesOptionsV1 options = {
                .language_display = {
                    .is_ok = true,
                    .ok = LanguageDisplay_Standard
                }
            };
            icu4x_LocaleDisplayNamesFormatter_create_v1_mv1_result result = icu4x_LocaleDisplayNamesFormatter_create_v1_mv1(
                otherLocale ?: locale,
                options
            );
            CHECK(result.is_ok, "Locale parse error: %s", m_icu_data_error[result.err]);
            formatter = result.ok;

            icu4x_LocaleDisplayNamesFormatter_of_mv1(formatter, locale, write);
            CHECK(!write->grow_failed);
        } break;

        case Iso3166CountryName: {
            // if we fail to get the region code just ignore it
            // and leave as an empty string
            (void)icu4x_Locale_region_mv1(locale, write);
            CHECK(!write->grow_failed);
        } break;

        case PositiveSign: {

        } break;

        default:
            CHECK_FAIL("Failed to get string data 0x%08x", localeStringData);
    }

    CHECK(utf8_to_utf16((utf8_t*)write->buf, write->len, NULL, 0) <= valueLength);
    utf8_to_utf16((utf8_t*)write->buf, write->len, value, valueLength);

cleanup:
    if (formatter != NULL) icu4x_LocaleDisplayNamesFormatter_destroy_mv1(formatter);
    if (write != NULL) diplomat_buffer_write_destroy(write);
    if (otherLocale != NULL) icu4x_Locale_destroy_mv1(otherLocale);
    if (locale != NULL) icu4x_Locale_destroy_mv1(locale);

    return !IS_ERROR(err);
}
NATIVE_FUNC(GetLocaleInfoString, OBJ_REF, INT32, BY_REF, INT32, OBJ_REF);

static bool Globalization_GetLocaleInfoInt(String localeName, uint32_t localeStringData, int* value) {
    ASSERT(!"Globalization_GetLocaleInfoInt");
    return false;
}
NATIVE_FUNC(GetLocaleInfoInt, OBJ_REF, INT32, BY_REF);

static bool Globalization_GetLocaleInfoGroupingSizes(String localeName, uint32_t localeGroupingData, int* primaryGroupSize, int* secondaryGroupSize) {
    ASSERT(!"Globalization_GetLocaleInfoGroupingSizes");
    return false;
}
NATIVE_FUNC(GetLocaleInfoGroupingSizes, OBJ_REF, INT32, BY_REF, BY_REF);

#include "Logger.h"

static bool Globalization_GetDefaultLocaleName(Char* value, int valueLength) {
    // TODO: allow to override from host
    if (valueLength >= 2) {
        value[0] = 'e';
        value[1] = 'n';
        return true;
    } else {
        return false;
    }
}
NATIVE_FUNC(GetDefaultLocaleName, BY_REF, INT32);
