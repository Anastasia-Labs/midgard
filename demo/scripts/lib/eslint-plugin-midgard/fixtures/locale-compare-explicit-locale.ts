declare const names: string[];
declare const a: string;
declare const b: string;

// ruleid: midgard/locale-compare-explicit-locale
names.sort((left, right) => left.localeCompare(right));

// ruleid: midgard/locale-compare-explicit-locale
export const undefinedLocale = a.localeCompare(b, undefined);

// ruleid: midgard/locale-compare-explicit-locale
export const optionalCall = a?.localeCompare(b);

// ok: midgard/locale-compare-explicit-locale
export const english = a.localeCompare(b, "en");

// ok: midgard/locale-compare-explicit-locale
export const codeUnit = a < b ? -1 : a > b ? 1 : 0;
