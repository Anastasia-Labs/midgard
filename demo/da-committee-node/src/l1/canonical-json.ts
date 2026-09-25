/**
 * Key-sorted JSON for comparing and digesting L1 observations.
 *
 * Unlike `@al-ft/midgard-core/canonical-json`, this encoding accepts bigints and
 * writes them as decimal strings, which the provider and attestation reader
 * rely on for the values they observe.
 */
export const canonicalJson = (value: unknown): string =>
  JSON.stringify(canonicalValue(value));

const canonicalValue = (value: unknown): unknown => {
  if (typeof value === "bigint") {
    return value.toString();
  }
  if (Array.isArray(value)) {
    return value.map(canonicalValue);
  }
  if (typeof value === "object" && value !== null) {
    return Object.fromEntries(
      Object.entries(value)
        .sort(([left], [right]) => left.localeCompare(right))
        .map(([key, entry]) => [key, canonicalValue(entry)]),
    );
  }
  return value;
};
