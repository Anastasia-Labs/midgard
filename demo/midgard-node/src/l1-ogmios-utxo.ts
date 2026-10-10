/**
 * The Ogmios v6 UTxO JSON shape, decoded into the output fields the node's
 * readers use (payout checks, state-queue recovery, own-block lookups). Pure:
 * no session, no query.
 */
import type { Assets } from "@lucid-evolution/lucid";

/** History readers need the presence of a reference script, never its bytes.
 * Keeping this distinct from Lucid's UTxO avoids inventing a script witness. */
export type LedgerSnapshotOutput = Readonly<{
  txHash: string;
  outputIndex: number;
  address: string;
  assets: Readonly<Assets>;
  datum?: string;
  datumHash?: string;
  hasReferenceScript: boolean;
}>;

const hex = /^(?:[0-9a-f]{2})*$/u;
export const ogmiosRecord = (
  value: unknown,
  label: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error(`${label} must be an object`);
  return value as Record<string, unknown>;
};
export const ogmiosBytes = (
  value: unknown,
  label: string,
  size?: number,
): string => {
  if (
    typeof value !== "string" ||
    !hex.test(value) ||
    (size !== undefined && value.length !== size * 2)
  )
    throw new Error(
      `${label} must be lowercase base16${size === undefined ? "" : ` (${size} bytes)`}`,
    );
  return value;
};
export const ogmiosNatural = (value: unknown, label: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
    throw new Error(`${label} must be a safe natural number`);
  return value;
};
const quantity = (value: unknown): bigint => {
  const parsed =
    typeof value === "bigint"
      ? value
      : BigInt(ogmiosNatural(value, "asset quantity"));
  if (parsed < 0n) throw new Error("UTxO asset quantity must be nonnegative");
  return parsed;
};

export const decodeLedgerSnapshotOutput = (
  value: unknown,
  addresses: ReadonlySet<string>,
): LedgerSnapshotOutput => {
  const parsed = ogmiosRecord(value, "Ogmios UTxO");
  if (typeof parsed.address !== "string" || !addresses.has(parsed.address))
    throw new Error("Ogmios UTxO lies outside the requested addresses");
  const valueRecord = ogmiosRecord(parsed.value, "Ogmios UTxO Value");
  const ada = ogmiosRecord(valueRecord.ada, "Ogmios UTxO ADA");
  const assets: Assets = { lovelace: quantity(ada.lovelace) };
  for (const [policy, rawTokens] of Object.entries(valueRecord)) {
    if (policy === "ada") continue;
    ogmiosBytes(policy, "Ogmios asset policy", 28);
    for (const [name, amount] of Object.entries(
      ogmiosRecord(rawTokens, "Ogmios asset map"),
    )) {
      ogmiosBytes(name, "Ogmios asset name");
      if (name.length > 64)
        throw new Error("Ogmios asset name exceeds 32 bytes");
      assets[policy + name] = quantity(amount);
    }
  }
  if (parsed.datum !== undefined && parsed.datumHash !== undefined)
    throw new Error("Ogmios UTxO has both inline datum and datum hash");
  return Object.freeze({
    txHash: ogmiosBytes(
      ogmiosRecord(parsed.transaction, "Ogmios transaction").id,
      "Ogmios transaction id",
      32,
    ),
    outputIndex: ogmiosNatural(parsed.index, "Ogmios output index"),
    address: parsed.address,
    assets: Object.freeze(assets),
    ...(parsed.datum === undefined
      ? {}
      : { datum: ogmiosBytes(parsed.datum, "Ogmios inline datum") }),
    ...(parsed.datumHash === undefined
      ? {}
      : { datumHash: ogmiosBytes(parsed.datumHash, "Ogmios datum hash", 32) }),
    hasReferenceScript: parsed.script !== undefined,
  });
};
