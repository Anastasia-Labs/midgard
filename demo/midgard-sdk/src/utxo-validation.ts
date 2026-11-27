import { getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { InvalidUtxoError } from "./errors.js";

const TX_HASH = /^[0-9a-f]{64}$/u;
const DATUM_HASH = /^[0-9a-f]{64}$/u;
const HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;
/** A 28-byte policy id followed by an asset name of at most 32 bytes. */
const ASSET_UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u;
const MAX_OUTPUT_INDEX = 0xffff;
const MAX_QUANTITY = 2n ** 64n - 1n;
const SCRIPT_TYPES: ReadonlySet<string> = new Set([
  "Native",
  "PlutusV1",
  "PlutusV2",
  "PlutusV3",
]);

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const isPositiveQuantity = (value: unknown): value is bigint =>
  typeof value === "bigint" && value > 0n && value <= MAX_QUANTITY;

const isAddress = (value: unknown): boolean => {
  if (typeof value !== "string" || value.length === 0) return false;
  try {
    getAddressDetails(value);
    return true;
  } catch {
    return false;
  }
};

const assetsReason = (assets: unknown): string | null => {
  if (!isRecord(assets)) return "assets is not an asset map";
  if (!isPositiveQuantity(assets.lovelace))
    return "lovelace is not a bigint between 1 and 2^64 - 1";
  for (const [unit, quantity] of Object.entries(assets)) {
    if (unit === "lovelace") continue;
    if (!ASSET_UNIT.test(unit))
      return `asset unit ${unit} is not a policy id and an asset name of at most 32 bytes`;
    if (!isPositiveQuantity(quantity))
      return `quantity of ${unit} is not a bigint between 1 and 2^64 - 1`;
  }
  return null;
};

const scriptRefReason = (scriptRef: unknown): string | null => {
  if (scriptRef == null) return null;
  if (
    !isRecord(scriptRef) ||
    typeof scriptRef.type !== "string" ||
    !SCRIPT_TYPES.has(scriptRef.type) ||
    typeof scriptRef.script !== "string" ||
    !HEX_BYTES.test(scriptRef.script)
  )
    return "scriptRef is not a Native or Plutus script with hex bytes";
  return null;
};

/**
 * Why `utxo` is not a well-formed ledger output, or `null` when it is. Checks
 * the shape Lucid's `UTxO` promises: a 32-byte lowercase transaction hash, a
 * 16-bit output index, a parseable address, a positive lovelace amount, token
 * units and positive quantities within ledger bounds, and hex datum, datum
 * hash and reference-script fields.
 */
export const invalidUtxoReason = (utxo: unknown): string | null => {
  if (!isRecord(utxo)) return "not a UTxO object";
  if (typeof utxo.txHash !== "string" || !TX_HASH.test(utxo.txHash))
    return "txHash is not 32 bytes of lowercase hex";
  if (
    typeof utxo.outputIndex !== "number" ||
    !Number.isInteger(utxo.outputIndex) ||
    utxo.outputIndex < 0 ||
    utxo.outputIndex > MAX_OUTPUT_INDEX
  )
    return "outputIndex is not an integer between 0 and 65535";
  if (!isAddress(utxo.address)) return "address is not a Cardano address";
  const assets = assetsReason(utxo.assets);
  if (assets !== null) return assets;
  if (
    utxo.datumHash != null &&
    (typeof utxo.datumHash !== "string" || !DATUM_HASH.test(utxo.datumHash))
  )
    return "datumHash is not 32 bytes of lowercase hex";
  if (
    utxo.datum != null &&
    (typeof utxo.datum !== "string" || !HEX_BYTES.test(utxo.datum))
  )
    return "datum is not lowercase hex CBOR";
  return scriptRefReason(utxo.scriptRef);
};

const readableOutRef = (utxo: unknown): string | null =>
  isRecord(utxo) &&
  typeof utxo.txHash === "string" &&
  typeof utxo.outputIndex === "number"
    ? `${utxo.txHash}#${utxo.outputIndex}`
    : null;

const refusal = (role: string, utxo: unknown, reason: string) => {
  const outRef = readableOutRef(utxo);
  return new InvalidUtxoError({
    message: `Invalid UTxO in ${role}${outRef === null ? "" : ` (${outRef})`}: ${reason}`,
    cause: utxo,
    role,
    outRef,
  });
};

/** Refuses a malformed optional UTxO under `role`; `undefined` passes. */
export const requireWellFormedUtxo = (
  role: string,
  utxo: UTxO | undefined,
): Effect.Effect<void, InvalidUtxoError> => {
  if (utxo === undefined) return Effect.void;
  const reason = invalidUtxoReason(utxo);
  return reason === null
    ? Effect.void
    : Effect.fail(refusal(role, utxo, reason));
};

/**
 * Refuses the first malformed entry of `utxos`, or the first output listed a
 * second time, as `role[index]`; `undefined` passes.
 */
export const requireWellFormedUtxos = (
  role: string,
  utxos: readonly UTxO[] | undefined,
): Effect.Effect<void, InvalidUtxoError> => {
  if (utxos === undefined) return Effect.void;
  const seen = new Set<string>();
  for (const [index, utxo] of utxos.entries()) {
    const entryRole = `${role}[${index}]`;
    const reason = invalidUtxoReason(utxo);
    if (reason !== null) return Effect.fail(refusal(entryRole, utxo, reason));
    const outRef = `${utxo.txHash}#${utxo.outputIndex}`;
    if (seen.has(outRef))
      return Effect.fail(
        refusal(entryRole, utxo, `lists output ${outRef} twice`),
      );
    seen.add(outRef);
  }
  return Effect.void;
};
