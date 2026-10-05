import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  type Assets,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  committeeScopedFetch,
  type CommitteeSourceReadLimits,
} from "../availability/scoped-transports.js";
import {
  getRecord,
  safeBlockHash,
  safeSlot,
} from "./provider.parse-persisted-chain-sync-state.js";

const hex = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^(?:[0-9a-f]{2})+$/u.test(value))
    throw new Error(`Malformed ${label}`);
  return value;
};
const quantity = (value: unknown): bigint => {
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0)
    return BigInt(value);
  if (typeof value === "string" && /^(?:0|[1-9][0-9]{0,19})$/u.test(value)) {
    const amount = BigInt(value);
    if (amount <= 18446744073709551615n) return amount;
  }
  throw new Error("Kupo quantity is not an exact nonnegative ledger quantity");
};
/** Kupo's resolve_hashes response, using the installed Kupmios UTxO mapping.
 * Raw count is checked before per-output conversion; HTTP bytes are streamed. */
type ScopedKupoInput = {
  kupoUrl: string;
  limits: CommitteeSourceReadLimits;
  fetchImpl?: typeof fetch;
};
export const committeeScopedKupoRows = async (
  input: ScopedKupoInput,
  pattern: string,
  suffix: string,
  scope: DaAvailabilityReadScope,
): Promise<unknown[]> => {
  const response = await committeeScopedFetch(
    scope,
    input.limits,
    input.fetchImpl,
  )(`${input.kupoUrl.replace(/\/$/u, "")}/matches/${pattern}${suffix}`);
  if (!response.ok)
    throw new Error(`Kupo UTxO read failed: ${response.status}`);
  const raw: unknown = await response.json();
  scope.assertCurrent();
  if (!Array.isArray(raw) || raw.length > input.limits.rawUtxos)
    throw new Error("Kupo raw UTxO count exceeds the adopted domain");
  return raw;
};
const scopedUtxoQuery =
  (input: ScopedKupoInput) =>
  async (
    pattern: string,
    scope: DaAvailabilityReadScope,
    address?: string,
  ): Promise<UTxO[]> => {
    const raw = await committeeScopedKupoRows(
      input,
      pattern,
      "?unspent&resolve_hashes",
      scope,
    );
    const seen = new Set<string>();
    return raw.map((item) => {
      scope.assertCurrent();
      const row = getRecord(item, "Kupo UTxO");
      const txHash = safeBlockHash(row.transaction_id, "Kupo transaction id");
      const outputIndex = safeSlot(row.output_index, "Kupo output index");
      const outRef = `${txHash}#${outputIndex}`;
      const parsedAddress = row.address;
      if (
        typeof parsedAddress !== "string" ||
        (address !== undefined && parsedAddress !== address) ||
        row.spent_at !== null ||
        seen.has(outRef)
      )
        throw new Error(
          "Kupo query contains foreign, spent or duplicate outputs",
        );
      seen.add(outRef);
      const value = getRecord(row.value, "Kupo value");
      const assets: Assets = { lovelace: quantity(value.coins) };
      for (const [unit, amount] of Object.entries(
        getRecord(value.assets, "Kupo assets"),
      )) {
        if (!/^[0-9a-f]{56}\.(?:[0-9a-f]{2}){0,32}$/u.test(unit))
          throw new Error("Malformed Kupo asset id");
        assets[unit.replace(".", "")] = quantity(amount);
      }
      let scriptRef: Script | undefined;
      if (row.script !== null) {
        const script = getRecord(row.script, "Kupo resolved script");
        const bytes = hex(script.script, "Kupo script bytes");
        switch (script.language) {
          case "native":
            scriptRef = { type: "Native", script: bytes };
            break;
          case "plutus:v1":
            scriptRef = {
              type: "PlutusV1",
              script: applyDoubleCborEncoding(bytes),
            };
            break;
          case "plutus:v2":
            scriptRef = {
              type: "PlutusV2",
              script: applyDoubleCborEncoding(bytes),
            };
            break;
          case "plutus:v3":
            scriptRef = {
              type: "PlutusV3",
              script: applyDoubleCborEncoding(bytes),
            };
            break;
          default:
            throw new Error("Unsupported Kupo script language");
        }
      }
      if (
        row.datum_type !== undefined &&
        row.datum_type !== "hash" &&
        row.datum_type !== "inline"
      )
        throw new Error("Unsupported Kupo datum type");
      return {
        txHash,
        outputIndex,
        address: parsedAddress,
        assets,
        datumHash:
          row.datum_type === "hash"
            ? safeBlockHash(row.datum_hash, "Kupo datum hash")
            : undefined,
        datum:
          row.datum_type === "inline"
            ? hex(row.datum, "Kupo inline datum")
            : undefined,
        scriptRef,
      };
    });
  };

export const committeeScopedUtxos = (input: ScopedKupoInput) => {
  const query = scopedUtxoQuery(input);
  return (address: string, scope: DaAvailabilityReadScope): Promise<UTxO[]> =>
    query(encodeURIComponent(address), scope, address);
};

/** Exact input reads use the same streamed byte/raw-count limits and signal. */
export const committeeScopedOutRefs = (input: ScopedKupoInput) => {
  const query = scopedUtxoQuery(input);
  return async (
    refs: readonly Readonly<{ txHash: string; outputIndex: number }>[],
    scope: DaAvailabilityReadScope,
  ): Promise<UTxO[]> => {
    if (refs.length > input.limits.rawUtxos)
      throw new Error("Input lookup count exceeds the adopted domain");
    const requested = new Set<string>();
    const hashes = new Set<string>();
    for (const ref of refs) {
      safeBlockHash(ref.txHash, "Input transaction hash");
      safeSlot(ref.outputIndex, "Input output index");
      requested.add(`${ref.txHash}#${ref.outputIndex}`);
      hashes.add(ref.txHash);
    }
    const result: UTxO[] = [];
    // Sequential queries preserve one aggregate budget and bounded request concurrency.
    for (const hash of hashes) {
      const rows = await query(`*@${hash}`, scope);
      for (const row of rows) {
        if (row.txHash !== hash)
          throw new Error("Kupo input query contains a foreign transaction");
        if (requested.has(`${row.txHash}#${row.outputIndex}`)) result.push(row);
      }
    }
    scope.assertCurrent();
    return result;
  };
};
