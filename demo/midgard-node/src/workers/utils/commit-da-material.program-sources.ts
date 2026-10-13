import type { MidgardCekProgramEnvelope } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
  type MidgardForcedTxFull,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import {
  collectMidgardAttachedProgramEnvelopes,
  decodeMidgardScriptProgramEnvelope,
} from "@al-ft/midgard-core/script-proof";

import { Columns, type MinimalEntry } from "../../database/utils/ledger.js";

export type DaMaterialTransaction = {
  readonly canonical: MidgardNativeTxFull | MidgardForcedTxFull;
  readonly sourceKind: "normal" | "forced";
};
export type PotentialProgramSources = ReadonlyMap<
  string,
  readonly MidgardCekProgramEnvelope[]
>;

/** A prefix may keep a parent output which a larger withdrawal window removes,
 * or create a referenced output in an earlier transaction. Keep all variants;
 * neither spending nor deduplication grants a sizing credit. Deposits run last
 * and cannot provide programs to forced or normal transactions in this block. */
export const potentialCommitProgramSources = (
  base: readonly MinimalEntry[],
  transactions: readonly DaMaterialTransaction[],
  includeCandidateOutputs = true,
): PotentialProgramSources => {
  const needed = new Set(
    transactions.flatMap(({ canonical }) =>
      decodeMidgardNativeByteListPreimage(
        canonical.body.referenceInputsPreimageCbor,
        "da_bound.references",
      ).map((ref) => Buffer.from(ref).toString("hex")),
    ),
  );
  const sources = new Map<string, MidgardCekProgramEnvelope[]>();
  const add = (key: string, output: Uint8Array): void => {
    if (!needed.has(key)) return;
    const script = decodeMidgardTxOutput(output).script_ref;
    if (script === undefined) return;
    const envelope = decodeMidgardScriptProgramEnvelope(script);
    if (envelope === null) return;
    const variants = sources.get(key) ?? [];
    variants.push(envelope);
    sources.set(key, variants);
  };
  for (const entry of base)
    add(entry[Columns.OUTREF].toString("hex"), entry[Columns.OUTPUT]);
  for (const { canonical } of includeCandidateOutputs ? transactions : []) {
    const txId = computeMidgardNativeTxId(canonical.compact);
    const outputs = decodeMidgardNativeByteListPreimage(
      canonical.body.outputsPreimageCbor,
      "da_bound.outputs",
    );
    for (const [index, output] of outputs.entries()) {
      // A malformed output cannot be created by an accepted transaction.
      // Its deterministic rejection remains with the validation machine.
      try {
        add(
          encodeMidgardSpendInputItem({ txId, outputIndex: index }).toString(
            "hex",
          ),
          output,
        );
      } catch {
        continue;
      }
    }
  }
  return sources;
};

/** Normal sources pay every possible attached/reference program variant,
 * including candidate creators outside the selected prefix. Malformed attached
 * normal source is rejected by validation; admitted forced-native events have
 * no CEK material under the current A2 protocol boundary. */
export const potentialEventProgramEnvelopes = (
  transaction: DaMaterialTransaction,
  sources: PotentialProgramSources,
): readonly MidgardCekProgramEnvelope[] => {
  // A2's admitted native-only forced semantics execute no CEK program,
  // including reference scripts present in the authenticated parent ledger.
  if (transaction.sourceKind === "forced") return [];
  let attached: readonly MidgardCekProgramEnvelope[];
  try {
    attached = collectMidgardAttachedProgramEnvelopes(transaction.canonical);
  } catch {
    return [];
  }
  const envelopes = [...attached];
  for (const outRef of decodeMidgardNativeByteListPreimage(
    transaction.canonical.body.referenceInputsPreimageCbor,
    "da_bound.references",
  )) {
    envelopes.push(...(sources.get(Buffer.from(outRef).toString("hex")) ?? []));
  }
  return envelopes;
};

/** A verified envelope declares the exact reachable node count and preimage
 * bytes (verify-one-program-material.ts). Each DA value adds array/version/kind
 * and at most five byte-length bytes: <= preimage + 8. A Plutus byte string is
 * <= length + 2 + 2*ceil(length/64). Charge keys and tuple framing per node and
 * an extra chunk rounding per node, with no shared-node/program credit. */
export const programMaterialDaTupleBytesUpperBound = (
  envelopes: readonly MidgardCekProgramEnvelope[],
): bigint =>
  envelopes.reduce((sum, envelope) => {
    const n = envelope.nodeCount;
    const m = envelope.materialByteLength;
    if (n < 1n || m < n)
      throw new Error("invalid admitted CEK material envelope budget");
    return sum + m + 48n * n + 2n * ((m + 8n * n + 63n) / 64n);
  }, 0n);

const attachedProgramBytes = (
  transaction: DaMaterialTransaction,
): bigint | undefined => {
  try {
    return programMaterialDaTupleBytesUpperBound(
      collectMidgardAttachedProgramEnvelopes(transaction.canonical),
    );
  } catch {
    return undefined;
  }
};
export const potentialProgramSourceBytes = (
  sources: PotentialProgramSources,
): ReadonlyMap<string, bigint> =>
  new Map(
    [...sources].map(([key, envelopes]) => [
      key,
      programMaterialDaTupleBytesUpperBound(envelopes),
    ]),
  );

/** Aggregating a source's variants once avoids repeated graph/variant scans for
 * many events referencing the same output. Occurrences still pay the full sum. */
export const potentialEventProgramBytes = (
  transaction: DaMaterialTransaction,
  sourceBytes: ReadonlyMap<string, bigint>,
): bigint => {
  if (transaction.sourceKind === "forced") return 0n;
  const attached = attachedProgramBytes(transaction);
  if (attached === undefined) return 0n;
  return decodeMidgardNativeByteListPreimage(
    transaction.canonical.body.referenceInputsPreimageCbor,
    "da_bound.references",
  ).reduce(
    (sum, ref) =>
      sum + (sourceBytes.get(Buffer.from(ref).toString("hex")) ?? 0n),
    attached,
  );
};
