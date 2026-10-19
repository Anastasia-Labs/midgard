import {
  computeMidgardNativeTxId,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import type * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  signedKeyOutput,
  signNativeTx,
} from "./support/forced-reason-signed-native-tx.js";
import { makeReferenceNativeTx } from "./support/no-reference-input-native.js";

/**
 * Every input-resolution reason the node's forced classifier writes names an
 * input inside the field it cites: source kind 0 or 1, and an index from 0 to
 * that field's item count minus one. The validation machine's own reason for
 * these arms carries an unset (-1) index, so a leaf whose coordinate falls
 * outside its field is never one an honest operator writes. The resolved
 * reference-script arms are not written by the classifier at all.
 */

const outRef = (txIdByte: number, outputIndex: number): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, txIdByte),
    outputIndex,
  });
const spend = [outRef(0x31, 0), outRef(0x31, 1)];
const references = [outRef(0x32, 0), outRef(0x32, 1)];
const lonelySpend = outRef(0x33, 0);

/** Not a canonical output: the canonical form plus one trailing byte. */
const nonCanonicalOutput = Buffer.concat([signedKeyOutput(), Buffer.from([0])]);
/** A canonical output locked by a key that never signs. */
const unsignedKeyOutput = encodeMidgardTxOutput({
  address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x7e)]),
  value: { lovelace: 2_000_000n, assets: new Map() },
});
/** A reference output whose native script payload is the bytes `payload`. */
const rawNativeReferenceOutput = (payload: Buffer): Buffer => {
  const output = encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]),
    value: { lovelace: 2_000_000n, assets: new Map() },
    script_ref: { language: "PlutusV3", scriptBytes: payload },
  });
  const marker = output.indexOf(Buffer.from("8203", "hex"));
  if (marker < 0) throw new Error("versioned script marker absent");
  output[marker + 1] = 0;
  return output;
};
/** `[0, h'02…02']`: one signature node. */
const sigBytes = Buffer.concat([
  Buffer.from("8200581c", "hex"),
  Buffer.alloc(28, 2),
]);
/** `all [sig × count]`, `count` at least 256 and below 65536. */
const wideBytes = (count: number): Buffer =>
  Buffer.concat([
    Buffer.from([0x82, 0x01, 0x99, count >> 8, count & 0xff]),
    ...Array.from({ length: count }, () => sigBytes),
  ]);
/** `all [all [... sig]]`, `depth` containers deep. */
const nestedBytes = (depth: number): Buffer =>
  Buffer.concat([
    ...Array.from({ length: depth }, () => Buffer.from("820181", "hex")),
    sigBytes,
  ]);

const signedSpend = signNativeTx(
  makeNativeTx({ spendInputCbors: spend, fee: 7n, outputCbors: [] }),
);
const referenceTx = makeReferenceNativeTx({
  spendInputCbors: [lonelySpend],
  referenceInputCbors: references,
  fee: 7n,
});

const verdictOf = async (
  nativeTx: MidgardNativeTxFull,
  ledger: readonly (readonly [Buffer, Buffer])[],
) => {
  const forced = materializeMidgardForcedTxFromCanonical(nativeTx);
  return nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
    ledger,
  });
};

/** The (source kind, index) an input-resolution reason names, if any. */
const resolutionCoordinate = (
  reason: SDK.RejectionReason,
): readonly [bigint, bigint] | undefined => {
  if (typeof reason !== "object") return undefined;
  if ("InputNotFound" in reason)
    return [reason.InputNotFound.source_kind, reason.InputNotFound.input_index];
  if ("InputSpentOutputNonCanonical" in reason)
    return [
      reason.InputSpentOutputNonCanonical.source_kind,
      reason.InputSpentOutputNonCanonical.input_index,
    ];
  if ("ResolvedReferenceScriptMalformed" in reason)
    return [1n, reason.ResolvedReferenceScriptMalformed.input_index];
  if ("ResolvedReferenceScriptNodeLimit" in reason)
    return [1n, reason.ResolvedReferenceScriptNodeLimit.input_index];
  if ("ResolvedReferenceScriptDepthLimit" in reason)
    return [1n, reason.ResolvedReferenceScriptDepthLimit.input_index];
  if ("SpendInputSignerMissing" in reason)
    return [0n, reason.SpendInputSignerMissing.input_index];
  return undefined;
};

const expectInDomain = (
  reason: SDK.RejectionReason,
  fieldCounts: readonly [number, number],
) => {
  const coordinate = resolutionCoordinate(reason);
  expect(coordinate).toBeDefined();
  const [sourceKind, index] = coordinate!;
  expect([0n, 1n]).toContain(sourceKind);
  expect(index >= 0n).toBe(true);
  expect(index < BigInt(fieldCounts[Number(sourceKind)]!)).toBe(true);
};

describe("forced input-resolution reasons name an input inside their field", () => {
  const cases = [
    {
      label: "absent spend input",
      nativeTx: signedSpend,
      ledger: [[spend[0]!, signedKeyOutput()]],
      counts: [2, 0],
      reason: { InputNotFound: { source_kind: 0n, input_index: 1n } },
    },
    {
      label: "absent reference input",
      nativeTx: referenceTx,
      ledger: [[references[0]!, signedKeyOutput()]],
      counts: [1, 2],
      reason: { InputNotFound: { source_kind: 1n, input_index: 1n } },
    },
    {
      label: "non-canonical spent output",
      nativeTx: signedSpend,
      ledger: [
        [spend[0]!, signedKeyOutput()],
        [spend[1]!, nonCanonicalOutput],
      ],
      counts: [2, 0],
      reason: {
        InputSpentOutputNonCanonical: { source_kind: 0n, input_index: 1n },
      },
    },
    {
      label: "non-canonical referenced output",
      nativeTx: referenceTx,
      ledger: [
        [references[0]!, signedKeyOutput()],
        [references[1]!, nonCanonicalOutput],
      ],
      counts: [1, 2],
      reason: {
        InputSpentOutputNonCanonical: { source_kind: 1n, input_index: 1n },
      },
    },
    {
      label: "spend input whose key never signs",
      nativeTx: signedSpend,
      ledger: [
        [spend[0]!, signedKeyOutput()],
        [spend[1]!, unsignedKeyOutput],
      ],
      counts: [2, 0],
      reason: { SpendInputSignerMissing: { input_index: 1n } },
    },
  ] as const;

  for (const { label, nativeTx, ledger, counts, reason } of cases) {
    it(`${label}: the written index lies inside its field`, async () => {
      const verdict = await verdictOf(nativeTx, ledger);
      expect(verdict).toStrictEqual({ ForcedTxInvalid: { reason } });
      expectInDomain(reason, counts);
    });
  }
});

describe("resolved reference scripts are not cited by a resolution reason", () => {
  const ledgerWith = (output: Buffer) =>
    [
      [references[0]!, signedKeyOutput()],
      [references[1]!, output],
    ] as const;
  const probes = [
    ["a malformed", rawNativeReferenceOutput(Buffer.from("ff", "hex"))],
    ["an over-node-limit", rawNativeReferenceOutput(wideBytes(16_384))],
    ["an over-depth-limit", rawNativeReferenceOutput(nestedBytes(16_385))],
  ] as const;

  for (const [label, output] of probes) {
    it(`${label} referenced script is cited as its output's non-canonicity`, async () => {
      const reason = {
        InputSpentOutputNonCanonical: { source_kind: 1n, input_index: 1n },
      };
      expect(await verdictOf(referenceTx, ledgerWith(output))).toStrictEqual({
        ForcedTxInvalid: { reason },
      });
      expectInDomain(reason, [1, 2]);
    });
  }

  it("scripts at the node and depth limits resolve, so the spend is reached", async () => {
    for (const output of [
      rawNativeReferenceOutput(wideBytes(16_383)),
      rawNativeReferenceOutput(nestedBytes(16_383)),
    ])
      expect(await verdictOf(referenceTx, ledgerWith(output))).toStrictEqual({
        ForcedTxInvalid: {
          reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
        },
      });
  });
});
