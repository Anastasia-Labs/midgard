/**
 * The off-chain native evaluation walk agrees with the ledger's native script
 * verifier. Each row evaluates one committed native source against address
 * witnesses and a validity interval through the walk, at every page size, and
 * through `verifyMidgardNativeScript`, with an absent bound given to the
 * verifier as `undefined`.
 */
import {
  buildMidgardBoundedItem,
  encodeMidgardVersionedScript,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import {
  type MidgardNativeScript,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  nativeEvaluationPages,
  nativeEvaluationResult,
  nativeSignerSet,
} from "../src/execution-native-script-invalid/index.js";
import {
  keyHashOf,
  native,
  sig,
  signingKey,
} from "./support/execution-native-script-invalid-emulator.subject.js";

const A = signingKey(10);
const B = signingKey(11);
const C = signingKey(12);

/** A field-7 address witness by `key` over an arbitrary body hash. */
const witness = (key: CML.PrivateKey): Buffer =>
  Buffer.from(
    CML.make_vkey_witness(
      CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 0x5a)),
      key,
    ).to_cbor_bytes(),
  );

type Interval = { readonly start?: bigint; readonly end?: bigint };

const SCRIPTS: readonly (readonly [string, MidgardNativeScript])[] = [
  ["sig", sig(A)],
  ["all", { type: "all", scripts: [sig(A), sig(B)] }],
  ["empty all", { type: "all", scripts: [] }],
  ["any", { type: "any", scripts: [sig(B), sig(C)] }],
  ["empty any", { type: "any", scripts: [] }],
  [
    "atLeast",
    { type: "atLeast", required: 2n, scripts: [sig(A), sig(B), sig(C)] },
  ],
  ["atLeast zero", { type: "atLeast", required: 0n, scripts: [sig(C)] }],
  ["after", { type: "after", slot: 100n }],
  ["before", { type: "before", slot: 100n }],
  [
    "nested",
    {
      type: "any",
      scripts: [
        {
          type: "all",
          scripts: [sig(A), { type: "after", slot: 50n }],
        },
        {
          type: "atLeast",
          required: 1n,
          scripts: [
            { type: "before", slot: 20n },
            { type: "all", scripts: [sig(B), sig(C)] },
          ],
        },
      ],
    },
  ],
];

const SIGNERS: readonly (readonly CML.PrivateKey[])[] = [
  [],
  [A],
  [A, B],
  [B, C],
];

const INTERVALS: readonly Interval[] = [
  {},
  { start: 0n },
  { start: 100n },
  { end: 10n },
  { end: 101n },
  { start: 60n, end: 100n },
];

const ROWS = SCRIPTS.flatMap(([name, script]) =>
  SIGNERS.flatMap((signers) =>
    INTERVALS.map((interval) => ({ name, script, signers, interval })),
  ),
);

describe("native evaluation walk against the ledger verifier", () => {
  it.each(ROWS)(
    "$name agrees for each signer set and interval",
    ({ script, signers, interval }) => {
      const context = {
        item: buildMidgardBoundedItem({
          fieldIndex: 6,
          itemIndex: 0,
          bytes: encodeMidgardVersionedScript(native(script)),
        }),
        signers: nativeSignerSet(signers.map(witness)),
        validityIntervalStart: interval.start ?? MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: interval.end ?? MIDGARD_POSIX_TIME_NONE,
      };
      const expected = verifyMidgardNativeScript(script, {
        validityIntervalStart: interval.start,
        validityIntervalEnd: interval.end,
        witnessSigners: new Set(
          signers.map((key) => keyHashOf(key).toString("hex")),
        ),
      });
      expect(nativeEvaluationResult(context)).toBe(expected);
      for (const budget of [1, 2, 3, 64])
        expect(nativeEvaluationPages(context, budget).satisfied).toBe(expected);
    },
  );
});
