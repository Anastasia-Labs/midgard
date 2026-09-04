import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, expectTypeOf, it } from "vitest";

import {
  type InvalidRangeEvidence,
  invalidRangeEvidenceCloses,
  invalidRangeFaultForReason,
} from "../src/invalid-range/family.js";
import { detectInvalidRangeForcedReplay } from "../src/invalid-range/replay.js";
import type { InvalidRangeForcedInput } from "../src/invalid-range/v1.js";
import { INVALID_RANGE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

const tx = "11".repeat(32);
const subject = (
  reason: "ValidityIntervalMalformed" | "ValidityIntervalExcludesBlockSlot",
) =>
  forcedVerdictSubject({
    transactionId: tx,
    sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
    rejectionReason: reason,
  });
const evidence = (
  reason: Parameters<typeof subject>[0],
  normalizedRange: InvalidRangeEvidence["normalizedRange"],
): InvalidRangeEvidence => ({
  subject: subject(reason),
  blockSlot: 10n,
  normalizedRange,
});
describe("invalidRange wrongful rejection V1", () => {
  it("refuses malformed conviction when the interval is well formed", () =>
    expect(
      invalidRangeEvidenceCloses(
        evidence("ValidityIntervalMalformed", {
          ClosedRange: { lower: 10n, upper: 10n },
        }),
      ),
    ).toBe(true));
  it("keeps malformed rejection honest for InvalidRange", () =>
    expect(
      invalidRangeEvidenceCloses(
        evidence("ValidityIntervalMalformed", "InvalidRange"),
      ),
    ).toBe(false));
  it("treats both exact boundaries as included", () => {
    expect(
      invalidRangeFaultForReason(
        "ValidityIntervalExcludesBlockSlot",
        { ClosedRange: { lower: 10n, upper: 12n } },
        10n,
      ),
    ).toBe(false);
    expect(
      invalidRangeFaultForReason(
        "ValidityIntervalExcludesBlockSlot",
        { ClosedRange: { lower: 8n, upper: 10n } },
        10n,
      ),
    ).toBe(false);
  });
  it("preserves accepted malformed and excluded behavior", () => {
    expect(
      invalidRangeEvidenceCloses({
        ...evidence("ValidityIntervalMalformed", "InvalidRange"),
        subject: acceptedVerdictSubject(tx),
      }),
    ).toBe(true);
    expect(
      invalidRangeEvidenceCloses({
        ...evidence("ValidityIntervalExcludesBlockSlot", {
          ClosedRange: { lower: 11n, upper: 12n },
        }),
        subject: acceptedVerdictSubject(tx),
      }),
    ).toBe(true);
  });
  it("keeps production authority callback-free", () =>
    expectTypeOf<keyof InvalidRangeForcedInput>().toEqualTypeOf<"block">());

  it("discovers authenticated wrongful rejections through complete replay", async () => {
    const invalid = adjudicateMidgardNativeTxFullValidity(
      makeNativeTx({
        spendInputCbors: [],
        fee: 0n,
        validityIntervalStart: 5n,
        validityIntervalEnd: 20n,
      }),
      "TxIsInvalid",
    );
    const transactionId = computeMidgardNativeTxId(invalid).toString("hex");
    const source = deriveMidgardNativeTxProofSource(invalid);
    const block = {
      headerHash: "33".repeat(32),
      payloadEnvelopeSha256: "44".repeat(32),
      payloadSha256: "55".repeat(32),
      header: { blockSlot: 10n },
      transactions: [],
      reconstruction: {
        forcedTransactions: [
          {
            key: { transactionId: "22".repeat(32), outputIndex: 0n },
            value: {
              tx_id: transactionId,
              source: {
                compact_cbor: source.compactCbor.toString("hex"),
                witness_set_compact_cbor:
                  source.witnessSetCompactCbor.toString("hex"),
                field_preimage_lengths_cbor:
                  source.fieldPreimageLengthsCbor.toString("hex"),
              },
              verdict: {
                ForcedTxInvalid: {
                  reason: "ValidityIntervalExcludesBlockSlot",
                },
              },
            },
          },
        ],
      },
    } as never;

    expect(detectInvalidRangeForcedReplay(block)).toHaveLength(1);
    await expect(
      INVALID_RANGE_COMPLETE_CANONICAL_REPLAY.replay(block),
    ).resolves.toMatchObject({
      detections: [
        {
          detectionId: `invalid-range:forced:0:${transactionId}:ValidityIntervalExcludesBlockSlot`,
          violationId: "invalid-range",
          position: 0n,
        },
      ],
    });
  });
});
