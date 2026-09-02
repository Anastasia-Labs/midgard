import {
  adjudicateMidgardNativeTxFullV1Validity,
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, expectTypeOf, it } from "vitest";

import {
  invalidRangeEvidenceClosesV1,
  type InvalidRangeEvidenceV1,
  invalidRangeFaultForReasonV1,
} from "../src/invalid-range/family-v1.js";
import type { InvalidRangeForcedProductionInputV1 } from "../src/invalid-range/production-v1.js";
import { detectInvalidRangeForcedReplayV1 } from "../src/invalid-range/replay-v1.js";
import { INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1 } from "../src/workflow/complete-replay-v1.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

const tx = "11".repeat(32);
const subject = (
  reason: "ValidityIntervalMalformed" | "ValidityIntervalExcludesBlockSlot",
) =>
  forcedVerdictSubjectV1({
    transactionId: tx,
    sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
    rejectionReason: reason,
  });
const evidence = (
  reason: Parameters<typeof subject>[0],
  normalizedRange: InvalidRangeEvidenceV1["normalizedRange"],
): InvalidRangeEvidenceV1 => ({
  subject: subject(reason),
  blockSlot: 10n,
  normalizedRange,
});
describe("invalidRange wrongful rejection V1", () => {
  it("refuses malformed conviction when the interval is well formed", () =>
    expect(
      invalidRangeEvidenceClosesV1(
        evidence("ValidityIntervalMalformed", {
          ClosedRange: { lower: 10n, upper: 10n },
        }),
      ),
    ).toBe(true));
  it("keeps malformed rejection honest for InvalidRange", () =>
    expect(
      invalidRangeEvidenceClosesV1(
        evidence("ValidityIntervalMalformed", "InvalidRange"),
      ),
    ).toBe(false));
  it("treats both exact boundaries as included", () => {
    expect(
      invalidRangeFaultForReasonV1(
        "ValidityIntervalExcludesBlockSlot",
        { ClosedRange: { lower: 10n, upper: 12n } },
        10n,
      ),
    ).toBe(false);
    expect(
      invalidRangeFaultForReasonV1(
        "ValidityIntervalExcludesBlockSlot",
        { ClosedRange: { lower: 8n, upper: 10n } },
        10n,
      ),
    ).toBe(false);
  });
  it("preserves accepted malformed and excluded behavior", () => {
    expect(
      invalidRangeEvidenceClosesV1({
        ...evidence("ValidityIntervalMalformed", "InvalidRange"),
        subject: acceptedVerdictSubjectV1(tx),
      }),
    ).toBe(true);
    expect(
      invalidRangeEvidenceClosesV1({
        ...evidence("ValidityIntervalExcludesBlockSlot", {
          ClosedRange: { lower: 11n, upper: 12n },
        }),
        subject: acceptedVerdictSubjectV1(tx),
      }),
    ).toBe(true);
  });
  it("keeps production authority callback-free", () =>
    expectTypeOf<
      keyof InvalidRangeForcedProductionInputV1
    >().toEqualTypeOf<"block">());

  it("discovers authenticated wrongful rejections through complete replay", async () => {
    const invalid = adjudicateMidgardNativeTxFullV1Validity(
      makeNativeTx({
        spendInputCbors: [],
        fee: 0n,
        validityIntervalStart: 5n,
        validityIntervalEnd: 20n,
      }),
      "TxIsInvalid",
    );
    const transactionId = computeMidgardNativeTxIdV1(invalid).toString("hex");
    const source = deriveMidgardNativeTxProofSourceV1(invalid);
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

    expect(detectInvalidRangeForcedReplayV1(block)).toHaveLength(1);
    await expect(
      INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1.replay(block),
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
