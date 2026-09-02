import {
  adjudicateMidgardNativeTxFullV1Validity,
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1,
  encodeMidgardFieldPreimageV1,
  encodeMidgardNativeTxCanonicalV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItemV1 } from "@al-ft/midgard-core/codec";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, expectTypeOf, it } from "vitest";

import {
  classifyZeroInputFindingV1,
  prepareZeroInputEvidenceV1,
  ZERO_INPUT_CATEGORY_ID_V1,
  ZERO_INPUT_CATEGORY_V1,
  ZERO_INPUT_FIELD_INDEX_V1,
  zeroInputEvidenceClosesV1,
  ZeroInputStateV1Schema,
} from "../src/zero-input/family-v1.js";
import {
  prepareZeroInputForcedProductionPlanV1,
  type ZeroInputForcedProductionInputV1,
} from "../src/zero-input/production-v1.js";
import {
  detectZeroInputForcedReplayV1,
  selectCanonicalZeroInputForcedDetectionV1,
} from "../src/zero-input/replay-v1.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

const transactionId = "01".repeat(32);
const accepted = acceptedVerdictSubjectV1(transactionId);
const forced = forcedVerdictSubjectV1({
  transactionId,
  sourceKey: { transactionId: "02".repeat(32), outputIndex: 0n },
  rejectionReason: "EmptyInputs",
});
const inputField = (count: number) =>
  encodeMidgardFieldPreimageV1(
    Array.from({ length: count }, (_, index) => Buffer.alloc(34, index + 1)),
  );
const evidence = (subject: typeof accepted, count: number) => {
  const field = inputField(count);
  return prepareZeroInputEvidenceV1({
    finding: { subject },
    inputFieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
  });
};

describe("zeroInput direction-complete V1 semantics", () => {
  it("preserves the existing category and field identity", () => {
    expect(ZERO_INPUT_CATEGORY_V1).toBe("zeroInput");
    expect(ZERO_INPUT_CATEGORY_ID_V1).toBe("00000005");
    expect(ZERO_INPUT_FIELD_INDEX_V1).toBe(0);
  });

  it("preserves accepted-invalid semantics at the exact boundary", () => {
    expect(zeroInputEvidenceClosesV1(evidence(accepted, 0))).toBe(true);
    expect(zeroInputEvidenceClosesV1(evidence(accepted, 1))).toBe(false);
  });

  it("proves complete negation for wrongful forced rejection", () => {
    expect(zeroInputEvidenceClosesV1(evidence(forced, 1))).toBe(true);
    expect(zeroInputEvidenceClosesV1(evidence(forced, 2))).toBe(true);
    expect(zeroInputEvidenceClosesV1(evidence(forced, 0))).toBe(false);
  });

  it("derives the count from authenticated field bytes", () => {
    const prepared = evidence(forced, 1);
    expect(prepared.inputCount).toBe(1);
    expect(prepared.inputFieldCommitment).toBe(
      midgardFieldCommitmentV1(
        Buffer.from(prepared.inputFieldPreimageCbor, "hex"),
      ).toString("hex"),
    );
    expect(() =>
      prepareZeroInputEvidenceV1({
        finding: { subject: forced },
        inputFieldPreimage: inputField(1),
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/changed commitment/u);
  });

  it("binds exactly EmptyInputs and refuses reason injection", () => {
    const wrong = forcedVerdictSubjectV1({
      transactionId,
      sourceKey: { transactionId: "02".repeat(32), outputIndex: 0n },
      rejectionReason: "NetworkIdMismatch",
    });
    expect(() => classifyZeroInputFindingV1({ subject: wrong })).toThrow(
      /typed rejection reason changed/u,
    );
    expect(() =>
      classifyZeroInputFindingV1({
        subject: { ...accepted, rejection_reason: "EmptyInputs" },
      }),
    ).toThrow(/not canonical|polarity changed/u);
  });

  it("round-trips the canonical directional state wire", () => {
    const state = { subject: forced };
    const encoded = Data.to(state as never, ZeroInputStateV1Schema as never);
    expect(
      Data.from(encoded, ZeroInputStateV1Schema as never) as unknown,
    ).toEqual(state);
  });

  it("selects direct and publication carriage from real field bytes", () => {
    expect(evidence(forced, 1).carriage).toBe("Inline");
    expect(evidence(forced, 400).carriage).not.toBe("Inline");
  });

  it("derives the wrongful-rejection evidence from the authenticated forced leaf", () => {
    const item = encodeMidgardSpendInputItemV1({
      txId: Buffer.from("03".repeat(32), "hex"),
      outputIndex: 0,
    });
    const submitted = makeNativeTx({ spendInputCbors: [item], fee: 0n });
    const invalid = adjudicateMidgardNativeTxFullV1Validity(
      submitted,
      "TxIsInvalid",
    );
    const transactionId = computeMidgardNativeTxIdV1(invalid).toString("hex");
    const source = deriveMidgardNativeTxProofSourceV1(invalid);
    const detections = detectZeroInputForcedReplayV1({
      headerHash: "04".repeat(28),
      reconstruction: {
        forcedTransactions: [
          {
            key: { transactionId: "05".repeat(32), outputIndex: 0n },
            value: {
              tx_id: transactionId,
              source: {
                compact_cbor: source.compactCbor.toString("hex"),
                witness_set_compact_cbor:
                  source.witnessSetCompactCbor.toString("hex"),
                field_preimage_lengths_cbor:
                  source.fieldPreimageLengthsCbor.toString("hex"),
              },
              verdict: { ForcedTxInvalid: { reason: "EmptyInputs" } },
            },
            fullTransactionCbor: encodeMidgardNativeTxCanonicalV1(invalid),
          },
        ],
      },
    } as never);
    expect(detections).toHaveLength(1);
    expect(detections[0]!.evidence.inputCount).toBe(1);
    expect(selectCanonicalZeroInputForcedDetectionV1(detections)).toBe(
      detections[0],
    );
  });

  it("keeps the production authority boundary callback-free", () => {
    expectTypeOf<
      keyof ZeroInputForcedProductionInputV1
    >().toEqualTypeOf<"block">();
    expect(prepareZeroInputForcedProductionPlanV1).toHaveLength(1);
    expect(Object.keys({ block: null })).toEqual(["block"]);
  });
});
