import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, expectTypeOf, it } from "vitest";

import {
  classifyZeroInputFinding,
  prepareZeroInputEvidence,
  ZERO_INPUT_CATEGORY,
  ZERO_INPUT_CATEGORY_ID,
  ZERO_INPUT_FIELD_INDEX,
  zeroInputEvidenceCloses,
  ZeroInputStateSchema,
} from "../src/zero-input/family.js";
import {
  detectZeroInputForcedReplay,
  selectCanonicalZeroInputForcedDetection,
} from "../src/zero-input/replay.js";
import {
  prepareZeroInputForcedPlan,
  type ZeroInputForcedInput,
} from "../src/zero-input/v1.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

const transactionId = "01".repeat(32);
const accepted = acceptedVerdictSubject(transactionId);
const forced = forcedVerdictSubject({
  transactionId,
  sourceKey: { transactionId: "02".repeat(32), outputIndex: 0n },
  rejectionReason: "EmptyInputs",
});
const inputField = (count: number) =>
  encodeMidgardFieldPreimage(
    Array.from({ length: count }, (_, index) => Buffer.alloc(34, index + 1)),
  );
const evidence = (subject: typeof accepted, count: number) => {
  const field = inputField(count);
  return prepareZeroInputEvidence({
    finding: { subject },
    inputFieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
};

describe("zeroInput direction-complete V1 semantics", () => {
  it("preserves the existing category and field identity", () => {
    expect(ZERO_INPUT_CATEGORY).toBe("zeroInput");
    expect(ZERO_INPUT_CATEGORY_ID).toBe("00000005");
    expect(ZERO_INPUT_FIELD_INDEX).toBe(0);
  });

  it("preserves accepted-invalid semantics at the exact boundary", () => {
    expect(zeroInputEvidenceCloses(evidence(accepted, 0))).toBe(true);
    expect(zeroInputEvidenceCloses(evidence(accepted, 1))).toBe(false);
  });

  it("proves complete negation for wrongful forced rejection", () => {
    expect(zeroInputEvidenceCloses(evidence(forced, 1))).toBe(true);
    expect(zeroInputEvidenceCloses(evidence(forced, 2))).toBe(true);
    expect(zeroInputEvidenceCloses(evidence(forced, 0))).toBe(false);
  });

  it("derives the count from authenticated field bytes", () => {
    const prepared = evidence(forced, 1);
    expect(prepared.inputCount).toBe(1);
    expect(prepared.inputFieldCommitment).toBe(
      midgardFieldCommitment(
        Buffer.from(prepared.inputFieldPreimageCbor, "hex"),
      ).toString("hex"),
    );
    expect(() =>
      prepareZeroInputEvidence({
        finding: { subject: forced },
        inputFieldPreimage: inputField(1),
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/changed commitment/u);
  });

  it("binds exactly EmptyInputs and refuses reason injection", () => {
    const wrong = forcedVerdictSubject({
      transactionId,
      sourceKey: { transactionId: "02".repeat(32), outputIndex: 0n },
      rejectionReason: "NetworkIdMismatch",
    });
    expect(() => classifyZeroInputFinding({ subject: wrong })).toThrow(
      /typed rejection reason changed/u,
    );
    expect(() =>
      classifyZeroInputFinding({
        subject: { ...accepted, rejection_reason: "EmptyInputs" },
      }),
    ).toThrow(/not canonical|polarity changed/u);
  });

  it("round-trips the canonical directional state wire", () => {
    const state = { subject: forced };
    const encoded = Data.to(state as never, ZeroInputStateSchema as never);
    expect(
      Data.from(encoded, ZeroInputStateSchema as never) as unknown,
    ).toEqual(state);
  });

  it("selects direct and publication carriage from real field bytes", () => {
    expect(evidence(forced, 1).carriage).toBe("Inline");
    expect(evidence(forced, 400).carriage).not.toBe("Inline");
  });

  it("derives the wrongful-rejection evidence from the authenticated forced leaf", () => {
    const item = encodeMidgardSpendInputItem({
      txId: Buffer.from("03".repeat(32), "hex"),
      outputIndex: 0,
    });
    const submitted = makeNativeTx({ spendInputCbors: [item], fee: 0n });
    const invalid = adjudicateMidgardNativeTxFullValidity(
      submitted,
      "TxIsInvalid",
    );
    const transactionId = computeMidgardNativeTxId(invalid).toString("hex");
    const source = deriveMidgardNativeTxProofSource(invalid);
    const detections = detectZeroInputForcedReplay({
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
            fullTransactionCbor: encodeMidgardNativeTxCanonical(invalid),
          },
        ],
      },
    } as never);
    expect(detections).toHaveLength(1);
    expect(detections[0]!.evidence.inputCount).toBe(1);
    expect(selectCanonicalZeroInputForcedDetection(detections)).toBe(
      detections[0],
    );
  });

  it("keeps the production authority boundary callback-free", () => {
    expectTypeOf<keyof ZeroInputForcedInput>().toEqualTypeOf<"block">();
    expect(prepareZeroInputForcedPlan).toHaveLength(1);
    expect(Object.keys({ block: null })).toEqual(["block"]);
  });
});
