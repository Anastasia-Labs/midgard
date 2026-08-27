import { describe, expect, it } from "vitest";

import {
  CANONICAL_DECODABILITY_FRAUD_CATEGORY_ID_V1,
  COMMITTED_FIELD_SHAPE_FRAUD_CATEGORY_ID_V1,
  CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID_V1,
  DOUBLE_WITHDRAW_FRAUD_CATEGORY_ID_V1,
  FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
  FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  L2_TX_MISTAG_FRAUD_CATEGORY_ID_V1,
  MIN_FEE_FRAUD_CATEGORY_ID_V1,
  MISSING_NATIVE_SCRIPT_TX_FRAUD_CATEGORY_ID_V1,
  MISSING_SIGNATURE_FRAUD_CATEGORY_ID_V1,
  NATIVE_SCRIPT_DECODING_FRAUD_CATEGORY_ID_V1,
  WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID_V1,
  WITHDRAWN_INPUT_FRAUD_CATEGORY_ID_V1,
  WITHDRAWN_REFERENCE_INPUT_FRAUD_CATEGORY_ID_V1,
} from "../src/index.js";

describe("production fraud-proof catalogue registration", () => {
  it("pins the append-only category order and four-byte ids through 0x18", () => {
    expect(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER).toEqual([
      "doubleSpend",
      "nonExistentInput",
      "nonExistentInputNoIndex",
      "invalidRange",
      "transitionTrace",
      "zeroInput",
      "validationTraceDispute",
      "daHashPreimage",
      "noReferenceInput",
      "referenceInputNoIdx",
      "invalidSignature",
      "fabricatedDeposit",
      "fabricatedWithdrawal",
      "nativeScriptDecoding",
      "missingSignature",
      "missingNativeScriptTx",
      "withdrawnReferenceInput",
      "canonicalDecodability",
      "committedFieldShape",
      "minFee",
      "withdrawalMistag",
      "doubleWithdraw",
      "crossBlockDuplicateEvent",
      "l2TxMistag",
      "withdrawnInput",
    ]);

    expect(
      Object.fromEntries(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name, index) => [
          name,
          index.toString(16).padStart(8, "0"),
        ]),
      ),
    ).toEqual(FRAUD_PROOF_CATALOGUE_CATEGORY_IDS);
  });

  it("exports canonical ids for every newly registered family", () => {
    expect({
      fabricatedDeposit: FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
      fabricatedWithdrawal: FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
      nativeScriptDecoding: NATIVE_SCRIPT_DECODING_FRAUD_CATEGORY_ID_V1,
      missingSignature: MISSING_SIGNATURE_FRAUD_CATEGORY_ID_V1,
      missingNativeScriptTx: MISSING_NATIVE_SCRIPT_TX_FRAUD_CATEGORY_ID_V1,
      withdrawnReferenceInput: WITHDRAWN_REFERENCE_INPUT_FRAUD_CATEGORY_ID_V1,
      canonicalDecodability: CANONICAL_DECODABILITY_FRAUD_CATEGORY_ID_V1,
      committedFieldShape: COMMITTED_FIELD_SHAPE_FRAUD_CATEGORY_ID_V1,
      minFee: MIN_FEE_FRAUD_CATEGORY_ID_V1,
      withdrawalMistag: WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID_V1,
      doubleWithdraw: DOUBLE_WITHDRAW_FRAUD_CATEGORY_ID_V1,
      crossBlockDuplicateEvent:
        CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID_V1,
      l2TxMistag: L2_TX_MISTAG_FRAUD_CATEGORY_ID_V1,
      withdrawnInput: WITHDRAWN_INPUT_FRAUD_CATEGORY_ID_V1,
    }).toEqual({
      fabricatedDeposit: "0000000b",
      fabricatedWithdrawal: "0000000c",
      nativeScriptDecoding: "0000000d",
      missingSignature: "0000000e",
      missingNativeScriptTx: "0000000f",
      withdrawnReferenceInput: "00000010",
      canonicalDecodability: "00000011",
      committedFieldShape: "00000012",
      minFee: "00000013",
      withdrawalMistag: "00000014",
      doubleWithdraw: "00000015",
      crossBlockDuplicateEvent: "00000016",
      l2TxMistag: "00000017",
      withdrawnInput: "00000018",
    });
  });
});
