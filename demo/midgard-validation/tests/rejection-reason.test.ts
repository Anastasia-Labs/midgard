import { describe, expect, it } from "vitest";

import {
  type RejectCode,
  RejectCodes,
  rejectionReasonFitsCode,
  type RejectSubject,
  resolveRejectionReason,
} from "../src/index.js";

/**
 * The exact `RejectionReasonV1` the validation machine emits (the `machine`
 * convention) for a rejection code and the subject the failing rule recorded. Every expected reason is
 * written out by hand: the machine's coordinate convention, including the -1
 * it writes where its state carries no exact ordinal yet (every
 * `DuplicateInput` coordinate, the `input_index` of the input-resolution
 * arms and of `InputAssetAccumulationLimit`).
 */
const LOCATED: readonly (readonly [
  string,
  RejectCode,
  RejectSubject,
  unknown,
])[] = [
  [
    "field item width",
    RejectCodes.InvalidFieldType,
    { arm: "FieldItemWidthIllegal", fieldIndex: 4n, itemIndex: 2n },
    { FieldItemWidthIllegal: { field_index: 4n, item_index: 2n } },
  ],
  [
    "duplicate input",
    RejectCodes.DuplicateInputInTx,
    {
      arm: "DuplicateInput",
      first: { fieldIndex: 0n, itemIndex: 1n },
      second: { fieldIndex: 1n, itemIndex: 3n },
    },
    {
      DuplicateInput: {
        first_field_index: -1n,
        first_item_index: -1n,
        second_field_index: -1n,
        second_item_index: -1n,
      },
    },
  ],
  [
    "invalid address witness",
    RejectCodes.InvalidSignature,
    { arm: "AddressWitnessSignatureInvalid", index: 3n },
    { AddressWitnessSignatureInvalid: { witness_index: 3n } },
  ],
  [
    "unsigned required signer",
    RejectCodes.MissingRequiredWitness,
    { arm: "RequiredSignerUnsigned", index: 2n },
    { RequiredSignerUnsigned: { signer_index: 2n } },
  ],
  [
    "false witness native script",
    RejectCodes.NativeScriptInvalid,
    { arm: "WitnessNativeScriptFalse", index: 5n },
    { WitnessNativeScriptFalse: { script_index: 5n } },
  ],
  [
    "observer order",
    RejectCodes.InvalidFieldType,
    { arm: "ObserverOrderInvalid", index: 1n },
    { ObserverOrderInvalid: { observer_index: 1n } },
  ],
  [
    "missing reference input",
    RejectCodes.InputNotFound,
    { arm: "InputNotFound", sourceKind: 1n, index: 4n },
    { InputNotFound: { source_kind: 1n, input_index: -1n } },
  ],
  [
    "non-canonical spent output",
    RejectCodes.InvalidOutput,
    { arm: "InputSpentOutputNonCanonical", sourceKind: 0n, index: 2n },
    { InputSpentOutputNonCanonical: { source_kind: 0n, input_index: -1n } },
  ],
  [
    "spend input signer",
    RejectCodes.MissingRequiredWitness,
    { arm: "SpendInputSignerMissing", index: 3n },
    { SpendInputSignerMissing: { input_index: -1n } },
  ],
  [
    "protected output signer",
    RejectCodes.MissingRequiredWitness,
    { arm: "ProtectedOutputSignerMissing", index: 6n },
    { ProtectedOutputSignerMissing: { output_index: 6n } },
  ],
  [
    "missing script source",
    RejectCodes.MissingRequiredWitness,
    { arm: "ScriptSourceMissing", purposeKind: 3n, purposeIndex: 1n },
    { ScriptSourceMissing: { purpose_kind: 3n, purpose_index: 1n } },
  ],
  [
    "missing redeemer",
    RejectCodes.MissingRequiredWitness,
    { arm: "RedeemerMissing", purposeKind: 1n, purposeIndex: 2n },
    { RedeemerMissing: { purpose_kind: 1n, purpose_index: 2n } },
  ],
  [
    "malformed redeemer",
    RejectCodes.InvalidFieldType,
    { arm: "RedeemerMalformed", index: 2n },
    { RedeemerMalformed: { redeemer_index: 2n } },
  ],
  [
    "unused script witness",
    RejectCodes.InvalidFieldType,
    { arm: "UnusedScriptWitness", index: 4n },
    { UnusedScriptWitness: { script_index: 4n } },
  ],
  [
    "false execution native script",
    RejectCodes.NativeScriptInvalid,
    { arm: "ExecutionNativeScriptFalse", purposeKind: 2n, purposeIndex: 1n },
    { ExecutionNativeScriptFalse: { purpose_kind: 2n, purpose_index: 1n } },
  ],
  [
    "PlutusV3 receive",
    RejectCodes.PlutusScriptInvalid,
    {
      arm: "ReceivePurposePlutusV3Forbidden",
      purposeKind: 3n,
      purposeIndex: 0n,
    },
    {
      ReceivePurposePlutusV3Forbidden: { purpose_kind: 3n, purpose_index: 0n },
    },
  ],
  [
    "failed Plutus execution",
    RejectCodes.PlutusScriptInvalid,
    { arm: "PlutusExecutionFailed", purposeKind: 1n, purposeIndex: 7n },
    { PlutusExecutionFailed: { purpose_kind: 1n, purpose_index: 7n } },
  ],
  [
    "output below min ADA",
    RejectCodes.MinAda,
    { arm: "OutputBelowMinAda", index: 2n },
    { OutputBelowMinAda: { output_index: 2n } },
  ],
  [
    "malformed output reference script",
    RejectCodes.InvalidFieldType,
    { arm: "OutputReferenceScriptMalformed", index: 1n },
    { OutputReferenceScriptMalformed: { output_index: 1n } },
  ],
  [
    "output reference script over the node limit",
    RejectCodes.NativeScriptNodeCount,
    { arm: "OutputReferenceScriptNodeLimit", index: 3n },
    { OutputReferenceScriptNodeLimit: { output_index: 3n } },
  ],
  [
    "output reference script over the depth limit",
    RejectCodes.NativeScriptDepth,
    { arm: "OutputReferenceScriptDepthLimit", index: 4n },
    { OutputReferenceScriptDepthLimit: { output_index: 4n } },
  ],
  [
    "input asset accumulation",
    RejectCodes.AssetCount,
    { arm: "InputAssetAccumulationLimit", index: 3n, assetIndex: 9n },
    { InputAssetAccumulationLimit: { input_index: -1n, asset_index: 9n } },
  ],
  [
    "output asset accumulation",
    RejectCodes.AssetCount,
    { arm: "OutputAssetAccumulationLimit", index: 1n, assetIndex: 8n },
    { OutputAssetAccumulationLimit: { output_index: 1n, asset_index: 8n } },
  ],
  [
    "mint asset accumulation",
    RejectCodes.AssetCount,
    { arm: "MintAssetAccumulationLimit", index: 5n },
    { MintAssetAccumulationLimit: { mint_index: 5n } },
  ],
  [
    "script integrity mismatch",
    RejectCodes.InvalidFieldType,
    { arm: "ScriptIntegrityHashMismatch" },
    "ScriptIntegrityHashMismatch",
  ],
  [
    "witness script the decoder refuses",
    RejectCodes.ScriptProgramEncoding,
    {
      arm: "PlutusScriptUndecodable",
      itemKind: 0n,
      index: 2n,
      refusalClass: 5n,
    },
    { PlutusScriptUndecodable: { item_kind: 0n, item_index: 2n, class: 5n } },
  ],
  [
    "output reference script the decoder refuses",
    RejectCodes.ScriptProgramEncoding,
    {
      arm: "PlutusScriptUndecodable",
      itemKind: 1n,
      index: 7n,
      refusalClass: 17n,
    },
    { PlutusScriptUndecodable: { item_kind: 1n, item_index: 7n, class: 17n } },
  ],
];

/**
 * The arms whose machine coordinates are interim, with the reason a forced
 * leaf records for the same subject: every coordinate exactly as recorded.
 */
const INTERIM: readonly (readonly [
  string,
  RejectCode,
  RejectSubject,
  unknown,
])[] = [
  [
    "duplicate input",
    RejectCodes.DuplicateInputInTx,
    {
      arm: "DuplicateInput",
      first: { fieldIndex: 0n, itemIndex: 1n },
      second: { fieldIndex: 1n, itemIndex: 3n },
    },
    {
      DuplicateInput: {
        first_field_index: 0n,
        first_item_index: 1n,
        second_field_index: 1n,
        second_item_index: 3n,
      },
    },
  ],
  [
    "missing reference input",
    RejectCodes.InputNotFound,
    { arm: "InputNotFound", sourceKind: 1n, index: 4n },
    { InputNotFound: { source_kind: 1n, input_index: 4n } },
  ],
  [
    "non-canonical spent output",
    RejectCodes.InvalidOutput,
    { arm: "InputSpentOutputNonCanonical", sourceKind: 0n, index: 2n },
    { InputSpentOutputNonCanonical: { source_kind: 0n, input_index: 2n } },
  ],
  [
    "spend input signer",
    RejectCodes.MissingRequiredWitness,
    { arm: "SpendInputSignerMissing", index: 3n },
    { SpendInputSignerMissing: { input_index: 3n } },
  ],
  [
    "input asset accumulation",
    RejectCodes.AssetCount,
    { arm: "InputAssetAccumulationLimit", index: 3n, assetIndex: 9n },
    { InputAssetAccumulationLimit: { input_index: 3n, asset_index: 9n } },
  ],
];

describe("resolveRejectionReason", () => {
  it.each(LOCATED)(
    "writes the machine reason for a %s",
    (_name, code, subject, expected) => {
      expect(
        resolveRejectionReason({ code, subject }, "machine"),
      ).toStrictEqual({
        kind: "reason",
        reason: expected,
      });
    },
  );

  it.each(INTERIM)(
    "keeps every recorded coordinate of a %s under the forced leaf's convention",
    (_name, code, subject, expected) => {
      expect(
        resolveRejectionReason({ code, subject }, "subject"),
      ).toStrictEqual({ kind: "reason", reason: expected });
    },
  );

  it.each([
    [RejectCodes.EmptyInputs, "EmptyInputs"],
    [RejectCodes.InvalidValidityIntervalFormat, "ValidityIntervalMalformed"],
    [RejectCodes.ValidityIntervalMismatch, "ValidityIntervalExcludesBlockSlot"],
    [RejectCodes.MinFee, "FeeBelowMinimum"],
    [RejectCodes.ValueNotPreserved, "ValueNotPreserved"],
    [RejectCodes.NetworkIdMismatch, "NetworkIdMismatch"],
  ] as const)(
    "writes the single field-less arm of %s without a subject",
    (code, reason) => {
      expect(resolveRejectionReason({ code }, "machine")).toStrictEqual({
        kind: "reason",
        reason,
      });
    },
  );

  it.each([RejectCodes.PlutusScriptInvalid, RejectCodes.ScriptProgramEncoding])(
    "needs a subject for %s, whose arms carry coordinates",
    (code) => {
      expect(resolveRejectionReason({ code }, "machine")).toStrictEqual({
        kind: "subjectMissing",
      });
    },
  );

  it("requires the carried-script coordinates for the script-decode code", () => {
    expect(
      resolveRejectionReason(
        { code: RejectCodes.ScriptProgramEncoding },
        "subject",
      ),
    ).toStrictEqual({ kind: "subjectMissing" });
    expect(
      rejectionReasonFitsCode(RejectCodes.ScriptProgramEncoding, {
        PlutusScriptUndecodable: { item_kind: 0n, item_index: 2n, class: 4n },
      }),
    ).toBe(true);
  });

  it("never writes a subject whose arm the code cannot end in", () => {
    expect(
      resolveRejectionReason(
        {
          code: RejectCodes.InvalidSignature,
          subject: { arm: "RequiredSignerUnsigned", index: 0n },
        },
        "subject",
      ),
    ).toStrictEqual({ kind: "subjectMissing" });
    expect(
      resolveRejectionReason(
        {
          code: RejectCodes.PlutusScriptInvalid,
          subject: {
            arm: "PlutusScriptUndecodable",
            itemKind: 0n,
            index: 0n,
            refusalClass: 1n,
          },
        },
        "machine",
      ),
    ).toStrictEqual({ kind: "subjectMissing" });
  });

  it.each([
    RejectCodes.AuxDataForbidden,
    RejectCodes.PlutusEvaluationUnavailable,
  ])("has no machine reason for %s", (code) => {
    expect(resolveRejectionReason({ code }, "subject")).toStrictEqual({
      kind: "unsupported",
    });
  });
});

describe("rejectionReasonFitsCode", () => {
  it("accepts an arm the code can end in and refuses another", () => {
    expect(
      rejectionReasonFitsCode(RejectCodes.AssetCount, {
        MintItemMalformed: { policy_index: 0n },
      }),
    ).toBe(false);
    expect(
      rejectionReasonFitsCode(RejectCodes.InvalidFieldType, {
        MintItemMalformed: { policy_index: 0n },
      }),
    ).toBe(true);
  });

  it("throws on a reason that is not a RejectionReasonV1", () => {
    expect(() =>
      rejectionReasonFitsCode(RejectCodes.MinFee, {
        FeeBelowMinimum: {},
      } as never),
    ).toThrow();
  });
});
