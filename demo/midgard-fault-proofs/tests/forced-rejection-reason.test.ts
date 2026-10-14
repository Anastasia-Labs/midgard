import * as SDK from "@al-ft/midgard-sdk";
import {
  type RejectCode,
  RejectCodes,
  REJECTION_REASON_ARMS_BY_CODE,
  rejectionReasonArmName,
  type RejectSubject,
  UNSET_REASON_COORDINATE,
} from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  FORCED_REJECTION_NO_ARM_CODES,
  forcedRejectionReason,
  ForcedRejectionStopped,
  ForcedRejectionSubjectMissing,
  ForcedRejectionUnsupported,
  forcedVerdictForRejection,
} from "../src/workflow/forced-rejection-reason.js";

const ALL_CODES = Object.values(RejectCodes) as readonly RejectCode[];
const ARM_CODES = ALL_CODES.filter((code) =>
  REJECTION_REASON_ARMS_BY_CODE.has(code),
);

/** Whether the machine can end a `code` rejection in `reason`'s arm. */
const armOfCode = (code: RejectCode, reason: SDK.RejectionReason): boolean =>
  REJECTION_REASON_ARMS_BY_CODE.get(code)?.includes(
    rejectionReasonArmName(reason),
  ) ?? false;

/** One subject per `RejectSubject` arm, each at a non-zero ordinal, with its code. */
const SUBJECT_CASES: readonly (readonly [
  RejectCode,
  RejectSubject,
  SDK.RejectionReason,
])[] = [
  [
    RejectCodes.InvalidFieldType,
    { arm: "FieldItemWidthIllegal", fieldIndex: 2n, itemIndex: 1n },
    { FieldItemWidthIllegal: { field_index: 2n, item_index: 1n } },
  ],
  [
    RejectCodes.DuplicateInputInTx,
    {
      arm: "DuplicateInput",
      first: { fieldIndex: 0n, itemIndex: 1n },
      second: { fieldIndex: 1n, itemIndex: 2n },
    },
    // The machine carries no duplicate coordinates yet: every one is unset.
    {
      DuplicateInput: {
        first_field_index: UNSET_REASON_COORDINATE,
        first_item_index: UNSET_REASON_COORDINATE,
        second_field_index: UNSET_REASON_COORDINATE,
        second_item_index: UNSET_REASON_COORDINATE,
      },
    },
  ],
  [
    RejectCodes.InvalidSignature,
    { arm: "AddressWitnessSignatureInvalid", index: 3n },
    { AddressWitnessSignatureInvalid: { witness_index: 3n } },
  ],
  [
    RejectCodes.MissingRequiredWitness,
    { arm: "RequiredSignerUnsigned", index: 2n },
    { RequiredSignerUnsigned: { signer_index: 2n } },
  ],
  [
    RejectCodes.NativeScriptInvalid,
    { arm: "WitnessNativeScriptFalse", index: 4n },
    { WitnessNativeScriptFalse: { script_index: 4n } },
  ],
  [
    RejectCodes.InvalidFieldType,
    { arm: "ObserverOrderInvalid", index: 5n },
    { ObserverOrderInvalid: { observer_index: 5n } },
  ],
  [
    RejectCodes.InvalidFieldType,
    { arm: "ScriptIntegrityHashMissing" },
    "ScriptIntegrityHashMissing",
  ],
  [
    RejectCodes.InvalidFieldType,
    { arm: "ObserversForbiddenOnUntaggedNetwork" },
    "ObserversForbiddenOnUntaggedNetwork",
  ],
  [
    RejectCodes.InputNotFound,
    { arm: "InputNotFound", sourceKind: 1n, index: 6n },
    {
      InputNotFound: {
        source_kind: 1n,
        input_index: UNSET_REASON_COORDINATE,
      },
    },
  ],
  [
    RejectCodes.InvalidOutput,
    { arm: "InputSpentOutputNonCanonical", sourceKind: 0n, index: 7n },
    {
      InputSpentOutputNonCanonical: {
        source_kind: 0n,
        input_index: UNSET_REASON_COORDINATE,
      },
    },
  ],
  [
    RejectCodes.MissingRequiredWitness,
    { arm: "SpendInputSignerMissing", index: 8n },
    { SpendInputSignerMissing: { input_index: UNSET_REASON_COORDINATE } },
  ],
  [
    RejectCodes.MissingRequiredWitness,
    { arm: "ProtectedOutputSignerMissing", index: 9n },
    { ProtectedOutputSignerMissing: { output_index: 9n } },
  ],
  [
    RejectCodes.MissingRequiredWitness,
    { arm: "ScriptSourceMissing", purposeKind: 1n, purposeIndex: 2n },
    { ScriptSourceMissing: { purpose_kind: 1n, purpose_index: 2n } },
  ],
  [
    RejectCodes.MissingRequiredWitness,
    { arm: "RedeemerMissing", purposeKind: 2n, purposeIndex: 3n },
    { RedeemerMissing: { purpose_kind: 2n, purpose_index: 3n } },
  ],
  [
    RejectCodes.InvalidFieldType,
    { arm: "RedeemerMalformed", index: 11n },
    { RedeemerMalformed: { redeemer_index: 11n } },
  ],
  [
    RejectCodes.InvalidFieldType,
    { arm: "UnusedRedeemer", index: 10n },
    { UnusedRedeemer: { redeemer_index: 10n } },
  ],
  [
    RejectCodes.InvalidFieldType,
    { arm: "UnusedScriptWitness", index: 11n },
    { UnusedScriptWitness: { script_index: 11n } },
  ],
  [
    RejectCodes.InvalidFieldType,
    { arm: "ScriptIntegrityHashMismatch" },
    "ScriptIntegrityHashMismatch",
  ],
  [
    RejectCodes.NativeScriptInvalid,
    { arm: "ExecutionNativeScriptFalse", purposeKind: 0n, purposeIndex: 12n },
    { ExecutionNativeScriptFalse: { purpose_kind: 0n, purpose_index: 12n } },
  ],
  [
    RejectCodes.PlutusScriptInvalid,
    {
      arm: "ReceivePurposePlutusV3Forbidden",
      purposeKind: 3n,
      purposeIndex: 13n,
    },
    {
      ReceivePurposePlutusV3Forbidden: { purpose_kind: 3n, purpose_index: 13n },
    },
  ],
  [
    RejectCodes.PlutusScriptInvalid,
    { arm: "PlutusExecutionFailed", purposeKind: 1n, purposeIndex: 14n },
    { PlutusExecutionFailed: { purpose_kind: 1n, purpose_index: 14n } },
  ],
  [
    RejectCodes.InvalidFieldType,
    { arm: "WitnessNativeScriptMalformed", index: 17n },
    { WitnessNativeScriptMalformed: { script_index: 17n } },
  ],
  [
    RejectCodes.InvalidOutput,
    { arm: "OutputNonCanonical", index: 15n },
    { OutputNonCanonical: { output_index: 15n } },
  ],
  [
    RejectCodes.MinAda,
    { arm: "OutputBelowMinAda", index: 16n },
    { OutputBelowMinAda: { output_index: 16n } },
  ],
  [
    RejectCodes.AssetCount,
    { arm: "InputAssetAccumulationLimit", index: 18n, assetIndex: 19n },
    {
      InputAssetAccumulationLimit: {
        input_index: UNSET_REASON_COORDINATE,
        asset_index: 19n,
      },
    },
  ],
  [
    RejectCodes.AssetCount,
    { arm: "OutputAssetAccumulationLimit", index: 20n, assetIndex: 21n },
    { OutputAssetAccumulationLimit: { output_index: 20n, asset_index: 21n } },
  ],
  [
    RejectCodes.AssetCount,
    { arm: "MintAssetAccumulationLimit", index: 22n },
    { MintAssetAccumulationLimit: { mint_index: 22n } },
  ],
];

/** Codes whose single arm carries no coordinate, with the arm the leaf must name. */
const SIMPLE_FAULTS: readonly (readonly [RejectCode, SDK.RejectionReason])[] = [
  [RejectCodes.EmptyInputs, "EmptyInputs"],
  [RejectCodes.InvalidValidityIntervalFormat, "ValidityIntervalMalformed"],
  [RejectCodes.NetworkIdMismatch, "NetworkIdMismatch"],
  [RejectCodes.MinFee, "FeeBelowMinimum"],
  [RejectCodes.ValidityIntervalMismatch, "ValidityIntervalExcludesBlockSlot"],
  [RejectCodes.ValueNotPreserved, "ValueNotPreserved"],
];

/** Codes whose arms carry coordinates, so a leaf needs the rule's subject. */
const LOCATED_CODES = ARM_CODES.filter(
  (code) => !SIMPLE_FAULTS.some(([simple]) => simple === code),
);

describe("forced rejection reason", () => {
  it("writes an arm of the rejection's own code for every code an arm covers", () => {
    expect(ARM_CODES).toHaveLength(19);
    for (const [code] of SIMPLE_FAULTS) {
      expect(armOfCode(code, forcedRejectionReason({ code })), code).toBe(true);
    }
    for (const [code, subject] of SUBJECT_CASES) {
      expect(
        armOfCode(code, forcedRejectionReason({ code, subject })),
        subject.arm,
      ).toBe(true);
    }
  });

  it("writes the arm and coordinates the failing rule recorded", () => {
    expect(new Set(SUBJECT_CASES.map(([, subject]) => subject.arm)).size).toBe(
      SUBJECT_CASES.length,
    );
    for (const [code, subject, reason] of SUBJECT_CASES) {
      expect(
        forcedRejectionReason({ code, subject }),
        subject.arm,
      ).toStrictEqual(reason);
    }
    expect(
      forcedVerdictForRejection({
        code: SUBJECT_CASES[0]![0],
        subject: SUBJECT_CASES[0]![1],
      }),
    ).toStrictEqual({ ForcedTxInvalid: { reason: SUBJECT_CASES[0]![2] } });
  });

  it.each(SIMPLE_FAULTS)("names %s by its single arm", (code, reason) => {
    expect(forcedRejectionReason({ code })).toStrictEqual(reason);
  });

  it("refuses a located code without the subject of the failing rule", () => {
    expect(LOCATED_CODES).toHaveLength(13);
    for (const code of LOCATED_CODES) {
      for (const consensusPhase of [
        undefined,
        "resolveInputs",
        "nativeScripts",
      ] as const) {
        expect(
          () => forcedRejectionReason({ code, consensusPhase }),
          `${code} ${String(consensusPhase)}`,
        ).toThrow(ForcedRejectionSubjectMissing);
      }
    }
  });

  it("never writes a subject whose arm bridges to another code", () => {
    expect(() =>
      forcedRejectionReason({
        code: RejectCodes.MinAda,
        subject: { arm: "UnusedRedeemer", index: 3n },
      }),
    ).toThrow(ForcedRejectionSubjectMissing);
  });

  it("never guesses canonical-decode coordinates when the subject is absent", () => {
    for (const code of LOCATED_CODES) {
      expect(
        () =>
          forcedRejectionReason({ code, consensusPhase: "canonicalDecode" }),
        code,
      ).toThrow(ForcedRejectionSubjectMissing);
    }
  });
});

describe("rejection codes without an arm", () => {
  it("are exactly the node codes no machine arm covers", () => {
    const derived = ALL_CODES.filter(
      (code) => !REJECTION_REASON_ARMS_BY_CODE.has(code),
    );
    expect([...FORCED_REJECTION_NO_ARM_CODES].sort()).toStrictEqual(
      [...derived].sort(),
    );
    expect(FORCED_REJECTION_NO_ARM_CODES).toHaveLength(32);
  });

  it("stops every unsupported code without writing a guessed verdict", () => {
    for (const code of FORCED_REJECTION_NO_ARM_CODES) {
      for (const subject of [
        undefined,
        { arm: "UnusedRedeemer", index: 2n } as const,
      ]) {
        expect(
          () => forcedVerdictForRejection({ code, subject }),
          code,
        ).toThrow(ForcedRejectionUnsupported);
      }
    }
  });

  it("marks only unavailable evaluation as a retryable stop", () => {
    for (const code of ALL_CODES) {
      try {
        const reason = forcedRejectionReason({
          code,
          consensusPhase: "canonicalDecode",
        });
        // Only the six actual coordinate-free machine faults may be written
        // without a subject. This pins every RejectCodes value's disposition.
        expect(
          SIMPLE_FAULTS.some(([simple]) => simple === code),
          code,
        ).toBe(true);
        expect(armOfCode(code, reason), code).toBe(true);
      } catch (error) {
        expect(error, code).toBeInstanceOf(ForcedRejectionStopped);
        expect((error as ForcedRejectionStopped).code, code).toBe(code);
        expect((error as ForcedRejectionStopped).retryable, code).toBe(
          code === RejectCodes.PlutusEvaluationUnavailable,
        );
      }
    }
  });
});
