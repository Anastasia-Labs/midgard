import * as SDK from "@al-ft/midgard-sdk";
import {
  type RejectCode,
  RejectCodes,
  type RejectSubject,
} from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  FORCED_REJECTION_NO_ARM_CODES,
  forcedRejectionReason,
  ForcedRejectionSubjectMissing,
  forcedVerdictForRejection,
} from "../src/workflow/forced-rejection-reason.js";

/** The descriptor-code bytes of a node reject code, as `rejection_code_of` returns them. */
const codeHex = (code: string): string =>
  Buffer.from(code, "ascii").toString("hex");

const DESCRIPTOR_CODE_HEXES = new Set<string>(
  Object.values(SDK.RejectionCodes),
);
const ALL_CODES = Object.values(RejectCodes) as readonly RejectCode[];
const ARM_CODES = ALL_CODES.filter((code) =>
  DESCRIPTOR_CODE_HEXES.has(codeHex(code)),
);

/** One subject per `RejectSubject` arm, each at a non-zero ordinal, with its code. */
const SUBJECT_CASES: readonly (readonly [
  RejectCode,
  RejectSubject,
  SDK.RejectionReason,
])[] = [
  [
    RejectCodes.DuplicateInputInTx,
    {
      arm: "DuplicateInput",
      first: { fieldIndex: 0n, itemIndex: 1n },
      second: { fieldIndex: 1n, itemIndex: 2n },
    },
    {
      DuplicateInput: {
        first_field_index: 0n,
        first_item_index: 1n,
        second_field_index: 1n,
        second_item_index: 2n,
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
    { InputNotFound: { source_kind: 1n, input_index: 6n } },
  ],
  [
    RejectCodes.InvalidOutput,
    { arm: "InputSpentOutputNonCanonical", sourceKind: 0n, index: 7n },
    { InputSpentOutputNonCanonical: { source_kind: 0n, input_index: 7n } },
  ],
  [
    RejectCodes.MissingRequiredWitness,
    { arm: "SpendInputSignerMissing", index: 8n },
    { SpendInputSignerMissing: { input_index: 8n } },
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
    { arm: "ExecutionNativeScriptFalse", index: 12n },
    { ExecutionNativeScriptFalse: { execution_index: 12n } },
  ],
  [
    RejectCodes.PlutusScriptInvalid,
    { arm: "ReceivePurposePlutusV3Forbidden", index: 13n },
    { ReceivePurposePlutusV3Forbidden: { execution_index: 13n } },
  ],
  [
    RejectCodes.PlutusScriptInvalid,
    { arm: "PlutusExecutionFailed", index: 14n },
    { PlutusExecutionFailed: { execution_index: 14n } },
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
    { InputAssetAccumulationLimit: { input_index: 18n, asset_index: 19n } },
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

/** The reason recorded for a canonical-decode rejection that has no subject. */
const UNCOMMITTED_DECODE_REASONS: readonly (readonly [
  RejectCode,
  SDK.RejectionReason,
])[] = [
  [
    RejectCodes.InvalidFieldType,
    { FieldItemWidthIllegal: { field_index: 0n, item_index: 0n } },
  ],
  [RejectCodes.InvalidOutput, { OutputNonCanonical: { output_index: 0n } }],
  [
    RejectCodes.FieldPreimageSize,
    { FieldPreimageLengthMismatch: { field_index: 0n } },
  ],
];

describe("forced rejection reason", () => {
  it("round-trips the rejection code of every code an arm covers", () => {
    expect(ARM_CODES).toHaveLength(19);
    for (const [code] of SIMPLE_FAULTS) {
      expect(SDK.rejectionCodeOf(forcedRejectionReason({ code })), code).toBe(
        codeHex(code),
      );
    }
    for (const [code, subject] of SUBJECT_CASES) {
      expect(
        SDK.rejectionCodeOf(forcedRejectionReason({ code, subject })),
        subject.arm,
      ).toBe(codeHex(code));
    }
    for (const [code, reason] of UNCOMMITTED_DECODE_REASONS) {
      expect(SDK.rejectionCodeOf(reason), code).toBe(codeHex(code));
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
        code: RejectCodes.DuplicateInputInTx,
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

  it("records only the three canonical-decode codes that can lack a subject", () => {
    const recorded = new Map(UNCOMMITTED_DECODE_REASONS);
    for (const code of LOCATED_CODES) {
      const rejection = { code, consensusPhase: "canonicalDecode" } as const;
      const expected = recorded.get(code);
      if (expected === undefined) {
        expect(() => forcedRejectionReason(rejection), code).toThrow(
          ForcedRejectionSubjectMissing,
        );
      } else {
        expect(forcedRejectionReason(rejection), code).toStrictEqual(expected);
      }
    }
  });
});

describe("rejection codes without an arm", () => {
  it("are exactly the node codes outside the descriptor code set", () => {
    const derived = ALL_CODES.filter(
      (code) => !DESCRIPTOR_CODE_HEXES.has(codeHex(code)),
    );
    expect([...FORCED_REJECTION_NO_ARM_CODES].sort()).toStrictEqual(
      [...derived].sort(),
    );
    expect(FORCED_REJECTION_NO_ARM_CODES).toHaveLength(31);
  });

  it("keep the verdict the node has always written for them", () => {
    for (const code of FORCED_REJECTION_NO_ARM_CODES) {
      const expected: SDK.RejectionReason =
        code === RejectCodes.PlutusEvaluationUnavailable
          ? { PlutusExecutionFailed: { execution_index: 0n } }
          : "ValueNotPreserved";
      expect(forcedRejectionReason({ code }), code).toStrictEqual(expected);
      expect(
        forcedRejectionReason({
          code,
          subject: { arm: "UnusedRedeemer", index: 2n },
        }),
        code,
      ).toStrictEqual(expected);
    }
  });
});
