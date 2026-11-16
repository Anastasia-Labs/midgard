/**
 * A forced leaf's `ForcedTxInvalid{reason}` names coordinates the block
 * producer chose. Classifying a block whose leaf names a coordinate its valid
 * native forced transaction does not have returns a decision; it never
 * throws. Each such wrongful rejection is decided as the fault of the family
 * that owns the reason: the coordinate may be negative, at or past the item
 * count of the field it names, past 2^53, or at either bound of the codec.
 */
import type * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  BIG,
  classify,
  fault,
  MAX,
  MIN,
  reason,
} from "./forced-claim.test-support.js";

/** A duplicate claim pairing the first spend input with the given input. */
const duplicateOf = (second_field_index: bigint, second_item_index: bigint) =>
  reason("DuplicateInput", {
    first_field_index: 0n,
    first_item_index: 0n,
    second_field_index,
    second_item_index,
  });

const plutusUndecodable = (
  item_kind: bigint,
  item_index: bigint,
  decodeClass: bigint,
) =>
  reason("PlutusScriptUndecodable", {
    item_kind,
    item_index,
    class: decodeClass,
  });

/** One claim per coordinate: past the field, negative, past 2^53, at MAX. */
const absentAt = (
  label: string,
  claim: (coordinate: bigint) => SDK.RejectionReason,
  past: bigint,
  category: string,
): readonly (readonly [string, SDK.RejectionReason, string])[] =>
  (
    [
      ["past the last item", past],
      ["at a negative index", -1n],
      ["past 2^53", BIG],
      ["at the largest index", MAX],
      ["at the smallest index", MIN],
    ] as const
  ).map(([where, coordinate]) => [
    `${label} ${where}`,
    claim(coordinate),
    fault(category),
  ]);

const CASES: readonly (readonly [string, SDK.RejectionReason, string])[] = [
  // Field-shape claims.
  ...absentAt(
    "field length of a field",
    (field_index) => reason("FieldPreimageLengthMismatch", { field_index }),
    9n,
    "fieldPreimageLengthMismatch",
  ),
  [
    "empty reference-input field length",
    reason("FieldPreimageLengthMismatch", { field_index: 1n }),
    fault("fieldPreimageLengthMismatch"),
  ],
  ...absentAt(
    "item width of an item",
    (item_index) =>
      reason("FieldItemWidthIllegal", { field_index: 5n, item_index }),
    1n,
    "fieldItemWidthIllegal",
  ),
  ...absentAt(
    "item width of a field",
    (field_index) =>
      reason("FieldItemWidthIllegal", { field_index, item_index: 0n }),
    9n,
    "fieldItemWidthIllegal",
  ),
  // Input claims.
  ...absentAt(
    "duplicate naming a second spend input",
    (index) => duplicateOf(0n, index),
    1n,
    "inputSetUniqueness",
  ),
  ...absentAt(
    "duplicate naming a second input field",
    (field) => duplicateOf(field, 0n),
    2n,
    "inputSetUniqueness",
  ),
  [
    "duplicate naming an absent reference input",
    duplicateOf(1n, 0n),
    fault("inputSetUniqueness"),
  ],
  [
    "duplicate naming one coordinate twice",
    duplicateOf(0n, 0n),
    fault("inputSetUniqueness"),
  ],
  ...absentAt(
    "missing spend input",
    (input_index) => reason("InputNotFound", { source_kind: 0n, input_index }),
    1n,
    "nonExistentInput",
  ),
  [
    "missing reference input",
    reason("InputNotFound", { source_kind: 1n, input_index: 0n }),
    fault("noReferenceInput"),
  ],
  ...absentAt(
    "missing input of an input kind",
    (source_kind) => reason("InputNotFound", { source_kind, input_index: 0n }),
    2n,
    "nonExistentInput",
  ),
  ...absentAt(
    "non-canonical spent output of a spend input",
    (input_index) =>
      reason("InputSpentOutputNonCanonical", { source_kind: 0n, input_index }),
    1n,
    "resolvedOutputNonCanonical",
  ),
  [
    "non-canonical spent output of an absent reference input",
    reason("InputSpentOutputNonCanonical", {
      source_kind: 1n,
      input_index: 0n,
    }),
    fault("resolvedOutputNonCanonical"),
  ],
  ...absentAt(
    "non-canonical spent output of an input kind",
    (source_kind) =>
      reason("InputSpentOutputNonCanonical", { source_kind, input_index: 0n }),
    2n,
    "resolvedOutputNonCanonical",
  ),
  ...(
    [
      "ResolvedReferenceScriptMalformed",
      "ResolvedReferenceScriptNodeLimit",
      "ResolvedReferenceScriptDepthLimit",
    ] as const
  ).flatMap((arm) => [
    ...absentAt(
      `${arm} of a spend input`,
      (input_index) => reason(arm, { source_kind: 0n, input_index }),
      1n,
      "nativeScriptDecoding",
    ),
    ...absentAt(
      `${arm} of an input kind`,
      (source_kind) => reason(arm, { source_kind, input_index: 0n }),
      2n,
      "nativeScriptDecoding",
    ),
    [
      `${arm} of an absent reference input`,
      reason(arm, { source_kind: 1n, input_index: 0n }),
      fault("nativeScriptDecoding"),
    ] as const,
  ]),
  ...absentAt(
    "spend signer",
    (input_index) => reason("SpendInputSignerMissing", { input_index }),
    1n,
    "spendInputSignerMissing",
  ),
  // Signature claims.
  ...absentAt(
    "address witness",
    (witness_index) =>
      reason("AddressWitnessSignatureInvalid", { witness_index }),
    1n,
    "invalidSignature",
  ),
  ...absentAt(
    "required signer",
    (signer_index) => reason("RequiredSignerUnsigned", { signer_index }),
    0n,
    "missingSignature",
  ),
  // Witness-script claims.
  ...(
    [
      "WitnessScriptHeaderMalformed",
      "WitnessNativeScriptMalformed",
      "WitnessNativeScriptNodeLimit",
      "WitnessNativeScriptDepthLimit",
    ] as const
  ).flatMap((arm) =>
    absentAt(
      `${arm} of a witness script`,
      (script_index) => reason(arm, { script_index }),
      1n,
      "witnessScriptDecoding",
    ),
  ),
  [
    "in-range witness script node limit",
    reason("WitnessNativeScriptNodeLimit", { script_index: 0n }),
    fault("witnessScriptDecoding"),
  ],
  ...absentAt(
    "false witness script",
    (script_index) => reason("WitnessNativeScriptFalse", { script_index }),
    1n,
    "nativeScriptInvalid",
  ),
  ...absentAt(
    "unused witness script",
    (script_index) => reason("UnusedScriptWitness", { script_index }),
    1n,
    "unusedScriptWitness",
  ),
  // Redeemer claims, against a transaction with no redeemers.
  ...absentAt(
    "malformed redeemer",
    (redeemer_index) => reason("RedeemerMalformed", { redeemer_index }),
    0n,
    "redeemerCanonicity",
  ),
  ...absentAt(
    "unused redeemer",
    (redeemer_index) => reason("UnusedRedeemer", { redeemer_index }),
    0n,
    "unusedRedeemer",
  ),
  [
    "failed Plutus execution of a native mint",
    reason("PlutusExecutionFailed", { purpose_kind: 1n, purpose_index: 0n }),
    fault("redeemerCanonicity"),
  ],
  ...absentAt(
    "failed Plutus execution of a mint",
    (purpose_index) =>
      reason("PlutusExecutionFailed", { purpose_kind: 1n, purpose_index }),
    1n,
    "redeemerCanonicity",
  ),
  ...absentAt(
    "failed Plutus execution of a purpose kind",
    (purpose_kind) =>
      reason("PlutusExecutionFailed", { purpose_kind, purpose_index: 0n }),
    6n,
    "redeemerCanonicity",
  ),
  // Output claims.
  ...absentAt(
    "non-canonical output",
    (output_index) => reason("OutputNonCanonical", { output_index }),
    1n,
    "transactionOutputNonCanonical",
  ),
  ...absentAt(
    "output below min-ADA",
    (output_index) => reason("OutputBelowMinAda", { output_index }),
    1n,
    "minAda",
  ),
  ...(
    [
      "OutputReferenceScriptMalformed",
      "OutputReferenceScriptNodeLimit",
      "OutputReferenceScriptDepthLimit",
    ] as const
  ).flatMap((arm) => [
    [
      `${arm} of an output without one`,
      reason(arm, { output_index: 0n }),
      fault("outputReferenceScriptDecoding"),
    ] as const,
    ...absentAt(
      `${arm} of an output`,
      (output_index) => reason(arm, { output_index }),
      1n,
      "outputReferenceScriptDecoding",
    ),
  ]),
  ...absentAt(
    "protected output signer",
    (output_index) => reason("ProtectedOutputSignerMissing", { output_index }),
    1n,
    "protectedOutputSignerMissing",
  ),
  ...absentAt(
    "output asset accumulation",
    (output_index) =>
      reason("OutputAssetAccumulationLimit", { output_index, asset_index: 0n }),
    1n,
    "distinctAssetAccumulationLimit",
  ),
  // Mint claims.
  ...absentAt(
    "declared asset limit",
    (policy_index) => reason("MintDeclaredAssetLimit", { policy_index }),
    1n,
    "mintDeclaredAssetLimit",
  ),
  ...absentAt(
    "malformed mint item",
    (policy_index) => reason("MintItemMalformed", { policy_index }),
    1n,
    "mintItemNonCanonical",
  ),
  // Observer claims, against a transaction with no observers.
  ...absentAt(
    "observer order",
    (observer_index) => reason("ObserverOrderInvalid", { observer_index }),
    0n,
    "observerOrderInvalid",
  ),
  // Plutus decode claims.
  ...absentAt(
    "undecodable Plutus witness",
    (index) => plutusUndecodable(0n, index, 1n),
    1n,
    "witnessScriptDecoding",
  ),
  [
    "undecodable Plutus reference script of an output without one",
    plutusUndecodable(1n, 0n, 1n),
    fault("outputReferenceScriptDecoding"),
  ],
  ...absentAt(
    "undecodable Plutus reference script",
    (index) => plutusUndecodable(1n, index, 1n),
    1n,
    "outputReferenceScriptDecoding",
  ),
  ...absentAt(
    "undecodable Plutus item of a kind",
    (kind) => plutusUndecodable(kind, 0n, 1n),
    2n,
    "witnessScriptDecoding",
  ),
  [
    "undecodable Plutus reference script naming no decode class",
    plutusUndecodable(1n, 0n, 0n),
    fault("outputReferenceScriptDecoding"),
  ],
  [
    "undecodable Plutus witness naming no decode class",
    plutusUndecodable(0n, 0n, 0n),
    fault("witnessScriptDecoding"),
  ],
];

describe("classifying a wrongful forced rejection whose reason names a coordinate the transaction lacks", () => {
  it.each(CASES)(
    "%s",
    async (_label, claimed, expected) => {
      expect(await classify(claimed)).toBe(expected);
    },
    120_000,
  );
});
