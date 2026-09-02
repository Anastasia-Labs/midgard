import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

export const PRODUCTION_LINEAR_FAMILY_SPEC_V1 =
  "midgard-production-linear-family-spec-v1" as const;

export const PRODUCTION_LINEAR_FAMILY_CATEGORIES_V1 = Object.freeze([
  "nonExistentInput",
  "nonExistentInputNoIndex",
  "invalidRange",
  "zeroInput",
  "daHashPreimage",
  "noReferenceInput",
  "referenceInputNoIdx",
  "invalidSignature",
  "fabricatedDeposit",
  "fabricatedWithdrawal",
  "withdrawnReferenceInput",
  "canonicalDecodability",
  "committedFieldShape",
  "minFee",
  "doubleWithdraw",
  "crossBlockDuplicateEvent",
  "l2TxMistag",
  "withdrawnInput",
  "inputSetUniqueness",
] as const satisfies readonly FraudProofCatalogueCategoryName[]);

export type ProductionLinearFamilyCategoryV1 =
  (typeof PRODUCTION_LINEAR_FAMILY_CATEGORIES_V1)[number];

export type ProductionLinearFamilyStepV1 = Readonly<{
  /** One-based position in the authenticated computation-thread chain. */
  ordinal: 1 | 2 | 3 | 4;
  actionId: `step_0${1 | 2 | 3 | 4}`;
  rawL1Role: `computation_thread_step_0${1 | 2 | 3 | 4}`;
  manifestContractName: string;
  /** The final step burns the computation thread and mints the proof token. */
  terminalStep: boolean;
}>;

export type ProductionLinearFamilySpecV1 = Readonly<{
  schemaVersion: typeof PRODUCTION_LINEAR_FAMILY_SPEC_V1;
  category: ProductionLinearFamilyCategoryV1;
  steps: readonly ProductionLinearFamilyStepV1[];
  terminalSemantics: Readonly<{
    proofToken: "permanent_retained_v1";
    correction: "state_queue_removal_references_proof_token_v1";
    economics: "manifest_exact_slash_reward_fee_v1";
  }>;
}>;

const TERMINAL_SEMANTICS = Object.freeze({
  proofToken: "permanent_retained_v1",
  correction: "state_queue_removal_references_proof_token_v1",
  economics: "manifest_exact_slash_reward_fee_v1",
} as const);

const steps = (
  ...manifestContractNames: readonly [string, ...string[]]
): readonly ProductionLinearFamilyStepV1[] =>
  Object.freeze(
    manifestContractNames.map((manifestContractName, index) => {
      const ordinal = (index + 1) as 1 | 2 | 3 | 4;
      if (ordinal > 4) {
        throw new Error("production linear family cannot exceed four steps");
      }
      return Object.freeze({
        ordinal,
        actionId: `step_0${ordinal}` as const,
        rawL1Role: `computation_thread_step_0${ordinal}` as const,
        manifestContractName,
        terminalStep: index === manifestContractNames.length - 1,
      });
    }),
  );

const spec = (
  category: ProductionLinearFamilyCategoryV1,
  manifestContractNames: readonly [string, ...string[]],
): ProductionLinearFamilySpecV1 =>
  Object.freeze({
    schemaVersion: PRODUCTION_LINEAR_FAMILY_SPEC_V1,
    category,
    steps: steps(...manifestContractNames),
    terminalSemantics: TERMINAL_SEMANTICS,
  });

const rows = [
  spec("nonExistentInput", [
    "fraudProofNonExistentInput",
    "fraudProofNonExistentInputStep02",
    "fraudProofNonExistentInputStep03",
    "fraudProofNonExistentInputStep04",
  ]),
  spec("nonExistentInputNoIndex", [
    "fraudProofNonExistentInputNoIndex",
    "fraudProofNonExistentInputNoIndexStep02",
    "fraudProofNonExistentInputNoIndexStep03",
    "fraudProofNonExistentInputNoIndexStep04",
  ]),
  spec("invalidRange", [
    "fraudProofInvalidRange",
    "fraudProofInvalidRangeStep02",
  ]),
  spec("zeroInput", ["fraudProofZeroInput", "fraudProofZeroInputStep02"]),
  spec("daHashPreimage", [
    "fraudProofDaHashPreimage",
    "fraudProofDaHashPreimageStep02",
  ]),
  spec("noReferenceInput", [
    "fraudProofNoReferenceInput",
    "fraudProofNoReferenceInputStep02",
    "fraudProofNoReferenceInputStep03",
    "fraudProofNoReferenceInputStep04",
  ]),
  spec("referenceInputNoIdx", [
    "fraudProofReferenceInputNoIdx",
    "fraudProofReferenceInputNoIdxStep02",
    "fraudProofReferenceInputNoIdxStep03",
    "fraudProofReferenceInputNoIdxStep04",
  ]),
  spec("invalidSignature", [
    "fraudProofInvalidSignature",
    "fraudProofInvalidSignatureStep02",
  ]),
  spec("fabricatedDeposit", [
    "fraudProofFabricatedDeposit",
    "fraudProofFabricatedDepositStep02",
    "fraudProofFabricatedDepositStep03",
    "fraudProofFabricatedDepositStep04",
  ]),
  spec("fabricatedWithdrawal", [
    "fraudProofFabricatedWithdrawal",
    "fraudProofFabricatedWithdrawalStep02",
    "fraudProofFabricatedWithdrawalStep03",
    "fraudProofFabricatedWithdrawalStep04",
  ]),
  spec("withdrawnReferenceInput", [
    "fraudProofWithdrawnReferenceInput",
    "fraudProofWithdrawnReferenceInputStep02",
    "fraudProofWithdrawnReferenceInputStep03",
  ]),
  spec("canonicalDecodability", [
    "fraudProofCanonicalDecodability",
    "fraudProofCanonicalDecodabilityStep02",
  ]),
  spec("committedFieldShape", [
    "fraudProofCommittedFieldShape",
    "fraudProofCommittedFieldShapeStep02",
  ]),
  spec("minFee", ["fraudProofMinFee", "fraudProofMinFeeStep02"]),
  spec("doubleWithdraw", [
    "fraudProofDoubleWithdraw",
    "fraudProofDoubleWithdrawStep02",
  ]),
  spec("crossBlockDuplicateEvent", [
    "fraudProofCrossBlockDuplicateEvent",
    "fraudProofCrossBlockDuplicateEventStep02",
  ]),
  spec("l2TxMistag", ["fraudProofL2TxMistag", "fraudProofL2TxMistagStep02"]),
  spec("withdrawnInput", [
    "fraudProofWithdrawnInput",
    "fraudProofWithdrawnInputStep02",
    "fraudProofWithdrawnInputStep03",
  ]),
  spec("inputSetUniqueness", [
    "fraudProofInputSetUniqueness",
    "fraudProofInputSetUniquenessStep02",
    "fraudProofInputSetUniquenessStep03",
    "fraudProofInputSetUniquenessStep04",
  ]),
] as const satisfies readonly ProductionLinearFamilySpecV1[];

if (
  rows.length !== PRODUCTION_LINEAR_FAMILY_CATEGORIES_V1.length ||
  rows.some(
    (row, index) =>
      row.category !== PRODUCTION_LINEAR_FAMILY_CATEGORIES_V1[index],
  )
) {
  throw new Error("production linear family spec order is not canonical");
}

export const PRODUCTION_LINEAR_FAMILY_SPECS_V1 = Object.freeze(rows);

export const productionLinearFamilySpecV1 = <
  Category extends ProductionLinearFamilyCategoryV1,
>(
  category: Category,
): ProductionLinearFamilySpecV1 & { readonly category: Category } => {
  const found = PRODUCTION_LINEAR_FAMILY_SPECS_V1.find(
    (candidate) => candidate.category === category,
  );
  if (found === undefined) {
    throw new Error(`no production linear family spec for ${category}`);
  }
  return found as ProductionLinearFamilySpecV1 & {
    readonly category: Category;
  };
};
