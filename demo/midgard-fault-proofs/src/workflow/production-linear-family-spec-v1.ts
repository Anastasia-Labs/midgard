import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

export const LINEAR_FAMILY_SPEC =
  "midgard-production-linear-family-spec-v1" as const;

export const LINEAR_FAMILY_CATEGORIES = Object.freeze([
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

export type LinearFamilyCategory = (typeof LINEAR_FAMILY_CATEGORIES)[number];

export type LinearFamilyStep = Readonly<{
  /** One-based position in the authenticated computation-thread chain. */
  ordinal: 1 | 2 | 3 | 4;
  actionId: `step_0${1 | 2 | 3 | 4}`;
  rawL1Role: `computation_thread_step_0${1 | 2 | 3 | 4}`;
  manifestContractName: string;
  /** The final step burns the computation thread and mints the proof token. */
  terminalStep: boolean;
}>;

export type LinearFamilySpec = Readonly<{
  schemaVersion: typeof LINEAR_FAMILY_SPEC;
  category: LinearFamilyCategory;
  steps: readonly LinearFamilyStep[];
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
): readonly LinearFamilyStep[] =>
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
  category: LinearFamilyCategory,
  manifestContractNames: readonly [string, ...string[]],
): LinearFamilySpec =>
  Object.freeze({
    schemaVersion: LINEAR_FAMILY_SPEC,
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
] as const satisfies readonly LinearFamilySpec[];

if (
  rows.length !== LINEAR_FAMILY_CATEGORIES.length ||
  rows.some((row, index) => row.category !== LINEAR_FAMILY_CATEGORIES[index])
) {
  throw new Error("production linear family spec order is not canonical");
}

export const LINEAR_FAMILY_SPECS = Object.freeze(rows);

export const linearFamilySpec = <Category extends LinearFamilyCategory>(
  category: Category,
): LinearFamilySpec & { readonly category: Category } => {
  const found = LINEAR_FAMILY_SPECS.find(
    (candidate) => candidate.category === category,
  );
  if (found === undefined) {
    throw new Error(`no production linear family spec for ${category}`);
  }
  return found as LinearFamilySpec & {
    readonly category: Category;
  };
};
