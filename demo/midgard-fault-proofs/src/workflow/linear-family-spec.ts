import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

export const LINEAR_FAMILY_SPEC =
  "midgard-production-linear-family-spec-v1" as const;

/**
 * Categories whose production workflows use the linear adapter. A cursor
 * workflow stays in the cursor spec even when its chain has no self-loop.
 */
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
  "minFee",
  "doubleWithdraw",
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
  /**
   * Exact chain successors one confirmed transaction from this step may
   * produce: later step ordinals, or the permanent proof token. Most chains
   * are straight lines; a branching chain (accepted versus forced evidence)
   * lists every on-chain continuation target.
   */
  successors: readonly LinearFamilySuccessor[];
  /** A step that may burn the computation thread and mint the proof token. */
  terminalStep: boolean;
}>;
export type LinearFamilySuccessor = 2 | 3 | 4 | "proof_token";

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

type SuccessorOverrides = Readonly<
  Partial<Record<1 | 2 | 3 | 4, readonly LinearFamilySuccessor[]>>
>;

type LinearFamilyStepCount = LinearFamilyStep["ordinal"];

/**
 * `Count` spec steps as a tuple of that length, so a family's step count is
 * visible at the type level (the reference-script types in
 * `family-definition.ts` derive their step tuples from it).
 */
export type LinearFamilyStepsFor<
  Count extends LinearFamilyStepCount,
  Steps extends readonly LinearFamilyStep[] = readonly [],
> = Steps["length"] extends Count
  ? Steps
  : LinearFamilyStepsFor<Count, readonly [...Steps, LinearFamilyStep]>;

const steps = (
  manifestContractNames: readonly string[],
  overrides: SuccessorOverrides,
): readonly LinearFamilyStep[] => {
  const stepCount = manifestContractNames.length;
  return Object.freeze(
    manifestContractNames.map((manifestContractName, index) => {
      const ordinal = (index + 1) as 1 | 2 | 3 | 4;
      if (ordinal > 4) {
        throw new Error("production linear family cannot exceed four steps");
      }
      const successors: readonly LinearFamilySuccessor[] =
        overrides[ordinal] ??
        (ordinal === stepCount
          ? ["proof_token"]
          : [(ordinal + 1) as 2 | 3 | 4]);
      if (successors.length === 0) {
        throw new Error(
          `production linear family step ${ordinal.toString()} has no successor`,
        );
      }
      for (const successor of successors) {
        if (
          successor !== "proof_token" &&
          (successor <= ordinal || successor > stepCount)
        ) {
          throw new Error(
            `production linear family step ${ordinal.toString()} successor ${successor.toString()} leaves its exact chain`,
          );
        }
      }
      if (ordinal === stepCount && !successors.includes("proof_token")) {
        throw new Error(
          "production linear family last step must mint the proof token",
        );
      }
      return Object.freeze({
        ordinal,
        actionId: `step_0${ordinal}` as const,
        rawL1Role: `computation_thread_step_0${ordinal}` as const,
        manifestContractName,
        successors: Object.freeze([...successors]),
        terminalStep: successors.includes("proof_token"),
      });
    }),
  );
};

/**
 * A category's spec over core's deployment step names, one spec step per
 * name. The declared count carries the step tuple length into the types and
 * must equal core's.
 */
const spec = <
  Category extends LinearFamilyCategory,
  const Count extends LinearFamilyStepCount,
>(
  category: Category,
  stepCount: Count,
  overrides: SuccessorOverrides = {},
): LinearFamilySpec &
  Readonly<{ category: Category; steps: LinearFamilyStepsFor<Count> }> => {
  const manifestContractNames = deploymentFamilyStepNames(category);
  if (manifestContractNames.length !== stepCount) {
    throw new Error(
      `production linear family ${category} declares ${stepCount.toString()} steps but core deploys ${manifestContractNames.length.toString()}`,
    );
  }
  return Object.freeze({
    schemaVersion: LINEAR_FAMILY_SPEC,
    category,
    steps: steps(
      manifestContractNames,
      overrides,
    ) as unknown as LinearFamilyStepsFor<Count>,
    terminalSemantics: TERMINAL_SEMANTICS,
  });
};

const rows = [
  spec("nonExistentInput", 4),
  spec("nonExistentInputNoIndex", 4),
  spec("invalidRange", 2),
  spec("zeroInput", 2),
  spec("daHashPreimage", 2),
  spec("noReferenceInput", 4),
  spec("referenceInputNoIdx", 4),
  spec("invalidSignature", 2),
  spec("fabricatedDeposit", 4),
  spec("fabricatedWithdrawal", 4),
  spec("withdrawnReferenceInput", 3),
  spec("minFee", 2),
  spec("doubleWithdraw", 2),
  spec("l2TxMistag", 2),
  spec("withdrawnInput", 3),
  // Accepted evidence finalizes at step-02; forced evidence branches from
  // step-01 to step-03 and finalizes at step-04 (validators/fraud-proofs/
  // input-set-uniqueness/step-0{1,2,3,4}.ak).
  spec("inputSetUniqueness", 4, {
    1: [2, 3],
    2: ["proof_token"],
    3: [4],
    4: ["proof_token"],
  }),
] as const satisfies readonly LinearFamilySpec[];

if (
  rows.length !== LINEAR_FAMILY_CATEGORIES.length ||
  rows.some((row, index) => row.category !== LINEAR_FAMILY_CATEGORIES[index])
) {
  throw new Error("production linear family spec order is not canonical");
}

export const LINEAR_FAMILY_SPECS = Object.freeze(rows);

/** The exact spec row of one category, with its step tuple length. */
export type LinearFamilySpecOf<Category extends LinearFamilyCategory> = Extract<
  (typeof LINEAR_FAMILY_SPECS)[number],
  { readonly category: Category }
>;

export const linearFamilySpec = <Category extends LinearFamilyCategory>(
  category: Category,
): LinearFamilySpecOf<Category> => {
  const found = LINEAR_FAMILY_SPECS.find(
    (candidate) => candidate.category === category,
  );
  if (found === undefined) {
    throw new Error(`no production linear family spec for ${category}`);
  }
  return found as unknown as LinearFamilySpecOf<Category>;
};
