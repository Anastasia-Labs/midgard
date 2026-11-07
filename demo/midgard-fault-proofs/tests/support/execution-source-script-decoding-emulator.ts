/** Real-contract emulator fixtures for the `executionSourceScriptDecoding`
 * family: the harness, its three published role scripts, and a driver from a
 * genuine Init through step-01 and the event selection to the terminal. */
import * as SDK from "@al-ft/midgard-sdk";

import {
  directPurposeFieldPlans,
  type DirectPurposeRole,
} from "../../src/direct-purpose-family/index.js";
import {
  EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY,
  type ExecutionSourceScriptDecodingProofPlan,
  prepareExecutionSourceScriptDecodingProof,
} from "../../src/execution-source-script-decoding/index.js";
import {
  DIRECT_PURPOSE_STEP_BOUNDS,
  type DirectPurposeHarness,
  driveDirectPurposeThread,
  publishDirectPurposeRoles,
  type RoleReference,
} from "./direct-purpose-emulator.js";
import type { MissingRedeemerScenario } from "./missing-redeemer-emulator.subject.js";
import {
  makeFaultProofEmulatorHarness,
  registerChunkedVerifyRewardAccount,
} from "./submit-init-emulator-shared.js";

export {
  type RoleReference,
  type StepMeasurement,
  stepTable,
} from "./direct-purpose-emulator.js";

export const makeExecutionSourceScriptDecodingHarness =
  async (): Promise<ExecutionSourceScriptDecodingHarness> => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realExecutionSourceScriptDecoding: true,
        alwaysFraudProofCatalogue: true,
      },
      registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
    });
    const direct = harness.contracts.executionSourceScriptDecoding;
    const category = harness.catalogue.categories.executionSourceScriptDecoding;
    if (direct === undefined || category === undefined)
      throw new Error(
        "execution-source-script-decoding harness contracts/category were omitted",
      );
    if (
      category.categoryId !==
      SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.executionSourceScriptDecoding
    )
      throw new Error(
        "unexpected execution-source-script-decoding category id",
      );
    return { ...harness, direct, category, essd: direct };
  };

export type ExecutionSourceScriptDecodingHarness = DirectPurposeHarness & {
  readonly essd: DirectPurposeHarness["direct"];
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishExecutionSourceScriptDecodingRoles = (
  harness: DirectPurposeHarness,
) =>
  publishDirectPurposeRoles(harness, EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY);

type Plan = ExecutionSourceScriptDecodingProofPlan & {
  readonly plan: ReturnType<typeof directPurposeFieldPlans>;
};

/** The family's plan with the kit's field plans, for the kit drivers. */
export const prepareExecutionSourceScriptDecodingPlan = async ({
  material,
  prover,
}: Parameters<
  typeof prepareExecutionSourceScriptDecodingProof
>[0]): Promise<Plan> => ({
  ...prepareExecutionSourceScriptDecodingProof({ material, prover }),
  plan: directPurposeFieldPlans(material, prover),
});

/** Genuine Init, the source-appropriate step-01, and the event selection up
 * to the terminal's input. Every thread transaction is measured. */
export const driveExecutionSourceScriptDecodingThread = ({
  harness,
  scenario,
  references,
  transitions,
  beforeStep01,
}: {
  readonly harness: DirectPurposeHarness;
  readonly scenario: MissingRedeemerScenario;
  readonly references: ReadonlyMap<DirectPurposeRole, RoleReference>;
  /** Replaces the planned custody (e.g. an empty prefix for forged rows). */
  readonly transitions?: (prepared: Plan) => Plan["transitions"];
  readonly beforeStep01?: Parameters<
    typeof driveDirectPurposeThread<Plan>
  >[0]["beforeStep01"];
}) =>
  driveDirectPurposeThread<Plan>({
    harness,
    family: EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY,
    scenario,
    references,
    prepare: prepareExecutionSourceScriptDecodingPlan,
    ...(transitions === undefined ? {} : { transitions }),
    ...(beforeStep01 === undefined ? {} : { beforeStep01 }),
  });

/** Per-step ex-unit and size bounds every thread transaction must meet. */
export const EXECUTION_SOURCE_SCRIPT_DECODING_STEP_BOUNDS =
  DIRECT_PURPOSE_STEP_BOUNDS;
