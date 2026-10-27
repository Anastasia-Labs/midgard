/** The manifest-bound `executionSourceScriptDecoding` cursor transaction
 * port: each stage is one role of the 3-role graph, and the observed thread
 * datum selects the planned transition that continues it. */
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type DirectPurposeContracts,
  type DirectPurposeRole,
  directPurposeRoleIndex,
} from "../direct-purpose-family/family.js";
import { submitDirectPurposeStep01Forced } from "../direct-purpose-family/step01.js";
import {
  type DirectPurposeCustody,
  submitDirectPurposeTransition,
} from "../direct-purpose-family/submit.js";
import { submitDirectPurposeTerminal } from "../direct-purpose-family/terminal.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { parseOutRef, type ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import { parseComputationThreadStage } from "../workflow/computation-thread-steps.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import type {
  FamilyStepTuple,
  FieldPreimageCertificateBinding,
} from "../workflow/family-definition.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import { prepareExecutionSourceScriptDecodingWorkflowArtifact } from "./artifact.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY } from "./contracts.js";
import { planExecutionSourceScriptDecodingArtifact } from "./workflow.plan.js";

const CATEGORY = "executionSourceScriptDecoding";

export type ExecutionSourceScriptDecodingWorkflowReferenceScripts = Readonly<{
  /** One per role of `SDK.EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER`. */
  steps: FamilyStepTuple<typeof CATEGORY, 3, UTxO>;
  witnesses: FaultProofWitnessReferenceScripts &
    Readonly<{
      computationThreadMint: UTxO;
      fraudProofMint: UTxO;
      phasMembershipWithdraw: UTxO;
      chunkedVerifyWithdraw: UTxO;
    }>;
  fieldPreimageCertificateMint: UTxO;
}>;

/** The step a role occupies (1-based). */
export const executionSourceScriptDecodingRoleAt = (
  step: number,
): DirectPurposeRole => {
  const role = SDK.EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER[step - 1];
  if (role === undefined)
    throw new Error(`${CATEGORY} has no step ${step.toString()}`);
  return role;
};

/** The deployment the submitters read, from the bound manifest. */
export const executionSourceScriptDecodingWorkflowContracts = (
  binding: FraudProofWorkflowDeploymentBinding<typeof CATEGORY>,
  certificate: FieldPreimageCertificateBinding,
): DirectPurposeContracts => {
  const resolved = binding.resolvedContracts;
  const chain = resolved.contracts.executionSourceScriptDecoding;
  if (chain === undefined || resolved.stateQueuePolicyId === undefined)
    throw new Error(
      "execution-source-script-decoding manifest omitted required contracts",
    );
  return Object.freeze({
    steps: chain.steps,
    computationThread: resolved.contracts.computationThread,
    fraudProof: {
      policyId: resolved.contracts.fraudProof.policyId,
      mintingScript: resolved.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        resolved.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
};

type BoundConfig = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<typeof CATEGORY>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: DirectPurposeContracts;
  references: ExecutionSourceScriptDecodingWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const threadAt = async (lucid: LucidEvolution, threadOutRef: string) => {
  const [thread] = await lucid.utxosByOutRef([
    parseOutRef(threadOutRef, `${CATEGORY} thread`),
  ]);
  if (thread === undefined || typeof thread.datum !== "string")
    throw new Error(`${CATEGORY} thread datum absent`);
  return thread as UTxO & { readonly datum: string };
};

/** The thread's one computation-thread token. */
const threadUnit = (thread: UTxO, contracts: DirectPurposeContracts) => {
  const units = Object.keys(thread.assets).filter((unit) =>
    unit.startsWith(contracts.computationThread.policyId),
  );
  if (units.length !== 1)
    throw new Error(`${CATEGORY} thread carries no unique thread token`);
  return units[0]!;
};

/** The family opens no committed field. */
const noFieldPlan = (field: number): never => {
  throw new Error(`${CATEGORY} opens no field ${field.toString()}`);
};

export const createExecutionSourceScriptDecodingTransactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<typeof CATEGORY> => {
  const reference = (role: DirectPurposeRole) => {
    const utxo =
      config.references.steps[
        directPurposeRoleIndex(EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY, role)
      ];
    if (utxo === undefined)
      throw new Error(`${CATEGORY} ${role} reference absent`);
    return utxo;
  };
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: CATEGORY,
    prepare: async (args) =>
      await prepareExecutionSourceScriptDecodingWorkflowArtifact({
        ...args,
        replayContext: config.replayContext,
      }),
    capture: async ({ action, artifact }) => {
      const plan = await planExecutionSourceScriptDecodingArtifact({
        artifact,
        contracts: config.contracts,
        prover: config.signer.paymentKeyHash,
      });
      const { admitted } = plan;
      if (admitted.current.headerHash !== config.binding.definition.headerHash)
        throw new Error(`${CATEGORY} bound header changed`);
      const input = cursorFamilyActionInput({ category: CATEGORY, action });
      if (input.stage === "remove")
        return await captureCursorRemoval({
          category: CATEGORY,
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          deploymentInfo: config.binding.deploymentInfo,
          network: config.binding.network,
          signer: config.signer,
          headerHash: admitted.current.headerHash,
          input,
          stateQueueMutationLeaseCoordinator:
            config.stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
        });
      return {
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            if (input.stage === "init") {
              await submitInit({
                lucid: config.lucid,
                blueprint: config.binding.blueprint,
                deploymentInfo: config.binding.deploymentInfo,
                network: config.binding.network,
                signer: config.signer,
                fraudCategory: CATEGORY,
                fraudulentBlockOutRef: cursorStringField(
                  input,
                  "stateQueueBlockOutRef",
                ),
                fraudulentHeaderHash: admitted.current.headerHash,
                witnessReferenceScripts: config.references.witnesses,
                preSubmitBoundary,
                awaitConfirmation: false,
              });
              return;
            }
            const step = parseComputationThreadStage(input.stage);
            if (step === undefined)
              throw new Error(`${CATEGORY} unsupported stage ${input.stage}`);
            const role = executionSourceScriptDecodingRoleAt(step);
            const threadOutRef = cursorStringField(input, "threadOutRef");
            const common = {
              family: EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY,
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.binding.resolvedContracts.category.categoryId,
              signer: config.signer,
              threadOutRef,
              referenceScriptUtxo: reference(role),
              preSubmitBoundary,
              awaitConfirmation: false,
            };
            if (role === "step01") {
              await submitDirectPurposeStep01Forced({
                ...common,
                material: admitted.material,
                reconstruction: admitted.current,
              });
              return;
            }
            if (role === "terminal") {
              await submitDirectPurposeTerminal({
                ...common,
                witnessReferenceScripts: config.references.witnesses,
              });
              return;
            }
            const thread = await threadAt(config.lucid, threadOutRef);
            const custody: DirectPurposeCustody = {
              family: EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY,
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              signer: config.signer,
              unit: threadUnit(thread, config.contracts),
              references: reference,
              carriage: () => [],
              plan: noFieldPlan,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            };
            await submitDirectPurposeTransition(
              custody,
              thread,
              plan.transitionAt(role, thread.datum),
            );
          },
        ),
      };
    },
  };
};

export type ManifestBoundExecutionSourceScriptDecodingWorkflowConfig =
  Readonly<{
    replayContext?: CompleteCanonicalReplayContext;
    manifest: unknown;
    blueprintJson: string;
    deploymentInfo: unknown;
    headerHash: string;
    lucid: LucidEvolution;
    signer: ResolvedProverSigner;
    referenceScripts: ExecutionSourceScriptDecodingWorkflowReferenceScripts;
    l1Source: FraudProofL1Source;
    stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  }>;
