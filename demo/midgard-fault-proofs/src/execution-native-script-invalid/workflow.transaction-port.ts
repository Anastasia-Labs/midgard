/** The manifest-bound `executionNativeScriptInvalid` cursor transaction port:
 * each stage is one role of the 28-role graph, and the observed thread datum
 * selects the planned transition that continues it. */
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
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
import { workflowFieldCarriageOwnOutputs } from "../workflow/field-carriage-prerequisite.own-carriage-claims.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import { prepareExecutionNativeScriptInvalidWorkflowArtifact } from "./artifact.js";
import type {
  ExecutionNativeScriptInvalidContracts,
  ExecutionNativeScriptInvalidRole,
} from "./contracts.js";
import {
  fitExecutionNativeScriptInvalidSourceTransition,
  fitExecutionNativeScriptInvalidWalkPage,
} from "./fit.js";
import {
  submitExecutionNativeScriptInvalidStep01Accepted,
  submitExecutionNativeScriptInvalidStep01Forced,
} from "./step01.js";
import {
  type ExecutionNativeScriptInvalidCustody,
  submitExecutionNativeScriptInvalidTransition,
} from "./submit.js";
import { submitExecutionNativeScriptInvalidTerminal } from "./terminal.js";
import { isExecutionNativeScriptInvalidWalkRole } from "./walk.js";
import {
  executionNativeScriptInvalidSourceInclusion,
  planExecutionNativeScriptInvalidArtifact,
} from "./workflow.plan.js";

export type ExecutionNativeScriptInvalidWorkflowReferenceScripts = Readonly<{
  /** One per role of `SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER`, in that order. */
  steps: FamilyStepTuple<"executionNativeScriptInvalid", 28, UTxO>;
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
export const executionNativeScriptInvalidRoleAt = (
  step: number,
): ExecutionNativeScriptInvalidRole => {
  const role = SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER[step - 1];
  if (role === undefined)
    throw new Error(
      `executionNativeScriptInvalid has no step ${step.toString()}`,
    );
  return role;
};

/** The deployment the submitters read, from the bound manifest. */
export const executionNativeScriptInvalidWorkflowContracts = (
  binding: FraudProofWorkflowDeploymentBinding<"executionNativeScriptInvalid">,
  certificate: FieldPreimageCertificateBinding,
): ExecutionNativeScriptInvalidContracts => {
  const resolved = binding.resolvedContracts;
  const chain = resolved.contracts.executionNativeScriptInvalid;
  if (chain === undefined || resolved.stateQueuePolicyId === undefined)
    throw new Error(
      "execution-native-script-invalid manifest omitted required contracts",
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
  binding: FraudProofWorkflowDeploymentBinding<"executionNativeScriptInvalid">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ExecutionNativeScriptInvalidContracts;
  references: ExecutionNativeScriptInvalidWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const threadAt = async (lucid: LucidEvolution, threadOutRef: string) => {
  const [thread] = await lucid.utxosByOutRef([
    parseOutRef(threadOutRef, "executionNativeScriptInvalid thread"),
  ]);
  if (thread === undefined || typeof thread.datum !== "string")
    throw new Error("executionNativeScriptInvalid thread datum absent");
  return thread as UTxO & { readonly datum: string };
};

export const createExecutionNativeScriptInvalidTransactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"executionNativeScriptInvalid"> => {
  const reference = (role: ExecutionNativeScriptInvalidRole) => {
    const utxo =
      config.references.steps[
        SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER.indexOf(role)
      ];
    if (utxo === undefined)
      throw new Error(`executionNativeScriptInvalid ${role} reference absent`);
    return utxo;
  };
  const planned = (
    artifact: Parameters<
      typeof planExecutionNativeScriptInvalidArtifact
    >[0]["artifact"],
  ) =>
    planExecutionNativeScriptInvalidArtifact({
      artifact,
      contracts: config.contracts,
      prover: config.signer.paymentKeyHash,
    });
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: "executionNativeScriptInvalid",
    prepare: async (args) =>
      await prepareExecutionNativeScriptInvalidWorkflowArtifact({
        ...args,
        replayContext: config.replayContext,
      }),
    capture: async ({ action, artifact, entries }) => {
      const ownOutputs = workflowFieldCarriageOwnOutputs({
        category: "executionNativeScriptInvalid",
        entries,
      });
      const plan = await planned(artifact);
      const { admitted } = plan;
      if (admitted.current.headerHash !== config.binding.definition.headerHash)
        throw new Error("executionNativeScriptInvalid bound header changed");
      const input = cursorFamilyActionInput({
        category: "executionNativeScriptInvalid",
        action,
      });
      if (input.stage === "remove")
        return await captureCursorRemoval({
          category: "executionNativeScriptInvalid",
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
                fraudCategory: "executionNativeScriptInvalid",
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
              throw new Error(
                `executionNativeScriptInvalid unsupported stage ${input.stage}`,
              );
            const role = executionNativeScriptInvalidRoleAt(step);
            const threadOutRef = cursorStringField(input, "threadOutRef");
            const common = {
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
              if (admitted.coordinate.sourceKind === 1) {
                await submitExecutionNativeScriptInvalidStep01Forced({
                  ...common,
                  material: admitted.material,
                  reconstruction: admitted.current,
                });
                return;
              }
              const inclusion =
                await executionNativeScriptInvalidSourceInclusion(admitted);
              const chunks = await resolvePublishedProofChunks({
                lucid: config.lucid,
                address: config.signer.address,
                proofCbor: inclusion.txMembershipProofCbor,
              });
              await submitExecutionNativeScriptInvalidStep01Accepted({
                ...common,
                material: admitted.material,
                blueprint: config.binding.blueprint,
                network: config.binding.network,
                stateQueueBlockOutRef: cursorStringField(
                  input,
                  "stateQueueBlockOutRef",
                ),
                txInclusion: inclusion,
                ...(chunks === undefined
                  ? {}
                  : { publishedProofChunks: chunks }),
                witnessReferenceScripts: config.references.witnesses,
              });
              return;
            }
            if (role === "terminal") {
              await submitExecutionNativeScriptInvalidTerminal({
                ...common,
                witnessReferenceScripts: config.references.witnesses,
              });
              return;
            }
            const thread = await threadAt(config.lucid, threadOutRef);
            const continued = plan.transitionAt(role, thread.datum);
            const carriage = new Map<number, readonly UTxO[]>();
            for (const field of continued.fields) {
              const opening = plan.proof.plan(field).openingPlan;
              if (opening.plan.tier === "Inline") continue;
              const published =
                await resolveFaultProofFieldCarriagePublications({
                  ownOutputs,
                  lucid: config.lucid,
                  planned: opening,
                  publisherAddress: config.signer.address,
                });
              const certificate =
                opening.plan.tier === "Certified"
                  ? await resolveFaultProofFieldPreimageCertificate({
                      ownOutputs,
                      lucid: config.lucid,
                      planned: opening,
                      certificatePolicyId:
                        config.contracts.fieldPreimageCertificatePolicyId,
                      network: config.binding.network,
                    })
                  : undefined;
              if (
                published === undefined ||
                (opening.plan.tier === "Certified" && certificate === undefined)
              )
                throw new Error(
                  `executionNativeScriptInvalid field ${field.toString()} carriage is not published`,
                );
              carriage.set(field, [
                ...published,
                ...(certificate === undefined ? [] : [certificate]),
              ]);
            }
            const custody: ExecutionNativeScriptInvalidCustody = {
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              signer: config.signer,
              unit: threadUnit(thread, config.contracts),
              references: reference,
              carriage: (field) => carriage.get(field) ?? [],
              plan: plan.proof.plan,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            };
            // A page that opens MPF proofs or walks the native source is sized to
            // the transaction budget.
            const transition = plan.isSourceRole(role)
              ? await fitExecutionNativeScriptInvalidSourceTransition({
                  custody,
                  paging: plan.paging,
                  role,
                  thread,
                })
              : isExecutionNativeScriptInvalidWalkRole(role) &&
                  plan.proof.walk !== null
                ? await fitExecutionNativeScriptInvalidWalkPage({
                    custody,
                    walk: plan.proof.walk,
                    role,
                    thread,
                  })
                : continued;
            await submitExecutionNativeScriptInvalidTransition(
              custody,
              thread,
              transition,
            );
          },
        ),
      };
    },
  };
};

/** The thread's one computation-thread token. */
const threadUnit = (
  thread: UTxO,
  contracts: ExecutionNativeScriptInvalidContracts,
) => {
  const units = Object.keys(thread.assets).filter((unit) =>
    unit.startsWith(contracts.computationThread.policyId),
  );
  if (units.length !== 1)
    throw new Error(
      "executionNativeScriptInvalid thread carries no unique thread token",
    );
  return units[0]!;
};

export type ManifestBoundExecutionNativeScriptInvalidWorkflowConfig = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: ExecutionNativeScriptInvalidWorkflowReferenceScripts;
  l1Source: FraudProofL1Source;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
