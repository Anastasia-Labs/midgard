/** The manifest-bound `distinctAssetAccumulationLimit` cursor transaction port:
 * each stage is one role of the 12-role graph, and the observed thread datum
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
import { prepareDistinctAssetAccumulationLimitWorkflowArtifact } from "./artifact.js";
import type {
  DistinctAssetAccumulationLimitContracts,
  DistinctAssetAccumulationLimitRole,
} from "./contracts.js";
import { fitDistinctAssetAccumulationLimitPage } from "./fit.js";
import {
  submitDistinctAssetAccumulationLimitStep01Accepted,
  submitDistinctAssetAccumulationLimitStep01Forced,
} from "./step01.js";
import {
  type DistinctAssetAccumulationLimitCustody,
  submitDistinctAssetAccumulationLimitTransition,
} from "./submit.js";
import { submitDistinctAssetAccumulationLimitTerminal } from "./terminal.js";
import { isDistinctAssetAccumulationLimitPagedRole } from "./walk.js";
import {
  distinctAssetAccumulationLimitSourceInclusion,
  planDistinctAssetAccumulationLimitArtifact,
} from "./workflow.plan.js";

export type DistinctAssetAccumulationLimitWorkflowReferenceScripts = Readonly<{
  /** One per role of `SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER`, in that order. */
  steps: FamilyStepTuple<"distinctAssetAccumulationLimit", 12, UTxO>;
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
export const distinctAssetAccumulationLimitRoleAt = (
  step: number,
): DistinctAssetAccumulationLimitRole => {
  const role = SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER[step - 1];
  if (role === undefined)
    throw new Error(
      `distinctAssetAccumulationLimit has no step ${step.toString()}`,
    );
  return role;
};

/** The deployment the submitters read, from the bound manifest. */
export const distinctAssetAccumulationLimitWorkflowContracts = (
  binding: FraudProofWorkflowDeploymentBinding<"distinctAssetAccumulationLimit">,
  certificate: FieldPreimageCertificateBinding,
): DistinctAssetAccumulationLimitContracts => {
  const resolved = binding.resolvedContracts;
  const chain = resolved.contracts.distinctAssetAccumulationLimit;
  if (chain === undefined || resolved.stateQueuePolicyId === undefined)
    throw new Error(
      "distinct-asset-accumulation-limit manifest omitted required contracts",
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
  binding: FraudProofWorkflowDeploymentBinding<"distinctAssetAccumulationLimit">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: DistinctAssetAccumulationLimitContracts;
  references: DistinctAssetAccumulationLimitWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const threadAt = async (lucid: LucidEvolution, threadOutRef: string) => {
  const [thread] = await lucid.utxosByOutRef([
    parseOutRef(threadOutRef, "distinctAssetAccumulationLimit thread"),
  ]);
  if (thread === undefined || typeof thread.datum !== "string")
    throw new Error("distinctAssetAccumulationLimit thread datum absent");
  return thread as UTxO & { readonly datum: string };
};

export const createDistinctAssetAccumulationLimitTransactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"distinctAssetAccumulationLimit"> => {
  const reference = (role: DistinctAssetAccumulationLimitRole) => {
    const utxo =
      config.references.steps[
        SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.indexOf(role)
      ];
    if (utxo === undefined)
      throw new Error(
        `distinctAssetAccumulationLimit ${role} reference absent`,
      );
    return utxo;
  };
  const planned = (
    artifact: Parameters<
      typeof planDistinctAssetAccumulationLimitArtifact
    >[0]["artifact"],
  ) =>
    planDistinctAssetAccumulationLimitArtifact({
      artifact,
      contracts: config.contracts,
      prover: config.signer.paymentKeyHash,
    });
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: "distinctAssetAccumulationLimit",
    prepare: async (args) =>
      await prepareDistinctAssetAccumulationLimitWorkflowArtifact({
        ...args,
        replayContext: config.replayContext,
      }),
    capture: async ({ action, artifact, entries }) => {
      const ownOutputs = workflowFieldCarriageOwnOutputs({
        category: "distinctAssetAccumulationLimit",
        entries,
      });
      const plan = await planned(artifact);
      const { admitted } = plan;
      if (admitted.current.headerHash !== config.binding.definition.headerHash)
        throw new Error("distinctAssetAccumulationLimit bound header changed");
      const input = cursorFamilyActionInput({
        category: "distinctAssetAccumulationLimit",
        action,
      });
      if (input.stage === "remove")
        return await captureCursorRemoval({
          category: "distinctAssetAccumulationLimit",
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
                fraudCategory: "distinctAssetAccumulationLimit",
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
                `distinctAssetAccumulationLimit unsupported stage ${input.stage}`,
              );
            const role = distinctAssetAccumulationLimitRoleAt(step);
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
                await submitDistinctAssetAccumulationLimitStep01Forced({
                  ...common,
                  material: admitted.material,
                  reconstruction: admitted.current,
                });
                return;
              }
              const inclusion =
                await distinctAssetAccumulationLimitSourceInclusion(admitted);
              const chunks = await resolvePublishedProofChunks({
                lucid: config.lucid,
                address: config.signer.address,
                proofCbor: inclusion.txMembershipProofCbor,
              });
              await submitDistinctAssetAccumulationLimitStep01Accepted({
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
              await submitDistinctAssetAccumulationLimitTerminal({
                ...common,
                material: admitted.material,
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
                  `distinctAssetAccumulationLimit field ${field.toString()} carriage is not published`,
                );
              carriage.set(field, [
                ...published,
                ...(certificate === undefined ? [] : [certificate]),
              ]);
            }
            const custody: DistinctAssetAccumulationLimitCustody = {
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
            // A paged fold step is sized to the transaction budget.
            const transition = isDistinctAssetAccumulationLimitPagedRole(role)
              ? await fitDistinctAssetAccumulationLimitPage({
                  custody,
                  walk: plan.proof.walk,
                  role,
                  thread,
                })
              : continued;
            await submitDistinctAssetAccumulationLimitTransition(
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
  contracts: DistinctAssetAccumulationLimitContracts,
) => {
  const units = Object.keys(thread.assets).filter((unit) =>
    unit.startsWith(contracts.computationThread.policyId),
  );
  if (units.length !== 1)
    throw new Error(
      "distinctAssetAccumulationLimit thread carries no unique thread token",
    );
  return units[0]!;
};

export type ManifestBoundDistinctAssetAccumulationLimitWorkflowConfig =
  Readonly<{
    replayContext?: CompleteCanonicalReplayContext;
    manifest: unknown;
    blueprintJson: string;
    deploymentInfo: unknown;
    headerHash: string;
    lucid: LucidEvolution;
    signer: ResolvedProverSigner;
    referenceScripts: DistinctAssetAccumulationLimitWorkflowReferenceScripts;
    l1Source: FraudProofL1Source;
    stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  }>;
