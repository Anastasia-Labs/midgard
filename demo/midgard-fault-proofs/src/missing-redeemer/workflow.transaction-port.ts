/** The manifest-bound `missingRedeemer` cursor transaction port: each stage
 * is one role of the 39-role graph, and the observed thread datum selects
 * the planned transition that continues it. */
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
import { prepareMissingRedeemerWorkflowArtifact } from "./artifact.js";
import type {
  MissingRedeemerContracts,
  MissingRedeemerRole,
} from "./contracts.js";
import {
  submitMissingRedeemerStep01Accepted,
  submitMissingRedeemerStep01Forced,
} from "./step01.js";
import { submitMissingRedeemerTransition } from "./submit.js";
import { submitMissingRedeemerTerminal } from "./terminal.js";
import {
  missingRedeemerSourceInclusion,
  planMissingRedeemerArtifact,
} from "./workflow.plan.js";

export type MissingRedeemerWorkflowReferenceScripts = Readonly<{
  /** One per role of `SDK.MISSING_REDEEMER_ROLE_ORDER`, in that order. */
  steps: FamilyStepTuple<"missingRedeemer", 39, UTxO>;
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
export const missingRedeemerRoleAt = (step: number): MissingRedeemerRole => {
  const role = SDK.MISSING_REDEEMER_ROLE_ORDER[step - 1];
  if (role === undefined)
    throw new Error(`missingRedeemer has no step ${step.toString()}`);
  return role;
};

/** The deployment the submitters read, from the bound manifest. */
export const missingRedeemerWorkflowContracts = (
  binding: FraudProofWorkflowDeploymentBinding<"missingRedeemer">,
  certificate: FieldPreimageCertificateBinding,
): MissingRedeemerContracts => {
  const resolved = binding.resolvedContracts;
  const chain = resolved.contracts.missingRedeemer;
  if (chain === undefined || resolved.stateQueuePolicyId === undefined)
    throw new Error("missing-redeemer manifest omitted required contracts");
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
  binding: FraudProofWorkflowDeploymentBinding<"missingRedeemer">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MissingRedeemerContracts;
  references: MissingRedeemerWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const threadAt = async (lucid: LucidEvolution, threadOutRef: string) => {
  const [thread] = await lucid.utxosByOutRef([
    parseOutRef(threadOutRef, "missingRedeemer thread"),
  ]);
  if (thread === undefined || typeof thread.datum !== "string")
    throw new Error("missingRedeemer thread datum absent");
  return thread as UTxO & { readonly datum: string };
};

export const createMissingRedeemerTransactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"missingRedeemer"> => {
  const reference = (role: MissingRedeemerRole) => {
    const utxo =
      config.references.steps[SDK.MISSING_REDEEMER_ROLE_ORDER.indexOf(role)];
    if (utxo === undefined)
      throw new Error(`missingRedeemer ${role} reference absent`);
    return utxo;
  };
  const planned = (
    artifact: Parameters<typeof planMissingRedeemerArtifact>[0]["artifact"],
  ) =>
    planMissingRedeemerArtifact({
      artifact,
      contracts: config.contracts,
      prover: config.signer.paymentKeyHash,
    });
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: "missingRedeemer",
    prepare: async (args) =>
      await prepareMissingRedeemerWorkflowArtifact({
        ...args,
        replayContext: config.replayContext,
      }),
    capture: async ({ action, artifact, entries }) => {
      const ownOutputs = workflowFieldCarriageOwnOutputs({
        category: "missingRedeemer",
        entries,
      });
      const plan = await planned(artifact);
      const { admitted } = plan;
      if (admitted.current.headerHash !== config.binding.definition.headerHash)
        throw new Error("missingRedeemer bound header changed");
      const input = cursorFamilyActionInput({
        category: "missingRedeemer",
        action,
      });
      if (input.stage === "remove")
        return await captureCursorRemoval({
          category: "missingRedeemer",
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
                fraudCategory: "missingRedeemer",
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
                `missingRedeemer unsupported stage ${input.stage}`,
              );
            const role = missingRedeemerRoleAt(step);
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
                await submitMissingRedeemerStep01Forced({
                  ...common,
                  material: admitted.material,
                  reconstruction: admitted.current,
                });
                return;
              }
              const inclusion = await missingRedeemerSourceInclusion(admitted);
              const chunks = await resolvePublishedProofChunks({
                lucid: config.lucid,
                address: config.signer.address,
                proofCbor: inclusion.txMembershipProofCbor,
              });
              await submitMissingRedeemerStep01Accepted({
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
              await submitMissingRedeemerTerminal({
                ...common,
                witnessReferenceScripts: config.references.witnesses,
              });
              return;
            }
            const thread = await threadAt(config.lucid, threadOutRef);
            const transition = plan.transitionAt(role, thread.datum);
            const carriage = new Map<number, readonly UTxO[]>();
            for (const field of transition.fields) {
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
                  `missingRedeemer field ${field.toString()} carriage is not published`,
                );
              carriage.set(field, [
                ...published,
                ...(certificate === undefined ? [] : [certificate]),
              ]);
            }
            await submitMissingRedeemerTransition(
              {
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
              },
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
const threadUnit = (thread: UTxO, contracts: MissingRedeemerContracts) => {
  const units = Object.keys(thread.assets).filter((unit) =>
    unit.startsWith(contracts.computationThread.policyId),
  );
  if (units.length !== 1)
    throw new Error("missingRedeemer thread carries no unique thread token");
  return units[0]!;
};

export type ManifestBoundMissingRedeemerWorkflowConfig = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingRedeemerWorkflowReferenceScripts;
  l1Source: FraudProofL1Source;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
