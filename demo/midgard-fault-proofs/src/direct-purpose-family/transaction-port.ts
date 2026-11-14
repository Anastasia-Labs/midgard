/** The manifest-bound cursor transaction port of every direct walk family:
 * each stage is one role of the family's graph, and the observed thread
 * datum selects the planned transition that continues it. */
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
  CHUNKED_VERIFY_WITNESS_ROLES,
  FamilyCategory,
  FamilyStepTuple,
  FieldPreimageCertificateBinding,
} from "../workflow/family-definition.js";
import { workflowFieldCarriageOwnOutputs } from "../workflow/field-carriage-prerequisite.own-carriage-claims.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  directPurposeSourceInclusion,
  planDirectPurposeArtifact,
  prepareDirectPurposeArtifact,
} from "./artifact.js";
import type { DirectPurposeReplay } from "./artifact-family.js";
import type { DirectPurposeFieldPlan } from "./custody.js";
import {
  type DirectPurposeContracts,
  type DirectPurposeFamily,
  type DirectPurposeRole,
  directPurposeRoleIndex,
} from "./family.js";
import { fitDirectPurposeTransition } from "./fit.js";
import {
  submitDirectPurposeStep01Accepted,
  submitDirectPurposeStep01Forced,
} from "./step01.js";
import {
  type DirectPurposeCustody,
  submitDirectPurposeTransition,
} from "./submit.js";
import { submitDirectPurposeTerminal } from "./terminal.js";
import type {
  DirectWalkFamily,
  DirectWalkProof,
  DirectWalkTerminalInput,
} from "./walk-family.js";

export type DirectWalkReferenceScripts<
  Category extends FamilyCategory,
  StepCount extends number,
> = Readonly<{
  /** One per role of the family's role order, in that order. */
  steps: FamilyStepTuple<Category, StepCount, UTxO>;
  witnesses: FaultProofWitnessReferenceScripts &
    Readonly<Record<(typeof CHUNKED_VERIFY_WITNESS_ROLES)[number], UTxO>>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type DirectWalkPortConfig<
  Category extends FamilyCategory,
  StepCount extends number,
> = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<Category>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: DirectPurposeContracts;
  references: DirectWalkReferenceScripts<Category, StepCount>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type AnyDirectWalkFamily<Category extends FamilyCategory> = DirectWalkFamily<
  Category,
  number,
  DirectPurposeReplay,
  DirectWalkProof
>;

/** The step a role occupies (1-based). */
export const directWalkRoleAt = (
  walk: Pick<AnyDirectWalkFamily<FamilyCategory>, "spec" | "family">,
  step: number,
): DirectPurposeRole => {
  const role = walk.family.roleOrder[step - 1];
  if (role === undefined)
    throw new Error(`${walk.spec.category} has no step ${step.toString()}`);
  return role;
};

/** The deployment the submitters read, from the bound manifest. */
export const directWalkContracts = <Category extends FamilyCategory>(
  walk: Pick<AnyDirectWalkFamily<Category>, "spec" | "family">,
  binding: FraudProofWorkflowDeploymentBinding<Category>,
  certificate: FieldPreimageCertificateBinding,
): DirectPurposeContracts => {
  const resolved = binding.resolvedContracts;
  const chain = resolved.contracts[walk.spec.category];
  if (chain === undefined || resolved.stateQueuePolicyId === undefined)
    throw new Error(`${walk.family.label} manifest omitted required contracts`);
  return Object.freeze({
    steps: chain.steps as DirectPurposeContracts["steps"],
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

/** Every field the proof opens; a proof that plans none opens no field. */
export const directWalkFieldPlan =
  (category: string, proof: DirectWalkProof) =>
  (field: number): DirectPurposeFieldPlan => {
    if (proof.plan === undefined)
      throw new Error(`${category} opens no field ${field.toString()}`);
    return proof.plan(field);
  };

/** The family the terminal is judged by: the walk's own, or the one its
 * proof builds. */
export const directWalkTerminalFamily = <
  Prepared extends DirectPurposeReplay,
  Proof extends DirectWalkProof,
>(
  walk: DirectWalkFamily<FamilyCategory, number, Prepared, Proof>,
  input: DirectWalkTerminalInput<Proof>,
): DirectPurposeFamily<unknown> =>
  walk.terminal === undefined ? walk.family : walk.terminal(input);

const threadAt = async (
  category: string,
  lucid: LucidEvolution,
  threadOutRef: string,
) => {
  const [thread] = await lucid.utxosByOutRef([
    parseOutRef(threadOutRef, `${category} thread`),
  ]);
  if (thread === undefined || typeof thread.datum !== "string")
    throw new Error(`${category} thread datum absent`);
  return thread as UTxO & { readonly datum: string };
};

/** The thread's one computation-thread token. */
const threadUnit = (
  category: string,
  thread: UTxO,
  contracts: DirectPurposeContracts,
) => {
  const units = Object.keys(thread.assets).filter((unit) =>
    unit.startsWith(contracts.computationThread.policyId),
  );
  if (units.length !== 1)
    throw new Error(`${category} thread carries no unique thread token`);
  return units[0]!;
};

export const createDirectWalkTransactionPort = <
  Category extends FamilyCategory,
  StepCount extends number,
  Prepared extends DirectPurposeReplay,
  Proof extends DirectWalkProof,
>(
  walk: DirectWalkFamily<Category, StepCount, Prepared, Proof>,
  config: DirectWalkPortConfig<Category, StepCount>,
): CursorFamilyTransactionPort<Category> => {
  const { category } = walk.spec;
  const { artifact: artifactFamily, family } = walk;
  const reference = (role: DirectPurposeRole) => {
    const utxo = (config.references.steps as readonly UTxO[])[
      directPurposeRoleIndex(family, role)
    ];
    if (utxo === undefined)
      throw new Error(`${category} ${role} reference absent`);
    return utxo;
  };
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category,
    prepare: async (args) =>
      await prepareDirectPurposeArtifact(artifactFamily, {
        ...args,
        replayContext: config.replayContext,
      }),
    capture: async ({ action, artifact, entries }) => {
      const ownOutputs = workflowFieldCarriageOwnOutputs({
        category,
        entries,
      });
      const plan = await planDirectPurposeArtifact(artifactFamily, {
        artifact,
        contracts: config.contracts,
        prover: config.signer.paymentKeyHash,
      });
      const { admitted } = plan;
      if (admitted.current.headerHash !== config.binding.definition.headerHash)
        throw new Error(`${category} bound header changed`);
      const input = cursorFamilyActionInput({ category, action });
      if (input.stage === "remove")
        return await captureCursorRemoval({
          category,
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
                fraudCategory: category,
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
              throw new Error(`${category} unsupported stage ${input.stage}`);
            const role = directWalkRoleAt(walk, step);
            const threadOutRef = cursorStringField(input, "threadOutRef");
            const common = {
              family,
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
                await submitDirectPurposeStep01Forced({
                  ...common,
                  material: admitted.material,
                  reconstruction: admitted.current,
                });
                return;
              }
              const inclusion = await directPurposeSourceInclusion(
                artifactFamily,
                admitted,
              );
              const chunks = await resolvePublishedProofChunks({
                lucid: config.lucid,
                address: config.signer.address,
                proofCbor: inclusion.txMembershipProofCbor,
              });
              await submitDirectPurposeStep01Accepted({
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
              await submitDirectPurposeTerminal({
                ...common,
                family: directWalkTerminalFamily(walk, plan),
                witnessReferenceScripts: config.references.witnesses,
              });
              return;
            }
            const thread = await threadAt(category, config.lucid, threadOutRef);
            const fieldPlan = directWalkFieldPlan(category, plan.proof);
            // A page that opens MPF proofs is sized to the transaction
            // budget; the carriage covers every transaction of the page,
            // since the fit builds them all at the page start (page-fit.ts).
            const opened = new Set(
              (plan.paging.isPageStart(role, thread.datum)
                ? plan.paging.page(role, thread.datum).transitions
                : [plan.paging.transitionAt(role, thread.datum)]
              ).flatMap((transition) => transition.fields),
            );
            const carriage = new Map<number, readonly UTxO[]>();
            for (const field of opened) {
              const opening = fieldPlan(field).openingPlan;
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
                  `${category} field ${field.toString()} carriage is not published`,
                );
              carriage.set(field, [
                ...published,
                ...(certificate === undefined ? [] : [certificate]),
              ]);
            }
            const custody: DirectPurposeCustody = {
              family,
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              signer: config.signer,
              unit: threadUnit(category, thread, config.contracts),
              references: reference,
              carriage: (field) => carriage.get(field) ?? [],
              plan: fieldPlan,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            };
            const transition = await fitDirectPurposeTransition({
              custody,
              paging: plan.paging,
              role,
              thread,
            });
            await submitDirectPurposeTransition(custody, thread, transition);
          },
        ),
      };
    },
  };
};
