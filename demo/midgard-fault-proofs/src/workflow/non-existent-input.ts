import {
  FraudProofComputationThreadStepDatum,
  NonExistentInputStep02ThreadDatum,
  NonExistentInputStep03ThreadDatum,
  NonExistentInputStep04ThreadDatum,
} from "@al-ft/midgard-sdk";

import {
  type FaultProofFieldOpeningPlan,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import {
  admitNonExistentInputForcedArtifact,
  NON_EXISTENT_INPUT_FORCED_ARTIFACT,
  nonExistentInputForcedArtifact,
} from "../non-existent-input/artifact.js";
import {
  nonExistentInputForcedFieldPlan,
  submitNonExistentInputForcedStep,
} from "../non-existent-input/submit.js";
import { neSubmitStep01 } from "../non-existent-input/submit-step-01.js";
import { neSubmitStep02 } from "../non-existent-input/submit-step-02.js";
import { neSubmitStep03 } from "../non-existent-input/submit-step-03.js";
import { neSubmitStep04 } from "../non-existent-input/submit-step-04.js";
import {
  detectNonExistentInputWrongfulRejections,
  NON_EXISTENT_INPUT_WRONGFUL_REJECTION_VIOLATION_ID,
} from "../non-existent-input/wrongful-rejection.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import { resolveNonExistentInputDeploymentContracts } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { completeCanonicalReplayPredecessorEvidence } from "./complete-replay.js";
import { NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  defineLinearFamily,
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { type FieldCarriageRequirement } from "./field-carriage-prerequisite.js";
import {
  admitLedgerAbsenceArtifact,
  prepareLedgerAbsenceArtifact,
} from "./ledger-absence-artifact.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;

export type NonExistentInputWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "nonExistentInput",
    (typeof WITNESS_ROLES)[number],
    true
  >;

type BoundConfig = LinearFamilyAssemblyContext<
  "nonExistentInput",
  (typeof WITNESS_ROLES)[number],
  true
>;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "non-existent-input workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "nonExistentInput" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("non-existent-input workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`non-existent-input workflow action omitted ${field}`);
  }
  return value;
};

const resolveField = async (
  config: BoundConfig,
  planned: FaultProofFieldOpeningPlan,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("non-existent-input field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.certificate.policyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("non-existent-input field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const resolveChunks = async ({
  action,
  config,
  proofCbor,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly config: BoundConfig;
  readonly proofCbor: string;
}) => {
  const chunks = await resolveDirectFirstProofChunks({
    action,
    lucid: config.lucid,
    address: config.signer.address,
    proofCbor,
  });
  // Absence is the expected state for the direct-first attempt. Once a
  // publication route is journal-authorized, the outer prerequisite refuses
  // to expose the base step until the exact raw-L1-confirmed chunks exist.
  return chunks;
};

const captureRemoval = async (
  config: BoundConfig,
  input: Readonly<Record<string, unknown>>,
) => {
  let mutationLease: StateQueueMutationLease | undefined;
  const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
    acquire: async () => {
      const acquired =
        await config.stateQueueMutationLeaseCoordinator.acquire();
      mutationLease = acquired;
      return acquired;
    },
  };
  const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
  const fraudProofOutRef = stringField(input, "fraudProofOutRef");
  const transaction = await captureLocallyEvaluatedTransaction(
    async (boundary) => {
      await submitRemoveFraudulentBlock({
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        fraudCategory: "nonExistentInput",
        fraudulentHeaderHash: config.binding.definition.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "non-existent-input removal changed authenticated inputs",
            );
          }
          await boundary(built);
        },
      });
    },
  );
  return Object.freeze({
    transaction,
    ...(mutationLease === undefined ? {} : { mutationLease }),
  });
};

const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"nonExistentInput"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "nonExistentInput",
  prepare: async ({ evidence, replayContext, classification }) => {
    if (
      classification.selected.violationId ===
      NON_EXISTENT_INPUT_WRONGFUL_REJECTION_VIOLATION_ID
    ) {
      const detections = await detectNonExistentInputWrongfulRejections({
        block: evidence,
        predecessor: completeCanonicalReplayPredecessorEvidence({
          evidence,
          context: replayContext,
        }),
      });
      const detected = detections.find(
        (item) => item.detectionId === classification.selected.detectionId,
      );
      if (
        classification.category !== "nonExistentInput" ||
        classification.headerHash !== evidence.headerHash ||
        detected === undefined
      )
        throw new Error("nonExistentInput: forced classification changed");
      return nonExistentInputForcedArtifact(detected.prepared);
    }
    return await prepareLedgerAbsenceArtifact({
      category: "nonExistentInput",
      evidence,
      replayContext,
      classification,
      owner: config.signer.paymentKeyHash,
    });
  },
  capture: async ({ action, artifact }) => {
    if (artifact.schemaVersion === NON_EXISTENT_INPUT_FORCED_ARTIFACT) {
      const prepared = await admitNonExistentInputForcedArtifact(artifact);
      if (prepared.headerHash !== config.binding.definition.headerHash)
        throw new Error("nonExistentInput: forced workflow header changed");
      const input = actionInput(action);
      if (input.stage === "remove") return await captureRemoval(config, input);
      if (input.stage === "init")
        return {
          transaction: await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitInit({
                lucid: config.lucid,
                blueprint: config.binding.blueprint,
                deploymentInfo: config.binding.deploymentInfo,
                network: config.binding.network,
                signer: config.signer,
                fraudCategory: "nonExistentInput",
                fraudulentBlockOutRef: stringField(
                  input,
                  "stateQueueBlockOutRef",
                ),
                fraudulentHeaderHash: prepared.headerHash,
                witnessReferenceScripts: config.references.witnesses,
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          ),
        };
      const stepIndex = (
        ["step_01", "step_02", "step_03", "step_04"] as const
      ).findIndex((stage) => stage === input.stage);
      if (stepIndex < 0 || stepIndex > 3)
        throw new Error("nonExistentInput: unknown forced stage");
      const { contracts, nonExistentInputCategory } =
        await resolveNonExistentInputDeploymentContracts({
          blueprint: config.binding.blueprint,
          deploymentInfo: config.binding.deploymentInfo,
          network: config.binding.network,
          requireFraudProofSpend: true,
        });
      const carriage =
        stepIndex === 1
          ? await resolveField(
              config,
              nonExistentInputForcedFieldPlan(
                prepared,
                config.signer.paymentKeyHash,
              ),
            )
          : null;
      return {
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNonExistentInputForcedStep({
              lucid: config.lucid,
              contracts: {
                steps: contracts.nonExistentInput.steps,
                computationThread: contracts.computationThread,
                fraudProof: contracts.fraudProof,
              },
              categoryId: nonExistentInputCategory.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              prepared,
              stepIndex: stepIndex as 0 | 1 | 2 | 3,
              referenceScripts: {
                steps: config.references.steps,
                computationThreadMint:
                  config.references.witnesses.computationThreadMint,
                fraudProofMint: config.references.witnesses.fraudProofMint,
              },
              carriageUtxos:
                carriage === null
                  ? []
                  : [
                      ...carriage.publications,
                      ...(carriage.certificate === undefined
                        ? []
                        : [carriage.certificate]),
                    ],
              certificatePolicyId: config.certificate.policyId,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      };
    }
    const admitted = admitLedgerAbsenceArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (
      admitted.artifact.category !== "nonExistentInput" ||
      admitted.artifact.headerHash !== config.binding.definition.headerHash
    ) {
      throw new Error("non-existent-input artifact changed workflow identity");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "nonExistentInput",
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.artifact.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const chunks = await resolveChunks({
        action,
        config,
        proofCbor: admitted.artifact.badTx.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep01({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.txInclusion,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[0],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      const carriage = await resolveField(config, admitted.fieldPlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep02({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              inputsPreimage: admitted.inputPreimage.map((candidate) => ({
                txId: candidate.tx_id,
                index: candidate.output_index,
              })),
              nativeTxCompactCbor: admitted.artifact.badTx.nativeTxCompactCbor,
              badInputIndex: BigInt(admitted.artifact.badInputIndex),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : {
                    certificateUtxo: carriage.certificate,
                    certificatePolicyId: config.certificate.policyId,
                  }),
              referenceScriptUtxo: config.references.steps[1],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      const chunks = await resolveChunks({
        action,
        config,
        proofCbor: admitted.artifact.ledgerNonMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep03({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              ledgerNonMembershipProofCbor:
                admitted.artifact.ledgerNonMembershipProofCbor,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[2],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04") {
      const chunks = await resolveChunks({
        action,
        config,
        proofCbor: admitted.artifact.txsNonMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep04({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              txsNonMembershipProofCbor:
                admitted.artifact.txsNonMembershipProofCbor,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[3],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval(config, input);
    }
    throw new Error(
      `non-existent-input workflow cannot execute ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundNonExistentInputWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "nonExistentInput",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type ManifestBoundNonExistentInputWorkflow =
  ManifestBoundLinearFamilyWorkflow<"nonExistentInput", true>;

const fieldPreimageCertificate = (context: BoundConfig) => ({
  policyId: context.certificate.policyId,
  mintingScript: context.certificate.mintingScript,
  referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
});

export const NON_EXISTENT_INPUT_FAMILY_DEFINITION = defineLinearFamily({
  category: "nonExistentInput",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    NonExistentInputStep02ThreadDatum,
    NonExistentInputStep03ThreadDatum,
    NonExistentInputStep04ThreadDatum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "linear",
    transactionPort: createTransactionPort,
  },
  // Step-02 carries the disputed transaction's field preimage: the forced
  // artifact opens the forced source, the ledger-absence artifact opens the
  // accepted compact transaction.
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        if (actionInput(action).stage !== "step_02") return null;
        if (artifact.schemaVersion === NON_EXISTENT_INPUT_FORCED_ARTIFACT) {
          const prepared = await admitNonExistentInputForcedArtifact(artifact);
          return {
            planned: nonExistentInputForcedFieldPlan(
              prepared,
              context.signer.paymentKeyHash,
            ),
            compactCbor:
              prepared.forcedSource.membership.value.submitted_source
                .compact_cbor,
            witnessSetCompactCbor:
              prepared.forcedSource.membership.value.submitted_source
                .witness_set_compact_cbor,
            certificate: fieldPreimageCertificate(context),
          } satisfies FieldCarriageRequirement;
        }
        const admitted = admitLedgerAbsenceArtifact(
          artifact,
          context.signer.paymentKeyHash,
        );
        return {
          planned: admitted.fieldPlan,
          compactCbor: admitted.artifact.badTx.nativeTxCompactCbor,
          certificate: fieldPreimageCertificate(context),
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
  proofChunk: (context, { action, artifact }) => {
    if (artifact.schemaVersion === NON_EXISTENT_INPUT_FORCED_ARTIFACT)
      return null;
    const stage = actionInput(action).stage;
    const admitted = admitLedgerAbsenceArtifact(
      artifact,
      context.signer.paymentKeyHash,
    );
    return stage === "step_01"
      ? admitted.artifact.badTx.txMembershipProofCbor
      : stage === "step_03"
        ? admitted.artifact.ledgerNonMembershipProofCbor
        : stage === "step_04"
          ? admitted.artifact.txsNonMembershipProofCbor
          : null;
  },
});

export const createManifestBoundNonExistentInputWorkflow = (
  config: ManifestBoundNonExistentInputWorkflowConfig,
): Promise<ManifestBoundNonExistentInputWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    NON_EXISTENT_INPUT_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundNonExistentInputWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
