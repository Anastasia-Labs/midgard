import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import {
  fetchUtxoByOutRef,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
} from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  type CompleteCanonicalReplayContext,
  WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY,
} from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  type CursorFamilyBoundBase,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { WITHDRAWAL_MISTAG_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { defineFamily } from "../workflow/family-definition.js";
import type { FieldCarriagePrerequisitePort } from "../workflow/field-carriage-prerequisite.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import type { FraudProofWorkflowAction } from "../workflow/orchestrator.js";
import { createStructuredDataPreimageRequirement } from "../workflow/raw-datum-preimage-prerequisite.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  admitWithdrawalMistagWorkflowArtifact,
  prepareWithdrawalMistagWorkflowArtifact,
} from "./artifact.js";
import type { WithdrawalMistagContracts } from "./contracts.js";
import { submitWithdrawalMistagInit } from "./submit-withdrawal-mistag-init.js";
import { submitWithdrawalMistagStep05 } from "./submit-withdrawal-mistag-step-05.js";
import {
  submitWithdrawalMistagStep01,
  submitWithdrawalMistagStep02,
  submitWithdrawalMistagStep03,
  submitWithdrawalMistagStep04,
  withdrawalMistagStepPayloadCbor,
} from "./submit-withdrawal-mistag-steps.js";

export type WithdrawalMistagWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    computationThreadMint: UTxO;
    fraudProofMint: UTxO;
    phasMembershipWithdraw: UTxO;
  };
}>;

type BoundConfig = CursorFamilyBoundBase<"withdrawalMistag"> &
  Readonly<{
    evidencePrerequisite?: FieldCarriagePrerequisitePort<"withdrawalMistag">;
    replayContext?: CompleteCanonicalReplayContext;
    contracts: WithdrawalMistagContracts;
    references: WithdrawalMistagWorkflowReferenceScripts;
  }>;

export const withdrawalMistagEvidenceRequirement = async ({
  action,
  artifact,
}: {
  action: FraudProofWorkflowAction;
  artifact: JournalJsonObject;
}) => {
  const stage = cursorFamilyActionInput({
    category: "withdrawalMistag",
    action,
  }).stage;
  const index = ["step_01", "step_02", "step_03", "step_04"].indexOf(stage);
  if (index < 0) return null;
  const prepared = await admitWithdrawalMistagWorkflowArtifact(artifact);
  const bytes = Buffer.from(
    withdrawalMistagStepPayloadCbor(prepared, index as 0 | 1 | 2 | 3),
    "hex",
  );
  return bytes.length > 4096
    ? createStructuredDataPreimageRequirement({
        preimageHex: bytes.toString("hex"),
      })
    : null;
};

export const createWithdrawalMistagTransactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"withdrawalMistag"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "withdrawalMistag",
  prepare: async (args) =>
    await prepareWithdrawalMistagWorkflowArtifact({
      ...args,
      replayContext: config.replayContext,
    }),
  capture: async ({ action, artifact }) => {
    const prepared = await admitWithdrawalMistagWorkflowArtifact(artifact);
    if (prepared.challengedHeaderHash !== config.binding.definition.headerHash)
      throw new Error("withdrawalMistag bound header changed");
    const input = cursorFamilyActionInput({
      category: "withdrawalMistag",
      action,
    });
    if (input.stage === "remove")
      return await captureCursorRemoval({
        category: "withdrawalMistag",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: prepared.challengedHeaderHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    const requirement = await withdrawalMistagEvidenceRequirement({
      action,
      artifact,
    });
    if (requirement !== null && config.evidencePrerequisite === undefined)
      throw new Error(
        "withdrawalMistag large evidence requires authenticated publications",
      );
    const evidenceReferences =
      requirement === null
        ? undefined
        : (
            await config.evidencePrerequisite!.resolveAuthenticated({
              headerHash: prepared.challengedHeaderHash,
              action,
              artifact,
            })
          ).publications;
    return {
      transaction: await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          if (input.stage === "init") {
            if (
              config.binding.resolvedContracts.category.categoryId !==
              "00000014"
            )
              throw new Error("withdrawalMistag catalogue identity changed");
            await submitWithdrawalMistagInit({
              lucid: config.lucid,
              contracts: config.contracts,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              signer: config.signer,
              category: {
                ...config.binding.resolvedContracts.category,
                categoryId: "00000014",
              },
              catalogue: config.binding.catalogue,
              fraudulentBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: prepared.challengedHeaderHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
            return;
          }
          const common = {
            lucid: config.lucid,
            contracts: config.contracts,
            signer: config.signer,
            prepared,
            evidenceReferences,
            threadOutRef: cursorStringField(input, "threadOutRef"),
            preSubmitBoundary,
            awaitConfirmation: false,
          };
          switch (input.stage) {
            case "step_01": {
              const [hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
                requireSingletonUtxo({
                  lucid: config.lucid,
                  address: credentialToAddress(
                    config.binding.network,
                    scriptHashToCredential(config.contracts.hubOraclePolicyId),
                  ),
                  unit: toUnit(
                    config.contracts.hubOraclePolicyId,
                    SDK.HUB_ORACLE_ASSET_NAME,
                  ),
                  label: "withdrawalMistag hub",
                }),
                fetchUtxoByOutRef({
                  lucid: config.lucid,
                  outRef: parseOutRef(
                    cursorStringField(input, "stateQueueBlockOutRef"),
                    "withdrawalMistag block",
                  ),
                  label: "withdrawalMistag block",
                }),
              ]);
              await submitWithdrawalMistagStep01({
                ...common,
                hubOracleUtxo,
                stateQueueBlockUtxo,
                referenceScriptUtxo: config.references.steps[0],
              });
              break;
            }
            case "step_02":
              await submitWithdrawalMistagStep02({
                ...common,
                referenceScriptUtxo: config.references.steps[1],
              });
              break;
            case "step_03":
              await submitWithdrawalMistagStep03({
                ...common,
                referenceScriptUtxo: config.references.steps[2],
              });
              break;
            case "step_04":
              await submitWithdrawalMistagStep04({
                ...common,
                referenceScriptUtxo: config.references.steps[3],
              });
              break;
            case "step_05":
              await submitWithdrawalMistagStep05({
                ...common,
                referenceScriptUtxo: config.references.steps[4],
                witnessReferenceScripts: config.references.witnesses,
              });
              break;
            default:
              throw new Error(
                `withdrawalMistag unsupported stage ${input.stage}`,
              );
          }
        },
      ),
    };
  },
});

export type ManifestBoundWithdrawalMistagWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: WithdrawalMistagWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const;

export const WITHDRAWAL_MISTAG_FAMILY_DEFINITION = defineFamily({
  category: "withdrawalMistag",
  stepDatumSchemas: [
    SDK.WithdrawalMistagStep01Datum,
    SDK.WithdrawalMistagStep02Datum,
    SDK.WithdrawalMistagStep03Datum,
    SDK.WithdrawalMistagStep04Datum,
    SDK.WithdrawalMistagStep05Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: () => WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: WITHDRAWAL_MISTAG_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofWithdrawalMistag",
      "fraudProofWithdrawalMistagStep02",
      "fraudProofWithdrawalMistagStep03",
      "fraudProofWithdrawalMistagStep04",
      "fraudProofWithdrawalMistagStep05",
    ],
    transactionPort: (context) => {
      const { binding } = context;
      const chain = binding.resolvedContracts.contracts.withdrawalMistag;
      const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
      if (chain === undefined || stateQueuePolicyId === undefined)
        throw new Error("withdrawalMistag manifest omitted contracts");
      return createWithdrawalMistagTransactionPort({
        ...context,
        evidencePrerequisite: context.fieldCarriagePrerequisites[0]!,
        contracts: {
          steps: chain.steps,
          computationThread:
            binding.resolvedContracts.contracts.computationThread,
          fraudProof: binding.resolvedContracts.contracts.fraudProof,
          hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
          stateQueuePolicyId,
        },
      });
    },
  },
  fieldCarriage: [
    {
      rawDatum: true,
      requirementForAction: (_context, input) =>
        withdrawalMistagEvidenceRequirement(input),
    },
  ],
});

export const createManifestBoundWithdrawalMistagWorkflow = (
  config: ManifestBoundWithdrawalMistagWorkflowConfig,
) =>
  assembleManifestBoundFamilyWorkflow(
    WITHDRAWAL_MISTAG_FAMILY_DEFINITION,
    config,
  );

export type ManifestBoundWithdrawalMistagWorkflow = Awaited<
  ReturnType<typeof createManifestBoundWithdrawalMistagWorkflow>
>;

export const runOrResumeManifestBoundWithdrawalMistagWorkflow = (input: {
  workflow: ManifestBoundWithdrawalMistagWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}) => runOrResumeManifestBoundFamilyWorkflow(input);
