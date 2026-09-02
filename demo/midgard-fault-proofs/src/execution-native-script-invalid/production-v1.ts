import {
  computeHash32,
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardSpendInputItemV1,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  encodeMidgardNativeTxCompactV1,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardReferenceScriptSourceLeafV1,
} from "@al-ft/midgard-core";
import {
  forcedVerdictSubjectV1,
  FraudProofComputationThreadStepDatum,
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterialV1 } from "@al-ft/midgard-validation";
import { Data, type LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import { buildExecutionSourceMachineAuthenticationFromRetainedDaV1 } from "../execution-source-script-decoding/retained-witness-v1.js";
import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import { createFraudProofFamilyLocalKupmiosL1ObservationPortV1 } from "../workflow/family-l1-observation-v1.js";
import {
  computeFraudProofWorkflowIdV1,
  DirectoryFraudProofWorkflowJournalStoreV1,
  FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
  type FraudProofWorkflowIdentityV1,
  type FraudProofWorkflowJournalStoreV1,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  assertProductionWorkflowJournalActuationV1,
  bindProductionWorkflowActuationJournalV1,
} from "../workflow/production-actuation-permit-v1.js";
import {
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerV1,
} from "../workflow/production-adapters-v1.js";
import { captureProductionCursorRemovalV1 } from "../workflow/production-cursor-family-runtime-v1.js";
import { bindProductionWorkflowFundingReservationJournalV1 } from "../workflow/production-funding-reservation-permit-v1.js";
import type {
  ProductionHistoricalNativeScriptCheckpointStoreV1,
  ProductionHistoricalNativeScriptHistorySourceV1,
} from "../workflow/production-historical-native-script-corpus-v1.js";
import { resolveProductionHistoricalNativeScriptCorpusV1 } from "../workflow/production-historical-native-script-corpus-v1.js";
import { submitCapturedTransactionV1 } from "../workflow/transaction-boundary-v1.js";
import type { AcceptedReconstructionStateV1 } from "./accepted-reconstruction-machine-v1.js";
import { reconstructExecutionNativeScriptPurposesV1 } from "./canonical-reconstruction-v1.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES_V1,
  EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES_V1,
  type ExecutionNativeScriptInvalidContractsV1,
} from "./contracts-v1.js";
import { prepareExecutionNativeScriptInvalidEvidenceV1 } from "./family-v1.js";
import { detectExecutionNativeScriptInvalidCanonicalViolationsV1 } from "./production-replay-v1.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumV1Schema,
  ExecutionNativeScriptInvalidStep02DatumV1Schema,
  ExecutionNativeScriptInvalidStep03DatumV1Schema,
  ExecutionNativeScriptInvalidStep04DatumV1Schema,
  ExecutionNativeScriptInvalidStep05DatumV1Schema,
  ExecutionNativeScriptInvalidStep06DatumV1Schema,
} from "./schemas-v1.js";
import {
  submitExecutionNativeScriptInvalidAcceptedFinishInlineV1,
  submitExecutionNativeScriptInvalidAcceptedFinishPurposeV1,
  submitExecutionNativeScriptInvalidAcceptedFinishReceivePassV1,
  submitExecutionNativeScriptInvalidAcceptedFinishSpendsV1,
  submitExecutionNativeScriptInvalidAcceptedInitV1,
  submitExecutionNativeScriptInvalidAcceptedInlineSourceV1,
  submitExecutionNativeScriptInvalidAcceptedMintV1,
  submitExecutionNativeScriptInvalidAcceptedObserverV1,
  submitExecutionNativeScriptInvalidAcceptedReceiveV1,
  submitExecutionNativeScriptInvalidAcceptedReferenceSourceV1,
  submitExecutionNativeScriptInvalidAcceptedSpendV1,
} from "./submit-accepted-reconstruction-v1.js";
import { submitExecutionNativeScriptInvalidInitV1 } from "./submit-init-v1.js";
import {
  submitExecutionNativeScriptInvalidStep01AcceptedV1,
  submitExecutionNativeScriptInvalidStep01ForcedV1,
} from "./submit-step-01-v1.js";
import { submitExecutionNativeScriptInvalidStep02V1 } from "./submit-step-02-v1.js";
import { submitExecutionNativeScriptInvalidStep03 } from "./submit-step-03-v1.js";
import { submitExecutionNativeScriptInvalidStep04StartSignerScan } from "./submit-step-04-v1.js";
import { submitExecutionNativeScriptInvalidStep05 } from "./submit-step-05-v1.js";
import { submitExecutionNativeScriptInvalidStep06 } from "./submit-step-06-v1.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_PRODUCTION_WORKFLOW_V1 =
  "midgard-execution-native-script-invalid-production-workflow-v1" as const;

export const EXECUTION_NATIVE_SCRIPT_INVALID_PRODUCTION_CONFIG_KEYS_V1 =
  Object.freeze([
    "manifest",
    "blueprintJson",
    "deploymentInfo",
    "headerHash",
    "lucid",
    "signer",
    "source",
    "historicalCheckpointStore",
    "historicalSource",
    "stateQueueMutationLeaseCoordinator",
    "referenceScripts",
  ] as const);

export const EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS_V1 =
  Object.freeze([
    FraudProofComputationThreadStepDatum,
    ExecutionNativeScriptInvalidStep02DatumV1Schema,
    ExecutionNativeScriptInvalidStep03DatumV1Schema,
    ExecutionNativeScriptInvalidStep04DatumV1Schema,
    ExecutionNativeScriptInvalidStep05DatumV1Schema,
    ExecutionNativeScriptInvalidStep06DatumV1Schema,
    ExecutionNativeScriptInvalidStep02DatumV1Schema,
    ExecutionNativeScriptInvalidAcceptedDatumV1Schema,
    ExecutionNativeScriptInvalidAcceptedDatumV1Schema,
    ExecutionNativeScriptInvalidAcceptedDatumV1Schema,
    ExecutionNativeScriptInvalidAcceptedDatumV1Schema,
    ExecutionNativeScriptInvalidAcceptedDatumV1Schema,
    ExecutionNativeScriptInvalidAcceptedDatumV1Schema,
  ] as const);

export type ExecutionNativeScriptInvalidWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
  ];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
  removal: Readonly<{
    correctionLockSpend: UTxO;
    stateQueueSpend: UTxO;
    stateQueueMint: UTxO;
    stateQueueFraudRemovalWithdraw: UTxO;
    activeOperatorsSpend: UTxO;
    activeOperatorsMint: UTxO;
    retiredOperatorsSpend: UTxO;
    retiredOperatorsMint: UTxO;
    schedulerSpend: UTxO;
  }>;
}>;

export type ManifestBoundExecutionNativeScriptInvalidWorkflowConfigV1 =
  Readonly<{
    manifest: unknown;
    blueprintJson: string;
    deploymentInfo: unknown;
    headerHash: string;
    lucid: LucidEvolution;
    signer: ResolvedProverSigner;
    source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
    historicalCheckpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
    historicalSource: ProductionHistoricalNativeScriptHistorySourceV1;
    stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    referenceScripts: ExecutionNativeScriptInvalidWorkflowReferenceScriptsV1;
  }>;

export type ManifestBoundExecutionNativeScriptInvalidWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"executionNativeScriptInvalid">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  historicalCheckpointStore: ProductionHistoricalNativeScriptCheckpointStoreV1;
  historicalSource: ProductionHistoricalNativeScriptHistorySourceV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  contracts: ExecutionNativeScriptInvalidContractsV1;
  references: ExecutionNativeScriptInvalidWorkflowReferenceScriptsV1;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPortV1>;
}>;

const contractNames = Object.freeze([
  "fraudProofExecutionNativeScriptInvalid",
  "fraudProofExecutionNativeScriptInvalidStep02",
  "fraudProofExecutionNativeScriptInvalidStep03",
  "fraudProofExecutionNativeScriptInvalidStep04",
  "fraudProofExecutionNativeScriptInvalidStep05",
  "fraudProofExecutionNativeScriptInvalidStep06",
  "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
  "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
  "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
] as const);

/** Strict manifest/reference construction; no proof inputs or callbacks. */
export const createManifestBoundExecutionNativeScriptInvalidWorkflowV1 = async (
  config: ManifestBoundExecutionNativeScriptInvalidWorkflowConfigV1,
): Promise<ManifestBoundExecutionNativeScriptInvalidWorkflowV1> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...EXECUTION_NATIVE_SCRIPT_INVALID_PRODUCTION_CONFIG_KEYS_V1]
      .sort()
      .join("\0")
  )
    throw new Error(
      "executionNativeScriptInvalid production config contains callback authority",
    );
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "executionNativeScriptInvalid",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS_V1,
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain =
    binding.resolvedContracts.contracts.executionNativeScriptInvalid;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    chain.steps.length !== 13 ||
    stateQueuePolicyId === undefined ||
    certificate === null
  )
    throw new Error(
      "executionNativeScriptInvalid manifest omitted thirteen-step chain",
    );
  const steps = contractNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as ExecutionNativeScriptInvalidWorkflowReferenceScriptsV1["steps"];
  const bindReference = (contractName: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo,
    });
  const witnesses = Object.freeze({
    computationThreadMint: bindReference(
      "computationThreadMint",
      config.referenceScripts.witnesses.computationThreadMint,
    ),
    fraudProofMint: bindReference(
      "fraudProofMint",
      config.referenceScripts.witnesses.fraudProofMint,
    ),
    phasMembershipWithdraw: bindReference(
      "phasMembershipWithdraw",
      config.referenceScripts.witnesses.phasMembershipWithdraw,
    ),
    chunkedVerifyWithdraw: bindReference(
      "chunkedVerifyWithdraw",
      config.referenceScripts.witnesses.chunkedVerifyWithdraw,
    ),
    pexcludesWithdraw: bindReference(
      "pexcludesWithdraw",
      config.referenceScripts.witnesses.pexcludesWithdraw,
    ),
  });
  const removal = Object.freeze(
    Object.fromEntries(
      Object.entries(config.referenceScripts.removal).map(([role, utxo]) => [
        role,
        bindReference(role, utxo),
      ]),
    ) as unknown as ExecutionNativeScriptInvalidWorkflowReferenceScriptsV1["removal"],
  );
  const references = Object.freeze({
    steps,
    witnesses,
    fieldPreimageCertificateMint: bindReference(
      "fieldPreimageCertificateMint",
      config.referenceScripts.fieldPreimageCertificateMint,
    ),
    removal,
  });
  const contracts: ExecutionNativeScriptInvalidContractsV1 = Object.freeze({
    steps: chain.steps.slice(0, 6).map((step, index) => ({
      ...step,
      blueprintTitle:
        EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES_V1[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })),
    acceptedPrelude: chain.steps.slice(6).map((step, index) => ({
      ...step,
      blueprintTitle:
        EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES_V1[index]!,
      referenceOutRef: `${steps[index + 6]!.txHash}#${steps[index + 6]!.outputIndex.toString()}`,
    })),
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  return Object.freeze({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    source: config.source,
    historicalCheckpointStore: config.historicalCheckpointStore,
    historicalSource: config.historicalSource,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    contracts,
    references,
    l1: createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
      source: config.source,
      releaseFinality: binding.releaseFinality,
      releaseEconomics: binding.releaseEconomics,
      definition: binding.definition,
    }),
  });
};

/** Rebuild the one actionable ID32 decision solely from L1 and retained DA. */
export const prepareManifestBoundExecutionNativeScriptInvalidReplayV1 =
  async (input: {
    workflow: ManifestBoundExecutionNativeScriptInvalidWorkflowV1;
    sources: readonly import("../transition-trace/fetch.js").RetainedDaPayloadSource[];
  }) => {
    if (Object.keys(input).sort().join(",") !== "sources,workflow")
      throw new Error(
        "executionNativeScriptInvalid replay rejects caller-authored evidence",
      );
    const { workflow, sources } = input;
    const block = await fetchCanonicalBlockEvidenceV1({
      observation: await workflow.l1.observeHeader({
        headerHash: workflow.binding.definition.headerHash,
      }),
      sources,
    });
    const corpus = await resolveProductionHistoricalNativeScriptCorpusV1({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      checkpointStore: workflow.historicalCheckpointStore,
      historySource: workflow.historicalSource,
      currentEvidence: block,
      sources,
    });
    const detections = detectExecutionNativeScriptInvalidCanonicalViolationsV1({
      block,
      corpus,
    });
    if (detections.length !== 1)
      throw new Error(
        `executionNativeScriptInvalid replay yielded ${detections.length.toString()} exact findings`,
      );
    return Object.freeze({ block, corpus, detection: detections[0]! });
  };

export type ExecutionNativeScriptInvalidProductionRunResultV1 = Readonly<{
  kind: "pending" | "completed";
  headerHash: string;
  detectionId: string;
  direction: "wrongfulAcceptance" | "wrongfulRejection";
}>;

/**
 * Package-owned retained-DA entry point used by the watcher runner. The
 * transaction driver is deliberately kept in the workflow value constructed
 * from the exact manifest; callers cannot inject evidence or an actuator.
 */
export const runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflowV1 =
  async ({
    workflow,
    sources,
    journal,
    decisionDigest,
  }: {
    workflow: ManifestBoundExecutionNativeScriptInvalidWorkflowV1;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStoreV1;
    decisionDigest: string;
  }): Promise<ExecutionNativeScriptInvalidProductionRunResultV1> => {
    if (
      Object.keys({ workflow, sources, journal }).sort().join(",") !==
      "journal,sources,workflow"
    )
      throw new Error(
        "executionNativeScriptInvalid runner rejects caller-authored evidence",
      );
    const prepared =
      await prepareManifestBoundExecutionNativeScriptInvalidReplayV1({
        workflow,
        sources,
      });
    const observed = await workflow.l1.observe({
      headerHash: workflow.binding.definition.headerHash,
    });
    if (observed.stage.kind === "removed")
      return Object.freeze({
        kind: "completed",
        headerHash: prepared.block.headerHash,
        detectionId: prepared.detection.detectionId,
        direction: prepared.detection.direction,
      });
    const identity: FraudProofWorkflowIdentityV1 = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      category: "executionNativeScriptInvalid",
      target: {
        kind: "state_queue_header",
        headerHash: prepared.block.headerHash,
      },
      decisionDigest,
    };
    const workflowId = computeFraudProofWorkflowIdV1(identity);
    const append = async (
      event: Parameters<FraudProofWorkflowJournalStoreV1["append"]>[0]["event"],
    ) => {
      const sequence = (await journal.load(workflowId)).length;
      await journal.append(
        {
          schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
          workflowId,
          identity,
          sequence,
          recordedAt: new Date().toISOString(),
          event,
        },
        sequence,
      );
    };
    if ((await journal.load(workflowId)).length === 0)
      await append({ kind: "started" });
    const entries = await journal.load(workflowId);
    const pending = [...entries]
      .reverse()
      .find(({ event }) => event.kind === "submission_intent");
    if (pending?.event.kind === "submission_intent") {
      const intent = pending.event;
      const confirmed = entries.some(
        ({ event }) =>
          event.kind === "confirmed" && event.actionId === intent.actionId,
      );
      if (!confirmed) {
        if (
          !(await workflow.l1.transactionConfirmed({
            headerHash: prepared.block.headerHash,
            txHash: intent.txHash,
          }))
        )
          return Object.freeze({
            kind: "pending",
            headerHash: prepared.block.headerHash,
            detectionId: prepared.detection.detectionId,
            direction: prepared.detection.direction,
          });
        await append({
          kind: "confirmed",
          actionId: intent.actionId,
          txHash: intent.txHash,
        });
      }
    }
    if (observed.stage.kind === "proof_token") {
      const actionId = "executionNativeScriptInvalid:remove";
      const captured = await captureProductionCursorRemovalV1({
        category: "executionNativeScriptInvalid",
        lucid: workflow.lucid,
        blueprint: workflow.binding.blueprint,
        deploymentInfo: workflow.binding.deploymentInfo,
        network: workflow.binding.network,
        signer: workflow.signer,
        headerHash: prepared.block.headerHash,
        input: {
          schemaVersion: "midgard-production-cursor-family-action-v1",
          category: "executionNativeScriptInvalid",
          stage: "remove",
          fraudProofOutRef: observed.stage.fraudProofOutRef,
          stateQueueBlockOutRef: observed.stage.stateQueueBlockOutRef,
          nextRemovalOutRef: observed.stage.nextRemovalOutRef,
          requiresMutationLease:
            observed.stage.nextRemovalOutRef !==
            observed.stage.stateQueueBlockOutRef,
        },
        stateQueueMutationLeaseCoordinator:
          workflow.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          workflow.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
      await append({
        kind: "preflight_passed",
        actionId,
        txHash: captured.transaction.txHash,
        localEvaluator: "lucid-evolution-local-uplc-v1",
        referenceScripts: captured.transaction.referenceScripts,
        ...(captured.mutationLease === undefined
          ? {}
          : {
              durableRecovery: {
                stateQueueMutationLease: {
                  token: captured.mutationLease.token,
                  source: captured.mutationLease.source,
                },
              },
            }),
      });
      await append({
        kind: "submission_intent",
        actionId,
        actionInput: {
          schemaVersion: "midgard-production-cursor-family-action-v1",
          category: "executionNativeScriptInvalid",
          stage: "remove",
          nextRemovalOutRef: observed.stage.nextRemovalOutRef,
          fraudProofOutRef: observed.stage.fraudProofOutRef,
        },
        attempt: 1,
        txHash: captured.transaction.txHash,
      });
      const txHash = await submitCapturedTransactionV1(captured.transaction);
      await append({ kind: "submitted", actionId, attempt: 1, txHash });
      await workflow.lucid.awaitTx(txHash);
      await append({ kind: "confirmed", actionId, txHash });
      await captured.mutationLease?.release();
    } else if (observed.stage.kind === "not_started") {
      const actionId = "executionNativeScriptInvalid:init";
      const result = await submitExecutionNativeScriptInvalidInitV1({
        lucid: workflow.lucid,
        blueprint: workflow.binding.blueprint as Parameters<
          typeof submitExecutionNativeScriptInvalidInitV1
        >[0]["blueprint"],
        network: workflow.binding.network,
        contracts: workflow.contracts,
        category: workflow.binding.resolvedContracts.category,
        catalogue: workflow.binding.catalogue,
        signer: workflow.signer,
        fraudulentBlockOutRef: observed.stage.stateQueueBlockOutRef,
        fraudulentHeaderHash: prepared.block.headerHash,
        witnessReferenceScripts: workflow.references.witnesses,
        awaitConfirmation: false,
        preSubmitBoundary: async (transaction) => {
          await append({
            kind: "preflight_passed",
            actionId,
            txHash: transaction.txHash,
            localEvaluator: "lucid-evolution-local-uplc-v1",
            referenceScripts: transaction.referenceScripts,
          });
          await append({
            kind: "submission_intent",
            actionId,
            actionInput: {
              schemaVersion: "midgard-production-cursor-family-action-v1",
              category: "executionNativeScriptInvalid",
              stage: "init",
            },
            attempt: 1,
            txHash: transaction.txHash,
          });
        },
      });
      await append({
        kind: "submitted",
        actionId,
        attempt: 1,
        txHash: result.txHash,
      });
    } else {
      const activeStage = observed.stage;
      const actionId = `executionNativeScriptInvalid:step_${activeStage.step.toString().padStart(2, "0")}`;
      const transactionEntry =
        prepared.detection.source === "accepted"
          ? prepared.block.transactions.find(
              ({ nodeTxId }) => nodeTxId === prepared.detection.transactionId,
            )
          : undefined;
      const forcedEntry =
        prepared.detection.source === "forced"
          ? prepared.block.reconstruction.forcedTransactions[
              prepared.detection.forcedIndex!
            ]
          : undefined;
      const txCbor =
        transactionEntry === undefined
          ? forcedEntry?.fullTransactionCbor
          : Buffer.from(transactionEntry.txCbor, "hex");
      if (txCbor === undefined)
        throw new Error(
          "executionNativeScriptInvalid selected transaction disappeared",
        );
      const tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(txCbor);
      const compactCbor = encodeMidgardNativeTxCompactV1(tx.compact).toString(
        "hex",
      );
      const compactWitness = deriveMidgardNativeTxWitnessSetCompactV1(
        tx.witnessSet,
      );
      const witnessSet = {
        addr_tx_wits_hash: Buffer.from(compactWitness.addrTxWitsHash).toString(
          "hex",
        ),
        script_tx_wits_hash: Buffer.from(
          compactWitness.scriptTxWitsHash,
        ).toString("hex"),
        redeemer_tx_wits_hash: Buffer.from(
          compactWitness.redeemerTxWitsHash,
        ).toString("hex"),
      };
      const addressWitnessItems = decodeMidgardFieldPreimageV1(
        tx.witnessSet.addrTxWitsPreimageCbor,
      );
      const history = prepared.corpus as unknown as {
        reconstructions: readonly {
          headerHash: string;
          utxos: readonly { key: Uint8Array; value: Uint8Array }[];
        }[];
      };
      const predecessor = history.reconstructions.at(-2);
      const priorOutputs = new Map(
        (predecessor?.utxos ?? []).map(({ key, value }) => [
          Buffer.from(key).toString("hex"),
          Buffer.from(value),
        ]),
      );
      const reconstruction = reconstructExecutionNativeScriptPurposesV1({
        canonicalTransactionCbor: txCbor,
        resolvedOutputsByOutRef: priorOutputs,
      });
      const purpose =
        reconstruction.purposes[prepared.detection.executionIndex];
      if (purpose === undefined)
        throw new Error(
          "executionNativeScriptInvalid execution coordinate disappeared",
        );
      let capturedHash: string | undefined;
      const preSubmitBoundary = async (transaction: {
        txHash: string;
        referenceScripts: readonly {
          role: string;
          outRef: string;
          scriptHash: string;
        }[];
      }) => {
        capturedHash = transaction.txHash;
        await append({
          kind: "preflight_passed",
          actionId,
          txHash: transaction.txHash,
          localEvaluator: "lucid-evolution-local-uplc-v1",
          referenceScripts: transaction.referenceScripts,
        });
        await append({
          kind: "submission_intent",
          actionId,
          actionInput: {
            schemaVersion: "midgard-production-cursor-family-action-v1",
            category: "executionNativeScriptInvalid",
            stage: `step_${activeStage.step.toString().padStart(2, "0")}`,
          },
          attempt: 1,
          txHash: transaction.txHash,
        });
      };
      const common = {
        lucid: workflow.lucid,
        contracts: workflow.contracts,
        categoryId: workflow.binding.definition.categoryId,
        signer: workflow.signer,
        threadOutRef: activeStage.threadOutRef,
        awaitConfirmation: false,
        preSubmitBoundary,
      } as const;
      let result: { txHash: string };
      if (activeStage.step === 1) {
        if (transactionEntry !== undefined) {
          const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(txCbor);
          const trie = await buildTrieView(
            prepared.block.transactions.map((entry) => ({
              key: Buffer.from(entry.nodeTxId, "hex"),
              value: Buffer.from(entry.l2TransactionSourceCbor, "hex"),
            })),
          );
          result = await submitExecutionNativeScriptInvalidStep01AcceptedV1({
            ...common,
            blueprint: workflow.binding.blueprint as Parameters<
              typeof submitExecutionNativeScriptInvalidStep01AcceptedV1
            >[0]["blueprint"],
            network: workflow.binding.network,
            stateQueueBlockOutRef: activeStage.stateQueueBlockOutRef,
            txInclusion: parseSubmitStep01TxInclusion({
              nativeTxId: transactionEntry.nodeTxId,
              nativeTx: nativeTxFromCoreCompact(material.compact),
              nativeTxCompactCbor:
                material.proofSource.compactCbor.toString("hex"),
              l2TransactionSourceCbor: transactionEntry.l2TransactionSourceCbor,
              transactionsPhasRoot: trie.root,
              txMembershipProofCbor: requireProof(
                trie,
                Buffer.from(transactionEntry.nodeTxId, "hex"),
                "executionNativeScriptInvalid transaction",
              ),
            }),
            header: prepared.block.header,
            executionIndex: BigInt(prepared.detection.executionIndex),
            referenceScriptUtxo: workflow.references.steps[0],
            witnessReferenceScripts: workflow.references.witnesses,
          });
        } else {
          const eventKey = {
            ForcedTransactionEventKey: { tx_order_id: forcedEntry!.key },
          } as const;
          result = await submitExecutionNativeScriptInvalidStep01ForcedV1({
            ...common,
            header: prepared.block.header,
            membership: await buildForcedTransactionLeafMembershipProof({
              reconstruction: prepared.block.reconstruction,
              eventKey,
            }),
            executionIndex: BigInt(prepared.detection.executionIndex),
            referenceScriptUtxo: workflow.references.steps[0],
          });
        }
      } else if (activeStage.step === 2) {
        if (
          forcedEntry === undefined ||
          forcedEntry.value.verdict === "ForcedTxValid"
        )
          throw new Error("executionNativeScriptInvalid forced source changed");
        const eventKey = {
          ForcedTransactionEventKey: { tx_order_id: forcedEntry.key },
        } as const;
        const authentication =
          await buildExecutionSourceMachineAuthenticationFromRetainedDaV1({
            eventKey,
            executionIndex: prepared.detection.executionIndex,
            authenticatedValidationTraceEntries:
              prepared.block.reconstruction.payload.block_body.validation_traces.map(
                ([key, value]) => ({
                  key: Buffer.from(key, "hex"),
                  value: Buffer.from(value, "hex"),
                }),
              ),
            retainedValidationWitnessEntries:
              prepared.block.reconstruction.payload.block_body.validation_trace_witnesses.map(
                ([key, value]) => ({
                  key: Buffer.from(key, "hex"),
                  value: Buffer.from(value, "hex"),
                }),
              ),
            expectedValidationTracesRoot:
              prepared.block.header.validationTracesRoot,
            expectedPurposeKind: purpose.purposeKindTag,
          });
        const evidence = prepareExecutionNativeScriptInvalidEvidenceV1({
          finding: {
            subject: forcedVerdictSubjectV1({
              transactionId: forcedEntry.value.tx_id,
              sourceKey: forcedEntry.key,
              rejectionReason: forcedEntry.value.verdict.ForcedTxInvalid.reason,
            }),
            executionIndex: prepared.detection.executionIndex,
          },
          transactionIdHex: forcedEntry.value.tx_id,
          sourceDescriptorHashHex: (purpose.source.originKind === 0
            ? hashMidgardInlineScriptSourceLeafV1({
                sourceIndex: BigInt(purpose.source.sourceIndex),
                scriptLanguageTag: purpose.source.languageTag,
                scriptHash: Buffer.from(purpose.source.scriptHash, "hex"),
                scriptTotalLength: purpose.source.totalLength,
                itemCommitment: Buffer.from(
                  purpose.source.itemCommitment,
                  "hex",
                ),
              })
            : hashMidgardReferenceScriptSourceLeafV1({
                sourceKey: Buffer.from(purpose.source.sourceKey, "hex"),
                scriptLanguageTag: purpose.source.languageTag,
                scriptHash: Buffer.from(purpose.source.scriptHash, "hex"),
                scriptTotalLength: purpose.source.totalLength,
                itemCommitment: Buffer.from(
                  purpose.source.itemCommitment,
                  "hex",
                ),
              })
          ).toString("hex"),
          scriptItemHashHex: computeHash32(
            decodeMidgardVersionedScript(
              Buffer.from(purpose.source.versionedItemCbor, "hex"),
            ).scriptBytes,
          ).toString("hex"),
          scriptBytes: decodeMidgardVersionedScript(
            Buffer.from(purpose.source.versionedItemCbor, "hex"),
          ).scriptBytes,
          addressWitnessItems,
          validityIntervalStart: tx.body.validityIntervalStart,
          validityIntervalEnd: tx.body.validityIntervalEnd,
        });
        result = await submitExecutionNativeScriptInvalidStep02V1({
          ...common,
          evidence,
          authentication: authentication.authentication,
          referenceScriptUtxo: workflow.references.steps[1],
        });
      } else if (activeStage.step === 3) {
        result = await submitExecutionNativeScriptInvalidStep03({
          ...common,
          scriptItemCbor: Buffer.from(purpose.source.versionedItemCbor, "hex"),
          referenceScriptUtxo: workflow.references.steps[2],
        });
      } else if (activeStage.step === 4) {
        result = await submitExecutionNativeScriptInvalidStep04StartSignerScan({
          ...common,
          nativeTxCompactCbor: compactCbor,
          witnessSet,
          scriptItemCbor: Buffer.from(purpose.source.versionedItemCbor, "hex"),
          addressWitnessItems,
          referenceScriptUtxo: workflow.references.steps[3],
        });
      } else if (activeStage.step === 5) {
        result = await submitExecutionNativeScriptInvalidStep05({
          ...common,
          nativeTxCompactCbor: compactCbor,
          witnessSet,
          addressWitnessItems,
          referenceScriptUtxo: workflow.references.steps[4],
        });
      } else if (activeStage.step === 6) {
        result = await submitExecutionNativeScriptInvalidStep06({
          ...common,
          scriptItemCbor: Buffer.from(purpose.source.versionedItemCbor, "hex"),
          addressWitnessItems,
          referenceScriptUtxo: workflow.references.steps[5],
          witnessReferenceScripts: workflow.references.witnesses,
        });
      } else if (activeStage.step === 7) {
        result = await submitExecutionNativeScriptInvalidAcceptedInitV1({
          ...common,
          referenceScriptUtxo: workflow.references.steps[6],
        });
      } else {
        if (transactionEntry === undefined)
          throw new Error(
            "executionNativeScriptInvalid forced direction entered accepted reconstruction",
          );
        const accepted = workflow.contracts.acceptedPrelude;
        if (accepted === undefined || accepted.length !== 7)
          throw new Error(
            "executionNativeScriptInvalid accepted chain disappeared",
          );
        const acceptedStepIndex = activeStage.step - 7;
        const { threadUtxo } = await requireLinearFaultThreadUtxoV1({
          lucid: workflow.lucid,
          contracts: { ...workflow.contracts, steps: accepted },
          categoryId: workflow.binding.definition.categoryId,
          family: "execution-native-script-invalid",
          stepIndex: acceptedStepIndex,
          threadOutRef: activeStage.threadOutRef,
        });
        const state =
          requireLinearFaultStepStateV1<AcceptedReconstructionStateV1>({
            threadUtxo,
            signer: workflow.signer,
            schema: ExecutionNativeScriptInvalidAcceptedDatumV1Schema as never,
            family: "execution-native-script-invalid",
            stepIndex: acceptedStepIndex,
          });
        const membership = async (key: Buffer, output: Buffer) => {
          const entries = (predecessor?.utxos ?? []).map(
            ({ key: candidate, value }) => ({
              key: Buffer.from(candidate),
              value: buildCanonicalMidgardLedgerOutputMaterialV1({
                outputIndex:
                  decodeMidgardSpendInputItemV1(candidate).outputIndex,
                outputCbor: value,
              }).descriptorCbor,
            }),
          );
          const trie = await buildTrieView(entries);
          const descriptor = buildCanonicalMidgardLedgerOutputMaterialV1({
            outputIndex: decodeMidgardSpendInputItemV1(key).outputIndex,
            outputCbor: output,
          }).descriptorCbor;
          const proofCbor = requireProof(
            trie,
            key,
            "executionNativeScriptInvalid prior ledger",
          );
          return {
            descriptorCbor: descriptor.toString("hex"),
            membershipProof: Data.from(proofCbor, Proof),
            membershipProofCbor: proofCbor,
          };
        };
        if (activeStage.step === 8) {
          const items = decodeMidgardFieldPreimageV1(
            tx.body.spendInputsPreimageCbor,
          );
          const item = items[Number(state.field_cursor)];
          if (item === undefined) {
            result =
              await submitExecutionNativeScriptInvalidAcceptedFinishSpendsV1({
                ...common,
                nativeTxCompactCbor: compactCbor,
                spendInputsPreimageCbor:
                  tx.body.spendInputsPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[7],
              });
          } else {
            const output = priorOutputs.get(item.toString("hex"));
            if (output === undefined)
              throw new Error(
                "executionNativeScriptInvalid spend output disappeared",
              );
            result = await submitExecutionNativeScriptInvalidAcceptedSpendV1({
              ...common,
              network: workflow.binding.network,
              nativeTxCompactCbor: compactCbor,
              spendInputsPreimageCbor:
                tx.body.spendInputsPreimageCbor.toString("hex"),
              ...(await membership(Buffer.from(item), output)),
              membershipReferenceScriptUtxo:
                workflow.references.witnesses.phasMembershipWithdraw,
              referenceScriptUtxo: workflow.references.steps[7],
            });
          }
        } else if (activeStage.step === 9) {
          const items = decodeMidgardFieldPreimageV1(tx.body.mintPreimageCbor);
          result =
            items[Number(state.field_cursor)] === undefined
              ? await submitExecutionNativeScriptInvalidAcceptedFinishPurposeV1(
                  {
                    ...common,
                    phase: "mint",
                    nativeTxCompactCbor: compactCbor,
                    fieldPreimageCbor: tx.body.mintPreimageCbor.toString("hex"),
                    referenceScriptUtxo: workflow.references.steps[8],
                  },
                )
              : await submitExecutionNativeScriptInvalidAcceptedMintV1({
                  ...common,
                  nativeTxCompactCbor: compactCbor,
                  mintPreimageCbor: tx.body.mintPreimageCbor.toString("hex"),
                  referenceScriptUtxo: workflow.references.steps[8],
                });
        } else if (activeStage.step === 10) {
          const items = decodeMidgardFieldPreimageV1(
            tx.body.requiredObserversPreimageCbor,
          );
          result =
            items[Number(state.field_cursor)] === undefined
              ? await submitExecutionNativeScriptInvalidAcceptedFinishPurposeV1(
                  {
                    ...common,
                    phase: "observer",
                    nativeTxCompactCbor: compactCbor,
                    fieldPreimageCbor:
                      tx.body.requiredObserversPreimageCbor.toString("hex"),
                    referenceScriptUtxo: workflow.references.steps[9],
                  },
                )
              : await submitExecutionNativeScriptInvalidAcceptedObserverV1({
                  ...common,
                  nativeTxCompactCbor: compactCbor,
                  observersPreimageCbor:
                    tx.body.requiredObserversPreimageCbor.toString("hex"),
                  referenceScriptUtxo: workflow.references.steps[9],
                });
        } else if (activeStage.step === 11) {
          const items = decodeMidgardFieldPreimageV1(
            tx.body.outputsPreimageCbor,
          );
          result =
            items[Number(state.field_cursor)] === undefined
              ? await submitExecutionNativeScriptInvalidAcceptedFinishReceivePassV1(
                  {
                    ...common,
                    nativeTxCompactCbor: compactCbor,
                    outputsPreimageCbor:
                      tx.body.outputsPreimageCbor.toString("hex"),
                    referenceScriptUtxo: workflow.references.steps[10],
                  },
                )
              : await submitExecutionNativeScriptInvalidAcceptedReceiveV1({
                  ...common,
                  nativeTxCompactCbor: compactCbor,
                  outputsPreimageCbor:
                    tx.body.outputsPreimageCbor.toString("hex"),
                  referenceScriptUtxo: workflow.references.steps[10],
                });
        } else if (activeStage.step === 12) {
          const items = decodeMidgardFieldPreimageV1(
            tx.witnessSet.scriptTxWitsPreimageCbor,
          );
          result =
            items[Number(state.field_cursor)] === undefined
              ? await submitExecutionNativeScriptInvalidAcceptedFinishInlineV1({
                  ...common,
                  nativeTxCompactCbor: compactCbor,
                  witnessSet,
                  scriptsPreimageCbor:
                    tx.witnessSet.scriptTxWitsPreimageCbor.toString("hex"),
                  referenceScriptUtxo: workflow.references.steps[11],
                })
              : await submitExecutionNativeScriptInvalidAcceptedInlineSourceV1({
                  ...common,
                  nativeTxCompactCbor: compactCbor,
                  witnessSet,
                  scriptsPreimageCbor:
                    tx.witnessSet.scriptTxWitsPreimageCbor.toString("hex"),
                  referenceScriptUtxo: workflow.references.steps[11],
                });
        } else if (activeStage.step === 13) {
          const items = decodeMidgardFieldPreimageV1(
            tx.body.referenceInputsPreimageCbor,
          );
          const item = items[Number(state.field_cursor)];
          if (item === undefined)
            throw new Error(
              "executionNativeScriptInvalid reference source exhausted",
            );
          const output = priorOutputs.get(item.toString("hex"));
          if (output === undefined)
            throw new Error(
              "executionNativeScriptInvalid reference output disappeared",
            );
          result =
            await submitExecutionNativeScriptInvalidAcceptedReferenceSourceV1({
              ...common,
              network: workflow.binding.network,
              nativeTxCompactCbor: compactCbor,
              referenceInputsPreimageCbor:
                tx.body.referenceInputsPreimageCbor.toString("hex"),
              ...(await membership(Buffer.from(item), output)),
              membershipReferenceScriptUtxo:
                workflow.references.witnesses.phasMembershipWithdraw,
              referenceScriptUtxo: workflow.references.steps[12],
            });
        } else {
          throw new Error(
            "executionNativeScriptInvalid impossible accepted stage",
          );
        }
      }
      if (capturedHash !== result.txHash)
        throw new Error(
          "executionNativeScriptInvalid provider substituted transaction",
        );
      await append({
        kind: "submitted",
        actionId,
        attempt: 1,
        txHash: result.txHash,
      });
    }
    return Object.freeze({
      kind: "pending",
      headerHash: prepared.block.headerHash,
      detectionId: prepared.detection.detectionId,
      direction: prepared.detection.direction,
    });
  };

export type LoadedExecutionNativeScriptInvalidProductionWorkflowV1 = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundExecutionNativeScriptInvalidWorkflowConfigV1;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadExecutionNativeScriptInvalidProductionWorkflowV1 = (input: {
  runtimeConfigPath: string;
  invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedExecutionNativeScriptInvalidProductionWorkflowV1>;

export const createExecutionNativeScriptInvalidProductionWorkflowRunnerSurfaceV1 =
  ({
    loadRuntimeConfig,
  }: {
    loadRuntimeConfig: LoadExecutionNativeScriptInvalidProductionWorkflowV1;
  }): ProductionWorkflowAdapterRunnerV1 =>
    Object.freeze({
      runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
      runOrResume: async (invocation) => {
        if (invocation.category !== "executionNativeScriptInvalid")
          throw new Error(
            "executionNativeScriptInvalid runner category changed",
          );
        const journal = bindProductionWorkflowFundingReservationJournalV1({
          permit: invocation.fundingReservationPermit,
          journal: bindProductionWorkflowActuationJournalV1({
            journal: new DirectoryFraudProofWorkflowJournalStoreV1(
              invocation.journalDirectory,
            ),
            permit: invocation.actuationPermit,
            decisionDigest: invocation.decisionDigest,
            deploymentFingerprint: invocation.deploymentFingerprint,
            category: "executionNativeScriptInvalid",
            headerHash: invocation.headerHash,
          }),
        });
        assertProductionWorkflowJournalActuationV1({
          journal,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "executionNativeScriptInvalid",
          headerHash: invocation.headerHash,
          checkpoint: "runner_start",
        });
        const loaded = await loadRuntimeConfig({
          runtimeConfigPath: invocation.runtimeConfigPath,
          invocation,
        });
        try {
          if (
            loaded.schemaVersion !==
              "midgard-production-fraud-proof-runtime-config-v1" ||
            loaded.retainedDaSources.length === 0 ||
            loaded.retainedDaSources.some(
              (source) => !(source instanceof DaLibp2pRetainedDaSource),
            )
          )
            throw new Error(
              "executionNativeScriptInvalid requires concrete public retained DA",
            );
          const workflow =
            await createManifestBoundExecutionNativeScriptInvalidWorkflowV1(
              loaded.config,
            );
          if (
            workflow.binding.deploymentFingerprint !==
              invocation.deploymentFingerprint ||
            workflow.binding.definition.headerHash !== invocation.headerHash
          )
            throw new Error(
              "executionNativeScriptInvalid runtime binding changed invocation",
            );
          return await runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflowV1(
            {
              workflow,
              sources: loaded.retainedDaSources,
              journal,
              decisionDigest: invocation.decisionDigest,
            },
          );
        } finally {
          await loaded.close();
        }
      },
    });

export const createExecutionNativeScriptInvalidProductionWorkflowRunnerFactoryV1 =
  createExecutionNativeScriptInvalidProductionWorkflowRunnerSurfaceV1;
