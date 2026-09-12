import {
  computeHash32,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCompact,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
} from "@al-ft/midgard-core";
import {
  forcedVerdictSubject,
  FraudProofComputationThreadStepDatum,
  Proof,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Data, type LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildExecutionSourceMachineAuthenticationFromRetainedDa } from "../execution-source-script-decoding/retained-witness.js";
import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
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
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
  workflowActuationDecisionDigest,
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import {
  encodeWorkflowArtifact,
  requireWorkflowArtifactMatches,
} from "../workflow/artifact-codec.js";
import {
  admitCompleteCanonicalReplayHistoricalCorpus,
  EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
} from "../workflow/complete-replay.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import type {
  HistoricalNativeScriptCheckpointStore,
  HistoricalNativeScriptHistorySource,
} from "../workflow/historical-native-script-corpus.js";
import {
  requireHistoricalNativeScriptCorpus,
  resolveHistoricalNativeScriptCorpus,
} from "../workflow/historical-native-script-corpus.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { executeManifestBoundFamilyRecovery } from "../workflow/manifest-bound-family-recovery.js";
import {
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
} from "../workflow/orchestrator.js";
import { continuePendingWorkflow } from "../workflow/pending-continuation.js";
import {
  captureLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
} from "../workflow/transaction-boundary.js";
import type { AcceptedReconstructionState } from "./accepted-reconstruction-machine.js";
import { reconstructExecutionNativeScriptPurposes } from "./canonical-reconstruction.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES,
  EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES,
  type ExecutionNativeScriptInvalidContracts,
} from "./contracts.js";
import { prepareExecutionNativeScriptInvalidEvidence } from "./family.js";
import { detectExecutionNativeScriptInvalidCanonicalViolations } from "./replay.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ExecutionNativeScriptInvalidStep02DatumSchema,
  ExecutionNativeScriptInvalidStep03DatumSchema,
  ExecutionNativeScriptInvalidStep04DatumSchema,
  ExecutionNativeScriptInvalidStep05DatumSchema,
  ExecutionNativeScriptInvalidStep06DatumSchema,
} from "./schemas.js";
import {
  submitExecutionNativeScriptInvalidAcceptedFinishInline,
  submitExecutionNativeScriptInvalidAcceptedFinishPurpose,
  submitExecutionNativeScriptInvalidAcceptedFinishReceivePass,
  submitExecutionNativeScriptInvalidAcceptedFinishSpends,
  submitExecutionNativeScriptInvalidAcceptedInit,
  submitExecutionNativeScriptInvalidAcceptedInlineSource,
  submitExecutionNativeScriptInvalidAcceptedMint,
  submitExecutionNativeScriptInvalidAcceptedObserver,
  submitExecutionNativeScriptInvalidAcceptedReceive,
  submitExecutionNativeScriptInvalidAcceptedReferenceSource,
  submitExecutionNativeScriptInvalidAcceptedSpend,
} from "./submit-accepted-reconstruction.js";
import { submitExecutionNativeScriptInvalidInit } from "./submit-init.js";
import {
  submitExecutionNativeScriptInvalidStep01Accepted,
  submitExecutionNativeScriptInvalidStep01Forced,
} from "./submit-step-01.js";
import { submitExecutionNativeScriptInvalidStep02 } from "./submit-step-02.js";
import { submitExecutionNativeScriptInvalidStep03 } from "./submit-step-03.js";
import { submitExecutionNativeScriptInvalidStep04StartSignerScan } from "./submit-step-04.js";
import { submitExecutionNativeScriptInvalidStep05 } from "./submit-step-05.js";
import { submitExecutionNativeScriptInvalidStep06 } from "./submit-step-06.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "./workflow-spec.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_WORKFLOW =
  "midgard-execution-native-script-invalid-production-workflow-v1" as const;

export const EXECUTION_NATIVE_SCRIPT_INVALID_CONFIG_KEYS = Object.freeze([
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

export const EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS = Object.freeze(
  [
    FraudProofComputationThreadStepDatum,
    ExecutionNativeScriptInvalidStep02DatumSchema,
    ExecutionNativeScriptInvalidStep03DatumSchema,
    ExecutionNativeScriptInvalidStep04DatumSchema,
    ExecutionNativeScriptInvalidStep05DatumSchema,
    ExecutionNativeScriptInvalidStep06DatumSchema,
    ExecutionNativeScriptInvalidStep02DatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
    ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ] as const,
);

export type ExecutionNativeScriptInvalidWorkflowReferenceScripts = Readonly<{
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
  witnesses: Required<FaultProofWitnessReferenceScripts>;
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

export type ManifestBoundExecutionNativeScriptInvalidWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  historicalCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalSource: HistoricalNativeScriptHistorySource;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ExecutionNativeScriptInvalidWorkflowReferenceScripts;
}>;

export type ManifestBoundExecutionNativeScriptInvalidWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"executionNativeScriptInvalid">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  historicalCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalSource: HistoricalNativeScriptHistorySource;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  contracts: ExecutionNativeScriptInvalidContracts;
  references: ExecutionNativeScriptInvalidWorkflowReferenceScripts;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPort>;
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
export const createManifestBoundExecutionNativeScriptInvalidWorkflow = async (
  config: ManifestBoundExecutionNativeScriptInvalidWorkflowConfig,
): Promise<ManifestBoundExecutionNativeScriptInvalidWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...EXECUTION_NATIVE_SCRIPT_INVALID_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "executionNativeScriptInvalid production config contains callback authority",
    );
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "executionNativeScriptInvalid",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: EXECUTION_NATIVE_SCRIPT_INVALID_STEP_DATUM_SCHEMAS,
  });
  assertManifestBoundWorkflowSigner({
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
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as ExecutionNativeScriptInvalidWorkflowReferenceScripts["steps"];
  const bindReference = (contractName: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
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
    ) as unknown as ExecutionNativeScriptInvalidWorkflowReferenceScripts["removal"],
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
  const contracts: ExecutionNativeScriptInvalidContracts = Object.freeze({
    steps: chain.steps.slice(0, 6).map((step, index) => ({
      ...step,
      blueprintTitle: EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })),
    acceptedPrelude: chain.steps.slice(6).map((step, index) => ({
      ...step,
      blueprintTitle:
        EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES[index]!,
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
    l1: createFraudProofFamilyLocalKupmiosL1ObservationPort({
      source: config.source,
      releaseFinality: binding.releaseFinality,
      releaseEconomics: binding.releaseEconomics,
      definition: binding.definition,
    }),
  });
};

/** Rebuild the one actionable ID32 decision solely from L1 and retained DA. */
export const prepareManifestBoundExecutionNativeScriptInvalidReplay =
  async (input: {
    workflow: ManifestBoundExecutionNativeScriptInvalidWorkflow;
    sources: readonly import("../transition-trace/fetch.js").RetainedDaPayloadSource[];
  }) => {
    if (Object.keys(input).sort().join(",") !== "sources,workflow")
      throw new Error(
        "executionNativeScriptInvalid replay rejects caller-authored evidence",
      );
    const { workflow, sources } = input;
    const block = await fetchCanonicalBlockEvidence({
      observation: await observeFraudProofWorkflowHeader(workflow.l1, {
        headerHash: workflow.binding.definition.headerHash,
      }),
      sources,
    });
    const corpus = await resolveHistoricalNativeScriptCorpus({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      checkpointStore: workflow.historicalCheckpointStore,
      historySource: workflow.historicalSource,
      currentEvidence: block,
      sources,
    });
    const detections = detectExecutionNativeScriptInvalidCanonicalViolations({
      block,
      corpus,
    });
    if (detections.length !== 1)
      throw new Error(
        `executionNativeScriptInvalid replay yielded ${detections.length.toString()} exact findings`,
      );
    return Object.freeze({ block, corpus, detection: detections[0]! });
  };

export type ExecutionNativeScriptInvalidRunResult = FraudProofWorkflowRunResult;
type PreparedExecutionNativeScriptInvalid = Awaited<
  ReturnType<typeof prepareManifestBoundExecutionNativeScriptInvalidReplay>
>;

/** Builds one exact action; the common adapter owns signing records and submission. */
const captureExecutionNativeScriptInvalidAction = async ({
  workflow,
  prepared,
  action,
}: {
  workflow: ManifestBoundExecutionNativeScriptInvalidWorkflow;
  prepared: PreparedExecutionNativeScriptInvalid;
  action: FraudProofWorkflowAction;
}) => {
  const input = cursorFamilyActionInput({
    category: "executionNativeScriptInvalid",
    action,
  });
  if (input.stage === "remove")
    return captureCursorRemoval({
      category: "executionNativeScriptInvalid",
      lucid: workflow.lucid,
      blueprint: workflow.binding.blueprint,
      deploymentInfo: workflow.binding.deploymentInfo,
      network: workflow.binding.network,
      signer: workflow.signer,
      headerHash: prepared.block.headerHash,
      input,
      stateQueueMutationLeaseCoordinator:
        workflow.stateQueueMutationLeaseCoordinator,
      fraudProverRewardLovelace: BigInt(
        workflow.binding.releaseEconomics.policy.fraudProverRewardLovelace,
      ),
    });
  const transaction = await captureLocallyEvaluatedTransaction(
    async (preSubmitBoundary: FraudProofPreSubmitBoundary) => {
      if (input.stage === "init") {
        await submitExecutionNativeScriptInvalidInit({
          lucid: workflow.lucid,
          blueprint: workflow.binding.blueprint as Parameters<
            typeof submitExecutionNativeScriptInvalidInit
          >[0]["blueprint"],
          network: workflow.binding.network,
          contracts: workflow.contracts,
          category: workflow.binding.resolvedContracts.category,
          catalogue: workflow.binding.catalogue,
          signer: workflow.signer,
          fraudulentBlockOutRef: cursorStringField(
            input,
            "stateQueueBlockOutRef",
          ),
          fraudulentHeaderHash: prepared.block.headerHash,
          witnessReferenceScripts: workflow.references.witnesses,
          awaitConfirmation: false,
          preSubmitBoundary,
        });
        return;
      }
      const activeStage = {
        step: Number(input.ordinal),
        threadOutRef: cursorStringField(input, "threadOutRef"),
        stateQueueBlockOutRef: cursorStringField(
          input,
          "stateQueueBlockOutRef",
        ),
      };
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
      const tx = decodeMidgardNativeTxFullFromCanonicalCbor(txCbor);
      const compactCbor = encodeMidgardNativeTxCompact(tx.compact).toString(
        "hex",
      );
      const compactWitness = deriveMidgardNativeTxWitnessSetCompact(
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
      const addressWitnessItems = decodeMidgardFieldPreimage(
        tx.witnessSet.addrTxWitsPreimageCbor,
      );
      const history = requireHistoricalNativeScriptCorpus(prepared.corpus);
      const predecessor = history.reconstructions.at(-2);
      const priorOutputs = new Map(
        (predecessor?.utxos ?? []).map(({ key, value }) => [
          Buffer.from(key).toString("hex"),
          Buffer.from(value),
        ]),
      );
      const reconstruction = reconstructExecutionNativeScriptPurposes({
        canonicalTransactionCbor: txCbor,
        resolvedOutputsByOutRef: priorOutputs,
      });
      const purpose =
        reconstruction.purposes[prepared.detection.executionIndex];
      if (purpose === undefined)
        throw new Error(
          "executionNativeScriptInvalid execution coordinate disappeared",
        );
      const common = {
        lucid: workflow.lucid,
        contracts: workflow.contracts,
        categoryId: workflow.binding.definition.categoryId,
        signer: workflow.signer,
        threadOutRef: activeStage.threadOutRef,
        awaitConfirmation: false,
        preSubmitBoundary,
      } as const;
      if (activeStage.step === 1) {
        if (transactionEntry !== undefined) {
          const material = deriveMidgardNativeTxFaultEvidenceMaterial(txCbor);
          const trie = await buildTrieView(
            prepared.block.transactions.map((entry) => ({
              key: Buffer.from(entry.nodeTxId, "hex"),
              value: Buffer.from(entry.l2TransactionSourceCbor, "hex"),
            })),
          );
          await submitExecutionNativeScriptInvalidStep01Accepted({
            ...common,
            blueprint: workflow.binding.blueprint as Parameters<
              typeof submitExecutionNativeScriptInvalidStep01Accepted
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
          await submitExecutionNativeScriptInvalidStep01Forced({
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
          await buildExecutionSourceMachineAuthenticationFromRetainedDa({
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
        const evidence = prepareExecutionNativeScriptInvalidEvidence({
          finding: {
            subject: forcedVerdictSubject({
              transactionId: forcedEntry.value.tx_id,
              sourceKey: forcedEntry.key,
              rejectionReason: forcedEntry.value.verdict.ForcedTxInvalid.reason,
            }),
            executionIndex: prepared.detection.executionIndex,
          },
          transactionIdHex: forcedEntry.value.tx_id,
          sourceDescriptorHashHex: (purpose.source.originKind === 0
            ? hashMidgardInlineScriptSourceLeaf({
                sourceIndex: BigInt(purpose.source.sourceIndex),
                scriptLanguageTag: purpose.source.languageTag,
                scriptHash: Buffer.from(purpose.source.scriptHash, "hex"),
                scriptTotalLength: purpose.source.totalLength,
                itemCommitment: Buffer.from(
                  purpose.source.itemCommitment,
                  "hex",
                ),
              })
            : hashMidgardReferenceScriptSourceLeaf({
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
        await submitExecutionNativeScriptInvalidStep02({
          ...common,
          evidence,
          authentication: authentication.authentication,
          referenceScriptUtxo: workflow.references.steps[1],
        });
      } else if (activeStage.step === 3) {
        await submitExecutionNativeScriptInvalidStep03({
          ...common,
          scriptItemCbor: Buffer.from(purpose.source.versionedItemCbor, "hex"),
          referenceScriptUtxo: workflow.references.steps[2],
        });
      } else if (activeStage.step === 4) {
        await submitExecutionNativeScriptInvalidStep04StartSignerScan({
          ...common,
          nativeTxCompactCbor: compactCbor,
          witnessSet,
          scriptItemCbor: Buffer.from(purpose.source.versionedItemCbor, "hex"),
          addressWitnessItems,
          referenceScriptUtxo: workflow.references.steps[3],
        });
      } else if (activeStage.step === 5) {
        await submitExecutionNativeScriptInvalidStep05({
          ...common,
          nativeTxCompactCbor: compactCbor,
          witnessSet,
          addressWitnessItems,
          referenceScriptUtxo: workflow.references.steps[4],
        });
      } else if (activeStage.step === 6) {
        await submitExecutionNativeScriptInvalidStep06({
          ...common,
          scriptItemCbor: Buffer.from(purpose.source.versionedItemCbor, "hex"),
          addressWitnessItems,
          referenceScriptUtxo: workflow.references.steps[5],
          witnessReferenceScripts: workflow.references.witnesses,
        });
      } else if (activeStage.step === 7) {
        await submitExecutionNativeScriptInvalidAcceptedInit({
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
        const { threadUtxo } = await requireLinearFaultThreadUtxo({
          lucid: workflow.lucid,
          contracts: { ...workflow.contracts, steps: accepted },
          categoryId: workflow.binding.definition.categoryId,
          family: "execution-native-script-invalid",
          stepIndex: acceptedStepIndex,
          threadOutRef: activeStage.threadOutRef,
        });
        const state = requireLinearFaultStepState<AcceptedReconstructionState>({
          threadUtxo,
          signer: workflow.signer,
          schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
          family: "execution-native-script-invalid",
          stepIndex: acceptedStepIndex,
        });
        const membership = async (key: Buffer, output: Buffer) => {
          const entries = (predecessor?.utxos ?? []).map(
            ({ key: candidate, value }) => ({
              key: Buffer.from(candidate),
              value: buildCanonicalMidgardLedgerOutputMaterial({
                outputIndex: decodeMidgardSpendInputItem(candidate).outputIndex,
                outputCbor: value,
              }).descriptorCbor,
            }),
          );
          const trie = await buildTrieView(entries);
          const descriptor = buildCanonicalMidgardLedgerOutputMaterial({
            outputIndex: decodeMidgardSpendInputItem(key).outputIndex,
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
          const items = decodeMidgardFieldPreimage(
            tx.body.spendInputsPreimageCbor,
          );
          const item = items[Number(state.field_cursor)];
          if (item === undefined) {
            await submitExecutionNativeScriptInvalidAcceptedFinishSpends({
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
            await submitExecutionNativeScriptInvalidAcceptedSpend({
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
          const items = decodeMidgardFieldPreimage(tx.body.mintPreimageCbor);
          await (items[Number(state.field_cursor)] === undefined
            ? submitExecutionNativeScriptInvalidAcceptedFinishPurpose({
                ...common,
                phase: "mint",
                nativeTxCompactCbor: compactCbor,
                fieldPreimageCbor: tx.body.mintPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[8],
              })
            : submitExecutionNativeScriptInvalidAcceptedMint({
                ...common,
                nativeTxCompactCbor: compactCbor,
                mintPreimageCbor: tx.body.mintPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[8],
              }));
        } else if (activeStage.step === 10) {
          const items = decodeMidgardFieldPreimage(
            tx.body.requiredObserversPreimageCbor,
          );
          await (items[Number(state.field_cursor)] === undefined
            ? submitExecutionNativeScriptInvalidAcceptedFinishPurpose({
                ...common,
                phase: "observer",
                nativeTxCompactCbor: compactCbor,
                fieldPreimageCbor:
                  tx.body.requiredObserversPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[9],
              })
            : submitExecutionNativeScriptInvalidAcceptedObserver({
                ...common,
                nativeTxCompactCbor: compactCbor,
                observersPreimageCbor:
                  tx.body.requiredObserversPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[9],
              }));
        } else if (activeStage.step === 11) {
          const items = decodeMidgardFieldPreimage(tx.body.outputsPreimageCbor);
          await (items[Number(state.field_cursor)] === undefined
            ? submitExecutionNativeScriptInvalidAcceptedFinishReceivePass({
                ...common,
                nativeTxCompactCbor: compactCbor,
                outputsPreimageCbor:
                  tx.body.outputsPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[10],
              })
            : submitExecutionNativeScriptInvalidAcceptedReceive({
                ...common,
                nativeTxCompactCbor: compactCbor,
                outputsPreimageCbor:
                  tx.body.outputsPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[10],
              }));
        } else if (activeStage.step === 12) {
          const items = decodeMidgardFieldPreimage(
            tx.witnessSet.scriptTxWitsPreimageCbor,
          );
          await (items[Number(state.field_cursor)] === undefined
            ? submitExecutionNativeScriptInvalidAcceptedFinishInline({
                ...common,
                nativeTxCompactCbor: compactCbor,
                witnessSet,
                scriptsPreimageCbor:
                  tx.witnessSet.scriptTxWitsPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[11],
              })
            : submitExecutionNativeScriptInvalidAcceptedInlineSource({
                ...common,
                nativeTxCompactCbor: compactCbor,
                witnessSet,
                scriptsPreimageCbor:
                  tx.witnessSet.scriptTxWitsPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[11],
              }));
        } else if (activeStage.step === 13) {
          const items = decodeMidgardFieldPreimage(
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
          await submitExecutionNativeScriptInvalidAcceptedReferenceSource({
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
    },
  );
  return { transaction };
};

export const runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow =
  async ({
    workflow,
    sources,
    journal,
    decisionDigest,
  }: {
    workflow: ManifestBoundExecutionNativeScriptInvalidWorkflow;
    sources: readonly RetainedDaPayloadSource[];
    journal: FraudProofWorkflowJournalStore;
    decisionDigest: string;
  }): Promise<FraudProofWorkflowRunResult> => {
    if (workflowActuationDecisionDigest(journal) !== decisionDigest)
      throw new Error("executionNativeScriptInvalid journal decision changed");
    let fresh: PreparedExecutionNativeScriptInvalid | undefined;
    const prepareArtifact = async (
      block: import("../evidence/canonical-block-evidence.js").CanonicalBlockEvidence,
    ) => {
      const corpus = await resolveHistoricalNativeScriptCorpus({
        deploymentFingerprint: workflow.binding.deploymentFingerprint,
        checkpointStore: workflow.historicalCheckpointStore,
        historySource: workflow.historicalSource,
        currentEvidence: block,
        sources,
      });
      const detections = detectExecutionNativeScriptInvalidCanonicalViolations({
        block,
        corpus,
      });
      const detection = detections[0];
      if (detection === undefined)
        throw new Error(
          "executionNativeScriptInvalid replay has no selected artifact",
        );
      return { block, corpus, detection };
    };
    // The canonical envelope records payload identity; the family artifact records
    // exact selected proof material and the authenticated history corpus identity.
    const material = (prepared: PreparedExecutionNativeScriptInvalid) => ({
      header: prepared.block.header,
      headerHash: prepared.block.headerHash,
      detection: prepared.detection,
      corpus: prepared.corpus,
    });
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
      l1: workflow.l1,
      stateQueueMutationLeaseCoordinator:
        workflow.stateQueueMutationLeaseCoordinator,
      transactions: {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category: "executionNativeScriptInvalid",
        prepare: async ({ evidence }) => {
          fresh = await prepareArtifact(evidence);
          return encodeWorkflowArtifact(material(fresh));
        },
        validatePreparedArtifact: async ({ evidence, artifact }) => {
          fresh = await prepareArtifact(evidence);
          requireWorkflowArtifactMatches(artifact, material(fresh));
        },
        capture: async ({ action, artifact }) => {
          if (fresh === undefined)
            throw new Error(
              "executionNativeScriptInvalid capture requires current authenticated material",
            );
          requireWorkflowArtifactMatches(artifact, material(fresh));
          return captureExecutionNativeScriptInvalidAction({
            workflow,
            prepared: fresh,
            action,
          });
        },
      },
    });
    const terminalVerifier =
      createFraudProofFamilyAuthenticatedL1TerminalVerifier(workflow.l1);
    const releaseFinalityAuthority =
      releaseFinalityAuthorityFromDeploymentBinding(workflow.binding);
    return executeManifestBoundFamilyRecovery({
      binding: workflow.binding,
      l1: workflow.l1,
      adapter,
      decisionDigest,
      sources,
      journal,
      terminalVerifier,
      releaseFinalityAuthority,
      replayer: EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
      resolveReplayContext: async (evidence) => {
        fresh = await prepareArtifact(evidence);
        return {
          historicalCorpus: admitCompleteCanonicalReplayHistoricalCorpus({
            evidence,
            corpus: fresh.corpus,
          }),
        };
      },
    });
  };

export type LoadedExecutionNativeScriptInvalidWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundExecutionNativeScriptInvalidWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadExecutionNativeScriptInvalidWorkflow = (input: {
  runtimeConfigPath: string;
  invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedExecutionNativeScriptInvalidWorkflow>;

export const createExecutionNativeScriptInvalidWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadExecutionNativeScriptInvalidWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (invocation.category !== "executionNativeScriptInvalid")
        throw new Error("executionNativeScriptInvalid runner category changed");
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "executionNativeScriptInvalid",
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
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
          await createManifestBoundExecutionNativeScriptInvalidWorkflow(
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
        return await continuePendingWorkflow({
          invocation,
          journal: journal,
          execute: () =>
            runOrResumeManifestBoundExecutionNativeScriptInvalidWorkflow({
              workflow,
              sources: loaded.retainedDaSources,
              journal,
              decisionDigest: invocation.decisionDigest,
            }),
        });
      } finally {
        await loaded.close();
      }
    },
  });

export const createExecutionNativeScriptInvalidWorkflowRunnerFactory =
  createExecutionNativeScriptInvalidWorkflowRunnerSurface;
