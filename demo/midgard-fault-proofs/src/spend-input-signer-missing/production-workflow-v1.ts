import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core";
import {
  type ForcedInclusionTx,
  type FraudProofCatalogueCategoryName,
  FraudProofComputationThreadStepDatum,
  type Header,
  type OutputReference,
  OutputReferenceSchema,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
  RejectionReasonSchema,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import {
  type CanonicalBlockEvidence,
  fetchCanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence-v1.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import { deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus } from "../resolved-output-non-canonical/resolved-output-non-canonical-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  parseSubmitStep01TxInclusion,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation-v1.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
} from "../workflow/production-actuation-permit-v1.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/production-adapters-v1.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/production-funding-reservation-permit-v1.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptHistorySource,
  resolveHistoricalNativeScriptCorpus,
} from "../workflow/production-historical-native-script-corpus-v1.js";
import { createSpendInputSignerMissingCentralJournalAdapter } from "./central-journal-v1.js";
import type { SpendInputSignerMissingContracts } from "./contracts-v1.js";
import {
  SpendInputSignerStep02DatumSchema,
  SpendInputSignerStep03DatumSchema,
  SpendInputSignerStep04DatumSchema,
  SpendInputSignerStep05DatumSchema,
} from "./schemas-v1.js";
import {
  detectSpendInputSignerMissingCompleteReplay,
  type SpendInputSignerMissingEvidence,
} from "./spend-input-signer-missing-v1.js";
import { submitSpendInputSignerMissingCancel } from "./submit-cancel-v1.js";
import { submitSpendInputSignerMissingStep01Accepted } from "./submit-step-01-accepted-v1.js";
import { submitSpendInputSignerMissingStep01Forced } from "./submit-step-01-forced-v1.js";
import { submitSpendInputSignerMissingStep02 } from "./submit-step-02-v1.js";
import { submitSpendInputSignerMissingStep03 } from "./submit-step-03-v1.js";
import { submitSpendInputSignerMissingStep04 } from "./submit-step-04-v1.js";
import { submitSpendInputSignerMissingStep05 } from "./submit-step-05-v1.js";
import {
  nextSpendInputSignerAction,
  type SpendInputSignerJournal,
  type SpendInputSignerStage,
  spendInputSignerWorkflowEvidenceIdentity,
} from "./workflow-v1.js";

export const SPEND_INPUT_SIGNER_MISSING_WORKFLOW =
  "midgard-spend-input-signer-missing-production-workflow-v1" as const;
export const SPEND_INPUT_SIGNER_MISSING_VIOLATION_ID =
  "spend-input-signer-missing" as const;

export const SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS = Object.freeze({
  step01: "fraudProofSpendInputSignerMissing",
  step02: "fraudProofSpendInputSignerMissingStep02",
  step03: "fraudProofSpendInputSignerMissingStep03",
  step04: "fraudProofSpendInputSignerMissingStep04",
  step05: "fraudProofSpendInputSignerMissingStep05",
  computationThreadMint: "computationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type SpendInputSignerMissingReferenceScripts = Readonly<{
  step01: UTxO;
  step02: UTxO;
  step03: UTxO;
  step04: UTxO;
  step05: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundSpendInputSignerMissingConfig = Readonly<{
  schemaVersion: typeof SPEND_INPUT_SIGNER_MISSING_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: SpendInputSignerMissingDeploymentBinding;
  contracts: SpendInputSignerMissingContracts;
  referenceScripts: SpendInputSignerMissingReferenceScripts;
}>;

export type SpendInputSignerMissingDeploymentBinding = Omit<
  FraudProofWorkflowDeploymentBinding<FraudProofCatalogueCategoryName>,
  "definition"
> &
  Readonly<{
    definition: Omit<
      FraudProofWorkflowDeploymentBinding<FraudProofCatalogueCategoryName>["definition"],
      "category"
    > &
      Readonly<{ category: "spendInputSignerMissing" }>;
  }>;

export type LoadManifestBoundSpendInputSignerMissingConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: SpendInputSignerMissingReferenceScripts;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: SpendInputSignerMissingDeploymentBinding;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });

export const bindSpendInputSignerMissingReferenceScripts = ({
  binding,
  referenceScripts,
}: {
  readonly binding: SpendInputSignerMissingDeploymentBinding;
  readonly referenceScripts: SpendInputSignerMissingReferenceScripts;
}): SpendInputSignerMissingReferenceScripts => {
  const names = SPEND_INPUT_SIGNER_MISSING_MANIFEST_CONTRACTS;
  return Object.freeze({
    step01: bindReference({
      binding,
      contractName: names.step01,
      utxo: referenceScripts.step01,
    }),
    step02: bindReference({
      binding,
      contractName: names.step02,
      utxo: referenceScripts.step02,
    }),
    step03: bindReference({
      binding,
      contractName: names.step03,
      utxo: referenceScripts.step03,
    }),
    step04: bindReference({
      binding,
      contractName: names.step04,
      utxo: referenceScripts.step04,
    }),
    step05: bindReference({
      binding,
      contractName: names.step05,
      utxo: referenceScripts.step05,
    }),
    fieldPreimageCertificateMint: bindReference({
      binding,
      contractName: names.fieldPreimageCertificateMint,
      utxo: referenceScripts.fieldPreimageCertificateMint,
    }),
    witnesses: Object.freeze({
      ...referenceScripts.witnesses,
      computationThreadMint: bindReference({
        binding,
        contractName: names.computationThreadMint,
        utxo: referenceScripts.witnesses.computationThreadMint,
      }),
      fraudProofMint: bindReference({
        binding,
        contractName: names.fraudProofMint,
        utxo: referenceScripts.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: bindReference({
        binding,
        contractName: names.phasMembershipWithdraw,
        utxo: referenceScripts.witnesses.phasMembershipWithdraw,
      }),
    }),
  });
};

export const loadManifestBoundSpendInputSignerMissingConfig = async (
  input: LoadManifestBoundSpendInputSignerMissingConfig,
): Promise<ManifestBoundSpendInputSignerMissingConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "spendInputSignerMissing" as FraudProofCatalogueCategoryName,
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      SpendInputSignerStep02DatumSchema,
      SpendInputSignerStep03DatumSchema,
      SpendInputSignerStep04DatumSchema,
      SpendInputSignerStep05DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
  const localContracts = binding.resolvedContracts.contracts as unknown as {
    readonly spendInputSignerMissing?: SpendInputSignerMissingContracts;
  };
  const chain = localContracts.spendInputSignerMissing;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 5) {
    throw new Error(
      "spendInputSignerMissing deployment changed its five-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "spendInputSignerMissing deployment omitted field-preimage certificate",
    );
  }
  const referenceScripts = bindSpendInputSignerMissingReferenceScripts({
    binding: binding as unknown as SpendInputSignerMissingDeploymentBinding,
    referenceScripts: input.referenceScripts,
  });
  return Object.freeze({
    schemaVersion: SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
    lucid: input.lucid,
    signer: input.signer,
    binding: binding as unknown as SpendInputSignerMissingDeploymentBinding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/protected_output_signer_missing/step_01.main.spend",
          "fraud_proofs/protected_output_signer_missing/step_02.main.spend",
          "fraud_proofs/protected_output_signer_missing/step_03.main.spend",
          "fraud_proofs/protected_output_signer_missing/step_04.main.spend",
          "fraud_proofs/protected_output_signer_missing/step_05.main.spend",
        ][index]!,
        referenceOutRef: [
          referenceScripts.step01,
          referenceScripts.step02,
          referenceScripts.step03,
          referenceScripts.step04,
          referenceScripts.step05,
        ][index]!.txHash.concat(
          "#",
          [
            referenceScripts.step01,
            referenceScripts.step02,
            referenceScripts.step03,
            referenceScripts.step04,
            referenceScripts.step05,
          ][index]!.outputIndex.toString(),
        ),
      })) as unknown as SpendInputSignerMissingContracts["steps"],
      computationThread: binding.resolvedContracts.contracts.computationThread,
      fraudProof: binding.resolvedContracts.contracts.fraudProof,
      hubOraclePolicyId: binding.deploymentInfo.hubOracleMint!.scriptHash,
      stateQueuePolicyId: binding.definition.stateQueue.policyId,
      fieldPreimageCertificatePolicyId: certificate.policyId,
      fieldPreimageCertificateMintingScript: certificate.mintingScript,
    },
    referenceScripts,
  });
};

export type SpendInputSignerMissingStage = Readonly<{
  fraudulentBlockOutRef: string;
  threadOutRef?: string;
  threadUtxo?: UTxO;
  threadToken?: Readonly<{ unit: string; fraudulentHeaderHash: string }>;
  stateQueueBlockOutRef?: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTx>;
  forcedDirection?: bigint;
  nativeTxCompactCbor?: string;
  witnessSetCompactCbor?: string;
  publishedCarriageUtxos?: readonly UTxO[];
  certificateUtxo?: UTxO;
  validFrom?: bigint;
  validTo?: bigint;
}>;

/** Derives the only admissible family evidence from L1-bound public retained DA. */
export type SpendInputSignerMissingAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTx>;
  forcedDirection?: bigint;
}>;

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveSpendInputSignerMissingAuthenticatedSource = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly evidence: SpendInputSignerMissingEvidence;
}): Promise<SpendInputSignerMissingAuthenticatedSource> => {
  if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "spendInputSignerMissing accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "spendInputSignerMissing accepted source trie differs from authenticated reconstruction",
      );
    }
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(selected.txCbor, "hex"),
    );
    return Object.freeze({
      nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      acceptedInclusion: parseSubmitStep01TxInclusion({
        nativeTxId: selected.nodeTxId,
        nativeTx: selected.nativeTxCompact,
        nativeTxCompactCbor: selected.nativeCompactCbor,
        l2TransactionSourceCbor: selected.l2TransactionSourceCbor,
        transactionsPhasRoot: trie.root,
        txMembershipProofCbor: requireProof(
          trie,
          Buffer.from(selected.nodeTxId, "hex"),
          "spendInputSignerMissing accepted transaction",
        ),
      }),
    });
  }
  const forced = block.reconstruction.forcedTransactions.find(
    ({ key, value }) =>
      value.tx_id === evidence.subject.transaction_id &&
      Data.to(key as never, OutputReferenceSchema as never) ===
        Data.to(
          evidence.subject.source_key as never,
          OutputReferenceSchema as never,
        ),
  );
  if (forced === undefined || forced.value.verdict === "ForcedTxValid") {
    throw new Error(
      "spendInputSignerMissing forced subject disappeared from retained DA",
    );
  }
  const reason = forced.value.verdict.ForcedTxInvalid.reason;
  if (
    evidence.subject.rejection_reason === null ||
    Data.to(reason as never, RejectionReasonSchema as never) !==
      Data.to(
        evidence.subject.rejection_reason as never,
        RejectionReasonSchema as never,
      )
  ) {
    throw new Error(
      "spendInputSignerMissing forced reason differs from authenticated source",
    );
  }
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    encodeMidgardNativeTxCanonical(
      adjudicateMidgardNativeTxFullValidity(
        decodeMidgardNativeTxFullFromCanonicalCbor(forced.fullTransactionCbor),
        "TxIsInvalid",
      ),
    ),
  );
  if (
    material.proofSource.compactCbor.toString("hex") !==
      forced.value.source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      forced.value.source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.source.field_preimage_lengths_cbor
  ) {
    throw new Error(
      "spendInputSignerMissing forced source material differs from authenticated leaf",
    );
  }
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: forced.key },
  } as const;
  return Object.freeze({
    nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      material.proofSource.witnessSetCompactCbor.toString("hex"),
    forcedHeader: block.header,
    forcedMembership: await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    }),
    forcedDirection: 1n,
  });
};

/** Complete replay member: scans every accepted field-2 output and exact forced reason. */
type SpendInputSignerMissingRuntimeLoader = Readonly<{
  config: LoadManifestBoundSpendInputSignerMissingConfig;
  journal: SpendInputSignerJournal;
  observe: (identity: string) => Promise<SpendInputSignerStage>;
  resolveStage: (input: {
    readonly action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "submitScan"
      | "submitStep05"
      | "removeDescendants"
      | "cancel";
    readonly evidence: SpendInputSignerMissingEvidence;
  }) => Promise<SpendInputSignerMissingStage>;
}>;

export const createSpendInputSignerMissingRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundSpendInputSignerMissingConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
    readonly source: SpendInputSignerMissingAuthenticatedSource;
  }): SpendInputSignerMissingRuntimeLoader["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "spendInputSignerMissing init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "spendInputSignerMissing removal requires raw-L1 proof token",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    const expectedStep =
      action === "submitStep01"
        ? 1
        : action === "submitStep02"
          ? 2
          : action === "submitStep03"
            ? 3
            : action === "submitScan"
              ? 4
              : 5;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `spendInputSignerMissing ${action} differs from authenticated raw-L1 stage`,
      );
    }
    const common = {
      fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
      threadOutRef: stage.threadOutRef,
      nativeTxCompactCbor: source.nativeTxCompactCbor,
      witnessSetCompactCbor: source.witnessSetCompactCbor,
    };
    if (action !== "submitStep01") return common;
    if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_FORCED) {
      return {
        ...common,
        forcedHeader: required(
          source.forcedHeader,
          "authenticated forced header",
        ),
        forcedMembership: required(
          source.forcedMembership,
          "authenticated forced membership",
        ),
        forcedDirection: required(
          source.forcedDirection,
          "authenticated forced direction",
        ),
      };
    }
    const thread = await requireLinearFaultThreadUtxo({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      family: "spend-input-signer-missing",
      stepIndex: 0,
      threadOutRef: stage.threadOutRef,
    });
    return {
      ...common,
      threadUtxo: thread.threadUtxo,
      threadToken: thread.threadToken,
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      acceptedInclusion: required(
        source.acceptedInclusion,
        "authenticated accepted inclusion",
      ),
    };
  };

const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`spendInputSignerMissing missing ${label}`);
  return value;
};

const createManifestBoundSpendInputSignerMissingSubmission = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundSpendInputSignerMissingConfig;
  readonly observe: (identity: string) => Promise<SpendInputSignerStage>;
  readonly resolveStage: SpendInputSignerMissingRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createSpendInputSignerMissingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => ({
  observe,
  submit: async (
    action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "submitScan"
      | "submitStep05"
      | "removeDescendants",
    evidence: SpendInputSignerMissingEvidence,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "spendInputSignerMissing evidence transaction id is not canonical",
      );
    const familyIdentity = spendInputSignerWorkflowEvidenceIdentity(evidence);
    const transition =
      action === "submitInit"
        ? (["none", "step01"] as const)
        : action === "submitStep01"
          ? (["step01", "step02"] as const)
          : action === "submitStep02"
            ? (["step02", "step03"] as const)
            : action === "submitStep03"
              ? (["step03", "scanning"] as const)
              : action === "submitScan"
                ? (["scanning", "step05"] as const)
                : action === "submitStep05"
                  ? (["step05", "proven"] as const)
                  : (["proven", "removed"] as const);
    await centralJournal?.begin(
      action,
      familyIdentity,
      transition[0],
      transition[1],
    );
    const stage = await resolveStage({ action, evidence });
    if (action === "submitInit") {
      const result = await submitCommittedFieldShapeInit({
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        network: config.binding.network,
        contracts: config.contracts as never,
        category: config.binding.resolvedContracts.category,
        catalogue: config.binding.catalogue,
        signer: config.signer,
        fraudulentBlockOutRef: stage.fraudulentBlockOutRef,
        fraudulentHeaderHash: config.binding.definition.headerHash,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: "step01" as const,
        txHash: result.txHash,
        outputReference: `${result.txHash}#${result.firstStepOutputIndex.toString()}`,
      };
    }
    if (action === "submitStep01") {
      if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
        const result = await submitSpendInputSignerMissingStep01Accepted({
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          network: config.binding.network,
          contracts: config.contracts,
          signer: config.signer,
          evidence,
          threadUtxo: required(stage.threadUtxo, "step01 thread UTxO"),
          threadToken: required(stage.threadToken, "step01 thread token"),
          stateQueueBlockOutRef: required(
            stage.stateQueueBlockOutRef,
            "state-queue block out-ref",
          ),
          txInclusion: required(stage.acceptedInclusion, "accepted inclusion"),
          referenceScriptUtxo: config.referenceScripts.step01,
          witnessReferenceScripts: config.referenceScripts.witnesses,
          preSubmitBoundary: centralJournal?.boundary(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          ),
        });
        return {
          stage: "step02" as const,
          txHash: result.txHash,
          outputReference: result.nextThreadOutRef,
        };
      }
      if (evidence.subject.source_kind !== PROOF_THREAD_SOURCE_KIND_FORCED)
        throw new Error(
          "spendInputSignerMissing evidence source kind is invalid",
        );
      const result = await submitSpendInputSignerMissingStep01Forced({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step01 thread out-ref"),
        evidence,
        forcedSource: {
          header: required(stage.forcedHeader, "forced header"),
          membership: required(stage.forcedMembership, "forced membership"),
          direction: required(stage.forcedDirection, "forced direction"),
        },
        referenceScriptUtxo: config.referenceScripts.step01,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: "step02" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep02") {
      const auxiliaryHashes: string[] = [];
      const result = await submitSpendInputSignerMissingStep02({
        lucid: config.lucid,
        network: config.binding.network,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step02 thread out-ref"),
        evidence,
        nativeTxCompactCbor: required(
          stage.nativeTxCompactCbor,
          "native transaction compact CBOR",
        ),
        witnessSetCompactCbor: required(
          stage.witnessSetCompactCbor,
          "witness-set compact CBOR",
        ),
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        membershipReferenceScriptUtxo:
          config.referenceScripts.witnesses.phasMembershipWithdraw,
        publicationBoundary: centralJournal?.auxiliaryBoundary(
          "publication",
          familyIdentity,
          "step02",
          auxiliaryHashes,
        ),
        certificateBoundary: centralJournal?.auxiliaryBoundary(
          "certificate",
          familyIdentity,
          "step02",
          auxiliaryHashes,
        ),
        onCarriageReady:
          centralJournal === undefined
            ? undefined
            : async () => {
                for (const txHash of auxiliaryHashes)
                  await centralJournal.confirmAuxiliary(txHash);
              },
        referenceScriptUtxo: config.referenceScripts.step02,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: "step03" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep03") {
      const auxiliaryHashes: string[] = [];
      const result = await submitSpendInputSignerMissingStep03({
        lucid: config.lucid,
        network: config.binding.network,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step03 thread out-ref"),
        evidence,
        nativeTxCompactCbor: required(
          stage.nativeTxCompactCbor,
          "native transaction compact CBOR",
        ),
        witnessSetCompactCbor: required(
          stage.witnessSetCompactCbor,
          "witness-set compact CBOR",
        ),
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        publicationBoundary: centralJournal?.auxiliaryBoundary(
          "publication",
          familyIdentity,
          "step03",
          auxiliaryHashes,
        ),
        certificateBoundary: centralJournal?.auxiliaryBoundary(
          "certificate",
          familyIdentity,
          "step03",
          auxiliaryHashes,
        ),
        onCarriageReady:
          centralJournal === undefined
            ? undefined
            : async () => {
                for (const txHash of auxiliaryHashes)
                  await centralJournal.confirmAuxiliary(txHash);
              },
        referenceScriptUtxo: config.referenceScripts.step03,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: "scanning" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitScan") {
      const auxiliaryHashes: string[] = [];
      const result = await submitSpendInputSignerMissingStep04({
        lucid: config.lucid,
        network: config.binding.network,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(
          stage.threadOutRef,
          "reconstruction thread out-ref",
        ),
        evidence,
        nativeTxCompactCbor: required(
          stage.nativeTxCompactCbor,
          "native transaction compact CBOR",
        ),
        witnessSetCompactCbor: required(
          stage.witnessSetCompactCbor,
          "witness-set compact CBOR",
        ),
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        publicationBoundary: centralJournal?.auxiliaryBoundary(
          "publication",
          familyIdentity,
          "scanning",
          auxiliaryHashes,
        ),
        certificateBoundary: centralJournal?.auxiliaryBoundary(
          "certificate",
          familyIdentity,
          "scanning",
          auxiliaryHashes,
        ),
        onCarriageReady:
          centralJournal === undefined
            ? undefined
            : async () => {
                for (const txHash of auxiliaryHashes)
                  await centralJournal.confirmAuxiliary(txHash);
              },
        referenceScriptUtxo: config.referenceScripts.step04,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: result.stage,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep05") {
      const result = await submitSpendInputSignerMissingStep05({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step05 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step05,
        witnessReferenceScripts: config.referenceScripts.witnesses,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: "proven" as const,
        txHash: result.txHash,
        outputReference: null,
      };
    }
    const result = await submitRemoveFraudulentBlock({
      lucid: config.lucid,
      blueprint: config.binding.blueprint,
      deploymentInfo: config.binding.deploymentInfo,
      network: config.binding.network,
      signer: config.signer,
      fraudCategory:
        "spendInputSignerMissing" as FraudProofCatalogueCategoryName,
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "spendInputSignerMissing production removal requires a state-queue mutation lease coordinator",
          );
        })(),
      awaitConfirmation: true,
      validFrom: stage.validFrom,
      validTo: stage.validTo,
      preSubmitBoundary: centralJournal?.boundary(
        action,
        familyIdentity,
        transition[0],
        transition[1],
      ),
    });
    return {
      stage: "removed" as const,
      txHash: result.txHash,
      outputReference: null,
    };
  },
  cancel: async (
    current: "step01" | "step02" | "step03" | "scanning" | "step05",
    evidence: SpendInputSignerMissingEvidence,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index =
      current === "step01"
        ? 0
        : current === "step02"
          ? 1
          : current === "step03"
            ? 2
            : current === "scanning"
              ? 3
              : 4;
    const result = await submitSpendInputSignerMissingCancel({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      signer: config.signer,
      threadOutRef: required(stage.threadOutRef, "cancel thread out-ref"),
      referenceScriptUtxo: [
        config.referenceScripts.step01,
        config.referenceScripts.step02,
        config.referenceScripts.step03,
        config.referenceScripts.step04,
        config.referenceScripts.step05,
      ][index]!,
      witnessReferenceScripts: config.referenceScripts.witnesses,
    });
    return {
      stage: "cancelled" as const,
      txHash: result.txHash,
      outputReference: null,
    };
  },
});

const createManifestBoundSpendInputSignerMissingRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundSpendInputSignerMissingConfig;
  readonly journal: SpendInputSignerJournal;
  readonly observe: SpendInputSignerMissingRuntimeLoader["observe"];
  readonly resolveStage: SpendInputSignerMissingRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createSpendInputSignerMissingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundSpendInputSignerMissingSubmission({
    config,
    observe: async (identity) => {
      const observed = await observe(identity);
      await centralJournal?.reconcile(observed);
      return observed;
    },
    resolveStage,
    centralJournal,
    stateQueueMutationLeaseCoordinator,
  });
  return Object.freeze({
    runtimeVersion: SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
    config,
    runOrResume: async (evidence: SpendInputSignerMissingEvidence) => {
      const identity = spendInputSignerWorkflowEvidenceIdentity(evidence);
      for (;;) {
        const stage = await submission.observe(identity);
        const action = nextSpendInputSignerAction(stage);
        if (action === "done") return stage;
        if (action === "cancel")
          throw new Error(
            "spendInputSignerMissing automatic runner cannot synthesize cancellation",
          );
        const result = await submission.submit(action, evidence);
        await journal.append({
          sequence: (await journal.load(identity)).length,
          identity,
          sourceStage: stage,
          targetStage: result.stage,
          action,
          phase: "submitted",
          txHash: result.txHash,
        });
      }
    },
  });
};

export type ManifestBoundSpendInputSignerMissingWorkflowConfig =
  LoadManifestBoundSpendInputSignerMissingConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
      historicalCheckpointStore: HistoricalNativeScriptCheckpointStore;
      historicalSource: HistoricalNativeScriptHistorySource;
    }>;

export type ManifestBoundSpendInputSignerMissingWorkflow = Readonly<{
  workflowVersion: typeof SPEND_INPUT_SIGNER_MISSING_WORKFLOW;
  config: ManifestBoundSpendInputSignerMissingConfig;
  binding: SpendInputSignerMissingDeploymentBinding;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
  historicalCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalSource: HistoricalNativeScriptHistorySource;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundSpendInputSignerMissingWorkflow = async (
  input: ManifestBoundSpendInputSignerMissingWorkflowConfig,
): Promise<ManifestBoundSpendInputSignerMissingWorkflow> => {
  const config = await loadManifestBoundSpendInputSignerMissingConfig(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition as never,
  });
  return Object.freeze({
    workflowVersion: SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
    historicalCheckpointStore: input.historicalCheckpointStore,
    historicalSource: input.historicalSource,
  });
};

const spendInputSignerStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>["observe"]
    >
  >["stage"],
): SpendInputSignerStage => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      if (stage.step === 4) return "scanning";
      if (stage.step === 5) return "step05";
      throw new Error(
        "spendInputSignerMissing L1 stage exceeds five-step topology",
      );
    case "proof_token":
      return "proven";
    case "removed":
      return "removed";
  }
};

/**
 * Watcher-facing runner. Evidence is always reconstructed from authenticated
 * L1 plus public retained DA; unknown/caller-authored evidence fields fail.
 */
export const runOrResumeManifestBoundSpendInputSignerMissingWorkflow =
  async (input: {
    readonly workflow: ManifestBoundSpendInputSignerMissingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: SpendInputSignerJournal;
  }): Promise<SpendInputSignerStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "spendInputSignerMissing runner rejects caller-authored evidence inputs",
      );
    }
    const headerHash = input.workflow.binding.definition.headerHash;
    const observation = await input.workflow.l1.observeHeader({ headerHash });
    const canonical = await fetchCanonicalBlockEvidence({
      observation,
      sources: input.sources,
    });
    const corpus = await resolveHistoricalNativeScriptCorpus({
      deploymentFingerprint: input.workflow.binding.deploymentFingerprint,
      checkpointStore: input.workflow.historicalCheckpointStore,
      historySource: input.workflow.historicalSource,
      currentEvidence: canonical,
      sources: input.sources,
    });
    const priorLedger =
      await deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
        block: canonical,
        corpus,
      });
    const findings = detectSpendInputSignerMissingCompleteReplay({
      block: canonical,
      priorLedger,
    });
    if (findings.length !== 1)
      throw new Error(
        `spendInputSignerMissing public replay yielded ${findings.length.toString()} exact findings`,
      );
    const evidence = findings[0]!;
    const source = await deriveSpendInputSignerMissingAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundSpendInputSignerMissingRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        spendInputSignerStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createSpendInputSignerMissingRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const executeManifestBoundSpendInputSignerMissingWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundSpendInputSignerMissingWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<SpendInputSignerStage> => {
  const headerHash = workflow.binding.definition.headerHash;
  const canonical = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const corpus = await resolveHistoricalNativeScriptCorpus({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    checkpointStore: workflow.historicalCheckpointStore,
    historySource: workflow.historicalSource,
    currentEvidence: canonical,
    sources,
  });
  const priorLedger =
    await deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
      block: canonical,
      corpus,
    });
  const findings = detectSpendInputSignerMissingCompleteReplay({
    block: canonical,
    priorLedger,
  });
  if (findings.length !== 1)
    throw new Error(
      `spendInputSignerMissing public replay yielded ${findings.length.toString()} exact findings`,
    );
  const evidence = findings[0]!;
  const source = await deriveSpendInputSignerMissingAuthenticatedSource({
    block: canonical,
    evidence,
  });
  const centralJournal = createSpendInputSignerMissingCentralJournalAdapter({
    store: journal,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    headerHash,
    decisionDigest: workflow.decisionDigest,
    transactionConfirmed: async (txHash) =>
      await workflow.l1.transactionConfirmed({ headerHash, txHash }),
  });
  const runtime = createManifestBoundSpendInputSignerMissingRuntime({
    config: workflow.config,
    journal: centralJournal.familyJournal,
    observe: async () =>
      spendInputSignerStageFromL1(
        (await workflow.l1.observe({ headerHash })).stage,
      ),
    resolveStage: createSpendInputSignerMissingRawL1StageResolver({
      config: workflow.config,
      l1: workflow.l1,
      source,
    }),
    centralJournal,
    stateQueueMutationLeaseCoordinator:
      workflow.stateQueueMutationLeaseCoordinator,
  });
  return await runtime.runOrResume(evidence);
};

export type LoadedSpendInputSignerMissingWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundSpendInputSignerMissingWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadSpendInputSignerMissingWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedSpendInputSignerMissingWorkflow>;

/**
 * Family-local runner surface for central admission. It consumes only a
 * manifest/runtime path and concrete public-DA transports; neither evidence
 * nor a watcher-owned journal implementation can enter this boundary.
 */
export const createSpendInputSignerMissingWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadSpendInputSignerMissingWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "spendInputSignerMissing") {
        throw new Error(
          `spendInputSignerMissing production runner category mismatch: ${invocation.category}`,
        );
      }
      const journal = bindWorkflowFundingReservationJournal({
        permit: invocation.fundingReservationPermit,
        journal: bindWorkflowActuationJournal({
          journal: new DirectoryFraudProofWorkflowJournalStore(
            invocation.journalDirectory,
          ),
          permit: invocation.actuationPermit,
          decisionDigest: invocation.decisionDigest,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category:
            "spendInputSignerMissing" as FraudProofCatalogueCategoryName,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category: "spendInputSignerMissing" as FraudProofCatalogueCategoryName,
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      if (typeof loaded.close !== "function") {
        throw new Error(
          "spendInputSignerMissing runtime omitted its transport disposer",
        );
      }
      try {
        if (
          loaded.schemaVersion !==
          "midgard-production-fraud-proof-runtime-config-v1"
        ) {
          throw new Error(
            "spendInputSignerMissing runtime config has an unsupported schema",
          );
        }
        if (
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        ) {
          throw new Error(
            "spendInputSignerMissing production runner requires concrete public retained-DA sources",
          );
        }
        const workflow =
          await createManifestBoundSpendInputSignerMissingWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          String(workflow.binding.definition.category) !==
            "spendInputSignerMissing" ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        ) {
          throw new Error(
            "spendInputSignerMissing manifest-bound workflow identity differs from invocation",
          );
        }
        return await executeManifestBoundSpendInputSignerMissingWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
