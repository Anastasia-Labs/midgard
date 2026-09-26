import { deriveMidgardNativeTxFaultEvidenceMaterial } from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  type ForcedInclusionTxV1,
  type FraudProofCatalogueCategoryName,
  FraudProofComputationThreadStepDatum,
  type Header,
  type OutputReference,
  OutputReferenceSchema,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import {
  type CanonicalBlockEvidence,
  fetchCanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  parseSubmitStep01TxInclusion,
  type SubmitStep01TxInclusion,
} from "../step-support.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  workflowActuationDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "../workflow/actuation-permit.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { createMintItemNonCanonicalCentralJournalAdapter } from "./central-journal.js";
import type { MintItemNonCanonicalContracts } from "./contracts.js";
import {
  type MintItemEvidence,
  mintItemEvidenceIdentity,
  type MintItemJournal,
  type MintItemStage,
  runMintItemProof,
} from "./mint-item-non-canonical.js";
import { findMintItemNonCanonicalBlockEvidence } from "./replay.js";
import {
  MintItemStep02DatumSchema,
  MintItemStep03DatumSchema,
  MintItemStep04DatumSchema,
} from "./schemas.js";
import { submitMintItemNonCanonicalCancel } from "./submit-cancel.js";
import { submitMintItemNonCanonicalStep01Accepted } from "./submit-step-01-accepted.js";
import { submitMintItemNonCanonicalStep01Forced } from "./submit-step-01-forced.js";
import { submitMintItemNonCanonicalStep02 } from "./submit-step-02.js";
import { submitMintItemNonCanonicalStep03 } from "./submit-step-03.js";
import { submitMintItemNonCanonicalStep04 } from "./submit-step-04.js";

export const MINT_ITEM_NON_CANONICAL_WORKFLOW =
  "midgard-mint-item-non-canonical-production-workflow-v1" as const;
export const MINT_ITEM_NON_CANONICAL_VIOLATION_ID =
  "mint-item-non-canonical" as const;

export const MINT_ITEM_NON_CANONICAL_MANIFEST_CONTRACTS = Object.freeze({
  step01: "fraudProofMintItemNonCanonical",
  step02: "fraudProofMintItemNonCanonicalStep02",
  step03: "fraudProofMintItemNonCanonicalStep03",
  step04: "fraudProofMintItemNonCanonicalStep04",
  computationThreadMint: "computationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type MintItemNonCanonicalReferenceScripts = Readonly<{
  step01: UTxO;
  step02: UTxO;
  step03: UTxO;
  step04: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundMintItemNonCanonicalConfig = Readonly<{
  schemaVersion: typeof MINT_ITEM_NON_CANONICAL_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBinding<"mintItemNonCanonical">;
  contracts: MintItemNonCanonicalContracts;
  referenceScripts: MintItemNonCanonicalReferenceScripts;
}>;

export type LoadManifestBoundMintItemNonCanonicalConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: MintItemNonCanonicalReferenceScripts;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"mintItemNonCanonical">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });

export const bindMintItemNonCanonicalReferenceScripts = ({
  binding,
  referenceScripts,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"mintItemNonCanonical">;
  readonly referenceScripts: MintItemNonCanonicalReferenceScripts;
}): MintItemNonCanonicalReferenceScripts => {
  const names = MINT_ITEM_NON_CANONICAL_MANIFEST_CONTRACTS;
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

export const loadManifestBoundMintItemNonCanonicalConfig = async (
  input: LoadManifestBoundMintItemNonCanonicalConfig,
): Promise<ManifestBoundMintItemNonCanonicalConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "mintItemNonCanonical",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      MintItemStep02DatumSchema,
      MintItemStep03DatumSchema,
      MintItemStep04DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
  const localContracts = binding.resolvedContracts.contracts as unknown as {
    readonly mintItemNonCanonical?: MintItemNonCanonicalContracts;
  };
  const chain = localContracts.mintItemNonCanonical;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 4) {
    throw new Error(
      "mintItemNonCanonical deployment changed its four-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "mintItemNonCanonical deployment omitted field-preimage certificate",
    );
  }
  const referenceScripts = bindMintItemNonCanonicalReferenceScripts({
    binding,
    referenceScripts: input.referenceScripts,
  });
  return Object.freeze({
    schemaVersion: MINT_ITEM_NON_CANONICAL_WORKFLOW,
    lucid: input.lucid,
    signer: input.signer,
    binding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/mint_item_non_canonical/step_01.main.spend",
          "fraud_proofs/mint_item_non_canonical/step_02.main.spend",
          "fraud_proofs/mint_item_non_canonical/step_03.main.spend",
          "fraud_proofs/mint_item_non_canonical/step_04.main.spend",
        ][index]!,
        referenceOutRef: [
          referenceScripts.step01,
          referenceScripts.step02,
          referenceScripts.step03,
          referenceScripts.step04,
        ][index]!.txHash.concat(
          "#",
          [
            referenceScripts.step01,
            referenceScripts.step02,
            referenceScripts.step03,
            referenceScripts.step04,
          ][index]!.outputIndex.toString(),
        ),
      })) as unknown as MintItemNonCanonicalContracts["steps"],
      computationThread: binding.resolvedContracts.contracts.computationThread,
      fraudProof: binding.resolvedContracts.contracts.fraudProof,
      hubOraclePolicyId: binding.contractEntries.hubOracleMint!.scriptHash,
      stateQueuePolicyId: binding.definition.stateQueue.policyId,
      fieldPreimageCertificatePolicyId: certificate.policyId,
      fieldPreimageCertificateMintingScript: certificate.mintingScript,
    },
    referenceScripts,
  });
};

export type MintItemNonCanonicalStage = Readonly<{
  fraudulentBlockOutRef: string;
  nextRemovalOutRef?: string;
  fraudProofOutRef?: string;
  threadOutRef?: string;
  threadUtxo?: UTxO;
  threadToken?: Readonly<{ unit: string; fraudulentHeaderHash: string }>;
  stateQueueBlockOutRef?: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
  nativeTxCompactCbor?: string;
  witnessSetCompactCbor?: string;
  publishedCarriageUtxos?: readonly UTxO[];
  certificateUtxo?: UTxO;
  validFrom?: bigint;
  validTo?: bigint;
}>;

/** Derives the only admissible family evidence from L1-bound public retained DA. */
export const deriveMintItemNonCanonicalEvidenceFromCanonicalBlock = (
  block: CanonicalBlockEvidence,
): MintItemEvidence => {
  const findings = findMintItemNonCanonicalBlockEvidence(block);
  if (findings.length === 0)
    throw new Error("mintItemNonCanonical retained DA contains no finding");
  return findings[0]!.evidence;
};

export type MintItemNonCanonicalAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveMintItemNonCanonicalAuthenticatedSource = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly evidence: MintItemEvidence;
}): Promise<MintItemNonCanonicalAuthenticatedSource> => {
  if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "mintItemNonCanonical accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "mintItemNonCanonical accepted source trie differs from authenticated reconstruction",
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
          "mintItemNonCanonical accepted transaction",
        ),
      }),
    });
  }
  const forced = block.reconstruction.forcedTransactions.find(
    ({ key, value }) =>
      value.tx_id === evidence.subject.transaction_id &&
      Data.to(key as never, OutputReferenceSchema as never) ===
        evidence.subject.source_key,
  );
  if (forced === undefined || forced.value.verdict !== "ForcedTxValid") {
    throw new Error(
      "mintItemNonCanonical forced subject disappeared from retained DA",
    );
  }
  if (evidence.subject.rejection_reason !== null)
    throw new Error(
      "mintItemNonCanonical does not adjudicate rejected transactions",
    );
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    forced.fullTransactionCbor,
  );
  if (
    material.proofSource.compactCbor.toString("hex") !==
      forced.value.submitted_source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      forced.value.submitted_source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.submitted_source.field_preimage_lengths_cbor
  ) {
    throw new Error(
      "mintItemNonCanonical forced source material differs from authenticated leaf",
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
    forcedDirection: 0n,
  });
};

export type MintItemNonCanonicalRuntimeLoader = Readonly<{
  config: LoadManifestBoundMintItemNonCanonicalConfig;
  journal: MintItemJournal;
  observe: (identity: string) => Promise<MintItemStage>;
  resolveStage: (input: {
    readonly action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "submitStep04"
      | "removeDescendants"
      | "cancel";
    readonly evidence: MintItemEvidence;
  }) => Promise<MintItemNonCanonicalStage>;
}>;

export const createMintItemNonCanonicalRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundMintItemNonCanonicalConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
    readonly source: MintItemNonCanonicalAuthenticatedSource;
  }): MintItemNonCanonicalRuntimeLoader["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "mintItemNonCanonical init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "mintItemNonCanonical removal requires raw-L1 proof token",
        );
      }
      return {
        fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
        nextRemovalOutRef: stage.nextRemovalOutRef,
        fraudProofOutRef: stage.fraudProofOutRef,
      };
    }
    const expectedStep =
      action === "submitStep01"
        ? 1
        : action === "submitStep02"
          ? 2
          : action === "submitStep03"
            ? 3
            : 4;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `mintItemNonCanonical ${action} differs from authenticated raw-L1 stage`,
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
      family: "mint-item-non-canonical",
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
    throw new Error(`mintItemNonCanonical missing ${label}`);
  return value;
};

export const createManifestBoundMintItemNonCanonicalSubmission = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundMintItemNonCanonicalConfig;
  readonly observe: (identity: string) => Promise<MintItemStage>;
  readonly resolveStage: MintItemNonCanonicalRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createMintItemNonCanonicalCentralJournalAdapter
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
      | "submitStep04"
      | "removeDescendants",
    evidence: MintItemEvidence,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "mintItemNonCanonical evidence transaction id is not canonical",
      );
    const familyIdentity = mintItemEvidenceIdentity(evidence);
    let transition: readonly [MintItemStage, MintItemStage] =
      action === "submitInit"
        ? (["none", "step01"] as const)
        : action === "submitStep01"
          ? (["step01", "step02"] as const)
          : action === "submitStep02"
            ? (["step02", "step03"] as const)
            : action === "submitStep03"
              ? (["step03", "step04"] as const)
              : action === "submitStep04"
                ? (["step04", "proven"] as const)
                : (["proven", "removed"] as const);
    if (
      action !== "submitStep02" &&
      action !== "submitStep03" &&
      action !== "removeDescendants"
    )
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
        const result = await submitMintItemNonCanonicalStep01Accepted({
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          network: config.binding.network,
          contracts: config.contracts,
          signer: config.signer,
          finding: evidence,
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
        throw new Error("mintItemNonCanonical evidence source kind is invalid");
      const result = await submitMintItemNonCanonicalStep01Forced({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step01 thread out-ref"),
        finding: evidence,
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
      const result = await submitMintItemNonCanonicalStep02({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step02 thread out-ref"),
        evidence,
        onTransitionReady: async (terminal) => {
          transition = ["step02", terminal ? "step03" : "step02"];
          await centralJournal?.begin(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          );
        },
        nativeTxCompactCbor: required(
          stage.nativeTxCompactCbor,
          "native transaction compact CBOR",
        ),
        witnessSetCompactCbor: required(
          stage.witnessSetCompactCbor,
          "witness-set compact CBOR",
        ),
        publishCarriage: evidence.carriage === "RawUtxo",
        publishedCarriageUtxos: stage.publishedCarriageUtxos,
        certificateUtxo: stage.certificateUtxo,
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        publicationPreSubmitBoundary: centralJournal?.auxiliaryBoundary(
          "publication",
          familyIdentity,
          "step02",
          auxiliaryHashes,
        ),
        certificatePreSubmitBoundary: centralJournal?.auxiliaryBoundary(
          "certificate",
          familyIdentity,
          "step02",
          auxiliaryHashes,
        ),
        onCarriageReady:
          centralJournal === undefined
            ? undefined
            : async () => {
                for (const txHash of auxiliaryHashes) {
                  await centralJournal.confirmAuxiliary(txHash);
                }
              },
        referenceScriptUtxo: config.referenceScripts.step02,
        preSubmitBoundary:
          centralJournal === undefined
            ? undefined
            : (transaction) =>
                centralJournal.boundary(
                  action,
                  familyIdentity,
                  transition[0],
                  transition[1],
                )(transaction),
      });
      return {
        stage: result.terminal ? ("step03" as const) : ("step02" as const),
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep03") {
      const auxiliaryHashes: string[] = [];
      const result = await submitMintItemNonCanonicalStep03({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step03 thread out-ref"),
        evidence,
        onTransitionReady: async (terminal) => {
          transition = ["step03", terminal ? "step04" : "step03"];
          await centralJournal?.begin(
            action,
            familyIdentity,
            transition[0],
            transition[1],
          );
        },
        nativeTxCompactCbor: required(
          stage.nativeTxCompactCbor,
          "native transaction compact CBOR",
        ),
        witnessSetCompactCbor: required(
          stage.witnessSetCompactCbor,
          "witness-set compact CBOR",
        ),
        publishCarriage: evidence.carriage === "RawUtxo",
        publishedCarriageUtxos: stage.publishedCarriageUtxos,
        certificateUtxo: stage.certificateUtxo,
        certificateReferenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
        publicationPreSubmitBoundary: centralJournal?.auxiliaryBoundary(
          "publication",
          familyIdentity,
          "step03",
          auxiliaryHashes,
        ),
        certificatePreSubmitBoundary: centralJournal?.auxiliaryBoundary(
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
        preSubmitBoundary:
          centralJournal === undefined
            ? undefined
            : (transaction) =>
                centralJournal.boundary(
                  action,
                  familyIdentity,
                  transition[0],
                  transition[1],
                )(transaction),
      });
      return {
        stage: result.terminal ? ("step04" as const) : ("step03" as const),
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep04") {
      const result = await submitMintItemNonCanonicalStep04({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step04 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step04,
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
    const removalIdentity = (resolved: MintItemNonCanonicalStage) => ({
      nextRemovalOutRef: required(
        resolved.nextRemovalOutRef,
        "authenticated next removal out-ref",
      ),
      fraudProofOutRef: required(
        resolved.fraudProofOutRef,
        "authenticated fraud proof out-ref",
      ),
    });
    const removalTargetStage = (
      resolved: MintItemNonCanonicalStage,
    ): MintItemStage =>
      resolved.nextRemovalOutRef === resolved.fraudulentBlockOutRef
        ? "removed"
        : "proven";
    await centralJournal?.begin(
      action,
      familyIdentity,
      "proven",
      removalTargetStage(stage),
      removalIdentity(stage),
    );
    let removalsPrepared = 0;
    const result = await submitRemoveFraudulentBlock({
      lucid: config.lucid,
      blueprint: config.binding.blueprint,
      deploymentInfo: config.binding.deploymentInfo,
      network: config.binding.network,
      signer: config.signer,
      fraudCategory: "mintItemNonCanonical" as FraudProofCatalogueCategoryName,
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "mintItemNonCanonical production removal requires a state-queue mutation lease coordinator",
          );
        })(),
      awaitConfirmation: true,
      validFrom: stage.validFrom,
      validTo: stage.validTo,
      preSubmitBoundary:
        centralJournal === undefined
          ? undefined
          : async (transaction) => {
              // The removal builder may consume several descendants. Each physical
              // transaction gets its own authenticated action and durable intent.
              const current =
                removalsPrepared === 0
                  ? stage
                  : await resolveStage({ action, evidence });
              if (removalsPrepared > 0)
                await centralJournal.reconcile("proven");
              await centralJournal.boundary(
                action,
                familyIdentity,
                "proven",
                removalTargetStage(current),
                removalIdentity(current),
              )(transaction);
              removalsPrepared += 1;
            },
    });
    return {
      stage: "removed" as const,
      txHash: result.txHash,
      outputReference: null,
    };
  },
  cancel: async (
    current: "step01" | "step02" | "step03" | "step04",
    evidence: MintItemEvidence,
  ) => {
    const stage = await resolveStage({ action: "cancel", evidence });
    const index =
      current === "step01"
        ? 0
        : current === "step02"
          ? 1
          : current === "step03"
            ? 2
            : 3;
    const result = await submitMintItemNonCanonicalCancel({
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

export const loadMintItemNonCanonicalRuntime = async (
  input: MintItemNonCanonicalRuntimeLoader,
) => {
  const config = await loadManifestBoundMintItemNonCanonicalConfig(
    input.config,
  );
  return createManifestBoundMintItemNonCanonicalRuntime({
    config,
    journal: input.journal,
    observe: input.observe,
    resolveStage: input.resolveStage,
  });
};

export const createManifestBoundMintItemNonCanonicalRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundMintItemNonCanonicalConfig;
  readonly journal: MintItemJournal;
  readonly observe: MintItemNonCanonicalRuntimeLoader["observe"];
  readonly resolveStage: MintItemNonCanonicalRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createMintItemNonCanonicalCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundMintItemNonCanonicalSubmission({
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
    runtimeVersion: MINT_ITEM_NON_CANONICAL_WORKFLOW,
    config,
    runOrResume: async (evidence: MintItemEvidence) =>
      await runMintItemProof({
        evidence,
        journal,
        submission,
      }),
  });
};

export type ManifestBoundMintItemNonCanonicalWorkflowConfig =
  LoadManifestBoundMintItemNonCanonicalConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundMintItemNonCanonicalWorkflow = Readonly<{
  workflowVersion: typeof MINT_ITEM_NON_CANONICAL_WORKFLOW;
  config: ManifestBoundMintItemNonCanonicalConfig;
  binding: FraudProofWorkflowDeploymentBinding<"mintItemNonCanonical">;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundMintItemNonCanonicalWorkflow = async (
  input: ManifestBoundMintItemNonCanonicalWorkflowConfig,
): Promise<ManifestBoundMintItemNonCanonicalWorkflow> => {
  const config = await loadManifestBoundMintItemNonCanonicalConfig(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition,
  });
  return Object.freeze({
    workflowVersion: MINT_ITEM_NON_CANONICAL_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

const mintItemStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>["observe"]
    >
  >["stage"],
): MintItemStage => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      if (stage.step === 4) return "step04";
      throw new Error(
        "mintItemNonCanonical L1 stage exceeds four-step topology",
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
export const runOrResumeManifestBoundMintItemNonCanonicalWorkflow =
  async (input: {
    readonly workflow: ManifestBoundMintItemNonCanonicalWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: MintItemJournal;
  }): Promise<MintItemStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "mintItemNonCanonical runner rejects caller-authored evidence inputs",
      );
    }
    const headerHash = input.workflow.binding.definition.headerHash;
    const observation = await observeFraudProofWorkflowHeader(
      input.workflow.l1,
      { headerHash },
    );
    const canonical = await fetchCanonicalBlockEvidence({
      observation,
      sources: input.sources,
    });
    const evidence =
      deriveMintItemNonCanonicalEvidenceFromCanonicalBlock(canonical);
    const source = await deriveMintItemNonCanonicalAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundMintItemNonCanonicalRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        mintItemStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createMintItemNonCanonicalRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const executeManifestBoundMintItemNonCanonicalWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMintItemNonCanonicalWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) => {
  const headerHash = workflow.binding.definition.headerHash;
  const centralJournal = createMintItemNonCanonicalCentralJournalAdapter({
    store: journal,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    headerHash,
    // Fresh observation authority retains the original durable execution.
    decisionDigest:
      workflowActuationDecisionDigest(journal) ?? workflow.decisionDigest,
    transactionConfirmed: async (txHash) =>
      await workflow.l1.transactionConfirmed({ headerHash, txHash }),
  });
  const finish = async (
    terminal: Parameters<typeof centralJournal.finish>[0]["candidate"],
  ) => {
    await centralJournal.reconcile("removed");
    return await centralJournal.finish({
      candidate: terminal,
      releaseFinality: workflow.binding.releaseFinality,
      verifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(
        workflow.l1,
      ),
    });
  };
  const observed = await workflow.l1.observe({ headerHash });
  if (observed.stage.kind === "removed")
    return await finish(observed.stage.terminal);
  if (workflowJournalIsReconciliationOnly(journal))
    return {
      kind: "pending" as const,
      resumeOnObservation: true,
      reason: "existing mint-item correction is not yet removed",
    };
  const canonical = await fetchCanonicalBlockEvidence({
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash,
    }),
    sources,
  });
  const evidence =
    deriveMintItemNonCanonicalEvidenceFromCanonicalBlock(canonical);
  const source = await deriveMintItemNonCanonicalAuthenticatedSource({
    block: canonical,
    evidence,
  });
  const runtime = createManifestBoundMintItemNonCanonicalRuntime({
    config: workflow.config,
    journal: centralJournal.familyJournal,
    observe: async () =>
      mintItemStageFromL1((await workflow.l1.observe({ headerHash })).stage),
    resolveStage: createMintItemNonCanonicalRawL1StageResolver({
      config: workflow.config,
      l1: workflow.l1,
      source,
    }),
    centralJournal,
    stateQueueMutationLeaseCoordinator:
      workflow.stateQueueMutationLeaseCoordinator,
  });
  const result = await runtime.runOrResume(evidence);
  if (result !== "removed") return result;
  const removed = await workflow.l1.observe({ headerHash });
  if (removed.stage.kind !== "removed")
    throw new Error(
      "mintItemNonCanonical removal changed before terminal verification",
    );
  return await finish(removed.stage.terminal);
};
