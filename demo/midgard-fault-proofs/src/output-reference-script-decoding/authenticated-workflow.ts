import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
  MidgardNativeScriptDecodingDirections,
} from "@al-ft/midgard-core";
import {
  type ForcedInclusionTxV1,
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
} from "../evidence/canonical-block-evidence.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import {
  buildNativeScriptDecodingScanPlan,
  NativeScriptDecodingPlanRoutes,
} from "../native-script-decoding/scan-plan.js";
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
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { createOutputReferenceScriptDecodingCentralJournalAdapter } from "./central-journal.js";
import type { OutputReferenceScriptDecodingContracts } from "./contracts.js";
import {
  detectOutputReferenceScriptDecodingCompleteReplay,
  outputReferenceScriptControlData,
  type OutputReferenceScriptDecodingEvidence,
  OutputReferenceScriptResultClasses,
} from "./output-reference-script-decoding.js";
import {
  OutputReferenceOutputControlSchema,
  OutputReferenceStep02DatumSchema,
  OutputReferenceStep03DatumSchema,
  OutputReferenceStep04DatumSchema,
  OutputReferenceStep05DatumSchema,
  OutputReferenceStep06DatumSchema,
} from "./schemas.js";
import { submitOutputReferenceScriptDecodingCancel } from "./submit-cancel.js";
import { submitOutputReferenceScriptDecodingStep01Accepted } from "./submit-step-01-accepted.js";
import { submitOutputReferenceScriptDecodingStep01Forced } from "./submit-step-01-forced.js";
import { submitOutputReferenceScriptDecodingStep02 } from "./submit-step-02.js";
import { submitOutputReferenceScriptDecodingStep03 } from "./submit-step-03.js";
import { submitOutputReferenceScriptDecodingStep04 } from "./submit-step-04.js";
import { submitOutputReferenceScriptDecodingStep05 } from "./submit-step-05.js";
import { submitOutputReferenceScriptDecodingStep06 } from "./submit-step-06.js";
import {
  nextOutputReferenceScriptDecodingAction,
  outputReferenceScriptDecodingEvidenceIdentity,
  type OutputReferenceScriptDecodingJournal,
  type OutputReferenceScriptDecodingStage,
} from "./workflow.js";

export const OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW =
  "midgard-output-reference-script-decoding-production-workflow-v1" as const;

export const OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS =
  Object.freeze({
    step01: "fraudProofOutputReferenceScriptDecoding",
    step02: "fraudProofOutputReferenceScriptDecodingStep02",
    step03: "fraudProofOutputReferenceScriptDecodingStep03",
    step04: "fraudProofOutputReferenceScriptDecodingStep04",
    step05: "fraudProofOutputReferenceScriptDecodingStep05",
    step06: "fraudProofOutputReferenceScriptDecodingStep06",
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
  } as const);

export type OutputReferenceScriptDecodingReferenceScripts = Readonly<{
  step01: UTxO;
  step02: UTxO;
  step03: UTxO;
  step04: UTxO;
  step05: UTxO;
  step06: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundOutputReferenceScriptDecodingConfig = Readonly<{
  schemaVersion: typeof OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: OutputReferenceScriptDecodingDeploymentBinding;
  contracts: OutputReferenceScriptDecodingContracts;
  referenceScripts: OutputReferenceScriptDecodingReferenceScripts;
}>;

export type OutputReferenceScriptDecodingDeploymentBinding = Omit<
  FraudProofWorkflowDeploymentBinding<FraudProofCatalogueCategoryName>,
  "definition"
> &
  Readonly<{
    definition: Omit<
      FraudProofWorkflowDeploymentBinding<FraudProofCatalogueCategoryName>["definition"],
      "category"
    > &
      Readonly<{ category: "outputReferenceScriptDecoding" }>;
  }>;

export type LoadManifestBoundOutputReferenceScriptDecodingConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: OutputReferenceScriptDecodingReferenceScripts;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: OutputReferenceScriptDecodingDeploymentBinding;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });

export const bindOutputReferenceScriptDecodingReferenceScripts = ({
  binding,
  referenceScripts,
}: {
  readonly binding: OutputReferenceScriptDecodingDeploymentBinding;
  readonly referenceScripts: OutputReferenceScriptDecodingReferenceScripts;
}): OutputReferenceScriptDecodingReferenceScripts => {
  const names = OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS;
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
    step06: bindReference({
      binding,
      contractName: names.step06,
      utxo: referenceScripts.step06,
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

export const loadManifestBoundOutputReferenceScriptDecodingConfig = async (
  input: LoadManifestBoundOutputReferenceScriptDecodingConfig,
): Promise<ManifestBoundOutputReferenceScriptDecodingConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category:
      "outputReferenceScriptDecoding" as FraudProofCatalogueCategoryName,
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      OutputReferenceStep02DatumSchema,
      OutputReferenceStep03DatumSchema,
      OutputReferenceStep04DatumSchema,
      OutputReferenceStep05DatumSchema,
      OutputReferenceStep06DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
  const localContracts = binding.resolvedContracts.contracts as unknown as {
    readonly outputReferenceScriptDecoding?: OutputReferenceScriptDecodingContracts;
  };
  const chain = localContracts.outputReferenceScriptDecoding;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 6) {
    throw new Error(
      "outputReferenceScriptDecoding deployment changed its six-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "outputReferenceScriptDecoding deployment omitted field-preimage certificate",
    );
  }
  const referenceScripts = bindOutputReferenceScriptDecodingReferenceScripts({
    binding:
      binding as unknown as OutputReferenceScriptDecodingDeploymentBinding,
    referenceScripts: input.referenceScripts,
  });
  return Object.freeze({
    schemaVersion: OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW,
    lucid: input.lucid,
    signer: input.signer,
    binding:
      binding as unknown as OutputReferenceScriptDecodingDeploymentBinding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/output_reference_script_decoding/step_01.main.spend",
          "fraud_proofs/output_reference_script_decoding/step_02.main.spend",
          "fraud_proofs/output_reference_script_decoding/step_03.main.spend",
          "fraud_proofs/output_reference_script_decoding/step_04.main.spend",
          "fraud_proofs/output_reference_script_decoding/step_05.main.spend",
          "fraud_proofs/output_reference_script_decoding/step_06.main.spend",
        ][index]!,
        referenceOutRef: [
          referenceScripts.step01,
          referenceScripts.step02,
          referenceScripts.step03,
          referenceScripts.step04,
          referenceScripts.step05,
          referenceScripts.step06,
        ][index]!.txHash.concat(
          "#",
          [
            referenceScripts.step01,
            referenceScripts.step02,
            referenceScripts.step03,
            referenceScripts.step04,
            referenceScripts.step05,
            referenceScripts.step06,
          ][index]!.outputIndex.toString(),
        ),
      })) as unknown as OutputReferenceScriptDecodingContracts["steps"],
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

export type OutputReferenceScriptDecodingAuthenticatedStage = Readonly<{
  fraudulentBlockOutRef: string;
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
export type OutputReferenceScriptDecodingAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

/** Rebuilds all accepted/forced submitter material from the authenticated block. */
export const deriveOutputReferenceScriptDecodingAuthenticatedSource = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly evidence: OutputReferenceScriptDecodingEvidence;
}): Promise<OutputReferenceScriptDecodingAuthenticatedSource> => {
  if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.subject.transaction_id,
    );
    if (selected === undefined) {
      throw new Error(
        "outputReferenceScriptDecoding accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "outputReferenceScriptDecoding accepted source trie differs from authenticated reconstruction",
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
          "outputReferenceScriptDecoding accepted transaction",
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
      "outputReferenceScriptDecoding forced subject disappeared from retained DA",
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
      "outputReferenceScriptDecoding forced reason differs from authenticated source",
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
      "outputReferenceScriptDecoding forced source material differs from authenticated leaf",
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
type OutputReferenceScriptDecodingRuntimeLoader = Readonly<{
  config: LoadManifestBoundOutputReferenceScriptDecodingConfig;
  journal: OutputReferenceScriptDecodingJournal;
  observe: (identity: string) => Promise<OutputReferenceScriptDecodingStage>;
  resolveStage: (input: {
    readonly action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitOutputScan"
      | "submitReferenceBind"
      | "submitStructuralScan"
      | "submitStep06"
      | "removeDescendants"
      | "cancel";
    readonly evidence: OutputReferenceScriptDecodingEvidence;
    readonly currentStage?: OutputReferenceScriptDecodingStage;
  }) => Promise<OutputReferenceScriptDecodingAuthenticatedStage>;
}>;

export const createOutputReferenceScriptDecodingRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundOutputReferenceScriptDecodingConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
    readonly source: OutputReferenceScriptDecodingAuthenticatedSource;
  }): OutputReferenceScriptDecodingRuntimeLoader["resolveStage"] =>
  async ({ action, evidence, currentStage }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "outputReferenceScriptDecoding init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "outputReferenceScriptDecoding removal requires raw-L1 proof token",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    const effectiveAction =
      action === "cancel"
        ? currentStage === "step01"
          ? "submitStep01"
          : currentStage === "step02"
            ? "submitStep02"
            : currentStage === "outputScan"
              ? "submitOutputScan"
              : currentStage === "referenceBind"
                ? "submitReferenceBind"
                : currentStage === "scan"
                  ? "submitStructuralScan"
                  : (() => {
                      throw new Error(
                        "outputReferenceScriptDecoding cancel stage is not authenticated",
                      );
                    })()
        : action;
    const expectedStep =
      effectiveAction === "submitStep01"
        ? 1
        : effectiveAction === "submitStep02"
          ? 2
          : effectiveAction === "submitOutputScan"
            ? 3
            : effectiveAction === "submitReferenceBind"
              ? 4
              : effectiveAction === "submitStructuralScan"
                ? 5
                : 6;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `outputReferenceScriptDecoding ${action} differs from authenticated raw-L1 stage`,
      );
    }
    const thread = await requireLinearFaultThreadUtxo({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      family: "output-reference-script-decoding",
      stepIndex: expectedStep - 1,
      threadOutRef: stage.threadOutRef,
    });
    const common = {
      fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
      threadOutRef: stage.threadOutRef,
      threadUtxo: thread.threadUtxo,
      threadToken: thread.threadToken,
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
    throw new Error(`outputReferenceScriptDecoding missing ${label}`);
  return value;
};

export const outputReferenceScriptDecodingOutputScanTarget = ({
  stage,
  evidence,
}: {
  readonly stage: OutputReferenceScriptDecodingAuthenticatedStage;
  readonly evidence: OutputReferenceScriptDecodingEvidence;
}): "outputScan" | "referenceBind" => {
  const threadUtxo = required(stage.threadUtxo, "output-scan thread UTxO");
  if (threadUtxo.datum == null)
    throw new Error(
      "outputReferenceScriptDecoding output-scan thread datum is absent",
    );
  const datum = Data.from(
    threadUtxo.datum,
    OutputReferenceStep03DatumSchema as never,
  ) as {
    data: { control: unknown };
  };
  const encoded = Data.to(
    datum.data.control as never,
    OutputReferenceOutputControlSchema as never,
  );
  return outputReferenceScriptDecodingNextOutputScanStage({
    evidence,
    controlCbor: encoded,
  });
};

export const outputReferenceScriptDecodingNextOutputScanStage = ({
  evidence,
  controlCbor,
}: {
  readonly evidence: OutputReferenceScriptDecodingEvidence;
  readonly controlCbor: string;
}): "outputScan" | "referenceBind" => {
  const index = evidence.outputScanControls.findIndex(
    (control) =>
      Data.to(
        outputReferenceScriptControlData(control) as never,
        OutputReferenceOutputControlSchema as never,
      ) === controlCbor,
  );
  if (index < 0 || evidence.outputScanControls[index + 1] === undefined)
    throw new Error(
      "outputReferenceScriptDecoding authenticated output checkpoint is outside the deterministic trace",
    );
  return index + 1 === evidence.outputScanControls.length - 1
    ? "referenceBind"
    : "outputScan";
};

export const outputReferenceScriptDecodingStructuralTarget = ({
  stage,
  evidence,
}: {
  readonly stage: OutputReferenceScriptDecodingAuthenticatedStage;
  readonly evidence: OutputReferenceScriptDecodingEvidence;
}): "scan" | "step06" => {
  const threadUtxo = required(stage.threadUtxo, "structural-scan thread UTxO");
  if (threadUtxo.datum == null)
    throw new Error(
      "outputReferenceScriptDecoding structural-scan datum is absent",
    );
  const datum = Data.from(
    threadUtxo.datum,
    OutputReferenceStep05DatumSchema as never,
  ) as { data: { control_cbor: string; result_class: bigint } };
  return outputReferenceScriptDecodingNextStructuralStage({
    evidence,
    controlCbor: datum.data.control_cbor,
    resultClass: datum.data.result_class,
  });
};

export const outputReferenceScriptDecodingNextStructuralStage = ({
  evidence,
  controlCbor,
  resultClass,
}: {
  readonly evidence: OutputReferenceScriptDecodingEvidence;
  readonly controlCbor: string;
  readonly resultClass: bigint;
}): "scan" | "step06" => {
  if (resultClass !== BigInt(OutputReferenceScriptResultClasses.Pending))
    return "step06";
  const plan = buildNativeScriptDecodingScanPlan({
    itemBytes: Buffer.from(evidence.referenceScriptItemHex, "hex"),
    direction: Number(evidence.subject.direction) as 0 | 1,
  });
  if (plan.route !== NativeScriptDecodingPlanRoutes.Machine)
    throw new Error(
      "outputReferenceScriptDecoding pending state has no structural plan",
    );
  const segment = plan.segments.find(
    ({ controlBefore }) => controlBefore.cborHex === controlCbor,
  );
  if (segment !== undefined) {
    const last = plan.segments.at(-1) === segment;
    return last &&
      plan.direction === MidgardNativeScriptDecodingDirections.WrongfulRejection
      ? "step06"
      : "scan";
  }
  if (
    plan.verdict.control?.cborHex === controlCbor &&
    plan.verdict.refusalClass !== null
  )
    return "step06";
  throw new Error(
    "outputReferenceScriptDecoding authenticated structural checkpoint is outside the deterministic trace",
  );
};

const createManifestBoundOutputReferenceScriptDecodingSubmission = ({
  config,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundOutputReferenceScriptDecodingConfig;
  readonly observe: (
    identity: string,
  ) => Promise<OutputReferenceScriptDecodingStage>;
  readonly resolveStage: OutputReferenceScriptDecodingRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createOutputReferenceScriptDecodingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => ({
  observe,
  submit: async (
    action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitOutputScan"
      | "submitReferenceBind"
      | "submitStructuralScan"
      | "submitStep06"
      | "removeDescendants",
    evidence: OutputReferenceScriptDecodingEvidence,
  ) => {
    if (evidence.subject.transaction_id.length !== 64)
      throw new Error(
        "outputReferenceScriptDecoding evidence transaction id is not canonical",
      );
    const familyIdentity =
      outputReferenceScriptDecodingEvidenceIdentity(evidence);
    const stage = await resolveStage({ action, evidence });
    const transition =
      action === "submitInit"
        ? (["none", "step01"] as const)
        : action === "submitStep01"
          ? (["step01", "step02"] as const)
          : action === "submitStep02"
            ? (["step02", "outputScan"] as const)
            : action === "submitOutputScan"
              ? ([
                  "outputScan",
                  outputReferenceScriptDecodingOutputScanTarget({
                    stage,
                    evidence,
                  }),
                ] as const)
              : action === "submitReferenceBind"
                ? (["referenceBind", "scan"] as const)
                : action === "submitStructuralScan"
                  ? ([
                      "scan",
                      outputReferenceScriptDecodingStructuralTarget({
                        stage,
                        evidence,
                      }),
                    ] as const)
                  : action === "submitStep06"
                    ? (["step06", "proven"] as const)
                    : (["proven", "removed"] as const);
    await centralJournal?.begin(
      action,
      familyIdentity,
      transition[0],
      transition[1],
    );
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
        const result = await submitOutputReferenceScriptDecodingStep01Accepted({
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
        throw new Error(
          "outputReferenceScriptDecoding evidence source kind is invalid",
        );
      const result = await submitOutputReferenceScriptDecodingStep01Forced({
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
      const result = await submitOutputReferenceScriptDecodingStep02({
        lucid: config.lucid,
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
        publishCarriage: true,
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
        referenceScriptUtxo: config.referenceScripts.step02,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      for (const txHash of auxiliaryHashes)
        await centralJournal?.confirmAuxiliary(txHash);
      return {
        stage: "outputScan" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitOutputScan") {
      const result = await submitOutputReferenceScriptDecodingStep03({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step03 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step03,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: result.terminal
          ? ("referenceBind" as const)
          : ("outputScan" as const),
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitReferenceBind") {
      const auxiliaryHashes: string[] = [];
      const result = await submitOutputReferenceScriptDecodingStep04({
        lucid: config.lucid,
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
        publishCarriage: true,
        publicationPreSubmitBoundary: centralJournal?.auxiliaryBoundary(
          "publication",
          familyIdentity,
          "referenceBind",
          auxiliaryHashes,
        ),
        certificatePreSubmitBoundary: centralJournal?.auxiliaryBoundary(
          "certificate",
          familyIdentity,
          "referenceBind",
          auxiliaryHashes,
        ),
        referenceScriptUtxo: config.referenceScripts.step04,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      for (const txHash of auxiliaryHashes)
        await centralJournal?.confirmAuxiliary(txHash);
      return {
        stage: "scan" as const,
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStructuralScan") {
      const result = await submitOutputReferenceScriptDecodingStep05({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step05 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step05,
        preSubmitBoundary: centralJournal?.boundary(
          action,
          familyIdentity,
          transition[0],
          transition[1],
        ),
      });
      return {
        stage: result.closed ? ("step06" as const) : ("scan" as const),
        txHash: result.txHash,
        outputReference: result.nextThreadOutRef,
      };
    }
    if (action === "submitStep06") {
      const result = await submitOutputReferenceScriptDecodingStep06({
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId: config.binding.resolvedContracts.category.categoryId,
        signer: config.signer,
        threadOutRef: required(stage.threadOutRef, "step06 thread out-ref"),
        evidence,
        referenceScriptUtxo: config.referenceScripts.step06,
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
        "outputReferenceScriptDecoding" as FraudProofCatalogueCategoryName,
      fraudulentHeaderHash: config.binding.definition.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        stateQueueMutationLeaseCoordinator ??
        (() => {
          throw new Error(
            "outputReferenceScriptDecoding production removal requires a state-queue mutation lease coordinator",
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
    current: "step01" | "step02" | "outputScan" | "referenceBind" | "scan",
    evidence: OutputReferenceScriptDecodingEvidence,
  ) => {
    const stage = await resolveStage({
      action: "cancel",
      evidence,
      currentStage: current,
    });
    const index =
      current === "step01"
        ? 0
        : current === "step02"
          ? 1
          : current === "outputScan"
            ? 2
            : current === "referenceBind"
              ? 3
              : 4;
    const result = await submitOutputReferenceScriptDecodingCancel({
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
      preSubmitBoundary: centralJournal?.boundary(
        "cancel",
        outputReferenceScriptDecodingEvidenceIdentity(evidence),
        current,
        "cancelled",
      ),
    });
    return {
      stage: "cancelled" as const,
      txHash: result.txHash,
      outputReference: null,
    };
  },
});

const createManifestBoundOutputReferenceScriptDecodingRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundOutputReferenceScriptDecodingConfig;
  readonly journal: OutputReferenceScriptDecodingJournal;
  readonly observe: OutputReferenceScriptDecodingRuntimeLoader["observe"];
  readonly resolveStage: OutputReferenceScriptDecodingRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createOutputReferenceScriptDecodingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundOutputReferenceScriptDecodingSubmission(
    {
      config,
      observe: async (identity) => {
        const observed = await observe(identity);
        await centralJournal?.reconcile(observed);
        return observed;
      },
      resolveStage,
      centralJournal,
      stateQueueMutationLeaseCoordinator,
    },
  );
  return Object.freeze({
    runtimeVersion: OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW,
    config,
    runOrResume: async (evidence: OutputReferenceScriptDecodingEvidence) => {
      const identity = outputReferenceScriptDecodingEvidenceIdentity(evidence);
      for (;;) {
        const stage = await submission.observe(identity);
        const action = nextOutputReferenceScriptDecodingAction(stage);
        if (action === "done") return stage;
        if (action === "cancel")
          throw new Error(
            "outputReferenceScriptDecoding automatic runner cannot synthesize cancellation",
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

export type ManifestBoundOutputReferenceScriptDecodingWorkflowConfig =
  LoadManifestBoundOutputReferenceScriptDecodingConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundOutputReferenceScriptDecodingWorkflow = Readonly<{
  workflowVersion: typeof OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW;
  config: ManifestBoundOutputReferenceScriptDecodingConfig;
  binding: OutputReferenceScriptDecodingDeploymentBinding;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundOutputReferenceScriptDecodingWorkflow = async (
  input: ManifestBoundOutputReferenceScriptDecodingWorkflowConfig,
): Promise<ManifestBoundOutputReferenceScriptDecodingWorkflow> => {
  const config =
    await loadManifestBoundOutputReferenceScriptDecodingConfig(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition as never,
  });
  return Object.freeze({
    workflowVersion: OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

export const outputReferenceScriptDecodingStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>["observe"]
    >
  >["stage"],
): OutputReferenceScriptDecodingStage => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "outputScan";
      if (stage.step === 4) return "referenceBind";
      if (stage.step === 5) return "scan";
      if (stage.step === 6) return "step06";
      throw new Error(
        "outputReferenceScriptDecoding L1 stage exceeds six-step topology",
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
export const runOrResumeManifestBoundOutputReferenceScriptDecodingWorkflow =
  async (input: {
    readonly workflow: ManifestBoundOutputReferenceScriptDecodingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: OutputReferenceScriptDecodingJournal;
  }): Promise<OutputReferenceScriptDecodingStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "outputReferenceScriptDecoding runner rejects caller-authored evidence inputs",
      );
    }
    const headerHash = input.workflow.binding.definition.headerHash;
    const observation = await input.workflow.l1.observeHeader({ headerHash });
    const canonical = await fetchCanonicalBlockEvidence({
      observation,
      sources: input.sources,
    });
    const findings =
      detectOutputReferenceScriptDecodingCompleteReplay(canonical);
    if (findings.length !== 1)
      throw new Error(
        `outputReferenceScriptDecoding public replay yielded ${findings.length.toString()} exact findings`,
      );
    const evidence = findings[0]!;
    const source = await deriveOutputReferenceScriptDecodingAuthenticatedSource(
      {
        block: canonical,
        evidence,
      },
    );
    const runtime = createManifestBoundOutputReferenceScriptDecodingRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        outputReferenceScriptDecodingStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createOutputReferenceScriptDecodingRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
      stateQueueMutationLeaseCoordinator:
        input.workflow.stateQueueMutationLeaseCoordinator,
    });
    return await runtime.runOrResume(evidence);
  };

export const executeManifestBoundOutputReferenceScriptDecodingWorkflow =
  async ({
    workflow,
    sources,
    journal,
  }: {
    readonly workflow: ManifestBoundOutputReferenceScriptDecodingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FraudProofWorkflowJournalStore;
  }): Promise<OutputReferenceScriptDecodingStage> => {
    const headerHash = workflow.binding.definition.headerHash;
    const canonical = await fetchCanonicalBlockEvidence({
      observation: await workflow.l1.observeHeader({ headerHash }),
      sources,
    });
    const findings =
      detectOutputReferenceScriptDecodingCompleteReplay(canonical);
    if (findings.length !== 1)
      throw new Error(
        `outputReferenceScriptDecoding public replay yielded ${findings.length.toString()} exact findings`,
      );
    const evidence = findings[0]!;
    const source = await deriveOutputReferenceScriptDecodingAuthenticatedSource(
      {
        block: canonical,
        evidence,
      },
    );
    const centralJournal =
      createOutputReferenceScriptDecodingCentralJournalAdapter({
        store: journal,
        deploymentFingerprint: workflow.binding.deploymentFingerprint,
        headerHash,
        decisionDigest: workflow.decisionDigest,
        transactionConfirmed: async (txHash) =>
          await workflow.l1.transactionConfirmed({ headerHash, txHash }),
      });
    const runtime = createManifestBoundOutputReferenceScriptDecodingRuntime({
      config: workflow.config,
      journal: centralJournal.familyJournal,
      observe: async () =>
        outputReferenceScriptDecodingStageFromL1(
          (await workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createOutputReferenceScriptDecodingRawL1StageResolver({
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

export type LoadedOutputReferenceScriptDecodingWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundOutputReferenceScriptDecodingWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadOutputReferenceScriptDecodingWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedOutputReferenceScriptDecodingWorkflow>;

/**
 * Family-local runner surface for central admission. It consumes only a
 * manifest/runtime path and concrete public-DA transports; neither evidence
 * nor a watcher-owned journal implementation can enter this boundary.
 */
export const createOutputReferenceScriptDecodingWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadOutputReferenceScriptDecodingWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "outputReferenceScriptDecoding") {
        throw new Error(
          `outputReferenceScriptDecoding production runner category mismatch: ${invocation.category}`,
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
            "outputReferenceScriptDecoding" as FraudProofCatalogueCategoryName,
          headerHash: invocation.headerHash,
        }),
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: invocation.deploymentFingerprint,
        category:
          "outputReferenceScriptDecoding" as FraudProofCatalogueCategoryName,
        headerHash: invocation.headerHash,
        checkpoint: "runner_start",
      });
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      if (typeof loaded.close !== "function") {
        throw new Error(
          "outputReferenceScriptDecoding runtime omitted its transport disposer",
        );
      }
      try {
        if (
          loaded.schemaVersion !==
          "midgard-production-fraud-proof-runtime-config-v1"
        ) {
          throw new Error(
            "outputReferenceScriptDecoding runtime config has an unsupported schema",
          );
        }
        if (
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        ) {
          throw new Error(
            "outputReferenceScriptDecoding production runner requires concrete public retained-DA sources",
          );
        }
        const workflow =
          await createManifestBoundOutputReferenceScriptDecodingWorkflow(
            loaded.config,
          );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          String(workflow.binding.definition.category) !==
            "outputReferenceScriptDecoding" ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        ) {
          throw new Error(
            "outputReferenceScriptDecoding manifest-bound workflow identity differs from invocation",
          );
        }
        return await executeManifestBoundOutputReferenceScriptDecodingWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });
