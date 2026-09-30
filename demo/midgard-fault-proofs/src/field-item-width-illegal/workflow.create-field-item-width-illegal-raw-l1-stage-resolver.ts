import {
  type ForcedInclusionTxV1,
  type Header,
  type OutputReference,
  PROOF_THREAD_SOURCE_KIND_FORCED,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type SubmitStep01TxInclusion } from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { FieldItemWidthIllegalContracts } from "./contracts.js";
import {
  type FieldItemWidthEvidence,
  type FieldItemWidthJournal,
  type FieldItemWidthStage,
} from "./field-item-width-illegal.js";

export const FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW =
  "midgard-field-item-width-illegal-production-workflow-v1" as const;

export const FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID =
  "field-item-width-illegal" as const;

export const FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS = Object.freeze({
  step01: "fraudProofFieldItemWidthIllegal",
  step02: "fraudProofFieldItemWidthIllegalStep02",
  step03: "fraudProofFieldItemWidthIllegalStep03",
  computationThreadMint: "computationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type FieldItemWidthIllegalReferenceScripts = Readonly<{
  step01: UTxO;
  step02: UTxO;
  step03: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundFieldItemWidthIllegalConfig = Readonly<{
  schemaVersion: typeof FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal">;
  contracts: FieldItemWidthIllegalContracts;
  referenceScripts: FieldItemWidthIllegalReferenceScripts;
}>;

export type LoadManifestBoundFieldItemWidthIllegalConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: FieldItemWidthIllegalReferenceScripts;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });

export const bindFieldItemWidthIllegalReferenceScripts = ({
  binding,
  referenceScripts,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal">;
  readonly referenceScripts: FieldItemWidthIllegalReferenceScripts;
}): FieldItemWidthIllegalReferenceScripts => {
  const names = FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS;
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

export const createFieldItemWidthIllegalBoundConfig = (
  input: LoadManifestBoundFieldItemWidthIllegalConfig,
  binding: FraudProofWorkflowDeploymentBinding<"fieldItemWidthIllegal">,
): ManifestBoundFieldItemWidthIllegalConfig => {
  const chain = binding.resolvedContracts.contracts.fieldItemWidthIllegal;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 3) {
    throw new Error(
      "fieldItemWidthIllegal deployment changed its three-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "fieldItemWidthIllegal deployment omitted field-preimage certificate",
    );
  }
  const referenceScripts = bindFieldItemWidthIllegalReferenceScripts({
    binding,
    referenceScripts: input.referenceScripts,
  });
  return Object.freeze({
    schemaVersion: FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
    lucid: input.lucid,
    signer: input.signer,
    binding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/field_item_width_illegal/step_01.main.spend",
          "fraud_proofs/field_item_width_illegal/step_02.main.spend",
          "fraud_proofs/field_item_width_illegal/step_03.main.spend",
        ][index]!,
        referenceOutRef: [
          referenceScripts.step01,
          referenceScripts.step02,
          referenceScripts.step03,
        ][index]!.txHash.concat(
          "#",
          [
            referenceScripts.step01,
            referenceScripts.step02,
            referenceScripts.step03,
          ][index]!.outputIndex.toString(),
        ),
      })) as unknown as FieldItemWidthIllegalContracts["steps"],
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

export type FieldItemWidthIllegalStage = Readonly<{
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

export type FieldItemWidthIllegalAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

export type FieldItemWidthIllegalRuntimeLoader = Readonly<{
  config: LoadManifestBoundFieldItemWidthIllegalConfig;
  journal: FieldItemWidthJournal;
  observe: (identity: string) => Promise<FieldItemWidthStage>;
  resolveStage: (input: {
    readonly action:
      | "submitInit"
      | "submitStep01"
      | "submitStep02"
      | "submitStep03"
      | "removeDescendants"
      | "cancel";
    readonly evidence: FieldItemWidthEvidence;
  }) => Promise<FieldItemWidthIllegalStage>;
}>;

export const createFieldItemWidthIllegalRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundFieldItemWidthIllegalConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<"fieldItemWidthIllegal">;
    readonly source: FieldItemWidthIllegalAuthenticatedSource;
  }): FieldItemWidthIllegalRuntimeLoader["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "fieldItemWidthIllegal init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "fieldItemWidthIllegal removal requires raw-L1 proof token",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    const expectedStep =
      action === "submitStep01" ? 1 : action === "submitStep02" ? 2 : 3;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `fieldItemWidthIllegal ${action} differs from authenticated raw-L1 stage`,
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
      family: "field-item-width-illegal",
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

export const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`fieldItemWidthIllegal missing ${label}`);
  return value;
};
