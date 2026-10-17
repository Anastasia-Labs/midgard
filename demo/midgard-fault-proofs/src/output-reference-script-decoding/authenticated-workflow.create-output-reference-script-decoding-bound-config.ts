import {
  type ForcedInclusionTxV1,
  type FraudProofCatalogueCategoryName,
  type Header,
  type OutputReference,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { type SubmitStep01TxInclusion } from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import type { OutputReferenceScriptDecodingContracts } from "./contracts.js";
import { type OutputReferenceScriptDecodingEvidence } from "./output-reference-script-decoding.js";
import {
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

export const createOutputReferenceScriptDecodingBoundConfig = (
  input: LoadManifestBoundOutputReferenceScriptDecodingConfig,
  binding: FraudProofWorkflowDeploymentBinding<"outputReferenceScriptDecoding">,
): ManifestBoundOutputReferenceScriptDecodingConfig => {
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
      hubOraclePolicyId: binding.contractEntries.hubOracleMint!.scriptHash,
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

/** Complete replay member: scans every accepted field-2 output and exact forced reason. */
export type OutputReferenceScriptDecodingRuntimeLoader = Readonly<{
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

export const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`outputReferenceScriptDecoding missing ${label}`);
  return value;
};
