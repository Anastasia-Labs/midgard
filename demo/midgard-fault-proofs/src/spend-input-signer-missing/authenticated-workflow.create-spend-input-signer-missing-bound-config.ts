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
import type { SpendInputSignerMissingContracts } from "./contracts.js";
import { type SpendInputSignerMissingEvidence } from "./spend-input-signer-missing.js";
import {
  type SpendInputSignerJournal,
  type SpendInputSignerStage,
} from "./workflow.js";

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

export const createSpendInputSignerMissingBoundConfig = (
  input: LoadManifestBoundSpendInputSignerMissingConfig,
  binding: FraudProofWorkflowDeploymentBinding<"spendInputSignerMissing">,
): ManifestBoundSpendInputSignerMissingConfig => {
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
      hubOraclePolicyId: binding.contractEntries.hubOracleMint!.scriptHash,
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
export type SpendInputSignerMissingAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

/** Complete replay member: scans every accepted field-2 output and exact forced reason. */
export type SpendInputSignerMissingRuntimeLoader = Readonly<{
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

export const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`spendInputSignerMissing missing ${label}`);
  return value;
};
