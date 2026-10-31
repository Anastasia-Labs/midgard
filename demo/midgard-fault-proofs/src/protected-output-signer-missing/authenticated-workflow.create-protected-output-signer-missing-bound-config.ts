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
import type { ProtectedOutputSignerMissingContracts } from "./contracts.js";
import { type ProtectedOutputSignerMissingEvidence } from "./protected-output-signer-missing.js";
import {
  type ProtectedOutputSignerJournal,
  type ProtectedOutputSignerStage,
} from "./workflow.js";

export const PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW =
  "midgard-protected-output-signer-missing-production-workflow-v1" as const;

export const PROTECTED_OUTPUT_SIGNER_MISSING_VIOLATION_ID =
  "protected-output-signer-missing" as const;

export const PROTECTED_OUTPUT_SIGNER_MISSING_MANIFEST_CONTRACTS = Object.freeze(
  {
    step01: "fraudProofProtectedOutputSignerMissing",
    step02: "fraudProofProtectedOutputSignerMissingStep02",
    step03: "fraudProofProtectedOutputSignerMissingStep03",
    step04: "fraudProofProtectedOutputSignerMissingStep04",
    step05: "fraudProofProtectedOutputSignerMissingStep05",
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
  } as const,
);

export type ProtectedOutputSignerMissingReferenceScripts = Readonly<{
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

export type ManifestBoundProtectedOutputSignerMissingConfig = Readonly<{
  schemaVersion: typeof PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: ProtectedOutputSignerMissingDeploymentBinding;
  contracts: ProtectedOutputSignerMissingContracts;
  referenceScripts: ProtectedOutputSignerMissingReferenceScripts;
}>;

export type ProtectedOutputSignerMissingDeploymentBinding = Omit<
  FraudProofWorkflowDeploymentBinding<FraudProofCatalogueCategoryName>,
  "definition"
> &
  Readonly<{
    definition: Omit<
      FraudProofWorkflowDeploymentBinding<FraudProofCatalogueCategoryName>["definition"],
      "category"
    > &
      Readonly<{ category: "protectedOutputSignerMissing" }>;
  }>;

export type LoadManifestBoundProtectedOutputSignerMissingConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: ProtectedOutputSignerMissingReferenceScripts;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: ProtectedOutputSignerMissingDeploymentBinding;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });

export const bindProtectedOutputSignerMissingReferenceScripts = ({
  binding,
  referenceScripts,
}: {
  readonly binding: ProtectedOutputSignerMissingDeploymentBinding;
  readonly referenceScripts: ProtectedOutputSignerMissingReferenceScripts;
}): ProtectedOutputSignerMissingReferenceScripts => {
  const names = PROTECTED_OUTPUT_SIGNER_MISSING_MANIFEST_CONTRACTS;
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

export const createProtectedOutputSignerMissingBoundConfig = (
  input: LoadManifestBoundProtectedOutputSignerMissingConfig,
  binding: FraudProofWorkflowDeploymentBinding<"protectedOutputSignerMissing">,
): ManifestBoundProtectedOutputSignerMissingConfig => {
  const localContracts = binding.resolvedContracts.contracts as unknown as {
    readonly protectedOutputSignerMissing?: ProtectedOutputSignerMissingContracts;
  };
  const chain = localContracts.protectedOutputSignerMissing;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 5) {
    throw new Error(
      "protectedOutputSignerMissing deployment changed its five-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "protectedOutputSignerMissing deployment omitted field-preimage certificate",
    );
  }
  const referenceScripts = bindProtectedOutputSignerMissingReferenceScripts({
    binding:
      binding as unknown as ProtectedOutputSignerMissingDeploymentBinding,
    referenceScripts: input.referenceScripts,
  });
  return Object.freeze({
    schemaVersion: PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW,
    lucid: input.lucid,
    signer: input.signer,
    binding:
      binding as unknown as ProtectedOutputSignerMissingDeploymentBinding,
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
      })) as unknown as ProtectedOutputSignerMissingContracts["steps"],
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

export type ProtectedOutputSignerMissingStage = Readonly<{
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
export type ProtectedOutputSignerMissingAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;

/** Complete replay member: scans every accepted field-2 output and exact forced reason. */
export type ProtectedOutputSignerMissingRuntimeLoader = Readonly<{
  config: LoadManifestBoundProtectedOutputSignerMissingConfig;
  journal: ProtectedOutputSignerJournal;
  observe: (identity: string) => Promise<ProtectedOutputSignerStage>;
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
    readonly evidence: ProtectedOutputSignerMissingEvidence;
  }) => Promise<ProtectedOutputSignerMissingStage>;
}>;

export const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`protectedOutputSignerMissing missing ${label}`);
  return value;
};
