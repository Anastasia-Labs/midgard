import {
  type ForcedInclusionTxV1,
  FraudProofComputationThreadStepDatum,
  type Header,
  type OutputReference,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { type SubmitStep01TxInclusion } from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import type { ResolvedOutputNonCanonicalContracts } from "./contracts.js";
import {
  ResolvedOutputStep02DatumSchema,
  ResolvedOutputStep03DatumSchema,
  ResolvedOutputStep04DatumSchema,
  ResolvedOutputStep05DatumSchema,
} from "./schemas.js";

export const RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW =
  "midgard-resolved-output-non-canonical-production-workflow-v1" as const;

export const RESOLVED_OUTPUT_NON_CANONICAL_VIOLATION_ID =
  "resolved-output-non-canonical" as const;

export const RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS = Object.freeze({
  step01: "fraudProofResolvedOutputNonCanonical",
  step02: "fraudProofResolvedOutputNonCanonicalStep02",
  step03: "fraudProofResolvedOutputNonCanonicalStep03",
  step04: "fraudProofResolvedOutputNonCanonicalStep04",
  step05: "fraudProofResolvedOutputNonCanonicalStep05",
  computationThreadMint: "computationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type ResolvedOutputNonCanonicalReferenceScripts = Readonly<{
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

export type ManifestBoundResolvedOutputNonCanonicalConfig = Readonly<{
  schemaVersion: typeof RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBinding<"resolvedOutputNonCanonical">;
  contracts: ResolvedOutputNonCanonicalContracts;
  referenceScripts: ResolvedOutputNonCanonicalReferenceScripts;
}>;

export type LoadManifestBoundResolvedOutputNonCanonicalConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: ResolvedOutputNonCanonicalReferenceScripts;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"resolvedOutputNonCanonical">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });

export const bindResolvedOutputNonCanonicalReferenceScripts = ({
  binding,
  referenceScripts,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"resolvedOutputNonCanonical">;
  readonly referenceScripts: ResolvedOutputNonCanonicalReferenceScripts;
}): ResolvedOutputNonCanonicalReferenceScripts => {
  const names = RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS;
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

export const loadManifestBoundResolvedOutputNonCanonicalConfig = async (
  input: LoadManifestBoundResolvedOutputNonCanonicalConfig,
): Promise<ManifestBoundResolvedOutputNonCanonicalConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "resolvedOutputNonCanonical",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      ResolvedOutputStep02DatumSchema,
      ResolvedOutputStep03DatumSchema,
      ResolvedOutputStep04DatumSchema,
      ResolvedOutputStep05DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
  const referenceScripts = bindResolvedOutputNonCanonicalReferenceScripts({
    binding,
    referenceScripts: input.referenceScripts,
  });
  return resolvedOutputNonCanonicalConfigFromBinding({
    ...input,
    binding,
    referenceScripts,
  });
};

export const resolvedOutputNonCanonicalConfigFromBinding = (input: {
  binding: FraudProofWorkflowDeploymentBinding<"resolvedOutputNonCanonical">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: ResolvedOutputNonCanonicalReferenceScripts;
}): ManifestBoundResolvedOutputNonCanonicalConfig => {
  const { binding, referenceScripts } = input;
  const localContracts = binding.resolvedContracts.contracts as unknown as {
    readonly resolvedOutputNonCanonical?: ResolvedOutputNonCanonicalContracts;
  };
  const chain = localContracts.resolvedOutputNonCanonical;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 5) {
    throw new Error(
      "resolvedOutputNonCanonical deployment changed its five-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "resolvedOutputNonCanonical deployment omitted field-preimage certificate",
    );
  }
  return Object.freeze({
    schemaVersion: RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW,
    lucid: input.lucid,
    signer: input.signer,
    binding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/resolved_output_non_canonical/step_01.main.spend",
          "fraud_proofs/resolved_output_non_canonical/step_02.main.spend",
          "fraud_proofs/resolved_output_non_canonical/step_03.main.spend",
          "fraud_proofs/resolved_output_non_canonical/step_04.main.spend",
          "fraud_proofs/resolved_output_non_canonical/step_05.main.spend",
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
      })) as unknown as ResolvedOutputNonCanonicalContracts["steps"],
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

export type ResolvedOutputNonCanonicalStage = Readonly<{
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
export type ResolvedOutputNonCanonicalAuthenticatedSource = Readonly<{
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedDirection?: bigint;
}>;
