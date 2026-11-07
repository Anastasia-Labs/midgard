import {
  type ForcedInclusionTxV1,
  FraudProofComputationThreadStepDatum,
  type Header,
  type OutputReference,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { type SubmitStep01TxInclusion } from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import type { MintItemNonCanonicalContracts } from "./contracts.js";
import { type MintItemEvidence } from "./mint-item-non-canonical.js";
import { findMintItemNonCanonicalBlockEvidence } from "./replay.js";
import {
  MintItemStep02DatumSchema,
  MintItemStep03DatumSchema,
  MintItemStep04DatumSchema,
} from "./schemas.js";

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
