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
import type { TransactionOutputNonCanonicalContracts } from "./contracts.js";
import {
  TransactionOutputStep02DatumSchema,
  TransactionOutputStep03DatumSchema,
  TransactionOutputStep04DatumSchema,
} from "./schemas.js";

export const TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW =
  "midgard-transaction-output-non-canonical-production-workflow-v1" as const;

export const TRANSACTION_OUTPUT_NON_CANONICAL_VIOLATION_ID =
  "transaction-output-non-canonical" as const;

export const TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS =
  Object.freeze({
    step01: "fraudProofTransactionOutputNonCanonical",
    step02: "fraudProofTransactionOutputNonCanonicalStep02",
    step03: "fraudProofTransactionOutputNonCanonicalStep03",
    step04: "fraudProofTransactionOutputNonCanonicalStep04",
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
  } as const);

export type TransactionOutputNonCanonicalReferenceScripts = Readonly<{
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

export type ManifestBoundTransactionOutputNonCanonicalConfig = Readonly<{
  schemaVersion: typeof TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBinding<"transactionOutputNonCanonical">;
  contracts: TransactionOutputNonCanonicalContracts;
  referenceScripts: TransactionOutputNonCanonicalReferenceScripts;
}>;

export type LoadManifestBoundTransactionOutputNonCanonicalConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: TransactionOutputNonCanonicalReferenceScripts;
}>;

const bindReference = ({
  binding,
  contractName,
  utxo,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"transactionOutputNonCanonical">;
  readonly contractName: string;
  readonly utxo: UTxO;
}): UTxO =>
  requireManifestBoundReferenceScriptUtxo({ binding, contractName, utxo });

export const bindTransactionOutputNonCanonicalReferenceScripts = ({
  binding,
  referenceScripts,
}: {
  readonly binding: FraudProofWorkflowDeploymentBinding<"transactionOutputNonCanonical">;
  readonly referenceScripts: TransactionOutputNonCanonicalReferenceScripts;
}): TransactionOutputNonCanonicalReferenceScripts => {
  const names = TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS;
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

export const loadManifestBoundTransactionOutputNonCanonicalConfig = async (
  input: LoadManifestBoundTransactionOutputNonCanonicalConfig,
): Promise<ManifestBoundTransactionOutputNonCanonicalConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "transactionOutputNonCanonical",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      TransactionOutputStep02DatumSchema,
      TransactionOutputStep03DatumSchema,
      TransactionOutputStep04DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });
  const referenceScripts = bindTransactionOutputNonCanonicalReferenceScripts({
    binding,
    referenceScripts: input.referenceScripts,
  });
  return transactionOutputNonCanonicalConfigFromBinding({
    ...input,
    binding,
    referenceScripts,
  });
};

export const transactionOutputNonCanonicalConfigFromBinding = (input: {
  binding: FraudProofWorkflowDeploymentBinding<"transactionOutputNonCanonical">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: TransactionOutputNonCanonicalReferenceScripts;
}): ManifestBoundTransactionOutputNonCanonicalConfig => {
  const { binding, referenceScripts } = input;
  const localContracts = binding.resolvedContracts.contracts as unknown as {
    readonly transactionOutputNonCanonical?: TransactionOutputNonCanonicalContracts;
  };
  const chain = localContracts.transactionOutputNonCanonical;
  const certificate = binding.fieldPreimageCertificate;
  if (chain === undefined || chain.steps.length !== 4) {
    throw new Error(
      "transactionOutputNonCanonical deployment changed its four-step topology",
    );
  }
  if (certificate === null) {
    throw new Error(
      "transactionOutputNonCanonical deployment omitted field-preimage certificate",
    );
  }
  return Object.freeze({
    schemaVersion: TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW,
    lucid: input.lucid,
    signer: input.signer,
    binding,
    contracts: {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle: [
          "fraud_proofs/transaction_output_non_canonical/step_01.main.spend",
          "fraud_proofs/transaction_output_non_canonical/step_02.main.spend",
          "fraud_proofs/transaction_output_non_canonical/step_03.main.spend",
          "fraud_proofs/transaction_output_non_canonical/step_04.main.spend",
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
      })) as unknown as TransactionOutputNonCanonicalContracts["steps"],
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

export type TransactionOutputNonCanonicalStage = Readonly<{
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
