import {
  decodeAddressWitnessPreimage,
  MISSING_SIGNATURE_VIOLATION_ID,
  missingSignatureVkeyHash,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence.js";
import type { MissingSignatureContracts } from "../missing-signature/contracts.js";
import {
  admitMissingSignatureForcedArtifact,
  missingSignatureForcedArtifact,
} from "../missing-signature/forced-artifact.js";
import { submitMissingSignatureInit } from "../missing-signature/submit-missing-signature-init.js";
import { submitMissingSignatureStep01 } from "../missing-signature/submit-missing-signature-step-01.js";
import { submitMissingSignatureStep02 } from "../missing-signature/submit-missing-signature-step-02.js";
import { submitMissingSignatureStep03 } from "../missing-signature/submit-missing-signature-step-03.js";
import { submitMissingSignatureStep04 } from "../missing-signature/submit-missing-signature-step-04.js";
import {
  MISSING_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID,
  prepareMissingSignatureWrongfulRejection,
} from "../missing-signature/wrongful-rejection.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type PreparedTxInclusionJson,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import {
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { type FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import { type JournalJsonObject, normalizeJournalJson } from "./journal.js";
import {
  type AdmittedMissingSignatureArtifact,
  HEX_28,
  HEX_32,
  MISSING_SIGNATURE_ARTIFACT,
  type MissingSignatureArtifact,
  parseArtifact,
  publicCommittedVkeyFor,
  record,
  signerHashes,
  witnessSetCompact,
} from "./missing-signature.parse-artifact.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";

/**
 * Re-authenticates every durable byte, rebuilds the counted transaction root
 * and MPF proof, and recovers the vkey only from committed public L2 evidence.
 */
export const admitMissingSignatureArtifact = async (
  value: unknown,
): Promise<AdmittedMissingSignatureArtifact> => {
  const artifact = parseArtifact(value);
  const decoded = await Promise.all(
    artifact.transactions.map(decodeTransactionMaterial),
  );
  const selected = decoded[artifact.selectedTransactionIndex];
  if (selected === undefined) {
    throw new Error("missing-signature artifact selected no transaction");
  }
  const requiredSignerHashes = signerHashes(
    selected.nativeTx.body.requiredSignersPreimageCbor,
    `transaction ${selected.nodeTxId} required_signers`,
  );
  const accused = requiredSignerHashes[artifact.accusedRequiredSignerIndex];
  if (accused !== artifact.accusedRequiredSignerHash) {
    throw new Error(
      "missing-signature artifact accused ordinal differs from the committed required-signer list",
    );
  }
  const allWitnesses = decoded.map((transaction) =>
    decodeAddressWitnessPreimage(
      transaction.nativeTx.witnessSet.addrTxWitsPreimageCbor,
    ),
  );
  const addrTxWits = allWitnesses[artifact.selectedTransactionIndex]!;
  if (
    addrTxWits.some(
      (witness) =>
        missingSignatureVkeyHash(witness.verification_key) === accused,
    )
  ) {
    throw new Error(
      "missing-signature artifact accused key is present in the committed witness field",
    );
  }
  const resolvedVkey = publicCommittedVkeyFor({
    hash: accused,
    witnesses: allWitnesses,
  });
  if (resolvedVkey === undefined) {
    throw new Error(
      "missing-signature vkey preimage is absent from authenticated public evidence; route this case to validationTraceDispute",
    );
  }
  if (artifact.resolvedVkey !== resolvedVkey) {
    throw new Error(
      "missing-signature durable vkey is not the deterministic committed public preimage",
    );
  }
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: artifact.committedTransactionsRoot,
    count: BigInt(decoded.length),
  });
  const txInclusion: PreparedTxInclusionJson = Object.freeze({
    nativeTxId: selected.nodeTxId,
    nativeTx: selected.nativeTxCompact,
    nativeTxCompactCbor: selected.nativeCompactCbor,
    l2TransactionSourceCbor: selected.l2TransactionSourceCbor,
    transactionsPhasRoot: trie.root,
    txMembershipProofCbor: requireProof(
      trie,
      transactionSourceTrieItem(selected).key,
      "missing-signature transaction",
    ),
  });
  return Object.freeze({
    artifact,
    txInclusion: parseSubmitStep01TxInclusion(txInclusion),
    nativeTxCompactCbor: selected.nativeCompactCbor,
    requiredSignerHashes: Object.freeze([...requiredSignerHashes]),
    addrTxWits: Object.freeze([...addrTxWits]),
    witnessSetCompact: Object.freeze(
      witnessSetCompact(selected.nativeTx.witnessSet),
    ),
    accusedRequiredSignerIndex: BigInt(artifact.accusedRequiredSignerIndex),
    resolvedVkey,
  });
};

const selectedDetection = (
  classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "missingSignature" },
): Readonly<{
  transactionIndex: number;
  signerIndex: number;
  txId: string;
  signerHash: string;
}> => {
  const [violationId, transaction, signer, txId, signerHash, ...surplus] =
    classification.selected.detectionId.split(":");
  if (
    violationId !== MISSING_SIGNATURE_VIOLATION_ID ||
    surplus.length !== 0 ||
    !/^(?:0|[1-9][0-9]*)$/u.test(transaction ?? "") ||
    !/^(?:0|[1-9][0-9]*)$/u.test(signer ?? "") ||
    !HEX_32.test(txId ?? "") ||
    !HEX_28.test(signerHash ?? "")
  ) {
    throw new Error(
      "missing-signature classification has a malformed identity",
    );
  }
  const transactionIndex = Number(transaction);
  const signerIndex = Number(signer);
  if (
    !Number.isSafeInteger(transactionIndex) ||
    !Number.isSafeInteger(signerIndex) ||
    classification.selected.position !== BigInt(transactionIndex)
  ) {
    throw new Error("missing-signature classification has invalid ordinals");
  }
  return {
    transactionIndex,
    signerIndex,
    txId: txId!,
    signerHash: signerHash!,
  };
};

export const prepareMissingSignatureArtifact = async ({
  evidence,
  classification,
}: CanonicalEvidenceBuilderInput & {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "missingSignature" };
}): Promise<MissingSignatureArtifact> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  if (
    classification.headerHash !== admitted.headerHash ||
    classification.selected.violationId !== MISSING_SIGNATURE_VIOLATION_ID
  ) {
    throw new Error(
      "missing-signature classification differs from canonical evidence",
    );
  }
  const selected = selectedDetection(classification);
  const transactions = admitted.transactions.map((transaction) => ({
    nodeTxId: transaction.nodeTxId,
    txCbor: transaction.txCbor,
    l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
  }));
  if (transactions[selected.transactionIndex]?.nodeTxId !== selected.txId) {
    throw new Error(
      "missing-signature classification transaction differs from committed evidence",
    );
  }
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const allWitnesses = decoded.map((transaction) =>
    decodeAddressWitnessPreimage(
      transaction.nativeTx.witnessSet.addrTxWitsPreimageCbor,
    ),
  );
  const resolvedVkey = publicCommittedVkeyFor({
    hash: selected.signerHash,
    witnesses: allWitnesses,
  });
  if (resolvedVkey === undefined) {
    throw new Error(
      "missing-signature public evidence has no vkey preimage; the direct family must not accept operator input and this case requires validationTraceDispute",
    );
  }
  const artifact = normalizeJournalJson({
    schemaVersion: MISSING_SIGNATURE_ARTIFACT,
    headerHash: admitted.headerHash,
    committedTransactionsRoot: admitted.expectedTransactionsRoot,
    selectedTransactionIndex: selected.transactionIndex,
    accusedRequiredSignerIndex: selected.signerIndex,
    accusedRequiredSignerHash: selected.signerHash,
    resolvedVkey,
    transactions,
  }) as MissingSignatureArtifact;
  await admitMissingSignatureArtifact(artifact);
  return Object.freeze(artifact);
};

export type MissingSignatureWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  fieldPreimageCertificateMint?: UTxO;
  forced?: Readonly<{ bind: UTxO; signer: UTxO; witness: UTxO }>;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
  fieldCertificates?: Readonly<{
    step02?: UTxO;
    step04?: UTxO;
    forcedSigner?: UTxO;
    forcedWitness?: UTxO;
  }>;
}>;

export type BoundMissingSignatureTransactionsConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  network: FraudProofWorkflowDeploymentBinding<"missingSignature">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: MissingSignatureContracts;
  category: FraudProofWorkflowDeploymentBinding<"missingSignature">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"missingSignature">["catalogue"];
  referenceScripts: MissingSignatureWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
  deploymentInfo: unknown;
}>;

export type MissingSignatureBuilderSet = Readonly<{
  init: typeof submitMissingSignatureInit;
  step01: typeof submitMissingSignatureStep01;
  step02: typeof submitMissingSignatureStep02;
  step03: typeof submitMissingSignatureStep03;
  step04: typeof submitMissingSignatureStep04;
  remove: typeof submitRemoveFraudulentBlock;
}>;

export const productionBuilders: MissingSignatureBuilderSet = Object.freeze({
  init: submitMissingSignatureInit,
  step01: submitMissingSignatureStep01,
  step02: submitMissingSignatureStep02,
  step03: submitMissingSignatureStep03,
  step04: submitMissingSignatureStep04,
  remove: submitRemoveFraudulentBlock,
});

export const requiredAction = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "missing-signature workflow action");
  if (
    input.schemaVersion !== "midgard-production-missing-signature-action-v1" ||
    input.category !== "missingSignature" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("missing-signature workflow action changed identity");
  }
  return input;
};

export const stringField = (
  input: Readonly<Record<string, unknown>>,
  name: string,
): string => {
  const value = input[name];
  if (typeof value !== "string") {
    throw new Error(`missing-signature workflow action omitted ${name}`);
  }
  return value;
};

export const prepareMissingSignatureWorkflowArtifact = async (
  input: Parameters<typeof prepareMissingSignatureArtifact>[0],
): Promise<JournalJsonObject> => {
  if (
    input.classification.selected.violationId !==
    MISSING_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID
  )
    return prepareMissingSignatureArtifact(input);
  admitCanonicalEvidenceForProofBuild(input.evidence);
  const prepared = await prepareMissingSignatureWrongfulRejection({
    block: input.evidence,
  });
  if (
    prepared.headerHash !== input.classification.headerHash ||
    prepared.detectionId !== input.classification.selected.detectionId
  )
    throw new Error(
      "missingSignature: forced classification differs from authenticated evidence",
    );
  const artifact = missingSignatureForcedArtifact(
    prepared,
    input.evidence.reconstruction.forcedTransactions[
      prepared.forcedIndex
    ]!.fullTransactionCbor.toString("hex"),
  );
  await admitMissingSignatureForcedArtifact(artifact);
  return artifact;
};
