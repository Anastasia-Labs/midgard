import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { TransactionOutputNonCanonicalContracts } from "./contracts-v1.js";
import {
  TransactionOutputStep04DatumSchema,
  TransactionOutputStep04RedeemerSchema,
} from "./schemas-v1.js";
import {
  type TransactionOutputEvidence,
  transactionOutputEvidenceCloses,
} from "./transaction-output-non-canonical-v1.js";

export const submitTransactionOutputNonCanonicalStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: TransactionOutputNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: TransactionOutputEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!transactionOutputEvidenceCloses(evidence)) {
    throw new Error(
      "transaction-output-non-canonical: terminal scan does not contradict verdict",
    );
  }
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "transaction-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    output_index: bigint;
    item_length: bigint;
    item_hash: string;
    chunk_hashes: readonly string[];
    outcome: bigint;
  }>({
    threadUtxo,
    signer,
    schema: TransactionOutputStep04DatumSchema as never,
    family: "transaction-output-non-canonical",
    stepIndex,
  });
  const expectedOutcome = evidence.canonical ? 1n : 2n;
  if (
    state.output_index !== BigInt(evidence.itemIndex) ||
    state.item_length !== BigInt(evidence.itemLength) ||
    state.item_hash !== evidence.itemHash ||
    state.chunk_hashes.join(":") !== evidence.chunkHashes.join(":") ||
    state.outcome !== expectedOutcome
  ) {
    throw new Error(
      "transaction-output-non-canonical: terminal state differs from prepared evidence",
    );
  }
  return await submitLinearFaultFinalize({
    lucid,
    family: "transaction-output-non-canonical",
    stepIndex,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: TransactionOutputStep04RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
