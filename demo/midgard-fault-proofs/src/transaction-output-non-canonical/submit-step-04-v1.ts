import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { TransactionOutputNonCanonicalContractsV1 } from "./contracts-v1.js";
import {
  TransactionOutputStep04DatumV1Schema,
  TransactionOutputStep04RedeemerV1Schema,
} from "./schemas-v1.js";
import {
  transactionOutputEvidenceClosesV1,
  type TransactionOutputEvidenceV1,
} from "./transaction-output-non-canonical-v1.js";

export const submitTransactionOutputNonCanonicalStep04V1 = async ({
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
  readonly contracts: TransactionOutputNonCanonicalContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: TransactionOutputEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!transactionOutputEvidenceClosesV1(evidence)) {
    throw new Error(
      "transaction-output-non-canonical: terminal scan does not contradict verdict",
    );
  }
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "transaction-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    output_index: bigint;
    item_length: bigint;
    item_hash: string;
    chunk_hashes: readonly string[];
    outcome: bigint;
  }>({
    threadUtxo,
    signer,
    schema: TransactionOutputStep04DatumV1Schema as never,
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
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "transaction-output-non-canonical",
    stepIndex,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: TransactionOutputStep04RedeemerV1Schema,
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
