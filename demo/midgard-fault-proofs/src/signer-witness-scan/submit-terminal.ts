import type { LucidEvolution, Script, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  type SignerWitnessScanEvidence,
  signerWitnessVerdictCloses,
} from "./evidence.js";
import {
  SignerWitnessTerminalDatumSchema,
  SignerWitnessTerminalRedeemerSchema,
} from "./schemas.js";
import {
  SIGNER_WITNESS_TERMINAL_STEP_INDEX,
  type SignerWitnessScanContracts,
} from "./submit-scan.js";

export type SignerWitnessTerminalContracts = SignerWitnessScanContracts & {
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
};

/** Finalizes a signer-witness verdict that contradicts the block: burns the
 * thread token and mints the fraud proof. */
export const buildSignerWitnessTerminal = async ({
  lucid,
  family,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly family: string;
  readonly contracts: SignerWitnessTerminalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: SignerWitnessScanEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  if (!signerWitnessVerdictCloses(evidence))
    throw new Error(
      `${family}: terminal verdict does not contradict the block`,
    );
  const stepIndex = SIGNER_WITNESS_TERMINAL_STEP_INDEX;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    signer_required: boolean;
    signer_present: boolean;
  }>({
    threadUtxo,
    signer,
    schema: SignerWitnessTerminalDatumSchema as never,
    family,
    stepIndex,
  });
  if (
    state.signer_required !== evidence.signerRequired ||
    state.signer_present !== evidence.signerPresent
  )
    throw new Error(`${family}: terminal signer verdict changed`);
  return await buildLinearFaultFinalize({
    lucid,
    family,
    stepIndex,
    step: contracts.steps[stepIndex]!,
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: SignerWitnessTerminalRedeemerSchema,
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
  });
};
