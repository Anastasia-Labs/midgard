import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingScriptSourceContractsV1 } from "./contracts-v1.js";
import {
  missingScriptSourceEvidenceClosesV1,
  type MissingScriptSourceEvidenceV1,
} from "./family-v1.js";
import {
  ExecutionSourceScanStateV1Schema,
  ExecutionSourceStep06DatumV1Schema,
  ExecutionSourceStep06RedeemerV1Schema,
} from "./schemas-v1.js";
import { missingScriptSourceOnchainCheckpointV1 } from "./universe-scan-v1.js";

const FAMILY = "missing-script-source";
export const submitMissingScriptSourceStep06V1 = async ({
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
  lucid: LucidEvolution;
  contracts: MissingScriptSourceContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingScriptSourceEvidenceV1;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 5;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<
    Data.Static<typeof ExecutionSourceScanStateV1Schema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep06DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    !missingScriptSourceEvidenceClosesV1(evidence) ||
    state.cursor !== state.authenticated.purpose.scan_limit ||
    state.cursor !== BigInt(evidence.sources.length) ||
    state.found !== (evidence.foundAtSourceIndex !== null) ||
    state.checkpoint_hash !==
      missingScriptSourceOnchainCheckpointV1({
        sourceIdentityHex: state.authenticated.source_identity_hash,
        cursor: state.cursor,
        found: state.found,
        nextExpectedScriptHashHex: state.next_expected_script_hash,
      })
  )
    throw new Error(
      `${FAMILY}: terminal state is not the retained contradiction`,
    );
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ExecutionSourceStep06RedeemerV1Schema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
