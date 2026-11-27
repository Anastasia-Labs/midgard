import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { ObserverOrderInvalidContracts } from "./contracts.js";
import {
  type ObserverOrderInvalidEvidence,
  observerOrderInvalidEvidenceCloses,
} from "./family.js";
import {
  ObserverOrderInvalidStep04DatumSchema,
  ObserverOrderInvalidStep04RedeemerSchema,
} from "./schemas.js";

export const buildObserverOrderInvalidStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserverOrderInvalidEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  if (!observerOrderInvalidEvidenceCloses(evidence))
    throw new Error("observerOrderInvalid: terminal evidence is honest");
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "observer-order-invalid",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    observer_index: bigint;
    violation: boolean;
  }>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep04DatumSchema as never,
    family: "observer-order-invalid",
    stepIndex,
  });
  if (
    state.observer_index !== BigInt(evidence.observerIndex) ||
    state.violation !== evidence.violation
  )
    throw new Error(
      "observerOrderInvalid: terminal datum differs from evidence",
    );
  return await buildLinearFaultFinalize({
    lucid,
    family: "observer-order-invalid",
    stepIndex,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ObserverOrderInvalidStep04RedeemerSchema,
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
