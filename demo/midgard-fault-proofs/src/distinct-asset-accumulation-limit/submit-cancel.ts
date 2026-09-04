import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { DistinctAssetAccumulationContracts } from "./contracts.js";

/** Cancels any one of the six live physical thread steps. */
export const submitDistinctAssetAccumulationCancel = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DistinctAssetAccumulationContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) =>
  await submitLinearFaultCancel({
    lucid,
    family: "distinct-asset-accumulation-limit",
    steps: contracts.steps,
    computationThread: contracts.computationThread,
    categoryId,
    signer,
    threadOutRef,
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
