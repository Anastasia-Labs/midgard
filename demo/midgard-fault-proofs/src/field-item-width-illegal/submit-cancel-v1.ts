import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../linear-fault-cancel-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { FieldItemWidthIllegalContracts } from "./contracts-v1.js";

export const submitFieldItemWidthIllegalCancel = async ({
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
  readonly contracts: FieldItemWidthIllegalContracts;
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
    family: "field-item-width-illegal",
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
