import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../linear-fault-cancel-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { RedeemerCanonicityContracts } from "./contracts-v1.js";

export const submitRedeemerCanonicityCancel = async (input: {
  readonly lucid: LucidEvolution;
  readonly contracts: RedeemerCanonicityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) =>
  await submitLinearFaultCancel({
    ...input,
    family: "redeemer-canonicity",
    steps: input.contracts.steps,
    computationThread: input.contracts.computationThread,
  });
