import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { RedeemerCanonicityContractsV1 } from "./contracts-v1.js";

export const submitRedeemerCanonicityCancelV1 = async (input: {
  readonly lucid: LucidEvolution;
  readonly contracts: RedeemerCanonicityContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) =>
  await submitLinearFaultCancelV1({
    ...input,
    family: "redeemer-canonicity",
    steps: input.contracts.steps,
    computationThread: input.contracts.computationThread,
  });
