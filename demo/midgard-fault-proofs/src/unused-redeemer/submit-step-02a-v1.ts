import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerAuthenticationV1 } from "./authentication-v1.js";
import type { UnusedRedeemerContractsV1 } from "./contracts-v1.js";
import {
  UnusedRedeemerStep02aDatumV1Schema,
  UnusedRedeemerStep02aRedeemerV1Schema,
  UnusedRedeemerStep02bDatumV1Schema,
} from "./schemas-v1.js";
import { submitUnusedRedeemerLinearSplitV1 } from "./submit-linear-split-v1.js";
export const submitUnusedRedeemerStep02aV1 = async (input: {
  lucid: LucidEvolution;
  contracts: UnusedRedeemerContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  authentication: UnusedRedeemerAuthenticationV1;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) =>
  submitUnusedRedeemerLinearSplitV1({
    ...input,
    stepIndex: 2,
    nextState: input.authentication.controlState,
    sourceDatumSchema: UnusedRedeemerStep02aDatumV1Schema,
    nextDatumSchema: UnusedRedeemerStep02bDatumV1Schema,
    redeemerSchema: UnusedRedeemerStep02aRedeemerV1Schema,
    redeemerFields: {
      machine_state: input.authentication.machineState,
      trace_proof: input.authentication.traceProof,
      control: input.authentication.control,
    },
  });
