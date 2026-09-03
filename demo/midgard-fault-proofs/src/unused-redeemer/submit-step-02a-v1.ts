import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerAuthentication } from "./authentication-v1.js";
import type { UnusedRedeemerContracts } from "./contracts-v1.js";
import {
  UnusedRedeemerStep02aDatumSchema,
  UnusedRedeemerStep02aRedeemerSchema,
  UnusedRedeemerStep02bDatumSchema,
} from "./schemas-v1.js";
import { submitUnusedRedeemerLinearSplit } from "./submit-linear-split-v1.js";
export const submitUnusedRedeemerStep02a = async (input: {
  lucid: LucidEvolution;
  contracts: UnusedRedeemerContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  authentication: UnusedRedeemerAuthentication;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) =>
  submitUnusedRedeemerLinearSplit({
    ...input,
    stepIndex: 2,
    nextState: input.authentication.controlState,
    sourceDatumSchema: UnusedRedeemerStep02aDatumSchema,
    nextDatumSchema: UnusedRedeemerStep02bDatumSchema,
    redeemerSchema: UnusedRedeemerStep02aRedeemerSchema,
    redeemerFields: {
      machine_state: input.authentication.machineState,
      trace_proof: input.authentication.traceProof,
      control: input.authentication.control,
    },
  });
