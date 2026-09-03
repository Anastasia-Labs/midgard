import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerAuthentication } from "./authentication-v1.js";
import type { UnusedRedeemerContracts } from "./contracts-v1.js";
import {
  UnusedRedeemerStep02aDatumSchema,
  UnusedRedeemerStep02DatumSchema,
  UnusedRedeemerStep02RedeemerSchema,
} from "./schemas-v1.js";
import { submitUnusedRedeemerLinearSplit } from "./submit-linear-split-v1.js";
export type { UnusedRedeemerAuthentication } from "./authentication-v1.js";
export const submitUnusedRedeemerStep02 = async (input: {
  lucid: LucidEvolution;
  contracts: UnusedRedeemerContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  authentication: UnusedRedeemerAuthentication;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
  evidence?: unknown;
}) =>
  submitUnusedRedeemerLinearSplit({
    ...input,
    stepIndex: 1,
    nextState: input.authentication.descriptorState,
    sourceDatumSchema: UnusedRedeemerStep02DatumSchema,
    nextDatumSchema: UnusedRedeemerStep02aDatumSchema,
    redeemerSchema: UnusedRedeemerStep02RedeemerSchema,
    redeemerFields: { trace_membership: input.authentication.traceMembership },
  });
