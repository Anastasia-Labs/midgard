import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { UnusedRedeemerAuthentication } from "./authentication.js";
import type { UnusedRedeemerContracts } from "./contracts.js";
import {
  UnusedRedeemerStep02aDatumSchema,
  UnusedRedeemerStep02DatumSchema,
  UnusedRedeemerStep02RedeemerSchema,
} from "./schemas.js";
import { submitUnusedRedeemerLinearSplit } from "./submit-linear-split.js";
export type { UnusedRedeemerAuthentication } from "./authentication.js";
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
