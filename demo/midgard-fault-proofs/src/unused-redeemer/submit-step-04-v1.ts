import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerAuthentication } from "./authentication-v1.js";
import { initialUnusedRedeemerReverseScan } from "./checkpoint-v1.js";
import type { UnusedRedeemerContracts } from "./contracts-v1.js";
import {
  UnusedRedeemerStep04DatumSchema,
  UnusedRedeemerStep04RedeemerSchema,
  UnusedRedeemerStep05DatumSchema,
} from "./schemas-v1.js";
import { submitUnusedRedeemerLinearSplit } from "./submit-linear-split-v1.js";
export const submitUnusedRedeemerStep04 = async (input: {
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
    stepIndex: 6,
    nextState: initialUnusedRedeemerReverseScan(
      input.authentication.authenticatedState,
    ),
    sourceDatumSchema: UnusedRedeemerStep04DatumSchema,
    nextDatumSchema: UnusedRedeemerStep05DatumSchema,
    redeemerSchema: UnusedRedeemerStep04RedeemerSchema,
    redeemerFields: {},
  });
