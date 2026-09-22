import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { UnusedRedeemerAuthentication } from "./authentication.js";
import { initialUnusedRedeemerReverseScan } from "./checkpoint.js";
import type { UnusedRedeemerContracts } from "./contracts.js";
import {
  UnusedRedeemerStep04DatumSchema,
  UnusedRedeemerStep04RedeemerSchema,
  UnusedRedeemerStep05DatumSchema,
} from "./schemas.js";
import { submitUnusedRedeemerLinearSplit } from "./submit-linear-split.js";
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
