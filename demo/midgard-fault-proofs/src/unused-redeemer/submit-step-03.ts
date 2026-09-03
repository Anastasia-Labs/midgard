import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { UnusedRedeemerAuthentication } from "./authentication.js";
import { initialUnusedRedeemerReverseScan } from "./checkpoint.js";
import type { UnusedRedeemerContracts } from "./contracts.js";
import {
  UnusedRedeemerStep03DatumSchema,
  UnusedRedeemerStep03RedeemerSchema,
  UnusedRedeemerStep04DatumSchema,
} from "./schemas.js";
import { submitUnusedRedeemerLinearSplit } from "./submit-linear-split.js";
export const submitUnusedRedeemerStep03 = async (input: {
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
    stepIndex: 5,
    nextState: initialUnusedRedeemerReverseScan(
      input.authentication.authenticatedState,
    ),
    sourceDatumSchema: UnusedRedeemerStep03DatumSchema,
    nextDatumSchema: UnusedRedeemerStep04DatumSchema,
    redeemerSchema: UnusedRedeemerStep03RedeemerSchema,
    redeemerFields: {},
  });
