import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { UnusedRedeemerAuthentication } from "./authentication.js";
import type { UnusedRedeemerContracts } from "./contracts.js";
import {
  UnusedRedeemerStep02cDatumSchema,
  UnusedRedeemerStep02cRedeemerSchema,
  UnusedRedeemerStep03DatumSchema,
} from "./schemas.js";
import { submitUnusedRedeemerLinearSplit } from "./submit-linear-split.js";
export const submitUnusedRedeemerStep02c = async (input: {
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
    stepIndex: 4,
    nextState: input.authentication.authenticatedState,
    sourceDatumSchema: UnusedRedeemerStep02cDatumSchema,
    nextDatumSchema: UnusedRedeemerStep03DatumSchema,
    redeemerSchema: UnusedRedeemerStep02cRedeemerSchema,
    redeemerFields: {
      chunk_proof: input.authentication.tailChunkProof,
      next_chunk_proof: input.authentication.tailNextChunkProof,
    },
  });
