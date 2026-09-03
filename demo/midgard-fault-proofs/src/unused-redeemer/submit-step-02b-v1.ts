import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerAuthentication } from "./authentication-v1.js";
import type { UnusedRedeemerContracts } from "./contracts-v1.js";
import {
  UnusedRedeemerStep02bDatumSchema,
  UnusedRedeemerStep02bRedeemerSchema,
  UnusedRedeemerStep02cDatumSchema,
} from "./schemas-v1.js";
import { submitUnusedRedeemerLinearSplit } from "./submit-linear-split-v1.js";
export const submitUnusedRedeemerStep02b = async (input: {
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
    stepIndex: 3,
    nextState: input.authentication.headerState,
    sourceDatumSchema: UnusedRedeemerStep02bDatumSchema,
    nextDatumSchema: UnusedRedeemerStep02cDatumSchema,
    redeemerSchema: UnusedRedeemerStep02bRedeemerSchema,
    redeemerFields: {
      item_control: input.authentication.itemControl,
      chunk_proof: input.authentication.headerChunkProof,
      next_chunk_proof: input.authentication.headerNextChunkProof,
    },
  });
