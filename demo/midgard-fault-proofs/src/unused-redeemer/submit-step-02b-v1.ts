import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerAuthenticationV1 } from "./authentication-v1.js";
import type { UnusedRedeemerContractsV1 } from "./contracts-v1.js";
import {
  UnusedRedeemerStep02bDatumV1Schema,
  UnusedRedeemerStep02bRedeemerV1Schema,
  UnusedRedeemerStep02cDatumV1Schema,
} from "./schemas-v1.js";
import { submitUnusedRedeemerLinearSplitV1 } from "./submit-linear-split-v1.js";
export const submitUnusedRedeemerStep02bV1 = async (input: {
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
    stepIndex: 3,
    nextState: input.authentication.headerState,
    sourceDatumSchema: UnusedRedeemerStep02bDatumV1Schema,
    nextDatumSchema: UnusedRedeemerStep02cDatumV1Schema,
    redeemerSchema: UnusedRedeemerStep02bRedeemerV1Schema,
    redeemerFields: {
      item_control: input.authentication.itemControl,
      chunk_proof: input.authentication.headerChunkProof,
      next_chunk_proof: input.authentication.headerNextChunkProof,
    },
  });
