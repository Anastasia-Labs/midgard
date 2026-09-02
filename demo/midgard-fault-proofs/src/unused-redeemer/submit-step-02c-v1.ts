import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerAuthenticationV1 } from "./authentication-v1.js";
import type { UnusedRedeemerContractsV1 } from "./contracts-v1.js";
import {
  UnusedRedeemerStep02cDatumV1Schema,
  UnusedRedeemerStep02cRedeemerV1Schema,
  UnusedRedeemerStep03DatumV1Schema,
} from "./schemas-v1.js";
import { submitUnusedRedeemerLinearSplitV1 } from "./submit-linear-split-v1.js";
export const submitUnusedRedeemerStep02cV1 = async (input: {
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
    stepIndex: 4,
    nextState: input.authentication.authenticatedState,
    sourceDatumSchema: UnusedRedeemerStep02cDatumV1Schema,
    nextDatumSchema: UnusedRedeemerStep03DatumV1Schema,
    redeemerSchema: UnusedRedeemerStep02cRedeemerV1Schema,
    redeemerFields: {
      chunk_proof: input.authentication.tailChunkProof,
      next_chunk_proof: input.authentication.tailNextChunkProof,
    },
  });
