import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerAuthenticationV1 } from "./authentication-v1.js";
import type { UnusedRedeemerContractsV1 } from "./contracts-v1.js";
import {
  UnusedRedeemerStep02aDatumV1Schema,
  UnusedRedeemerStep02DatumV1Schema,
  UnusedRedeemerStep02RedeemerV1Schema,
} from "./schemas-v1.js";
import { submitUnusedRedeemerLinearSplitV1 } from "./submit-linear-split-v1.js";
export type { UnusedRedeemerAuthenticationV1 } from "./authentication-v1.js";
export const submitUnusedRedeemerStep02V1 = async (input: {
  lucid: LucidEvolution;
  contracts: UnusedRedeemerContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  authentication: UnusedRedeemerAuthenticationV1;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
  evidence?: unknown;
}) =>
  submitUnusedRedeemerLinearSplitV1({
    ...input,
    stepIndex: 1,
    nextState: input.authentication.descriptorState,
    sourceDatumSchema: UnusedRedeemerStep02DatumV1Schema,
    nextDatumSchema: UnusedRedeemerStep02aDatumV1Schema,
    redeemerSchema: UnusedRedeemerStep02RedeemerV1Schema,
    redeemerFields: { trace_membership: input.authentication.traceMembership },
  });
