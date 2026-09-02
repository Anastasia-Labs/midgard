import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerAuthenticationV1 } from "./authentication-v1.js";
import { initialUnusedRedeemerReverseScanV1 } from "./checkpoint-v1.js";
import type { UnusedRedeemerContractsV1 } from "./contracts-v1.js";
import {
  UnusedRedeemerStep04DatumV1Schema,
  UnusedRedeemerStep04RedeemerV1Schema,
  UnusedRedeemerStep05DatumV1Schema,
} from "./schemas-v1.js";
import { submitUnusedRedeemerLinearSplitV1 } from "./submit-linear-split-v1.js";
export const submitUnusedRedeemerStep04V1 = async (input: {
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
    stepIndex: 6,
    nextState: initialUnusedRedeemerReverseScanV1(
      input.authentication.authenticatedState,
    ),
    sourceDatumSchema: UnusedRedeemerStep04DatumV1Schema,
    nextDatumSchema: UnusedRedeemerStep05DatumV1Schema,
    redeemerSchema: UnusedRedeemerStep04RedeemerV1Schema,
    redeemerFields: {},
  });
