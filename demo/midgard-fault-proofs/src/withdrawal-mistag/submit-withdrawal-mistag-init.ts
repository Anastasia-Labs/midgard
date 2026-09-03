/** Pre-registration Init; reuses the protocol-generic catalogue/PHAS builder. */
import type { LucidEvolution, Network } from "@lucid-evolution/lucid";

import {
  type NativeScriptDecodingInitContracts,
  submitNativeScriptDecodingInit,
} from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  type WithdrawalMistagCatalogueCategory,
  type WithdrawalMistagContracts,
} from "./contracts.js";

const genericInitContracts = (
  contracts: WithdrawalMistagContracts,
): NativeScriptDecodingInitContracts => ({
  steps: [
    contracts.steps[0],
    contracts.steps[1],
    contracts.steps[2],
    contracts.steps[3],
  ],
  computationThread: contracts.computationThread,
  fraudProof: contracts.fraudProof,
  hubOraclePolicyId: contracts.hubOraclePolicyId,
  stateQueuePolicyId: contracts.stateQueuePolicyId,
  // Not read by the generic Init transaction.
  fieldPreimageCertificatePolicyId: "00".repeat(28),
});

export const submitWithdrawalMistagInit = async ({
  contracts,
  category,
  ...args
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: WithdrawalMistagContracts;
  readonly category: WithdrawalMistagCatalogueCategory;
  readonly catalogue: {
    readonly policyId: string;
    readonly spendingScriptAddress: string;
    readonly root: string;
  };
  readonly signer: ResolvedProverSigner;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash?: string;
  /**
   * Published witness reference scripts, forwarded to the generic init;
   * every role used by this transaction must be published.
   */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) =>
  await submitNativeScriptDecodingInit({
    ...args,
    contracts: genericInitContracts(contracts),
    category,
  });
