/** Pre-registration Init; reuses the protocol-generic catalogue/PHAS builder. */
import type { LucidEvolution, Network } from "@lucid-evolution/lucid";

import type { NativeScriptDecodingContractsV1 } from "../native-script-decoding/contracts-v1.js";
import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  type WithdrawalMistagCatalogueCategoryV1,
  type WithdrawalMistagContractsV1,
} from "./contracts-v1.js";

const genericInitContracts = (
  contracts: WithdrawalMistagContractsV1,
): NativeScriptDecodingContractsV1 => ({
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
  readonly contracts: WithdrawalMistagContractsV1;
  readonly category: WithdrawalMistagCatalogueCategoryV1;
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
   * each absent entry inline-attaches.
   */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}) =>
  await submitNativeScriptDecodingInit({
    ...args,
    contracts: genericInitContracts(contracts),
    category,
  });
