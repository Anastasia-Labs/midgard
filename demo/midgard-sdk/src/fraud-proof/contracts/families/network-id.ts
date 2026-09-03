import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
  type SharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const NETWORK_ID_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/network_id/step_01.main.spend",
  step02: "fraud_proofs/network_id/step_02.main.spend",
} as const;

export type NetworkIdFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly networkId: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildNetworkIdFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildNetworkIdChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  NetworkIdFaultProofContracts["networkId"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      NETWORK_ID_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build network-id step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      NETWORK_ID_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
        network === "Mainnet" ? 1n : 0n,
      ],
      "Failed to build network-id step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

export const buildNetworkIdFaultProofContracts = (
  params: BuildNetworkIdFaultProofContractsParams,
): Effect.Effect<NetworkIdFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const networkId = yield* buildNetworkIdChain({
      ...params,
      ...shared,
    });
    return { ...shared, networkId };
  });
