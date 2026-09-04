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
  forcedStep: "fraud_proofs/network_id/forced_step.main.spend",
  step02: "fraud_proofs/network_id/step_02.main.spend",
} as const;

export type NetworkIdFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly networkId: FraudProofChain & {
    readonly forcedStep: SpendingValidator;
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
    const expectedNetworkId = network === "Mainnet" ? 1n : 0n;
    const forcedStep = yield* buildFaultProofSpendingStep(
      context,
      NETWORK_ID_FAULT_PROOF_TITLES.forcedStep,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        expectedNetworkId,
      ],
      "Failed to build network-id forced step",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      NETWORK_ID_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        forcedStep.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
        expectedNetworkId,
      ],
      "Failed to build network-id step 01",
    );
    // `forcedStep` is compiled and parameterized like any other step, but it
    // is deliberately NOT a member of `steps`. `steps` is the linear chain the
    // deployer walks, and the canonical deployment ABI names
    // `fraudProofNetworkId` and `fraudProofNetworkIdStep02` only; the forced
    // door is a side entrance into step 02 rather than a third link, and the
    // family's own deployment shape in `midgard-fault-proofs` already carries
    // it as a separate optional contract. It is returned by name so callers
    // that need it can reach it without widening the chain.
    return {
      firstStep: step01,
      forcedStep,
      steps: [step01, step02],
    };
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
