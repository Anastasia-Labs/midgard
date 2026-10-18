import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
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

/**
 * The network ids the catalogue's network-id fault proof can be parameterized
 * with: 0 for testnets and 1 for mainnet. `step_01`/`forced_step` used to
 * re-check `expected_network_id` against this domain on every execution;
 * deployment parameterization is trusted on chain, so the check lives here.
 */
export const SUPPORTED_NETWORK_IDS: ReadonlySet<bigint> = new Set([0n, 1n]);

export const NETWORK_ID_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofNetworkId"),
  forcedStep: deployedTitle("fraudProofNetworkIdForcedStep"),
  forcedScan: deployedTitle("fraudProofNetworkIdForcedScan"),
  step02: deployedTitle("fraudProofNetworkIdStep02"),
} as const;

export type NetworkIdFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly networkId: FraudProofChain & {
    readonly forcedStep: SpendingValidator;
    readonly forcedScan: SpendingValidator;
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
    if (!SUPPORTED_NETWORK_IDS.has(expectedNetworkId)) {
      return yield* Effect.fail(
        new Error(
          `Network-id fault proof cannot be deployed for network id ${expectedNetworkId.toString()}`,
        ),
      );
    }
    // The forced door hands a rejected leaf to the resumable outputs scan,
    // which hands it to step 02, and an accepted leaf straight to step 02:
    // the parameterization chain is therefore step 02 -> forced scan ->
    // forced door, and it is built in that order.
    const forcedScan = yield* buildFaultProofSpendingStep(
      context,
      NETWORK_ID_FAULT_PROOF_TITLES.forcedScan,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build network-id forced scan",
    );
    const forcedStep = yield* buildFaultProofSpendingStep(
      context,
      NETWORK_ID_FAULT_PROOF_TITLES.forcedStep,
      [
        forcedScan.spendingScriptHash,
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
    // `forcedStep` and `forcedScan` are compiled and parameterized like any
    // other step, but they are deliberately NOT members of `steps`. `steps` is the linear chain the
    // deployer walks, and the canonical deployment ABI names it separately as
    // `fraudProofNetworkIdForcedStep` (role "V1 fraud-proof network-id forced
    // step") rather than as a third link: the forced door is a side entrance
    // into step 02. It is returned by name so the deployment manifest, the
    // reference-script publication set and every caller that needs it reach it
    // without widening the chain. `forcedScan` is named the same way, as
    // `fraudProofNetworkIdForcedScan` (role "V1 fraud-proof network-id forced
    // scan").
    return {
      firstStep: step01,
      forcedStep,
      forcedScan,
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
