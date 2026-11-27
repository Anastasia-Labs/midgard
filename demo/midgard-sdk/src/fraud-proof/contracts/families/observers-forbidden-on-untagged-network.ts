import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import type { CertifiedFaultProofChainInputs } from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofObserversForbiddenOnUntaggedNetwork"),
  step02: deployedTitle("fraudProofObserversForbiddenOnUntaggedNetworkStep02"),
} as const;

export type ObserversForbiddenOnUntaggedNetworkFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly observersForbiddenOnUntaggedNetwork: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildObserversForbiddenOnUntaggedNetworkFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildObserversForbiddenOnUntaggedNetworkChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  ObserversForbiddenOnUntaggedNetworkFaultProofContracts["observersForbiddenOnUntaggedNetwork"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build observers-forbidden-on-untagged-network step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build observers-forbidden-on-untagged-network step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

export const buildObserversForbiddenOnUntaggedNetworkFaultProofContracts = (
  params: BuildObserversForbiddenOnUntaggedNetworkFaultProofContractsParams,
): Effect.Effect<
  ObserversForbiddenOnUntaggedNetworkFaultProofContracts,
  Error
> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const observersForbiddenOnUntaggedNetwork =
      yield* buildObserversForbiddenOnUntaggedNetworkChain({
        ...params,
        ...shared,
      });
    return { ...shared, observersForbiddenOnUntaggedNetwork };
  });
