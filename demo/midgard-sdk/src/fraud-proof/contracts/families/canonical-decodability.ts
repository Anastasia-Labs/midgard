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

export const CANONICAL_DECODABILITY_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofCanonicalDecodability"),
  step02: deployedTitle("fraudProofCanonicalDecodabilityStep02"),
} as const;

export type CanonicalDecodabilityFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly canonicalDecodability: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildCanonicalDecodabilityFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildCanonicalDecodabilityChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  CanonicalDecodabilityFaultProofContracts["canonicalDecodability"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      CANONICAL_DECODABILITY_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build canonical-decodability step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      CANONICAL_DECODABILITY_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build canonical-decodability step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

export const buildCanonicalDecodabilityFaultProofContracts = (
  params: BuildCanonicalDecodabilityFaultProofContractsParams,
): Effect.Effect<CanonicalDecodabilityFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const canonicalDecodability = yield* buildCanonicalDecodabilityChain({
      ...params,
      ...shared,
    });
    return { ...shared, canonicalDecodability };
  });
