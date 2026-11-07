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

export const REDEEMER_CANONICITY_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofRedeemerCanonicity"),
  step02: deployedTitle("fraudProofRedeemerCanonicityStep02"),
  step02Data: deployedTitle("fraudProofRedeemerCanonicityStep02Data"),
  step03: deployedTitle("fraudProofRedeemerCanonicityStep03"),
} as const;

export type RedeemerCanonicityFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly redeemerCanonicity: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildRedeemerCanonicityFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildRedeemerCanonicityChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: CertifiedFaultProofChainInputs): Effect.Effect<
  RedeemerCanonicityFaultProofContracts["redeemerCanonicity"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      REDEEMER_CANONICITY_FAULT_PROOF_TITLES.step03,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build redeemer-canonicity step 03",
    );
    const step02Data = yield* buildFaultProofSpendingStep(
      context,
      REDEEMER_CANONICITY_FAULT_PROOF_TITLES.step02Data,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build redeemer-canonicity step 02 data",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      REDEEMER_CANONICITY_FAULT_PROOF_TITLES.step02,
      [
        step02Data.spendingScriptHash,
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build redeemer-canonicity step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      REDEEMER_CANONICITY_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build redeemer-canonicity step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step02Data, step03] };
  });

export const buildRedeemerCanonicityFaultProofContracts = (
  params: BuildRedeemerCanonicityFaultProofContractsParams,
): Effect.Effect<RedeemerCanonicityFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const redeemerCanonicity = yield* buildRedeemerCanonicityChain({
      ...params,
      ...shared,
    });
    return { ...shared, redeemerCanonicity };
  });
