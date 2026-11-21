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

export const MINT_AUTHORIZATION_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofMintAuthorization"),
  step02: deployedTitle("fraudProofMintAuthorizationStep02"),
  step03: deployedTitle("fraudProofMintAuthorizationStep03"),
  step04: deployedTitle("fraudProofMintAuthorizationStep04"),
  step05: deployedTitle("fraudProofMintAuthorizationStep05"),
  evaluate: deployedTitle("fraudProofMintAuthorizationStep06"),
  witnessScan: deployedTitle("fraudProofMintAuthorizationStep07"),
} as const;

export type MintAuthorizationFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly mintAuthorization: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildMintAuthorizationFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildMintAuthorizationChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MintAuthorizationFaultProofContracts["mintAuthorization"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step05,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build mint-authorization step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step04,
      [
        step05.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build mint-authorization step 04",
    );
    const witnessScan = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.witnessScan,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build mint witness scan",
    );
    const evaluate = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.evaluate,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build mint authorization evaluator",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        step05.spendingScriptHash,
        evaluate.spendingScriptHash,
        witnessScan.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build mint-authorization step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build mint-authorization step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build mint-authorization step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05, evaluate, witnessScan],
    };
  });

export const buildMintAuthorizationFaultProofContracts = (
  params: BuildMintAuthorizationFaultProofContractsParams,
): Effect.Effect<MintAuthorizationFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const mintAuthorization = yield* buildMintAuthorizationChain({
      ...params,
      ...shared,
    });
    return { ...shared, mintAuthorization };
  });
