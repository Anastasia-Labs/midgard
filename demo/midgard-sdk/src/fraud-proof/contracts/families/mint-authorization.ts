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
  step01: "fraud_proofs/mint_authorization/step_01.main.spend",
  step02: "fraud_proofs/mint_authorization/step_02.main.spend",
  step03: "fraud_proofs/mint_authorization/step_03.main.spend",
  step04: "fraud_proofs/mint_authorization/step_04.main.spend",
  step05: "fraud_proofs/mint_authorization/step_05.main.spend",
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
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MINT_AUTHORIZATION_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        step05.spendingScriptHash,
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
      steps: [step01, step02, step03, step04, step05],
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
