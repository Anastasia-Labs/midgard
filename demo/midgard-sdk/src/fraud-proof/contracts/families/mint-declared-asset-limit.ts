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

export const MINT_DECLARED_ASSET_LIMIT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/mint_declared_asset_limit/step_01.main.spend",
  step02: "fraud_proofs/mint_declared_asset_limit/step_02.main.spend",
  step03: "fraud_proofs/mint_declared_asset_limit/step_03.main.spend",
  step04: "fraud_proofs/mint_declared_asset_limit/step_04.main.spend",
} as const;

export type MintDeclaredAssetLimitFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly mintDeclaredAssetLimit: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildMintDeclaredAssetLimitFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildMintDeclaredAssetLimitChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MintDeclaredAssetLimitFaultProofContracts["mintDeclaredAssetLimit"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MINT_DECLARED_ASSET_LIMIT_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build mint-declared-asset-limit step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MINT_DECLARED_ASSET_LIMIT_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build mint-declared-asset-limit step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MINT_DECLARED_ASSET_LIMIT_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build mint-declared-asset-limit step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MINT_DECLARED_ASSET_LIMIT_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build mint-declared-asset-limit step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03, step04] };
  });

export const buildMintDeclaredAssetLimitFaultProofContracts = (
  params: BuildMintDeclaredAssetLimitFaultProofContractsParams,
): Effect.Effect<MintDeclaredAssetLimitFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const mintDeclaredAssetLimit = yield* buildMintDeclaredAssetLimitChain({
      ...params,
      ...shared,
    });
    return { ...shared, mintDeclaredAssetLimit };
  });
