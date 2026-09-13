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

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/distinct_asset_accumulation_limit/step_01.main.spend",
  step02: "fraud_proofs/distinct_asset_accumulation_limit/step_02.main.spend",
  step03: "fraud_proofs/distinct_asset_accumulation_limit/step_03.main.spend",
  step04: "fraud_proofs/distinct_asset_accumulation_limit/step_04.main.spend",
  step05: "fraud_proofs/distinct_asset_accumulation_limit/step_05.main.spend",
  step06: "fraud_proofs/distinct_asset_accumulation_limit/step_06.main.spend",
} as const;

export type DistinctAssetAccumulationLimitFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly distinctAssetAccumulationLimit: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildDistinctAssetAccumulationLimitFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildDistinctAssetAccumulationLimitChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  DistinctAssetAccumulationLimitFaultProofContracts["distinctAssetAccumulationLimit"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step06 = yield* buildFaultProofSpendingStep(
      context,
      DISTINCT_ASSET_ACCUMULATION_LIMIT_FAULT_PROOF_TITLES.step06,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build distinct-asset-accumulation-limit step 06",
    );
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      DISTINCT_ASSET_ACCUMULATION_LIMIT_FAULT_PROOF_TITLES.step05,
      [step06.spendingScriptHash, computationThread.policyId],
      "Failed to build distinct-asset-accumulation-limit step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      DISTINCT_ASSET_ACCUMULATION_LIMIT_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build distinct-asset-accumulation-limit step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      DISTINCT_ASSET_ACCUMULATION_LIMIT_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build distinct-asset-accumulation-limit step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      DISTINCT_ASSET_ACCUMULATION_LIMIT_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build distinct-asset-accumulation-limit step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      DISTINCT_ASSET_ACCUMULATION_LIMIT_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build distinct-asset-accumulation-limit step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05, step06],
    };
  });

export const buildDistinctAssetAccumulationLimitFaultProofContracts = (
  params: BuildDistinctAssetAccumulationLimitFaultProofContractsParams,
): Effect.Effect<DistinctAssetAccumulationLimitFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const distinctAssetAccumulationLimit =
      yield* buildDistinctAssetAccumulationLimitChain({ ...params, ...shared });
    return { ...shared, distinctAssetAccumulationLimit };
  });
