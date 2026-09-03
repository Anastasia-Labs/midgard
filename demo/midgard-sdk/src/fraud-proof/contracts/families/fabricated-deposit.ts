import { Data, Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeSpendingValidator,
  tryBuild,
} from "../blueprint.js";
import { buildSharedFaultProofContracts } from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const FABRICATED_DEPOSIT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/fabricated_deposit/step_01.main.spend",
  step02: "fraud_proofs/fabricated_deposit/step_02.main.spend",
  step03: "fraud_proofs/fabricated_deposit/step_03.main.spend",
  step04: "fraud_proofs/fabricated_deposit/step_04.main.spend",
} as const;

/**
 * Q39 `fabricated-deposit`: a committed `deposits_root` leaf that is not the
 * authentic L1 deposit event pair.
 */
export type FabricatedDepositFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fabricatedDeposit: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildFabricatedDepositFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildFabricatedDepositChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  FabricatedDepositFaultProofContracts["fabricatedDeposit"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build fabricated-deposit step 04",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step04,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build fabricated-deposit step 03",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step03,
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build fabricated-deposit step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step02,
            [
              step03.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build fabricated-deposit step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_DEPOSIT_FAULT_PROOF_TITLES.step01,
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

export const buildFabricatedDepositFaultProofContracts = (
  params: BuildFabricatedDepositFaultProofContractsParams,
): Effect.Effect<FabricatedDepositFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const fabricatedDeposit = yield* buildFabricatedDepositChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      fabricatedDeposit,
    };
  });
