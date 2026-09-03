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

export const FABRICATED_WITHDRAWAL_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/fabricated_withdrawal/step_01.main.spend",
  step02: "fraud_proofs/fabricated_withdrawal/step_02.main.spend",
  step03: "fraud_proofs/fabricated_withdrawal/step_03.main.spend",
  step04: "fraud_proofs/fabricated_withdrawal/step_04.main.spend",
} as const;

/**
 * Q40 `fabricated-withdrawal`: a committed `withdrawals_root` leaf that is not
 * the authentic L1 withdrawal order pair.
 */
export type FabricatedWithdrawalFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fabricatedWithdrawal: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildFabricatedWithdrawalFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildFabricatedWithdrawalChain = ({
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
  FabricatedWithdrawalFaultProofContracts["fabricatedWithdrawal"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build fabricated-withdrawal step 04",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_WITHDRAWAL_FAULT_PROOF_TITLES.step04,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build fabricated-withdrawal step 03",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_WITHDRAWAL_FAULT_PROOF_TITLES.step03,
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build fabricated-withdrawal step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_WITHDRAWAL_FAULT_PROOF_TITLES.step02,
            [
              step03.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build fabricated-withdrawal step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            FABRICATED_WITHDRAWAL_FAULT_PROOF_TITLES.step01,
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

export const buildFabricatedWithdrawalFaultProofContracts = (
  params: BuildFabricatedWithdrawalFaultProofContractsParams,
): Effect.Effect<FabricatedWithdrawalFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const fabricatedWithdrawal = yield* buildFabricatedWithdrawalChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      fabricatedWithdrawal,
    };
  });
