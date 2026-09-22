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

export const INVALID_RANGE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/invalid_range/step_01.main.spend",
  step02: "fraud_proofs/invalid_range/step_02.main.spend",
} as const;

export type InvalidRangeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly invalidRange: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildInvalidRangeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildInvalidRangeChain = ({
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
}): Effect.Effect<InvalidRangeFaultProofContracts["invalidRange"], Error> =>
  Effect.gen(function* () {
    const invalidRangeStep02 = yield* tryBuild(
      "Failed to build invalid-range step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            INVALID_RANGE_FAULT_PROOF_TITLES.step02,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const invalidRangeStep01 = yield* tryBuild(
      "Failed to build invalid-range step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            INVALID_RANGE_FAULT_PROOF_TITLES.step01,
            [
              invalidRangeStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: invalidRangeStep01,
      steps: [invalidRangeStep01, invalidRangeStep02],
    };
  });

export const buildInvalidRangeFaultProofContracts = (
  params: BuildInvalidRangeFaultProofContractsParams,
): Effect.Effect<InvalidRangeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const invalidRange = yield* buildInvalidRangeChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      invalidRange,
    };
  });
