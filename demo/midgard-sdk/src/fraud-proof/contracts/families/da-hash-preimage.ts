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

export const DA_HASH_PREIMAGE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/da_hash_preimage/step_01.main.spend",
  step02: "fraud_proofs/da_hash_preimage/step_02.main.spend",
} as const;

export type DaHashPreimageFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly daHashPreimage: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildDaHashPreimageFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildDaHashPreimageChain = ({
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
}): Effect.Effect<DaHashPreimageFaultProofContracts["daHashPreimage"], Error> =>
  Effect.gen(function* () {
    const daHashPreimageStep02 = yield* tryBuild(
      "Failed to build da-hash-preimage step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            DA_HASH_PREIMAGE_FAULT_PROOF_TITLES.step02,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const daHashPreimageStep01 = yield* tryBuild(
      "Failed to build da-hash-preimage step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            DA_HASH_PREIMAGE_FAULT_PROOF_TITLES.step01,
            [
              daHashPreimageStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: daHashPreimageStep01,
      steps: [daHashPreimageStep01, daHashPreimageStep02],
    };
  });

export const buildDaHashPreimageFaultProofContracts = (
  params: BuildDaHashPreimageFaultProofContractsParams,
): Effect.Effect<DaHashPreimageFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const daHashPreimage = yield* buildDaHashPreimageChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      daHashPreimage,
    };
  });
