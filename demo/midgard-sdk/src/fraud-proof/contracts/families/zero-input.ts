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

export const ZERO_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/zero_input/step_01.main.spend",
  step02: "fraud_proofs/zero_input/step_02.main.spend",
} as const;

export type ZeroInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly zeroInput: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildZeroInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildZeroInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
}): Effect.Effect<ZeroInputFaultProofContracts["zeroInput"], Error> =>
  Effect.gen(function* () {
    const zeroInputStep02 = yield* tryBuild(
      "Failed to build zero-input step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            ZERO_INPUT_FAULT_PROOF_TITLES.step02,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const zeroInputStep01 = yield* tryBuild(
      "Failed to build zero-input step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            ZERO_INPUT_FAULT_PROOF_TITLES.step01,
            [
              zeroInputStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: zeroInputStep01,
      steps: [zeroInputStep01, zeroInputStep02],
    };
  });

export const buildZeroInputFaultProofContracts = (
  params: BuildZeroInputFaultProofContractsParams,
): Effect.Effect<ZeroInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const zeroInput = yield* buildZeroInputChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      zeroInput,
    };
  });
