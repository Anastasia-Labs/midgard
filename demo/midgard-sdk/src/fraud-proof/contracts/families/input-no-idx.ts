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

export const INPUT_NO_IDX_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/input_no_idx/step_01.main.spend",
  step02: "fraud_proofs/input_no_idx/step_02.main.spend",
  step03: "fraud_proofs/input_no_idx/step_03.main.spend",
  step04: "fraud_proofs/input_no_idx/step_04.main.spend",
} as const;

/**
 * Q13 `input-no-idx` (`nonExistentInputNoIndex`): a committed transaction
 * spends an output index its in-block producing transaction never created.
 */
export type InputNoIdxFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly nonExistentInputNoIndex: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildInputNoIdxFaultProofContractsParams =
  BuildFaultProofContractsParams;

/**
 * Applied-parameter order is taken from the compiled blueprint
 * (`fraud_proofs/input_no_idx/step_0{1..4}.main.spend`), which differs from the
 * `no_input` chain at steps 02/03: step 02 takes only the next-step hash and
 * the thread policy, step 03 re-enters the block binding and therefore also
 * takes the hub oracle, and step 04 takes the thread policy first.
 */
export const buildInputNoIdxChain = ({
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
}): Effect.Effect<
  InputNoIdxFaultProofContracts["nonExistentInputNoIndex"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild("Failed to build input-no-idx step 04", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          INPUT_NO_IDX_FAULT_PROOF_TITLES.step04,
          [
            computationThread.policyId,
            fraudProof.policyId,
            fraudProofTokenAddressData,
            fieldPreimageCertificatePolicyId,
          ],
        ),
      ),
    );

    const step03 = yield* tryBuild("Failed to build input-no-idx step 03", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          INPUT_NO_IDX_FAULT_PROOF_TITLES.step03,
          [
            step04.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ],
        ),
      ),
    );

    const step02 = yield* tryBuild("Failed to build input-no-idx step 02", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          INPUT_NO_IDX_FAULT_PROOF_TITLES.step02,
          [
            step03.spendingScriptHash,
            computationThread.policyId,
            fieldPreimageCertificatePolicyId,
          ],
        ),
      ),
    );

    const step01 = yield* tryBuild("Failed to build input-no-idx step 01", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          INPUT_NO_IDX_FAULT_PROOF_TITLES.step01,
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

export const buildInputNoIdxFaultProofContracts = (
  params: BuildInputNoIdxFaultProofContractsParams,
): Effect.Effect<InputNoIdxFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const nonExistentInputNoIndex = yield* buildInputNoIdxChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      fieldPreimageCertificate: shared.fieldPreimageCertificate,
      nonExistentInputNoIndex,
    };
  });
