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

export const REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/reference_input_no_idx/step_01.main.spend",
  step02: "fraud_proofs/reference_input_no_idx/step_02.main.spend",
  step03: "fraud_proofs/reference_input_no_idx/step_03.main.spend",
  step04: "fraud_proofs/reference_input_no_idx/step_04.main.spend",
} as const;

/**
 * Q31 `reference-input-no-idx`: a committed transaction *reads* an output index
 * its in-block producing transaction never created. The reference-input mirror
 * of `input-no-idx`: steps 01 and 02 are distinct scripts, while steps 03 and 04
 * compile to the same UPLC as that chain's and are therefore shared, as with the
 * `no_input`/`no_reference_input` pair.
 */
export type ReferenceInputNoIdxFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly referenceInputNoIdx: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildReferenceInputNoIdxFaultProofContractsParams =
  BuildFaultProofContractsParams;

/**
 * Applied-parameter order is taken from the compiled blueprint
 * (`fraud_proofs/reference_input_no_idx/step_0{1..4}.main.spend`) and matches
 * `input-no-idx` position for position: step 02 takes the next-step hash and
 * the thread policy, step 03 re-enters the block binding and therefore also
 * takes the hub oracle, and step 04 takes the thread policy first. That order
 * must stay identical to `input-no-idx`'s: the two chains share their step-03
 * and step-04 scripts, so a divergent order here would fork those hashes.
 */
export const buildReferenceInputNoIdxChain = ({
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
  ReferenceInputNoIdxFaultProofContracts["referenceInputNoIdx"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build reference-input-no-idx step 04",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step04,
            [
              computationThread.policyId,
              fraudProof.policyId,
              fraudProofTokenAddressData,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build reference-input-no-idx step 03",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step03,
            [
              step04.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build reference-input-no-idx step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step02,
            [
              step03.spendingScriptHash,
              computationThread.policyId,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build reference-input-no-idx step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES.step01,
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

export const buildReferenceInputNoIdxFaultProofContracts = (
  params: BuildReferenceInputNoIdxFaultProofContractsParams,
): Effect.Effect<ReferenceInputNoIdxFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const referenceInputNoIdx = yield* buildReferenceInputNoIdxChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      fieldPreimageCertificate: shared.fieldPreimageCertificate,
      referenceInputNoIdx,
    };
  });
