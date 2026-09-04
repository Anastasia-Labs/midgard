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

export const NO_REFERENCE_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/no_reference_input/step_01.main.spend",
  step02: "fraud_proofs/no_reference_input/step_02.main.spend",
  step03: "fraud_proofs/no_reference_input/step_03.main.spend",
  step04: "fraud_proofs/no_reference_input/step_04.main.spend",
} as const;

/**
 * Q18 `no-reference-input`: a committed transaction references an input that
 * never existed in the block's prev ledger and was not produced in-block. The
 * chain mirrors `no_input`'s applied-parameter order step for step; only the
 * field lifted out of the bad transaction differs (reference inputs, not spend
 * inputs).
 */
export type NoReferenceInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly noReferenceInput: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildNoReferenceInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildNoReferenceInputChain = ({
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
  NoReferenceInputFaultProofContracts["noReferenceInput"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build no-reference-input step 04",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step04,
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build no-reference-input step 03",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step03,
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build no-reference-input step 02",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step02,
            [
              step03.spendingScriptHash,
              computationThread.policyId,
              fieldPreimageCertificatePolicyId,
            ],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build no-reference-input step 01",
      () =>
        makeSpendingValidator(
          network,
          applyBlueprintParams(
            blueprint,
            NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step01,
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

export const buildNoReferenceInputFaultProofContracts = (
  params: BuildNoReferenceInputFaultProofContractsParams,
): Effect.Effect<NoReferenceInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const noReferenceInput = yield* buildNoReferenceInputChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      noReferenceInput,
    };
  });
