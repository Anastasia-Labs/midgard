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

export const DOUBLE_SPEND_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/double_spend/step_01.main.spend",
  step02: "fraud_proofs/double_spend/step_02.main.spend",
  step03: "fraud_proofs/double_spend/step_03.main.spend",
  step04: "fraud_proofs/double_spend/step_04.main.spend",
} as const;

export type DoubleSpendFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly doubleSpend: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildDoubleSpendFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildDoubleSpendChain = ({
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
}): Effect.Effect<DoubleSpendFaultProofContracts["doubleSpend"], Error> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild("Failed to build double-spend step 04", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          DOUBLE_SPEND_FAULT_PROOF_TITLES.step04,
          [
            computationThread.policyId,
            fraudProof.policyId,
            fraudProofTokenAddressData,
            fieldPreimageCertificatePolicyId,
          ],
        ),
      ),
    );

    const step03 = yield* tryBuild("Failed to build double-spend step 03", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          DOUBLE_SPEND_FAULT_PROOF_TITLES.step03,
          [
            step04.spendingScriptHash,
            computationThread.policyId,
            fieldPreimageCertificatePolicyId,
          ],
        ),
      ),
    );

    const step02 = yield* tryBuild("Failed to build double-spend step 02", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          DOUBLE_SPEND_FAULT_PROOF_TITLES.step02,
          [
            step03.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ],
        ),
      ),
    );

    const step01 = yield* tryBuild("Failed to build double-spend step 01", () =>
      makeSpendingValidator(
        network,
        applyBlueprintParams(
          blueprint,
          DOUBLE_SPEND_FAULT_PROOF_TITLES.step01,
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

export const buildDoubleSpendFaultProofContracts = (
  params: BuildDoubleSpendFaultProofContractsParams,
): Effect.Effect<DoubleSpendFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const doubleSpend = yield* buildDoubleSpendChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      fieldPreimageCertificate: shared.fieldPreimageCertificate,
      doubleSpend,
    };
  });
