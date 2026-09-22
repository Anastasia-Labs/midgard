import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
  WithdrawalValidator,
} from "../../../common.js";
import {
  applyBlueprintParams,
  makeWithdrawalValidator,
  tryBuild,
} from "../blueprint.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
  type SharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const MIN_ADA_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/min_ada/step_01.main.spend",
  step02: "fraud_proofs/min_ada/step_02.main.spend",
  step03: "fraud_proofs/min_ada/step_03.main.spend",
  step04: "fraud_proofs/min_ada/step_04.main.spend",
  step05: "fraud_proofs/min_ada/step_05.main.spend",
  txYield: "fraud_proofs/min_ada/step_02_yields.tx.withdraw",
  utxoYield: "fraud_proofs/min_ada/step_02_yields.utxo.withdraw",
} as const;

export type MinAdaFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly minAda: FraudProofChain & {
    readonly yields: {
      readonly tx: WithdrawalValidator;
      readonly utxo: WithdrawalValidator;
    };
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildMinAdaFaultProofContractsParams =
  BuildFaultProofContractsParams & {
    readonly referenceScriptAuthPolicyId: string;
  };

export const buildMinAdaChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  referenceScriptAuthPolicyId,
}: BuildMinAdaFaultProofContractsParams &
  SharedFaultProofContracts): Effect.Effect<
  MinAdaFaultProofContracts["minAda"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      MIN_ADA_FAULT_PROOF_TITLES.step05,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build min-ada step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MIN_ADA_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build min-ada step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MIN_ADA_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        step05.spendingScriptHash,
        computationThread.policyId,
      ],
      "Failed to build min-ada step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MIN_ADA_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        step05.spendingScriptHash,
        computationThread.policyId,
        referenceScriptAuthPolicyId,
      ],
      "Failed to build min-ada step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MIN_ADA_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build min-ada step 01",
    );
    const txYield = yield* tryBuild("Failed to build min-ada tx yield", () =>
      makeWithdrawalValidator(
        applyBlueprintParams(blueprint, MIN_ADA_FAULT_PROOF_TITLES.txYield, [
          step02.spendingScriptHash,
          fieldPreimageCertificatePolicyId,
        ]),
      ),
    );
    const utxoYield = yield* tryBuild(
      "Failed to build min-ada UTxO yield",
      () =>
        makeWithdrawalValidator(
          applyBlueprintParams(
            blueprint,
            MIN_ADA_FAULT_PROOF_TITLES.utxoYield,
            [step02.spendingScriptHash],
          ),
        ),
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05],
      yields: { tx: txYield, utxo: utxoYield },
    };
  });

export const buildMinAdaFaultProofContracts = (
  params: BuildMinAdaFaultProofContractsParams,
): Effect.Effect<MinAdaFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const minAda = yield* buildMinAdaChain({ ...params, ...shared });
    return { ...shared, minAda };
  });
