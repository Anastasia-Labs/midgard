import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
  type SharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/missing_script_source/step_01.main.spend",
  step02: "fraud_proofs/missing_script_source/step_02.main.spend",
  step03: "fraud_proofs/missing_script_source/step_03.main.spend",
  step04: "fraud_proofs/missing_script_source/step_04.main.spend",
  step05: "fraud_proofs/missing_script_source/step_05.main.spend",
  step06: "fraud_proofs/missing_script_source/step_06.main.spend",
} as const;

export type MissingScriptSourceFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly missingScriptSource: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildMissingScriptSourceFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildMissingScriptSourceChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MissingScriptSourceFaultProofContracts["missingScriptSource"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step06 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES.step06,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build missing-script-source step 06",
    );
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES.step05,
      [step06.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-script-source step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-script-source step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-script-source step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build missing-script-source step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MISSING_SCRIPT_SOURCE_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build missing-script-source step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05, step06],
    };
  });

export const buildMissingScriptSourceFaultProofContracts = (
  params: BuildMissingScriptSourceFaultProofContractsParams,
): Effect.Effect<MissingScriptSourceFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const missingScriptSource = yield* buildMissingScriptSourceChain({
      ...params,
      ...shared,
    });
    return { ...shared, missingScriptSource };
  });
