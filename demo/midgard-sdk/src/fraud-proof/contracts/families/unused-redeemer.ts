import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import type { FaultProofChainInputs } from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const UNUSED_REDEEMER_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofUnusedRedeemer"),
  step02: deployedTitle("fraudProofUnusedRedeemerStep02"),
  step02a: deployedTitle("fraudProofUnusedRedeemerStep02a"),
  step02b: deployedTitle("fraudProofUnusedRedeemerStep02b"),
  step02c: deployedTitle("fraudProofUnusedRedeemerStep02c"),
  step03: deployedTitle("fraudProofUnusedRedeemerStep03"),
  step04: deployedTitle("fraudProofUnusedRedeemerStep04"),
  step05: deployedTitle("fraudProofUnusedRedeemerStep05"),
  step06: deployedTitle("fraudProofUnusedRedeemerStep06"),
} as const;

export type UnusedRedeemerFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly unusedRedeemer: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildUnusedRedeemerFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildUnusedRedeemerChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: FaultProofChainInputs): Effect.Effect<
  UnusedRedeemerFaultProofContracts["unusedRedeemer"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step06 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step06,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build unused-redeemer step 06",
    );
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step05,
      [step06.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 03",
    );
    const step02c = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step02c,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 02c",
    );
    const step02b = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step02b,
      [step02c.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 02b",
    );
    const step02a = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step02a,
      [step02b.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 02a",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step02,
      [step02a.spendingScriptHash, computationThread.policyId],
      "Failed to build unused-redeemer step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      UNUSED_REDEEMER_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build unused-redeemer step 01",
    );
    return {
      firstStep: step01,
      steps: [
        step01,
        step02,
        step02a,
        step02b,
        step02c,
        step03,
        step04,
        step05,
        step06,
      ],
    };
  });

export const buildUnusedRedeemerFaultProofContracts = (
  params: BuildUnusedRedeemerFaultProofContractsParams,
): Effect.Effect<UnusedRedeemerFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const unusedRedeemer = yield* buildUnusedRedeemerChain({
      ...params,
      ...shared,
    });
    return { ...shared, unusedRedeemer };
  });
