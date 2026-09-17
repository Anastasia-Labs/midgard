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

export const OBSERVER_ORDER_INVALID_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/observer_order_invalid/step_01.main.spend",
  step02: "fraud_proofs/observer_order_invalid/step_02.main.spend",
  step03: "fraud_proofs/observer_order_invalid/step_03.main.spend",
  step04: "fraud_proofs/observer_order_invalid/step_04.main.spend",
} as const;

export type ObserverOrderInvalidFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly observerOrderInvalid: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildObserverOrderInvalidFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildObserverOrderInvalidChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  ObserverOrderInvalidFaultProofContracts["observerOrderInvalid"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      OBSERVER_ORDER_INVALID_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build observer-order-invalid step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      OBSERVER_ORDER_INVALID_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build observer-order-invalid step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      OBSERVER_ORDER_INVALID_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build observer-order-invalid step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      OBSERVER_ORDER_INVALID_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build observer-order-invalid step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03, step04] };
  });

export const buildObserverOrderInvalidFaultProofContracts = (
  params: BuildObserverOrderInvalidFaultProofContractsParams,
): Effect.Effect<ObserverOrderInvalidFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const observerOrderInvalid = yield* buildObserverOrderInvalidChain({
      ...params,
      ...shared,
    });
    return { ...shared, observerOrderInvalid };
  });
