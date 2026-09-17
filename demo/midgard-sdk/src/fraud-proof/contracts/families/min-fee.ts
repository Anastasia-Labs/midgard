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

export const MIN_FEE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/min_fee/step_01.main.spend",
  step02: "fraud_proofs/min_fee/step_02.main.spend",
} as const;

export type MinFeeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly minFee: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildMinFeeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildMinFeeChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  MinFeeFaultProofContracts["minFee"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      MIN_FEE_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build min-fee step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      MIN_FEE_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build min-fee step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

export const buildMinFeeFaultProofContracts = (
  params: BuildMinFeeFaultProofContractsParams,
): Effect.Effect<MinFeeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const minFee = yield* buildMinFeeChain({ ...params, ...shared });
    return { ...shared, minFee };
  });
