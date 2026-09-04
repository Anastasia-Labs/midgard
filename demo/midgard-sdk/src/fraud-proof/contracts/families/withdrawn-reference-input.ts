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

export const WITHDRAWN_REFERENCE_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/withdrawn_reference_input/step_01.main.spend",
  step02: "fraud_proofs/withdrawn_reference_input/step_02.main.spend",
  step03: "fraud_proofs/withdrawn_reference_input/step_03.main.spend",
} as const;

export type WithdrawnReferenceInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly withdrawnReferenceInput: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildWithdrawnReferenceInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildWithdrawnReferenceInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  WithdrawnReferenceInputFaultProofContracts["withdrawnReferenceInput"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWN_REFERENCE_INPUT_FAULT_PROOF_TITLES.step03,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build withdrawn-reference-input step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWN_REFERENCE_INPUT_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build withdrawn-reference-input step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      WITHDRAWN_REFERENCE_INPUT_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build withdrawn-reference-input step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03] };
  });

export const buildWithdrawnReferenceInputFaultProofContracts = (
  params: BuildWithdrawnReferenceInputFaultProofContractsParams,
): Effect.Effect<WithdrawnReferenceInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const withdrawnReferenceInput = yield* buildWithdrawnReferenceInputChain({
      ...params,
      ...shared,
    });
    return { ...shared, withdrawnReferenceInput };
  });
