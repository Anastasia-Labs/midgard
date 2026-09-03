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

export const COMMITTED_FIELD_SHAPE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/committed_field_shape/step_01.main.spend",
  step02: "fraud_proofs/committed_field_shape/step_02.main.spend",
} as const;

export type CommittedFieldShapeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly committedFieldShape: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type BuildCommittedFieldShapeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildCommittedFieldShapeChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  CommittedFieldShapeFaultProofContracts["committedFieldShape"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      COMMITTED_FIELD_SHAPE_FAULT_PROOF_TITLES.step02,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build committed-field-shape step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      COMMITTED_FIELD_SHAPE_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build committed-field-shape step 01",
    );
    return { firstStep: step01, steps: [step01, step02] };
  });

export const buildCommittedFieldShapeFaultProofContracts = (
  params: BuildCommittedFieldShapeFaultProofContractsParams,
): Effect.Effect<CommittedFieldShapeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const committedFieldShape = yield* buildCommittedFieldShapeChain({
      ...params,
      ...shared,
    });
    return { ...shared, committedFieldShape };
  });
