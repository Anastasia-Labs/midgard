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

export const FIELD_ITEM_WIDTH_ILLEGAL_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/field_item_width_illegal/step_01.main.spend",
  step02: "fraud_proofs/field_item_width_illegal/step_02.main.spend",
  step03: "fraud_proofs/field_item_width_illegal/step_03.main.spend",
} as const;

export type FieldItemWidthIllegalFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly fieldItemWidthIllegal: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildFieldItemWidthIllegalFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildFieldItemWidthIllegalChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  FieldItemWidthIllegalFaultProofContracts["fieldItemWidthIllegal"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      FIELD_ITEM_WIDTH_ILLEGAL_FAULT_PROOF_TITLES.step03,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build field-item-width-illegal step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      FIELD_ITEM_WIDTH_ILLEGAL_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build field-item-width-illegal step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      FIELD_ITEM_WIDTH_ILLEGAL_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build field-item-width-illegal step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03] };
  });

export const buildFieldItemWidthIllegalFaultProofContracts = (
  params: BuildFieldItemWidthIllegalFaultProofContractsParams,
): Effect.Effect<FieldItemWidthIllegalFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const fieldItemWidthIllegal = yield* buildFieldItemWidthIllegalChain({
      ...params,
      ...shared,
    });
    return { ...shared, fieldItemWidthIllegal };
  });
