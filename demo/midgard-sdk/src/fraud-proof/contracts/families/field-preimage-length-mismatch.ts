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

export const FIELD_PREIMAGE_LENGTH_MISMATCH_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/field_preimage_length_mismatch/step_01.main.spend",
  step02Accepted:
    "fraud_proofs/field_preimage_length_mismatch/step_02_accepted.main.spend",
  step02Forced:
    "fraud_proofs/field_preimage_length_mismatch/step_02_forced.main.spend",
  step03: "fraud_proofs/field_preimage_length_mismatch/step_03.main.spend",
} as const;

export type FieldPreimageLengthMismatchFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly fieldPreimageLengthMismatch: FraudProofChain & {
    readonly acceptedStep02: SpendingValidator;
    readonly forcedStep02: SpendingValidator;
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildFieldPreimageLengthMismatchFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildFieldPreimageLengthMismatchChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  FieldPreimageLengthMismatchFaultProofContracts["fieldPreimageLengthMismatch"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      FIELD_PREIMAGE_LENGTH_MISMATCH_FAULT_PROOF_TITLES.step03,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build field-preimage-length-mismatch step 03",
    );
    const step02Accepted = yield* buildFaultProofSpendingStep(
      context,
      FIELD_PREIMAGE_LENGTH_MISMATCH_FAULT_PROOF_TITLES.step02Accepted,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build field-preimage-length-mismatch accepted step 02",
    );
    const step02Forced = yield* buildFaultProofSpendingStep(
      context,
      FIELD_PREIMAGE_LENGTH_MISMATCH_FAULT_PROOF_TITLES.step02Forced,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build field-preimage-length-mismatch forced step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      FIELD_PREIMAGE_LENGTH_MISMATCH_FAULT_PROOF_TITLES.step01,
      [
        step02Accepted.spendingScriptHash,
        step02Forced.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build field-preimage-length-mismatch step 01",
    );
    return {
      firstStep: step01,
      acceptedStep02: step02Accepted,
      forcedStep02: step02Forced,
      steps: [step01, step02Accepted, step02Forced, step03],
    };
  });

export const buildFieldPreimageLengthMismatchFaultProofContracts = (
  params: BuildFieldPreimageLengthMismatchFaultProofContractsParams,
): Effect.Effect<FieldPreimageLengthMismatchFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const fieldPreimageLengthMismatch =
      yield* buildFieldPreimageLengthMismatchChain({ ...params, ...shared });
    return { ...shared, fieldPreimageLengthMismatch };
  });
