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

export const TRANSACTION_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/transaction_output_non_canonical/step_01.main.spend",
  step02: "fraud_proofs/transaction_output_non_canonical/step_02.main.spend",
  step03: "fraud_proofs/transaction_output_non_canonical/step_03.main.spend",
  step04: "fraud_proofs/transaction_output_non_canonical/step_04.main.spend",
} as const;

export type TransactionOutputNonCanonicalFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly transactionOutputNonCanonical: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildTransactionOutputNonCanonicalFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildTransactionOutputNonCanonicalChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  TransactionOutputNonCanonicalFaultProofContracts["transactionOutputNonCanonical"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      TRANSACTION_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build transaction-output-non-canonical step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      TRANSACTION_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build transaction-output-non-canonical step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      TRANSACTION_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build transaction-output-non-canonical step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      TRANSACTION_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build transaction-output-non-canonical step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03, step04] };
  });

export const buildTransactionOutputNonCanonicalFaultProofContracts = (
  params: BuildTransactionOutputNonCanonicalFaultProofContractsParams,
): Effect.Effect<TransactionOutputNonCanonicalFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const transactionOutputNonCanonical =
      yield* buildTransactionOutputNonCanonicalChain({ ...params, ...shared });
    return { ...shared, transactionOutputNonCanonical };
  });
