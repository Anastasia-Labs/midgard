import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import type { CertifiedFaultProofChainInputs } from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const TRANSACTION_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofTransactionOutputNonCanonical"),
  step02: deployedTitle("fraudProofTransactionOutputNonCanonicalStep02"),
  step03: deployedTitle("fraudProofTransactionOutputNonCanonicalStep03"),
  step03Datum: deployedTitle(
    "fraudProofTransactionOutputNonCanonicalStep03Datum",
  ),
  step04: deployedTitle("fraudProofTransactionOutputNonCanonicalStep04"),
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
}: CertifiedFaultProofChainInputs): Effect.Effect<
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
    const step03Datum = yield* buildFaultProofSpendingStep(
      context,
      TRANSACTION_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step03Datum,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build transaction-output-non-canonical step 03 datum",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      TRANSACTION_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step03,
      [
        step03Datum.spendingScriptHash,
        step04.spendingScriptHash,
        computationThread.policyId,
      ],
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
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step03Datum, step04],
    };
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
