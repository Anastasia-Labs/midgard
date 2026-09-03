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

export const RESOLVED_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/resolved_output_non_canonical/step_01.main.spend",
  step02: "fraud_proofs/resolved_output_non_canonical/step_02.main.spend",
  step03: "fraud_proofs/resolved_output_non_canonical/step_03.main.spend",
  step04: "fraud_proofs/resolved_output_non_canonical/step_04.main.spend",
  step05: "fraud_proofs/resolved_output_non_canonical/step_05.main.spend",
} as const;

export type ResolvedOutputNonCanonicalFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly resolvedOutputNonCanonical: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildResolvedOutputNonCanonicalFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildResolvedOutputNonCanonicalChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  ResolvedOutputNonCanonicalFaultProofContracts["resolvedOutputNonCanonical"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step05 = yield* buildFaultProofSpendingStep(
      context,
      RESOLVED_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step05,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build resolved-output-non-canonical step 05",
    );
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      RESOLVED_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step04,
      [step05.spendingScriptHash, computationThread.policyId],
      "Failed to build resolved-output-non-canonical step 04",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      RESOLVED_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step03,
      [step04.spendingScriptHash, computationThread.policyId],
      "Failed to build resolved-output-non-canonical step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      RESOLVED_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step02,
      [
        step03.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build resolved-output-non-canonical step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      RESOLVED_OUTPUT_NON_CANONICAL_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build resolved-output-non-canonical step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04, step05],
    };
  });

export const buildResolvedOutputNonCanonicalFaultProofContracts = (
  params: BuildResolvedOutputNonCanonicalFaultProofContractsParams,
): Effect.Effect<ResolvedOutputNonCanonicalFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const resolvedOutputNonCanonical =
      yield* buildResolvedOutputNonCanonicalChain({ ...params, ...shared });
    return { ...shared, resolvedOutputNonCanonical };
  });
