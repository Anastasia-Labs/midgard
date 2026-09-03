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

export const RECEIVE_PURPOSE_LANGUAGE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/receive_purpose_language/step_01.main.spend",
  step02: "fraud_proofs/receive_purpose_language/step_02.main.spend",
  step03: "fraud_proofs/receive_purpose_language/step_03.main.spend",
} as const;

export type ReceivePurposeLanguageFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly receivePurposeLanguage: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildReceivePurposeLanguageFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildReceivePurposeLanguageChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  ReceivePurposeLanguageFaultProofContracts["receivePurposeLanguage"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      RECEIVE_PURPOSE_LANGUAGE_FAULT_PROOF_TITLES.step03,
      [
        computationThread.policyId,
        fraudProof.policyId,
        fraudProofTokenAddressData,
      ],
      "Failed to build receive-purpose-language step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      RECEIVE_PURPOSE_LANGUAGE_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build receive-purpose-language step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      RECEIVE_PURPOSE_LANGUAGE_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build receive-purpose-language step 01",
    );
    return { firstStep: step01, steps: [step01, step02, step03] };
  });

export const buildReceivePurposeLanguageFaultProofContracts = (
  params: BuildReceivePurposeLanguageFaultProofContractsParams,
): Effect.Effect<ReceivePurposeLanguageFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const receivePurposeLanguage = yield* buildReceivePurposeLanguageChain({
      ...params,
      ...shared,
    });
    return { ...shared, receivePurposeLanguage };
  });
