import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "../../../common.js";
import type {
  CertifiedFaultProofChainInputs,
  FieldGrammarCertifiedChainInput,
} from "../shared.js";
import {
  buildFaultProofSpendingStep,
  buildSharedFaultProofContracts,
} from "../shared.js";
import { buildSignerWitnessScanSteps } from "../signer-witness-scan.js";
import {
  type BuildFaultProofContractsParams,
  type FraudProofChain,
} from "../types.js";

export const PROTECTED_OUTPUT_SIGNER_MISSING_FAULT_PROOF_TITLES = {
  step01: deployedTitle("fraudProofProtectedOutputSignerMissing"),
  step02: deployedTitle("fraudProofProtectedOutputSignerMissingStep02"),
} as const;

export type ProtectedOutputSignerMissingFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly protectedOutputSignerMissing: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildProtectedOutputSignerMissingFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildProtectedOutputSignerMissingChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  fieldGrammarCertificatePolicyId,
}: CertifiedFaultProofChainInputs &
  FieldGrammarCertifiedChainInput): Effect.Effect<
  ProtectedOutputSignerMissingFaultProofContracts["protectedOutputSignerMissing"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const { scan, terminal } = yield* buildSignerWitnessScanSteps({
      blueprint,
      network,
      computationThread,
      fraudProof,
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
    });
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      PROTECTED_OUTPUT_SIGNER_MISSING_FAULT_PROOF_TITLES.step02,
      [
        scan.spendingScriptHash,
        terminal.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
        fieldGrammarCertificatePolicyId,
      ],
      "Failed to build protected-output-signer-missing step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      PROTECTED_OUTPUT_SIGNER_MISSING_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build protected-output-signer-missing step 01",
    );
    return {
      firstStep: step01,
      steps: [step01, step02, scan, terminal],
    };
  });

export const buildProtectedOutputSignerMissingFaultProofContracts = (
  params: BuildProtectedOutputSignerMissingFaultProofContractsParams,
): Effect.Effect<ProtectedOutputSignerMissingFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const protectedOutputSignerMissing =
      yield* buildProtectedOutputSignerMissingChain({ ...params, ...shared });
    return { ...shared, protectedOutputSignerMissing };
  });
