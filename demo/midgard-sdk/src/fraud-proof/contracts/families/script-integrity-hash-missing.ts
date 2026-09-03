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

export const SCRIPT_INTEGRITY_HASH_MISSING_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/script_integrity_hash_missing/step_01.main.spend",
  step02: "fraud_proofs/script_integrity_hash_missing/step_02.main.spend",
  step03: "fraud_proofs/script_integrity_hash_missing/step_03.main.spend",
  scriptGrammar:
    "fraud_proofs/script_integrity_hash_missing/script_grammar.main.spend",
  scriptScan:
    "fraud_proofs/script_integrity_hash_missing/script_scan.main.spend",
  redeemerGrammar:
    "fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main.spend",
  step04: "fraud_proofs/script_integrity_hash_missing/step_04.main.spend",
} as const;

export type ScriptIntegrityHashMissingFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fieldPreimageCertificate: MintingValidator;
  readonly scriptIntegrityHashMissing: FraudProofChain & {
    readonly scriptGrammar: SpendingValidator;
    readonly scriptScan: SpendingValidator;
    readonly redeemerGrammar: SpendingValidator;
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildScriptIntegrityHashMissingFaultProofContractsParams =
  BuildFaultProofContractsParams;

export const buildScriptIntegrityHashMissingChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: BuildFaultProofContractsParams & SharedFaultProofContracts): Effect.Effect<
  ScriptIntegrityHashMissingFaultProofContracts["scriptIntegrityHashMissing"],
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const step04 = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISSING_FAULT_PROOF_TITLES.step04,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build script-integrity-hash-missing step 04",
    );
    const redeemerGrammar = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISSING_FAULT_PROOF_TITLES.redeemerGrammar,
      [
        step04.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build script-integrity-hash-missing redeemer grammar",
    );
    const scriptScan = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISSING_FAULT_PROOF_TITLES.scriptScan,
      [
        redeemerGrammar.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build script-integrity-hash-missing script scan",
    );
    const scriptGrammar = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISSING_FAULT_PROOF_TITLES.scriptGrammar,
      [
        scriptScan.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build script-integrity-hash-missing script grammar",
    );
    const step03 = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISSING_FAULT_PROOF_TITLES.step03,
      [
        step04.spendingScriptHash,
        scriptGrammar.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build script-integrity-hash-missing step 03",
    );
    const step02 = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISSING_FAULT_PROOF_TITLES.step02,
      [step03.spendingScriptHash, computationThread.policyId],
      "Failed to build script-integrity-hash-missing step 02",
    );
    const step01 = yield* buildFaultProofSpendingStep(
      context,
      SCRIPT_INTEGRITY_HASH_MISSING_FAULT_PROOF_TITLES.step01,
      [
        step02.spendingScriptHash,
        computationThread.policyId,
        hubOraclePolicyId,
      ],
      "Failed to build script-integrity-hash-missing step 01",
    );
    return {
      firstStep: step01,
      scriptGrammar,
      scriptScan,
      redeemerGrammar,
      steps: [
        step01,
        step02,
        step03,
        scriptGrammar,
        scriptScan,
        redeemerGrammar,
        step04,
      ],
    };
  });

export const buildScriptIntegrityHashMissingFaultProofContracts = (
  params: BuildScriptIntegrityHashMissingFaultProofContractsParams,
): Effect.Effect<ScriptIntegrityHashMissingFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const scriptIntegrityHashMissing =
      yield* buildScriptIntegrityHashMissingChain({ ...params, ...shared });
    return { ...shared, scriptIntegrityHashMissing };
  });
