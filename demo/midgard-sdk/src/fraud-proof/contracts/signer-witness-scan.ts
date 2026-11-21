import { Effect } from "effect";

import type { SpendingValidator } from "../../common.js";
import {
  buildFaultProofSpendingStep,
  type CertifiedFaultProofChainInputs,
} from "./shared.js";

/** The signer-witness scan and terminal both signer-missing families run
 * after their own step 02. */
export const SIGNER_WITNESS_SCAN_TITLES = {
  scan: "fraud_proofs/signer_witness_scan/scan.main.spend",
  terminal: "fraud_proofs/signer_witness_scan/terminal.main.spend",
} as const;

export type SignerWitnessScanSteps = Readonly<{
  scan: SpendingValidator;
  terminal: SpendingValidator;
}>;

/**
 * Builds the shared scan and terminal. Both signer-missing chains call this,
 * so their applied scripts are equal by construction: the thread token, not
 * the script address, carries the family identity.
 */
export const buildSignerWitnessScanSteps = ({
  blueprint,
  network,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
}: Omit<CertifiedFaultProofChainInputs, "hubOraclePolicyId">): Effect.Effect<
  SignerWitnessScanSteps,
  Error
> =>
  Effect.gen(function* () {
    const context = { blueprint, network };
    const terminal = yield* buildFaultProofSpendingStep(
      context,
      SIGNER_WITNESS_SCAN_TITLES.terminal,
      [
        fraudProof.policyId,
        fraudProofTokenAddressData,
        computationThread.policyId,
      ],
      "Failed to build the signer-witness terminal",
    );
    const scan = yield* buildFaultProofSpendingStep(
      context,
      SIGNER_WITNESS_SCAN_TITLES.scan,
      [
        terminal.spendingScriptHash,
        computationThread.policyId,
        fieldPreimageCertificatePolicyId,
      ],
      "Failed to build the signer-witness scan",
    );
    return { scan, terminal };
  });
