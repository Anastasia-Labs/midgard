import {
  terminalVerdictContradiction,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";

import type { SignerWitnessScanCheckpoint } from "./signer-witness-scan.js";

/** What the shared scan and terminal read from either family's evidence. */
export type SignerWitnessScanEvidence = Readonly<{
  subject: VerdictSubject;
  witnessSetHashHex: string;
  addressWitnessFieldPreimageHex: string;
  /** Present only on the witness-scan route. */
  paymentCredentialHex?: string;
  /** `true` exactly on the witness-scan route. */
  signerRequired: boolean;
  signerPresent: boolean;
  checkpoints: readonly SignerWitnessScanCheckpoint[];
}>;

/** Mirrors `signer_witness_scan_v1/state.terminal_v1`: the decisive fault is
 * a required signer the scan did not find. */
export const signerWitnessVerdictCloses = (
  evidence: Pick<
    SignerWitnessScanEvidence,
    "subject" | "signerRequired" | "signerPresent"
  >,
): boolean =>
  terminalVerdictContradiction(
    evidence.subject,
    evidence.signerRequired && !evidence.signerPresent,
  );
