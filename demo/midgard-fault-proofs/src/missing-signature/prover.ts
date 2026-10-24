/**
 * Consumer-agnostic proving core for the `missing-signature` family.
 *
 * The core is deliberately a small state-machine over the four step
 * addresses.  Chain state, rather than a local checkpoint, is authoritative:
 * every invocation reconstructs the position from the computation-thread
 * asset name and resumes from the holding address.  An unexpected failure is
 * returned as `stalled`; cancellation remains an explicit prover operation.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../runtime.js";
import "./finding.js";
import "./submit-common.js";
import "./submit-missing-signature-init.js";
import "./submit-missing-signature-step-01.js";
import "./submit-missing-signature-step-02.js";
import "./submit-missing-signature-step-03.js";
import "./submit-missing-signature-step-04.js";
import "./prover.assert-evidence-coherent.js";
import "./prover.run-missing-signature-prover.js";
import "./prover.prove-missing-signature-fault.js";
export {
  locateMissingSignatureThread,
  MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS,
  type MissingSignatureFieldCertificates,
  type MissingSignatureProofOutcome,
  type MissingSignatureProverDeps,
  type MissingSignatureProverEvent,
  type MissingSignatureProverEvidence,
  type MissingSignatureProverObservations,
  type MissingSignatureProverPolicy,
  type MissingSignatureReferenceScripts,
  type MissingSignatureSubjectEvidence,
  type MissingSignatureThreadPosition,
} from "./prover.assert-evidence-coherent.js";
export { proveMissingSignatureFault } from "./prover.prove-missing-signature-fault.js";
export { runMissingSignatureProver } from "./prover.run-missing-signature-prover.js";
