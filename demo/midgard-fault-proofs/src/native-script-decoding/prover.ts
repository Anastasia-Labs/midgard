/**
 * The `native-script-decoding` proving core (offchain plan §4.3, ruled
 * 2026-08-25): a consumer-agnostic driver over the per-step submitters,
 * consumable by the watcher (autonomous) and the CLI (manual) alike.
 *
 * The core drives Init → step-01 → step-02 → OpenSubject → BindDescriptor →
 * (AdvanceOrClose)* → step-04, feeding `nextThreadOutRef` forward.
 *
 * - **Capability-injected.** `deps` carries everything environmental —
 *   signer, chain provider, evidence sources, observations, journal sink,
 *   policy. The core imports nothing from either consumer.
 * - **Resumable and idempotent-by-reconstruction** (§7.1). Invoked against
 *   a header whose thread already exists, it locates the thread by asset
 *   name across the six validator addresses, reads the on-chain `StepDatum`,
 *   recovers the position (including mid-loop via the `machine_state_hash`
 *   boundary search against the re-derived plan), and continues.
 * - **Policy as data.** The core enforces whatever
 *   `NativeScriptDecodingProverPolicy` it is handed and hard-codes none
 *   of it. Only the §3.2/3.3 provability classification is non-negotiable:
 *   unprovable corners are refused at the API boundary regardless of
 *   policy.
 * - **Outcome as data:** proven, refused (classification/policy, with the
 *   reason), or stalled (unexpected abort — surfaced loudly, never
 *   silently cancelled; cancellation is its own explicit call,
 *   `submitLinearFaultCancel`).
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../runtime.js";
import "./finding.js";
import "./scan-plan.js";
import "./submit-common.js";
import "./submit-native-script-decoding-init.js";
import "./submit-native-script-decoding-step-01.js";
import "./submit-native-script-decoding-step-02.js";
import "./submit-native-script-decoding-step-03.js";
import "./submit-native-script-decoding-step-04.js";
import "./prover.locate-native-script-decoding-thread.js";
import "./prover.remaining-tx-count.js";
import "./prover.run-native-script-decoding-prover.js";
import "./prover.prove-native-script-decoding-fault.js";
export {
  locateNativeScriptDecodingThread,
  NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS,
  type NativeScriptDecodingProofOutcome,
  type NativeScriptDecodingProverDeps,
  type NativeScriptDecodingProverEvent,
  type NativeScriptDecodingProverEvidence,
  type NativeScriptDecodingProverObservations,
  type NativeScriptDecodingProverPolicy,
  type NativeScriptDecodingThreadPosition,
} from "./prover.locate-native-script-decoding-thread.js";
export { proveNativeScriptDecodingFault } from "./prover.prove-native-script-decoding-fault.js";
export { runNativeScriptDecodingProver } from "./prover.run-native-script-decoding-prover.js";
