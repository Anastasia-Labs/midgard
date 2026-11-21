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
 *   `buildLinearFaultCancel`).
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk/core";
import "@al-ft/midgard-sdk/state-queue";
import "@al-ft/midgard-sdk/fraud-proof";
import "@lucid-evolution/lucid";
import "effect";
import "../../src/runtime.js";
import "../../src/native-script-decoding/finding.js";
import "../../src/native-script-decoding/scan-plan.js";
import "../../src/native-script-decoding/submit-common.js";
import "../../src/native-script-decoding/submit-native-script-decoding-init.js";
import "../../src/native-script-decoding/submit-native-script-decoding-step-01.js";
import "../../src/native-script-decoding/submit-native-script-decoding-step-02.js";
import "../../src/native-script-decoding/submit-native-script-decoding-step-03.js";
import "../../src/native-script-decoding/submit-native-script-decoding-step-04.js";
import "./native-script-decoding-prover.locate-native-script-decoding-thread.js";
import "./native-script-decoding-prover.remaining-tx-count.js";
import "./native-script-decoding-prover.run-native-script-decoding-prover.js";
import "./native-script-decoding-prover.prove-native-script-decoding-fault.js";
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
} from "./native-script-decoding-prover.locate-native-script-decoding-thread.js";
export { proveNativeScriptDecodingFault } from "./native-script-decoding-prover.prove-native-script-decoding-fault.js";
export { runNativeScriptDecodingProver } from "./native-script-decoding-prover.run-native-script-decoding-prover.js";
