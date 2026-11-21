/**
 * `native-script-decoding` family (#635, #633) — off-chain codec twins.
 *
 * Proves an operator verdict wrong about the decodability of a resolved
 * reference script — the `ResolvedReferenceScriptMalformed` / `NodeLimit` /
 * `DepthLimit` corner of the rejection catalogue — in either direction:
 * wrongful acceptance (direction A, the frozen scan refuses the accepted
 * script) or wrongful rejection (direction B, the accused script scans to the
 * exact canonical terminal).
 *
 * Violation: `native-script-decoding`.
 * Production catalogue category: `nativeScriptDecoding` (`0000000d`). The
 * asset-name helper still accepts the deployed category id so callers remain
 * explicitly bound to the manifest they are submitting against.
 *
 * Every schema below mirrors an Aiken type in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-script-decoding/
 * step-0{1,2,3,4}.ak` (and `engine.ak` for the thread states) field for field
 * and constructor index for constructor index; the exact bytes are pinned in
 * `tests/native-script-decoding.test.ts` against values measured out of
 * those Aiken modules over their own `thread_fixture_v1` fixtures.
 */

import "@al-ft/midgard-core/lucid-data";
import "@lucid-evolution/lucid";
import "effect";
import "../common.js";
import "../ledger-state.js";
import "../transition-trace.js";
import "./field-opening.js";
import "./native.js";
import "./validation-auxiliary-witness.js";
import "./native-script-decoding.native-script-decoding-scan-thread-state-schema.js";
import "./native-script-decoding.native-script-decoding-pre-bind-scan-state.js";
export {
  NATIVE_SCRIPT_DECODING_STEP_NAMES,
  nativeScriptDecodingBoundDescriptorState,
  nativeScriptDecodingOpenedSubjectState,
  nativeScriptDecodingPreBindScanState,
  NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
  NativeScriptDecodingStep04Args,
  NativeScriptDecodingStep04ArgsSchema,
  NativeScriptDecodingStep04Datum,
  NativeScriptDecodingStep04DatumSchema,
  NativeScriptDecodingStep04SpendRedeemer,
  NativeScriptDecodingStep04SpendRedeemerSchema,
  nativeScriptDecodingStepDatumSchema,
  type NativeScriptDecodingStepName,
} from "./native-script-decoding.native-script-decoding-pre-bind-scan-state.js";
export {
  NATIVE_SCRIPT_DECODING_CLASS_PENDING,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION,
  NATIVE_SCRIPT_DECODING_LANGUAGE_UNBOUND,
  NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_REFERENCE,
  NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_SPEND,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_DEPTH_LIMIT,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_NODE_LIMIT,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL,
  NATIVE_SCRIPT_DECODING_VIOLATION_ID,
  NativeScriptDecodingBindState,
  NativeScriptDecodingBindStateSchema,
  NativeScriptDecodingEventToStepMembershipSchema,
  NativeScriptDecodingForcedMembershipSchema,
  NativeScriptDecodingScanThreadState,
  NativeScriptDecodingScanThreadStateSchema,
  NativeScriptDecodingStep01Args,
  NativeScriptDecodingStep01ArgsSchema,
  NativeScriptDecodingStep01Datum,
  NativeScriptDecodingStep01DatumSchema,
  NativeScriptDecodingStep01SpendRedeemer,
  NativeScriptDecodingStep01SpendRedeemerSchema,
  NativeScriptDecodingStep02Args,
  NativeScriptDecodingStep02ArgsSchema,
  NativeScriptDecodingStep02Datum,
  NativeScriptDecodingStep02DatumSchema,
  NativeScriptDecodingStep02SpendRedeemer,
  NativeScriptDecodingStep02SpendRedeemerSchema,
  NativeScriptDecodingStep03AdvanceOrCloseArgs,
  NativeScriptDecodingStep03AdvanceOrCloseArgsSchema,
  NativeScriptDecodingStep03AdvanceOrCloseDatum,
  NativeScriptDecodingStep03AdvanceOrCloseDatumSchema,
  NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemerSchema,
  NativeScriptDecodingStep03BindDescriptorArgs,
  NativeScriptDecodingStep03BindDescriptorArgsSchema,
  NativeScriptDecodingStep03BindDescriptorDatum,
  NativeScriptDecodingStep03BindDescriptorDatumSchema,
  NativeScriptDecodingStep03BindDescriptorSpendRedeemer,
  NativeScriptDecodingStep03BindDescriptorSpendRedeemerSchema,
  NativeScriptDecodingStep03OpenSubjectArgs,
  NativeScriptDecodingStep03OpenSubjectArgsSchema,
  NativeScriptDecodingStep03OpenSubjectDatum,
  NativeScriptDecodingStep03OpenSubjectDatumSchema,
  NativeScriptDecodingStep03OpenSubjectSpendRedeemer,
  NativeScriptDecodingStep03OpenSubjectSpendRedeemerSchema,
  nativeScriptDecodingThreadTokenAssetName,
  NativeScriptDecodingTransitionStepMembershipSchema,
} from "./native-script-decoding.native-script-decoding-scan-thread-state-schema.js";
