import { Effect } from "effect";

import { type NativeScriptDecodingFinding } from "./finding.js";
import {
  type NativeScriptDecodingProofOutcome,
  type NativeScriptDecodingProverDeps,
} from "./prover.locate-native-script-decoding-thread.js";
import { toError } from "./prover.remaining-tx-count.js";
import { runNativeScriptDecodingProver } from "./prover.run-native-script-decoding-prover.js";

/** The §4.3 core as an Effect, for consumers composing in that idiom. */
export const proveNativeScriptDecodingFault = (
  finding: NativeScriptDecodingFinding,
  deps: NativeScriptDecodingProverDeps,
): Effect.Effect<NativeScriptDecodingProofOutcome, Error> =>
  Effect.tryPromise({
    try: () => runNativeScriptDecodingProver(finding, deps),
    catch: toError,
  });
