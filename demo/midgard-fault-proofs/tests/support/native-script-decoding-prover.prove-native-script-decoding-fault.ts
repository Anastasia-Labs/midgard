import { Effect } from "effect";

import { type NativeScriptDecodingFinding } from "../../src/native-script-decoding/finding.js";
import {
  type NativeScriptDecodingProofOutcome,
  type NativeScriptDecodingProverDeps,
} from "./native-script-decoding-prover.locate-native-script-decoding-thread.js";
import { toError } from "./native-script-decoding-prover.remaining-tx-count.js";
import { runNativeScriptDecodingProver } from "./native-script-decoding-prover.run-native-script-decoding-prover.js";

/** The §4.3 core as an Effect, for consumers composing in that idiom. */
export const proveNativeScriptDecodingFault = (
  finding: NativeScriptDecodingFinding,
  deps: NativeScriptDecodingProverDeps,
): Effect.Effect<NativeScriptDecodingProofOutcome, Error> =>
  Effect.tryPromise({
    try: () => runNativeScriptDecodingProver(finding, deps),
    catch: toError,
  });
