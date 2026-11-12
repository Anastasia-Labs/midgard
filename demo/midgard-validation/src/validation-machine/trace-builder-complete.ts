/**
 * The phases after phase A's resolution: NativeScripts, ScriptIntegrity,
 * CEK and ValueAndMint, each continuing from the NativeScripts handoff.
 */
import { encodeCbor } from "@al-ft/midgard-core";
import { Effect } from "effect";

import { produceCek } from "./cek-producer.js";
import {
  nativeScriptsHandoff,
  produceNativeScripts,
} from "./native-scripts-producer.js";
import { type PreparedValidationTransaction } from "./prepared-validation-transaction.js";
import type { ValidationTraceWalkState } from "./trace-builder-types.js";
import {
  phaseContinues,
  type PhaseOutcome,
  type TraceWalk,
} from "./trace-walk.js";
import { produceValueAndMint } from "./value-and-mint-producer.js";

/** The ScriptIntegrity producer: the hash binding, which replay steers. */
const produceScriptIntegrity = (
  {
    compactProofTransaction: { transactionBody, transactionWitnessSetHash },
    compactProofWitnessSet,
  }: Pick<
    PreparedValidationTransaction,
    "compactProofTransaction" | "compactProofWitnessSet"
  >,
  nativeControlCbor: Buffer,
  { push }: Pick<TraceWalk, "push">,
): PhaseOutcome => {
  const { scriptIntegrityHash } = transactionBody;
  for (const tail of [
    [0n],
    [1n],
    [2n, scriptIntegrityHash, transactionWitnessSetHash],
    [3n, scriptIntegrityHash, compactProofWitnessSet.redeemerTxWitsHash],
  ]) {
    push("scriptIntegrity", encodeCbor([nativeControlCbor, ...tail]));
  }
  return phaseContinues;
};

export const completeValidationTrace = (
  prepared: PreparedValidationTransaction,
  state: ValidationTraceWalkState,
  walk: TraceWalk,
): Effect.Effect<void, Error> =>
  Effect.gen(function* () {
    yield* walk.phase("nativeScripts", () =>
      produceNativeScripts(prepared, state, walk),
    );
    yield* walk.phase("scriptIntegrity", () =>
      Effect.sync(() =>
        produceScriptIntegrity(
          prepared,
          nativeScriptsHandoff(prepared, state),
          walk,
        ),
      ),
    );
    yield* walk.phase("cek", () =>
      Effect.sync(() =>
        produceCek(
          prepared,
          state,
          nativeScriptsHandoff(prepared, state),
          walk,
        ),
      ),
    );
    yield* walk.phase("valueAndMint", () =>
      produceValueAndMint(
        prepared,
        state,
        nativeScriptsHandoff(prepared, state),
        walk,
      ),
    );
  });
