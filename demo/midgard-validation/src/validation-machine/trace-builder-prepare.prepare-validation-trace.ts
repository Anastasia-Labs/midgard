/**
 * The prepare half of the trace walk: canonical decode through
 * ScriptSources, one producer per phase in walk order, each handed the
 * values the earlier phases left as explicit arguments.
 */
import { encodeCbor } from "@al-ft/midgard-core";
import { Effect } from "effect";

import { produceCanonicalDecode } from "./canonical-decode-producer.js";
import { produceInputSets } from "./input-sets-producer.js";
import { producePhaseANativeScripts } from "./phase-a-native-scripts-producer.js";
import { producePhaseAScriptPreconditions } from "./phase-a-script-preconditions-producer.js";
import {
  emptyMintFoldControl,
  proofBinding,
} from "./prepare-producers.shared.js";
import type { PreparedValidationTransaction } from "./prepared-validation-transaction.js";
import {
  produceResolveInputs,
  produceValidityInterval,
} from "./resolve-inputs-producer.js";
import { produceScriptSources } from "./script-sources-producer.js";
import { produceSignatures } from "./signatures-producer.js";
import type { ValidationTraceWalkState } from "./trace-builder-types.js";
import { phaseContinues, type TraceWalk } from "./trace-walk.js";

/**
 * The state a walk leaves when it stops before ScriptSources finishes.
 * Only the complete-side producers read state, and a stopped walk runs
 * none of them.
 */
const unreachedState = (
  prepared: PreparedValidationTransaction,
): ValidationTraceWalkState => ({
  scriptSourceEntries: prepared.inlineScriptSourceEntries,
  scriptPurposeEntries: [],
  scriptExecutionEntries: [],
  admittedOutputDescriptorCbors: [],
  admittedOutputDescriptorLeafHashes: [],
  mintFoldControl: emptyMintFoldControl(prepared.emptyValidationFrontier),
  authenticatedNativeScriptsWitnessCbor: null,
  authenticatedNativeScriptsBaseFields: null,
});

export const prepareValidationTrace = (
  prepared: PreparedValidationTransaction,
  walk: TraceWalk,
): Effect.Effect<ValidationTraceWalkState, Error> =>
  Effect.gen(function* () {
    yield* walk.phase("canonicalDecode", () =>
      Effect.sync(() => produceCanonicalDecode(prepared, walk)),
    );
    const macroWitness = {
      compactBinding: encodeCbor([
        prepared.input.transactionId,
        walk.frame.transactionCommitment,
        ...proofBinding(prepared),
      ]),
      staticLedgerRules: prepared.sourceContextWitnessCbor,
    };
    for (const phase of ["compactBinding", "staticLedgerRules"] as const) {
      yield* walk.phase(phase, () =>
        Effect.sync(() => {
          walk.push(phase, macroWitness[phase]);
          return phaseContinues;
        }),
      );
    }

    yield* walk.phase("inputSets", () => produceInputSets(prepared, walk));

    yield* walk.phase("signatures", () => produceSignatures(prepared, walk));

    let containsNonNativeScript: 0 | 1 = 0;
    yield* walk.phase("phaseANativeScripts", () =>
      Effect.sync(() => {
        const scan = producePhaseANativeScripts(prepared, walk);
        containsNonNativeScript = scan.containsNonNativeScript;
        return scan.outcome;
      }),
    );

    yield* walk.phase("phaseAScriptPreconditions", () =>
      Effect.sync(() =>
        producePhaseAScriptPreconditions(
          prepared,
          containsNonNativeScript,
          walk,
        ),
      ),
    );

    yield* walk.phase("validityInterval", () =>
      produceValidityInterval(prepared, walk),
    );
    yield* walk.phase("resolveInputs", () =>
      produceResolveInputs(prepared, walk),
    );

    let state = unreachedState(prepared);
    yield* walk.phase("scriptSources", () =>
      Effect.map(produceScriptSources(prepared, walk), (sources) => {
        if (sources.state !== null) state = sources.state;
        return sources.outcome;
      }),
    );
    return state;
  });
