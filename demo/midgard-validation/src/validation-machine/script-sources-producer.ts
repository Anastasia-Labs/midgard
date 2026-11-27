/**
 * The ScriptSources producer: its thirteen stages in order, each stage
 * handed the earlier stages' folds as explicit values.
 */
import { Effect } from "effect";

import type { PreparedValidationTransaction } from "./prepared-validation-transaction.js";
import { produceScriptAudit } from "./script-sources-producer.audit.js";
import {
  discoveryRun,
  produceScriptDiscovery,
} from "./script-sources-producer.discovery.js";
import {
  produceInlineSources,
  produceRedeemerItems,
} from "./script-sources-producer.early-stages.js";
import { produceOutputAdmission } from "./script-sources-producer.outputs.js";
import { produceScriptPurposes } from "./script-sources-producer.purposes.js";
import { produceResolvedInputReplay } from "./script-sources-producer.replay.js";
import type { ValidationTraceWalkState } from "./trace-builder-types.js";
import {
  phaseContinues,
  type PhaseOutcome,
  type TraceWalk,
} from "./trace-walk.js";

/**
 * Stages 0 to 12. A walk that passes them leaves the state the later
 * phases read; one that stops in them leaves none.
 */
export const produceScriptSources = (
  prepared: PreparedValidationTransaction,
  { push }: Pick<TraceWalk, "push">,
): Effect.Effect<
  {
    readonly outcome: PhaseOutcome;
    readonly state: ValidationTraceWalkState | null;
  },
  Error
> =>
  Effect.gen(function* () {
    const stopped = (outcome: PhaseOutcome) => ({ outcome, state: null });
    const inlineSourceCount = yield* produceInlineSources(prepared, push);
    const redeemers = yield* produceRedeemerItems(
      prepared,
      push,
      inlineSourceCount,
    );
    if (redeemers.kind === "stopped") return stopped(redeemers);
    const resolved = yield* produceResolvedInputReplay(prepared, push);
    const { replay } = resolved;
    const admission = yield* produceOutputAdmission(prepared, push, replay);
    if (admission.kind !== "admitted") return stopped(admission);
    const purposes = yield* produceScriptPurposes(
      prepared,
      push,
      replay,
      admission,
    );
    const sourceEntries = [
      ...prepared.inlineScriptSourceEntries,
      ...resolved.sourceEntries,
    ];
    const purposeEntries = [
      ...resolved.purposeEntries,
      ...purposes.purposeEntries,
    ];
    const run = discoveryRun(
      prepared,
      push,
      replay,
      admission,
      purposes,
      sourceEntries,
    );
    const discovered = produceScriptDiscovery(run, purposeEntries);
    if (discovered.kind !== "discovered") return stopped(discovered);
    const audit = produceScriptAudit(
      run,
      discovered.discovery,
      admission.receiveScan.descriptorFrontier,
    );
    if (audit.kind !== "audited") return stopped(audit);
    return {
      outcome: phaseContinues,
      state: {
        scriptSourceEntries: sourceEntries,
        scriptPurposeEntries: purposeEntries,
        scriptExecutionEntries: discovered.executionEntries,
        admittedOutputDescriptorCbors: admission.admittedOutputDescriptorCbors,
        admittedOutputDescriptorLeafHashes:
          admission.admittedOutputDescriptorLeafHashes,
        mintFoldControl: purposes.mintFoldControl,
        authenticatedNativeScriptsWitnessCbor:
          audit.authenticatedNativeScriptsWitnessCbor,
        authenticatedNativeScriptsBaseFields:
          audit.authenticatedNativeScriptsBaseFields,
      },
    };
  });
