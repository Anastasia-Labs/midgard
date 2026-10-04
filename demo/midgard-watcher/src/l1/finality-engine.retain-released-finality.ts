import { result } from "./finality-engine.external-provider-bindings-match-policy.js";
import { makeState } from "./finality-engine.parse-watcher-finality-state.js";
import type {
  WatcherFinalityBoundObservation,
  WatcherFinalityResult,
} from "./finality-engine.watcher-finality-reason-codes.js";

/** A successor checkpoint never forgets the release authority it follows. */
export const retainReleasedFinality = (
  evaluated: WatcherFinalityResult,
  released: WatcherFinalityBoundObservation | null,
): WatcherFinalityResult => {
  if (released === null || evaluated.state?.phase !== "pending")
    return evaluated;
  const { stateDigest: _digest, ...state } = evaluated.state;
  return result(
    evaluated.action,
    evaluated.protocolDecision,
    evaluated.reasonCodes,
    evaluated.alertCodes,
    makeState({ ...state, finalized: released }),
    evaluated.rewindInstruction,
  );
};
