import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import type { WatcherFaultProofProgressListeners } from "./fault-proof-progress-authority.types.js";

/**
 * The listeners with the proof-materials roster told of every objective an
 * admission returns, and of every objective settled, removed or forgotten,
 * after the listener it was given.
 */
export const withProofMaterialsRoster = <
  Input extends WatcherFaultProofProgressListeners,
>(
  input: Input,
): Input => {
  const roster = input.proofMaterials;
  if (roster === undefined) return input;
  const releasing =
    (listener?: (objective: WatcherProofObjective) => void) =>
    (objective: WatcherProofObjective) => {
      listener?.(objective);
      roster.released(objective);
    };
  return {
    ...input,
    onObjectiveSettled: releasing(input.onObjectiveSettled),
    onObjectiveRemoved: releasing(input.onObjectiveRemoved),
    onAdmitted: (observation, contexts) => {
      input.onAdmitted?.(observation, contexts);
      roster.admitted(contexts.map(({ decision }) => decision));
    },
  };
};
