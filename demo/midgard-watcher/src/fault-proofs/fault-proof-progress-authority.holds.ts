import type { HeaderFaultDecision } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type {
  WatcherProofRetention,
  WatcherProofRetentionTarget,
} from "../l1-follower/proof-retention.js";

/** An objective's or hold's key: `category:header`. */
export const objectiveKey = ({
  category,
  headerHash,
}: WatcherProofRetentionTarget): string => `${category}:${headerHash}`;

// Bounds the recently admitted decisions kept in memory.
const MAX_CACHED_DECISIONS = 2_048;

/**
 * Drops the oldest cached decisions no objective names, beyond the cap. A
 * completed objective may still have a coalesced invocation waiting: the
 * bounded recent identities spare those rechecks a scan of durable
 * history, while active original and latest identities stay cached.
 */
export const pruneCachedDecisions = (
  decisions: Map<string, HeaderFaultDecision>,
  objectives: Iterable<
    Readonly<{
      decision: HeaderFaultDecision;
      currentDecisionDigest?: string;
    }>
  >,
): void => {
  const retained = new Set(
    [...objectives].flatMap((objective) => [
      objective.decision.decisionDigest,
      objective.currentDecisionDigest,
    ]),
  );
  const inactive = [...decisions.keys()].filter(
    (digest) => !retained.has(digest),
  );
  for (const digest of inactive.slice(
    0,
    Math.max(0, inactive.length - MAX_CACHED_DECISIONS),
  ))
    decisions.delete(digest);
};

export type PinsInFlight = Readonly<{
  /** Runs `write` (a pin) with `key` counted as owned. */
  track<T>(key: string, write: () => Promise<T>): Promise<T>;
  has(key: string): boolean;
}>;

/**
 * The pins in flight, which the release of holds no objective owns counts
 * as owned. A key stays counted from before its pin is written until the
 * turn after the pin settles: its caller registers the objective in the
 * same turn as the pin's continuation, so no release finds it pinned and
 * unregistered.
 */
export const createPinsInFlight = (): PinsInFlight => {
  const counts = new Map<string, number>();
  return Object.freeze({
    has: (key) => counts.has(key),
    track: async (key, write) => {
      counts.set(key, (counts.get(key) ?? 0) + 1);
      try {
        return await write();
      } finally {
        setImmediate(() => {
          const left = (counts.get(key) ?? 1) - 1;
          if (left === 0) counts.delete(key);
          else counts.set(key, left);
        });
      }
    },
  });
};

/**
 * At an admission's final return: when the pass is at tip
 * (`WatcherProofRetention.passAtTip`, which lifts the proof roster's prune
 * floor), releases the holds the authority does not own (`owned`, by
 * `objectiveKey`) and whose history no reader needs any more
 * (`WatcherProofRetention.releaseUnowned`). No other path releases a hold
 * no objective owns.
 */
export const releaseAtTip = async (
  retention: WatcherProofRetention | undefined,
  observation: WatcherAuthenticatedStateQueueObservation,
  owned: (key: string) => boolean,
): Promise<void> => {
  if (
    retention === undefined ||
    !(await retention.passAtTip(observation.nativePoint))
  )
    return;
  await retention.releaseUnowned({
    owned: (target) => owned(objectiveKey(target)),
    queued: new Set(
      observation.finalizedHeaders.map(({ headerHash }) => headerHash),
    ),
  });
};
