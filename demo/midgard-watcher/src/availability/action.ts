import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export type WatcherAvailabilityAction = Readonly<{
  action: "open" | "settle" | "close" | "timeout" | "prune" | "remove";
  challengeAssetName?: string;
  tranche?: SDK.DaAvailabilityChallengeSnapshot["tranches"][number];
}>;

/**
 * The Open deadline: `OpenChallenge` admits only an inclusive upper validity
 * bound strictly before `header.end_time + da_challenge_window_ms`.
 */
export type WatcherAvailabilityOpenWindow = Readonly<{
  /** The inclusive upper bound of the transaction the watcher would build. */
  inclusiveValidityUpper: bigint;
  daChallengeWindowMs: bigint;
}>;

/** The DA status the queue node carries, decoded from its authenticated datum. */
export const watcherAvailabilityQueueStatus = (
  snapshot: SDK.DaAvailabilityChallengeSnapshot,
): SDK.DaAvailabilityStateQueueStatus | undefined =>
  snapshot.queue === undefined
    ? undefined
    : Data.castFrom(snapshot.queue.datum.data, SDK.StateQueueNode)
        .da_attestation;

/**
 * True when an Open with the given inclusive upper bound is past the header's
 * deadline; the header can then no longer be challenged.
 */
export const watcherAvailabilityOpenDeadlinePassed = (
  snapshot: SDK.DaAvailabilityChallengeSnapshot,
  window: WatcherAvailabilityOpenWindow,
): boolean => {
  if (snapshot.queue === undefined) return false;
  const node = Data.castFrom(snapshot.queue.datum.data, SDK.StateQueueNode);
  return (
    window.inclusiveValidityUpper >=
    node.header.endTime + window.daChallengeWindowMs
  );
};

/** A fresh authenticated snapshot determines every next step, including restart. */
export const selectWatcherAvailabilityAction = (
  snapshot: SDK.DaAvailabilityChallengeSnapshot,
  publiclyAvailable: boolean,
  inclusiveValidityLower: bigint,
  openWindow: WatcherAvailabilityOpenWindow,
): WatcherAvailabilityAction | null => {
  if (snapshot.correctionLock.datum == null)
    throw new Error("Availability snapshot omitted correction lock datum");
  const lock = Data.from(
    snapshot.correctionLock.datum,
    SDK.CorrectionLockDatum,
  );
  if (lock !== "Idle") {
    // A live removal prunes every later queue node, so no other header needs
    // (or can safely take) an availability step until it finishes.
    const identity = lock.Locked.correction_identity;
    if (
      lock.Locked.target_header_hash !== snapshot.headerHash ||
      typeof identity !== "object" ||
      !("AvailabilityChallenge" in identity)
    )
      return null;
    if (snapshot.record !== undefined)
      throw new Error(
        "Availability removal lock still has a live challenge record",
      );
    if (snapshot.queue === undefined)
      throw new Error("Availability removal lock lost its target queue node");
    return {
      action: snapshot.descendant === undefined ? "remove" : "prune",
      challengeAssetName: identity.AvailabilityChallenge.challenge_asset_name,
    };
  }
  const status = watcherAvailabilityQueueStatus(snapshot);
  if (status === undefined || status === "Unattested") return null;
  if ("Published" in status) return null;
  if ("Attested" in status) {
    if (snapshot.record !== undefined)
      throw new Error("Attested availability header has a challenge record");
    if (publiclyAvailable) return null;
    // Past the deadline no Open can land; the header merges unchallenged.
    if (watcherAvailabilityOpenDeadlinePassed(snapshot, openWindow))
      return null;
    return { action: "open" };
  }
  const record = snapshot.recordDatum;
  if (record === undefined || snapshot.record === undefined)
    throw new Error("Challenged availability header has no challenge record");
  if (
    record.challenge_asset_name !== status.Challenged.challenge_asset_name ||
    SDK.daAvailabilityCommitmentHash(record.commitment) !==
      status.Challenged.commitment_hash
  )
    throw new Error(
      "Challenge record differs from the queue node's Challenged status",
    );
  const terminal = snapshot.terminalDatum;
  if (terminal === undefined || snapshot.terminal === undefined)
    throw new Error("Live availability challenge has no terminal accumulator");
  if (
    terminal.next_tranche_index ===
    BigInt(record.commitment.tranche_descriptors.length)
  ) {
    // Queue removal is rooted at confirmed state. An unavailable later header
    // must wait for its predecessors to leave the queue before taking the lock.
    const next = snapshot.confirmedState.datum.next;
    if (
      terminal.has_timed_out_tranche &&
      (next === "Empty" || next.Key.key !== snapshot.headerHash)
    )
      return null;
    return {
      action: terminal.has_timed_out_tranche ? "timeout" : "close",
      challengeAssetName: record.challenge_asset_name,
    };
  }
  const tranche = snapshot.tranches.find(
    ({ datum }) =>
      ("Active" in datum ? datum.Active : datum.Receipt).descriptor
        .tranche_index === terminal.next_tranche_index,
  );
  if (tranche === undefined)
    throw new Error(
      "Live availability challenge lost its next unsettled tranche",
    );
  if (
    "Receipt" in tranche.datum ||
    inclusiveValidityLower >= record.response_deadline
  ) {
    return {
      action: "settle",
      tranche,
      challengeAssetName: record.challenge_asset_name,
    };
  }
  return null;
};

/**
 * Every header's next step, independently (spec #685 E3): a live challenge on
 * one withheld header never suppresses the Open of another, whose own
 * deadline could otherwise pass unchallenged.
 */
export const selectWatcherAvailabilityActions = (
  candidates: readonly Readonly<{
    snapshot: SDK.DaAvailabilityChallengeSnapshot;
    publiclyAvailable: boolean;
  }>[],
  inclusiveValidityLower: bigint,
  openWindow: WatcherAvailabilityOpenWindow,
): readonly Readonly<{
  snapshot: SDK.DaAvailabilityChallengeSnapshot;
  action: WatcherAvailabilityAction;
}>[] =>
  candidates.flatMap(({ snapshot, publiclyAvailable }) => {
    const action = selectWatcherAvailabilityAction(
      snapshot,
      publiclyAvailable,
      inclusiveValidityLower,
      openWindow,
    );
    return action === null ? [] : [{ snapshot, action }];
  });

/**
 * An Attested header whose payload is withheld but whose Open deadline has
 * passed under an Idle lock: it merges unchallenged. Reported, never actuated.
 */
export const watcherAvailabilityOpenDeadlineMissed = (
  snapshot: SDK.DaAvailabilityChallengeSnapshot,
  publiclyAvailable: boolean,
  openWindow: WatcherAvailabilityOpenWindow,
): boolean => {
  if (publiclyAvailable || snapshot.correctionLock.datum == null) return false;
  const status = watcherAvailabilityQueueStatus(snapshot);
  return (
    typeof status === "object" &&
    "Attested" in status &&
    Data.from(snapshot.correctionLock.datum, SDK.CorrectionLockDatum) ===
      "Idle" &&
    watcherAvailabilityOpenDeadlinePassed(snapshot, openWindow)
  );
};

/**
 * Actuation order for one reconciliation: Opens first, earliest Open deadline
 * (header end_time) first, then every other step in queue order.
 */
export const orderWatcherAvailabilityActions = <
  T extends Readonly<{
    snapshot: SDK.DaAvailabilityChallengeSnapshot;
    action: WatcherAvailabilityAction;
  }>,
>(
  selected: readonly T[],
): readonly T[] => {
  const endTime = (snapshot: SDK.DaAvailabilityChallengeSnapshot): bigint =>
    Data.castFrom(snapshot.queue!.datum.data, SDK.StateQueueNode).header
      .endTime;
  const opens = selected
    .map((entry, index) => ({ entry, index }))
    .filter(({ entry }) => entry.action.action === "open")
    .sort((left, right) => {
      const a = endTime(left.entry.snapshot);
      const b = endTime(right.entry.snapshot);
      return a === b ? left.index - right.index : a < b ? -1 : 1;
    })
    .map(({ entry }) => entry);
  return [
    ...opens,
    ...selected.filter((entry) => entry.action.action !== "open"),
  ];
};
