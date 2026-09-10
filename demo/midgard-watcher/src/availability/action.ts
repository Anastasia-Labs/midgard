import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export type WatcherAvailabilityAction = Readonly<{
  action: "open" | "settle" | "close" | "timeout" | "prune" | "remove";
  challengeAssetName?: string;
  tranche?: SDK.DaAvailabilityChallengeSnapshot["tranches"][number];
}>;

/** A fresh authenticated snapshot determines every next step, including restart. */
export const selectWatcherAvailabilityAction = (
  snapshot: SDK.DaAvailabilityChallengeSnapshot,
  publiclyAvailable: boolean,
  inclusiveValidityLower: bigint,
): WatcherAvailabilityAction | null => {
  if (snapshot.correctionLock.datum == null)
    throw new Error("Availability snapshot omitted correction lock datum");
  const lock = Data.from(
    snapshot.correctionLock.datum,
    SDK.CorrectionLockDatum,
  );
  if (lock !== "Idle") {
    const identity = lock.Locked.correction_identity;
    if (
      lock.Locked.target_header_hash !== snapshot.headerHash ||
      typeof identity !== "object" ||
      !("AvailabilityChallenge" in identity)
    )
      return null;
    if (snapshot.bond !== undefined)
      throw new Error("Availability removal lock still has a live DA bond");
    if (snapshot.queue === undefined)
      throw new Error("Availability removal lock lost its target queue node");
    return {
      action: snapshot.descendant === undefined ? "remove" : "prune",
      challengeAssetName: identity.AvailabilityChallenge.challenge_asset_name,
    };
  }
  const bond = snapshot.bondDatum;
  if (bond === undefined) return null;
  if ("Available" in bond) return publiclyAvailable ? null : { action: "open" };
  const terminal = snapshot.terminalDatum;
  if (terminal === undefined || snapshot.terminal === undefined)
    throw new Error("Live availability challenge has no terminal accumulator");
  const challenge = bond.ChallengedBond;
  if (
    terminal.next_tranche_index ===
    BigInt(challenge.commitment.tranche_descriptors.length)
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
      challengeAssetName: challenge.challenge_asset_name,
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
    inclusiveValidityLower >= challenge.response_deadline
  ) {
    return {
      action: "settle",
      tranche,
      challengeAssetName: challenge.challenge_asset_name,
    };
  }
  return null;
};
