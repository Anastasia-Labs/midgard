import type { WatcherNormalizedL1Block } from "../l1/l1-adapter.js";
import type { WatcherMultiProviderConsistency } from "../l1/multi-provider-consistency.js";
import { indexPersistedObservations } from "../l1/rollback-engine/state.verify-persisted-consistency-evidence.js";
import type { WatcherDurableRuntime } from "../storage/durable-runtime.js";

/** MAC-owned history is anchored by the exact current finality digest. */
export const retainedCanonicalPrefix = (
  durable: WatcherDurableRuntime,
): readonly WatcherMultiProviderConsistency[] => {
  const state = durable.read();
  if (state.currentStore === undefined) return Object.freeze([]);
  const frontier =
    state.currentFinalityState.pending ?? state.currentFinalityState.finalized;
  if (frontier == null) return Object.freeze([]);
  const history = state.authenticatedConsistencyHistory ?? [];
  let current = history.find(
    ({ consistencyDigest }) =>
      consistencyDigest === frontier.lastSeenConsistencyDigest,
  );
  if (current === undefined) return Object.freeze([]);
  const index = indexPersistedObservations(state.currentStore);
  const points = new Map<string, WatcherMultiProviderConsistency>();
  for (const candidate of history) {
    const agreement = candidate.agreement;
    if (
      candidate.status !== "agreed" ||
      candidate.protocolDecision !== "allowed" ||
      agreement === null
    )
      continue;
    const prior = points.get(agreement.blockHash);
    if (
      prior === undefined ||
      BigInt(prior.agreement!.minimumDepth) < BigInt(agreement.minimumDepth)
    )
      points.set(agreement.blockHash, candidate);
  }
  const reversed: WatcherMultiProviderConsistency[] = [];
  // The retained store's signed bound includes pending and released anchors.
  for (
    let remaining = 6_483;
    current !== undefined && remaining > 0;
    remaining -= 1
  ) {
    const agreement = current.agreement;
    if (agreement === null) break;
    const digest: string | undefined =
      current.chainAuthorityObservationDigest ??
      current.observationEvidenceDigests[0];
    const observation: WatcherNormalizedL1Block | undefined =
      digest === undefined ? undefined : index.get(digest)?.observation;
    if (
      observation === undefined ||
      observation.chainPoint.blockHash !== agreement.blockHash ||
      observation.blockContentDigest !== agreement.blockContentDigest
    )
      break;
    reversed.push(current);
    const parentHash: string | null = observation.chainPoint.parentBlockHash;
    const parent: WatcherMultiProviderConsistency | undefined =
      parentHash === null ? undefined : points.get(parentHash);
    if (
      parent?.agreement === null ||
      parent === undefined ||
      BigInt(parent.agreement.blockNo) + 1n !== BigInt(agreement.blockNo) ||
      BigInt(parent.agreement.slot) >= BigInt(agreement.slot)
    )
      break;
    current = parent;
  }
  return Object.freeze(reversed.reverse());
};

/** An intersection hint never advances the processed checkpoint. */
export const oldestRetainedCanonicalHint = (durable: WatcherDurableRuntime) => {
  const agreement = retainedCanonicalPrefix(durable)[0]?.agreement;
  return agreement === undefined || agreement === null
    ? null
    : Object.freeze({
        blockHash: agreement.blockHash,
        blockNo: agreement.blockNo,
        slot: agreement.slot,
      });
};
