import { admitFraudProofRawL1Point } from "@al-ft/midgard-fault-proofs";

import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherDurableRuntime,
} from "../../storage/durable-runtime.js";
import { type WatcherUserEventArchive } from "../../storage/user-event-checkpoint.js";
import { type WatcherStateQueueHeaderObservation } from ".././authenticated-state-queue-observation.js";
import { type WatcherUserEventOriginFacts } from ".././user-event-origin.js";
import { localAuthorityUnavailable } from "./local-history.assert-watcher-local-user-event-head-current.js";
import { localEntryAtBlock } from "./local-history.local-entry-at-block.js";
import {
  localCutoffTransactionOrder,
  localEventAtHeaderCutoff,
  localHeaderBlock,
  readWatcherLocalUserEventAuthority,
  type WatcherLocalUserEventPointCoverage,
} from "./local-history.local-event-at-header-cutoff.js";
import {
  localEventAuthorities,
  localEventAuthorityBrand,
  localOwner,
  type LocalPair,
  localRefuse,
  localRetainedEvidence,
  type WatcherLocalUserEventAuthority,
  type WatcherLocalUserEventHistory,
} from "./local-history.local-history-owner.js";
import { same, sha256Bytes } from "./policy.js";
import { type WatcherUserEventKind } from "./types.js";

/** Verify an older stream intersection against the accepted private/archive
 * lineage. A matching height alone never permits skipping native blocks. */
export const assertWatcherLocalUserEventPointCovered = async (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    point: WatcherUserEventOriginFacts["parentPoint"];
    runtime: WatcherDurableRuntime;
    archive: WatcherUserEventArchive;
  }>,
): Promise<WatcherLocalUserEventPointCoverage> => {
  const owner = localOwner(input.history);
  const generation = owner.generation;
  const checkpoint = owner.checkpoint;
  if (
    checkpoint === null ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null
  )
    return localRefuse("coverage requires a settled publication");
  const point = admitFraudProofRawL1Point(input.point);
  const head = owner.entries.at(-1)!;
  if (BigInt(point.blockNo) > BigInt(head.cursor.blockNo))
    return localRefuse(
      "point lies above the head entry; coverage of the quiet stretch is a runtime lookup",
    );
  const found = await localEntryAtBlock(
    owner,
    BigInt(point.blockNo),
    input.archive,
  );
  if (found !== null) {
    // An event block: the point must be exactly the accepted block.
    const block = await localHeaderBlock(
      owner,
      {
        observedBlockHash: point.blockHash,
        observedBlockNo: point.blockNo,
        observedSlot: point.slot,
      },
      input.archive,
    );
    localCutoffTransactionOrder(block);
  }
  // A quiet block at or below the head entry lies inside the strict successor
  // chain the head's lineage established. Callers below the release-final
  // boundary need no hash check; the head entry's canonical corroboration
  // already fixes every ancestor by construction.
  const publication = readWatcherProtectedUserEventCheckpointReceipt(
    await readWatcherProtectedUserEventCheckpoint(input.runtime),
  );
  const finality = input.runtime.read().currentFinalityState;
  if (
    localOwner(input.history) !== owner ||
    owner.generation !== generation ||
    owner.checkpoint !== checkpoint ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    finality.phase === "quarantined" ||
    finality.incident !== null ||
    !same(publication.checkpoint, checkpoint) ||
    publication.payload === null ||
    sha256Bytes(publication.payload) !== checkpoint.payloadDigest
  )
    return localRefuse("coverage changed during protected archive read");
  return found === null ? "quiet" : "event";
};

/** Issues one retained event from the privately published whole-block fold. */
export const admitWatcherLocalUserEventAuthority = async (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      runtime: WatcherDurableRuntime;
      eventId: string;
      kind: WatcherUserEventKind;
      throughHeader?: WatcherStateQueueHeaderObservation;
      archive?: WatcherUserEventArchive;
    }>,
): Promise<WatcherLocalUserEventAuthority> => {
  const {
    history,
    runtime,
    eventId,
    kind,
    throughHeader,
    archive,
    finality,
    observation,
    referenceAuthority,
  } = input;
  const owner = localOwner(history);
  const generation = owner.generation;
  const checkpoint = owner.checkpoint;
  const head = owner.entries.at(-1);
  if (checkpoint === null || head === undefined)
    return localRefuse("event authority requires a published history");
  const matches = [
    ...owner.snapshot.activeEvents,
    ...owner.snapshot.terminalEvents,
  ].filter((event) => event.eventId === eventId && event.kind === kind);
  if (matches.length === 0)
    return localAuthorityUnavailable("event is not retained");
  if (matches.length !== 1)
    return localRefuse("event is not uniquely retained");
  const event = matches[0]!;
  const retainedEvidence = localRetainedEvidence(owner);
  const originIndex = retainedEvidence.findIndex(
    ({ pointDigest }) => pointDigest === event.originPointDigest,
  );
  const terminalIndex =
    "terminalPointDigest" in event
      ? retainedEvidence.findIndex(
          ({ pointDigest }) => pointDigest === event.terminalPointDigest,
        )
      : originIndex;
  if (
    originIndex < 0 ||
    terminalIndex < originIndex ||
    event.finalityStatus !== "final" ||
    ("terminalFinalityStatus" in event &&
      event.terminalFinalityStatus !== "final")
  )
    return localRefuse(
      "event origin or terminal membership is absent from finalized history",
    );
  const terminal =
    owner.snapshot.terminalEvents.find(
      (candidate) => candidate.eventId === eventId && candidate.kind === kind,
    ) ?? null;
  const scoped =
    throughHeader === undefined
      ? null
      : await localEventAtHeaderCutoff(
          owner,
          event,
          terminal,
          retainedEvidence[originIndex]!,
          retainedEvidence[terminalIndex]!,
          throughHeader,
          archive ?? localRefuse("header cutoff requires the history archive"),
        );
  if (
    localOwner(history) !== owner ||
    owner.generation !== generation ||
    owner.checkpoint !== checkpoint ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null
  )
    return localRefuse("history changed during header cutoff acquisition");
  const receipt = Object.freeze({ [localEventAuthorityBrand]: true as const });
  localEventAuthorities.set(
    receipt,
    Object.freeze({
      history,
      header: throughHeader ?? null,
      runtime,
      pair: Object.freeze({ finality, observation, referenceAuthority }),
      generation,
      protectedRead: { receipt: null },
      value: Object.freeze({
        deploymentManifestId: owner.origin.deploymentFingerprint,
        blueprintHash: owner.policy.blueprintHash,
        network: owner.policy.network,
        event: scoped?.event ?? event,
        throughHeader: scoped?.throughHeader ?? null,
        checkpointDigest: checkpoint.checkpointDigest,
        checkpointPayloadDigest: checkpoint.payloadDigest,
        snapshotDigest: owner.snapshot.snapshotDigest,
        headEntryDigest: head.entryDigest,
        historyEntryDigests: Object.freeze([
          ...new Set([
            retainedEvidence[originIndex]!.entry.entryDigest,
            ...(scoped === null || scoped.includeTerminal
              ? [retainedEvidence[terminalIndex]!.entry.entryDigest]
              : []),
            ...(scoped === null
              ? []
              : [scoped.throughHeader.historyEntryDigest]),
          ]),
        ]),
      }),
    }),
  );
  await readWatcherLocalUserEventAuthority(receipt);
  return receipt;
};

export type WatcherLocalUserEventReplaySource = (
  point: WatcherUserEventOriginFacts["parentPoint"],
) => Promise<LocalPair & Readonly<{ close(): Promise<void> }>>;
