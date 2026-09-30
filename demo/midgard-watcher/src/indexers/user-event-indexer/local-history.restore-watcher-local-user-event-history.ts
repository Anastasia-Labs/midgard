import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherDurableRuntime,
} from "../../storage/durable-runtime.js";
import {
  watcherCanonicalJson,
  type WatcherDurableStore,
} from "../../storage/durable-store.js";
import {
  WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION,
  type WatcherUserEventArchive,
} from "../../storage/user-event-checkpoint.js";
import { readWatcherUserEventArchiveIndex } from ".././user-event-history-archive.js";
import { type WatcherUserEventReferenceAuthority } from ".././user-event-reference-authority.js";
import { type WatcherLocalUserEventReplaySource } from "./local-history.admit-watcher-local-user-event-authority.js";
import { closeWatcherLocalUserEventHistory } from "./local-history.assert-watcher-local-user-event-head-current.js";
import {
  createLocalUserEventHistory,
  createWatcherLocalUserEventHistory,
} from "./local-history.create-local-user-event-history.js";
import {
  localArchivedEntry,
  localArchiveField,
} from "./local-history.local-entry-at-block.js";
import {
  localArchiveBudgets,
  localArchiveEvidence,
  type LocalArchiveObject,
  localArchiveObject,
  localCoverageOnHead,
  localHistories,
  localOwner,
  localRefuse,
  type LocalRetainedEvidence,
  type WatcherLocalUserEventHistory,
} from "./local-history.local-history-owner.js";
import { localStableOrigin } from "./local-history.local-stable-snapshot.js";
import { objectForLocalRestart } from "./local-history.prepare-watcher-local-user-event-canonical-replay.js";
import { localLivePair } from "./local-history.restore-watcher-local-user-event-coverage.js";
import {
  exactRecord,
  immutableWireValue,
  isHex32,
  isHexBytes,
  same,
  sha256Bytes,
} from "./policy.js";
import {
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  type WatcherUserEventSnapshot,
} from "./types.js";

/** Restore a completed fold. Original archive facts remain historical data;
 * only the newly acquired head pair supplies current source/finality authority. */
export const restoreWatcherLocalUserEventHistory = async (
  input: Omit<
    Parameters<typeof createWatcherLocalUserEventHistory>[0],
    "publication"
  > &
    Readonly<{
      runtime: WatcherDurableRuntime;
      archive: WatcherUserEventArchive;
      readHead: WatcherLocalUserEventReplaySource;
      referenceAuthority: WatcherUserEventReferenceAuthority;
    }>,
): Promise<WatcherLocalUserEventHistory> => {
  const publication = await readWatcherProtectedUserEventCheckpoint(
    input.runtime,
  );
  const published = readWatcherProtectedUserEventCheckpointReceipt(publication);
  const checkpoint = published.checkpoint;
  if (
    checkpoint === null ||
    published.payload === null ||
    published.validation?.schemaVersion !==
      WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION ||
    published.validation.checkpointDigest !== checkpoint.checkpointDigest ||
    published.validation.payloadDigest !== checkpoint.payloadDigest ||
    published.validation.policyDigest !== checkpoint.userEventPolicyDigest
  )
    return localRefuse(
      "restart requires durable semantic validation; explicit recovery is required",
    );
  const runtimeFinality = input.runtime.readFinality();
  if (
    runtimeFinality.phase === "quarantined" ||
    runtimeFinality.incident !== null
  )
    return localRefuse("restart runtime is quarantined");
  const history = createLocalUserEventHistory({
    ...input,
    publication,
    semanticReplay: true,
  });
  const provisional = localOwner(history);
  let headPair:
    | Awaited<ReturnType<WatcherLocalUserEventReplaySource>>
    | undefined;
  try {
    const payload = objectForLocalRestart(published.payload);
    if (
      payload.schemaVersion !==
        "midgard-watcher-local-user-event-checkpoint-payload-v1" ||
      !same(payload.policy, provisional.policy) ||
      checkpoint.userEventPolicyDigest !== provisional.policy.policyDigest ||
      !isHex32(payload.originArchiveDigest) ||
      !isHex32(payload.originDigest) ||
      !isHex32(payload.storeArchiveDigest) ||
      !Array.isArray(payload.retainedEntries) ||
      payload.retainedEntries.length === 0 ||
      payload.retainedEntries.length >
        Number(provisional.policy.maximumActiveHistoryEntries)
    )
      return localRefuse("saved semantic state dependencies differ");
    const objects = new Map<
      string,
      Readonly<{ object: LocalArchiveObject; value: unknown }>
    >();
    let retainedBytes = 0;
    let retainedNodes = 0;
    // Only the bounded current closure is loaded. Sealed historical segments
    // are left in the archive; no block is replayed or semantically revalidated.
    for (const key of checkpoint.requiredArchiveDigests) {
      const bytes = await input.archive.read(key);
      if (bytes === null || sha256Bytes(bytes) !== key)
        return localRefuse("saved semantic state archive is absent or corrupt");
      retainedBytes += bytes.length;
      if (
        retainedBytes >
        WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
      )
        return localRefuse("saved semantic state archive exceeds its bound");
      const value: unknown = JSON.parse(
        new TextDecoder("utf-8", { fatal: true }).decode(bytes),
      );
      const archived = localArchiveObject(value);
      retainedNodes += localArchiveBudgets.get(archived)!.nodes;
      if (
        archived.digest !== key ||
        retainedNodes >
          WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
      )
        return localRefuse("saved semantic state archive framing differs");
      objects.set(key, { object: archived, value });
    }
    const originArchive = objects.get(payload.originArchiveDigest);
    const savedOrigin = originArchive?.value;
    if (
      originArchive === undefined ||
      localArchiveField(savedOrigin, ["schemaVersion"]) !==
        "midgard-watcher-local-user-event-origin-archive-v1" ||
      !same(
        localStableOrigin(localArchiveField(savedOrigin, ["facts"])),
        localArchiveEvidence(localStableOrigin(provisional.origin)),
      ) ||
      localArchiveField(savedOrigin, ["facts", "originDigest"]) !==
        payload.originDigest
    )
      return localRefuse("saved origin differs from the admitted deployment");
    const stored = objects.get(payload.storeArchiveDigest)?.value;
    if (stored === undefined)
      return localRefuse("saved materialized event state is absent");
    const entries = Object.freeze(
      payload.retainedEntries.map(localArchivedEntry),
    );
    const head = entries.at(-1)!;
    if (
      !same(payload.head, head) ||
      entries.some(
        (entry, index) =>
          entry.originDigest !== payload.originDigest ||
          entry.policyDigest !== provisional.policy.policyDigest ||
          (index > 0 &&
            entry.predecessorEntryDigest !== entries[index - 1]!.entryDigest),
      )
    )
      return localRefuse("saved semantic progress marker differs");
    const snapshot = immutableWireValue(
      payload.snapshot,
    ) as WatcherUserEventSnapshot;
    const retainedIds = new Set(entries.map((entry) => entry.entryDigest));
    const pinnedPoints = new Set(
      [...snapshot.activeEvents, ...snapshot.terminalEvents].flatMap((event) =>
        "terminalPointDigest" in event
          ? [event.originPointDigest, event.terminalPointDigest]
          : [event.originPointDigest],
      ),
    );
    const evidence: LocalRetainedEvidence[] = [];
    for (const [entryArchiveDigest, archived] of objects) {
      const candidate = exactRecord(archived.value, ["entry", "observation"]);
      if (candidate === null) continue;
      const entry = localArchivedEntry(candidate.entry);
      if (
        entry.originDigest !== payload.originDigest ||
        entry.policyDigest !== provisional.policy.policyDigest
      )
        continue;
      const original = objects.get(entry.evidenceDigest)?.value;
      if (original === undefined) continue;
      const rawBlockCbor = localArchiveField(original, [
        "witnesses",
        "current",
        "observation",
        "capture",
        "nativeBlock",
        "rawBlockCbor",
      ]);
      const pointDigest = localArchiveField(original, [
        "witnesses",
        "current",
        "observation",
        "native",
        "chainPoint",
        "pointDigest",
      ]);
      const chainPointId = localArchiveField(original, [
        "witnesses",
        "current",
        "observation",
        "native",
        "chainPoint",
        "chainPointId",
      ]);
      if (
        !isHex32(pointDigest) ||
        !isHex32(chainPointId) ||
        !isHexBytes(rawBlockCbor)
      )
        return localRefuse("saved historical block facts are malformed");
      if (retainedIds.has(entry.entryDigest) || pinnedPoints.has(pointDigest))
        evidence.push(
          Object.freeze({
            entry,
            entryArchiveDigest,
            rawBlockCbor,
            pointDigest,
            chainPointId,
          }),
        );
    }
    const acceptedEvidence = Object.freeze(
      entries.map((entry) => {
        const matches = evidence.filter(
          (record) => record.entry.entryDigest === entry.entryDigest,
        );
        if (matches.length !== 1)
          return localRefuse("saved progress evidence is absent or ambiguous");
        return matches[0]!;
      }),
    );
    const pinnedEvidence = Object.freeze(
      evidence.filter(({ entry }) => !retainedIds.has(entry.entryDigest)),
    );
    if (
      [...pinnedPoints].some(
        (point) => !evidence.some((record) => record.pointDigest === point),
      )
    )
      return localRefuse("saved event provenance is absent");
    const anchor = objectForLocalRestart(
      Buffer.from(watcherCanonicalJson(payload.anchor), "utf8"),
    );
    const archiveIndex =
      anchor.kind === "activation_origin"
        ? null
        : anchor.kind === "materialized_history" && isHex32(anchor.indexDigest)
          ? await readWatcherUserEventArchiveIndex(
              input.archive,
              anchor.indexDigest,
            )
          : localRefuse("saved history anchor is invalid");
    if (
      archiveIndex !== null &&
      archiveIndex.index.indexSequence !== anchor.indexSequence
    )
      return localRefuse("saved history anchor sequence differs");
    headPair = await input.readHead(head.cursor);
    const live = localLivePair(provisional, headPair);
    if (
      !same(live.witness.current.observation.capture.point, head.cursor) ||
      live.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
        acceptedEvidence.at(-1)!.rawBlockCbor
    )
      return localRefuse(
        "saved head is no longer canonical; explicit recovery is required",
      );
    const fresh = readWatcherProtectedUserEventCheckpointReceipt(
      await readWatcherProtectedUserEventCheckpoint(input.runtime),
    );
    localLivePair(provisional, headPair);
    if (
      !same(fresh.checkpoint, checkpoint) ||
      !same(fresh.validation, published.validation)
    )
      return localRefuse("saved semantic progress changed during restart");
    localHistories.set(history, {
      ...provisional,
      originDigest: payload.originDigest,
      originArchive: originArchive.object,
      store: immutableWireValue(stored) as WatcherDurableStore,
      snapshot,
      entries,
      acceptedEvidence,
      pinnedEvidence,
      archiveIndex,
      archiveObjects: Object.freeze(
        [...objects.values()].map(({ object }) => object),
      ),
      checkpoint,
      coverage: localCoverageOnHead(head),
      semanticReplay: false,
      acceptedAtMonotonicMs: performance.now(),
    });
    return history;
  } catch (cause) {
    closeWatcherLocalUserEventHistory(history);
    throw cause;
  } finally {
    await headPair?.close();
  }
};
