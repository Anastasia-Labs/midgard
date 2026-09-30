import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherDurableRuntime,
} from "../../storage/durable-runtime.js";
import {
  journalWatcherProtocolUtxoTransition,
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  parseWatcherDurableStore,
} from "../../storage/durable-store.js";
import {
  makeWatcherUserEventCheckpoint,
  type WatcherUserEventArchive,
} from "../../storage/user-event-checkpoint.js";
import {
  findWatcherUserEventArchiveIndex,
  readWatcherUserEventArchiveIndex,
  type WatcherUserEventArchiveIndexRead,
} from ".././user-event-history-archive.js";
import { type WatcherUserEventReferenceAuthority } from ".././user-event-reference-authority.js";
import { type WatcherLocalUserEventReplaySource } from "./local-history.admit-watcher-local-user-event-authority.js";
import {
  closeWatcherLocalUserEventHistory,
  commitLocalUserEventTransition,
  readWatcherLocalUserEventTransition,
} from "./local-history.assert-watcher-local-user-event-head-current.js";
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
  localOwner,
  localReadmissionBrand,
  localReadmissions,
  localRefuse,
  type WatcherLocalUserEventEntry,
  type WatcherLocalUserEventReadmission,
} from "./local-history.local-history-owner.js";
import {
  localAnchors,
  localMaterializedStoreFromPoints,
  localStableEventStore,
  localStableOrigin,
  localStableSnapshot,
} from "./local-history.local-stable-snapshot.js";
import {
  commitLocalUserEventAnchor,
  prepareLocalUserEventAnchor,
  readWatcherLocalUserEventAnchor,
} from "./local-history.prepare-local-user-event-anchor.js";
import { prepareLocalUserEventTransition } from "./local-history.prepare-local-user-event-transition.js";
import { localLivePair } from "./local-history.restore-watcher-local-user-event-coverage.js";
import {
  evidenceWithinBounds,
  exactRecord,
  isHex32,
  same,
  sha256Bytes,
  sha256Canonical,
} from "./policy.js";
import {
  parseObservationStructural,
  protocolRole,
  storeDigest,
  topologyMatches,
} from "./snapshot.js";
import {
  type EvidenceGraphBudget,
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  type WatcherUserEventObservation,
} from "./types.js";

/** A protected archive is descriptive input. Only the actual fresh W12 pairs
 * drive this private fold; archived observations never become W12 receipts.
 * Indexed sealed segments are replayed chronologically with bounded live state.
 */
export const prepareWatcherLocalUserEventReadmission = async (
  input: Omit<
    Parameters<typeof createWatcherLocalUserEventHistory>[0],
    "publication"
  > &
    Readonly<{
      referenceAuthority: WatcherUserEventReferenceAuthority;
      runtime: WatcherDurableRuntime;
      archive: WatcherUserEventArchive;
      replayBlock: WatcherLocalUserEventReplaySource;
    }>,
): Promise<WatcherLocalUserEventReadmission> => {
  const {
    origin,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
    referenceAuthority,
    runtime,
    archive,
    replayBlock,
  } = input;
  const firstPair = Object.freeze({
    finality,
    observation,
    referenceAuthority,
  });
  const publication = await readWatcherProtectedUserEventCheckpoint(runtime);
  const protectedHead =
    readWatcherProtectedUserEventCheckpointReceipt(publication);
  const previousCheckpoint = protectedHead.checkpoint;
  const runtimeFinality = runtime.read().currentFinalityState;
  if (
    runtimeFinality.phase === "quarantined" ||
    runtimeFinality.incident !== null
  )
    return localRefuse("semantic readmission runtime is quarantined");
  if (previousCheckpoint === null || protectedHead.payload === null)
    return localRefuse("semantic readmission requires a published checkpoint");
  const history = createLocalUserEventHistory({
    origin,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
    publication,
    semanticReplay: true,
  });
  const owner = localOwner(history);
  const replayState: {
    retainedSource: Awaited<
      ReturnType<WatcherLocalUserEventReplaySource>
    > | null;
    priorIndex: WatcherUserEventArchiveIndexRead | null;
  } = { retainedSource: null, priorIndex: null };
  try {
    if (
      previousCheckpoint.userEventPolicyDigest !== owner.policy.policyDigest ||
      previousCheckpoint.finalityPolicyDigest !==
        owner.finalityPolicy.policyDigest ||
      previousCheckpoint.blueprintHash !== owner.policy.blueprintHash ||
      previousCheckpoint.network !== owner.policy.network
    )
      return localRefuse("archived policy differs from the fresh deployment");
    const bootstrapStore = owner.store;
    const readClosure = async (requiredDigests: readonly string[]) => {
      const objects = new Map<
        string,
        Readonly<{ object: LocalArchiveObject; value: unknown }>
      >();
      const budget: EvidenceGraphBudget = { nodes: 0, bytes: 0 };
      let bytesRead = 0;
      for (const digest of requiredDigests) {
        const bytes = await archive.read(digest);
        if (bytes === null || sha256Bytes(bytes) !== digest)
          return localRefuse("archived closure is absent or corrupt");
        bytesRead += bytes.byteLength;
        if (
          bytesRead > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes
        )
          return localRefuse("archived closure byte bound exceeded");
        let value: unknown;
        try {
          value = JSON.parse(
            new TextDecoder("utf-8", { fatal: true }).decode(bytes),
          );
        } catch {
          return localRefuse("archived closure is not canonical JSON");
        }
        if (!evidenceWithinBounds(value, budget))
          return localRefuse("archived closure evidence bound exceeded");
        const object = localArchiveObject(value);
        if (object.digest !== digest)
          return localRefuse("archived closure JSON encoding differs");
        objects.set(digest, Object.freeze({ object, value }));
      }
      return objects;
    };
    const readValue = async (digest: string): Promise<unknown> => {
      const objects = await readClosure([digest]);
      return objects.get(digest)!.value;
    };
    const parsePayload = (payloadValue: unknown) => {
      const hasReadmission =
        typeof payloadValue === "object" &&
        payloadValue !== null &&
        Object.hasOwn(payloadValue, "readmission");
      const payload = exactRecord(payloadValue, [
        "schemaVersion",
        "originArchiveDigest",
        "originDigest",
        "policy",
        "anchor",
        "head",
        "storeArchiveDigest",
        "snapshot",
        "retainedEntries",
        "requiredSemanticResume",
        ...(hasReadmission ? ["readmission"] : []),
      ]);
      if (
        payload === null ||
        payload.schemaVersion !==
          "midgard-watcher-local-user-event-checkpoint-payload-v1" ||
        payload.requiredSemanticResume !==
          "authenticated_origin_replay_or_semantic_publication_receipt" ||
        !isHex32(payload.originArchiveDigest) ||
        !isHex32(payload.originDigest) ||
        !isHex32(payload.storeArchiveDigest) ||
        !same(payload.policy, owner.policy) ||
        !Array.isArray(payload.retainedEntries) ||
        payload.retainedEntries.length === 0 ||
        payload.retainedEntries.length >
          Number(owner.policy.maximumActiveHistoryEntries)
      )
        return localRefuse("archived semantic payload framing differs");
      return {
        payload: {
          schemaVersion: payload.schemaVersion,
          originArchiveDigest: payload.originArchiveDigest,
          originDigest: payload.originDigest,
          policy: payload.policy,
          anchor: payload.anchor,
          head: payload.head,
          storeArchiveDigest: payload.storeArchiveDigest,
          snapshot: payload.snapshot,
          retainedEntries: payload.retainedEntries,
          requiredSemanticResume: payload.requiredSemanticResume,
          readmission: payload.readmission,
        },
        hasReadmission,
      };
    };
    const currentObjects = await readClosure(
      previousCheckpoint.requiredArchiveDigests,
    );
    const currentPayloadValue =
      currentObjects.get(previousCheckpoint.payloadDigest)?.value ??
      localRefuse("current payload is absent");
    const { payload } = parsePayload(currentPayloadValue);
    const anchorDescriptor = exactRecord(payload.anchor, [
      "kind",
      "indexDigest",
      "indexSequence",
      "retainedSuffixEntries",
    ]);
    let rootIndex: WatcherUserEventArchiveIndexRead | null = null;
    if (anchorDescriptor !== null) {
      if (
        anchorDescriptor.kind !== "materialized_history" ||
        !isHex32(anchorDescriptor.indexDigest) ||
        anchorDescriptor.retainedSuffixEntries !== "64"
      )
        return localRefuse("archive anchor descriptor differs");
      rootIndex = await readWatcherUserEventArchiveIndex(
        archive,
        anchorDescriptor.indexDigest,
      );
      if (rootIndex.index.indexSequence !== anchorDescriptor.indexSequence)
        return localRefuse("archive root sequence differs");
    }
    let lastProcessedSequence = -1n;
    let lastProcessedEntry: WatcherLocalUserEventEntry | null = null;
    let oldRetainedEntries: readonly WatcherLocalUserEventEntry[] = [];
    let archivedSourceStore = bootstrapStore;
    const writePrivateObjects = async (
      objects: readonly LocalArchiveObject[],
    ) => {
      for (const object of objects) {
        const digest = await archive.put(Buffer.from(object.bytesHex, "hex"));
        if (digest !== object.digest)
          return localRefuse("semantic replay archive write differs");
      }
    };
    const replaySegment = async (
      objects: Awaited<ReturnType<typeof readClosure>>,
      payloadDigest: string,
      sealedIndex: WatcherUserEventArchiveIndexRead | null,
    ) => {
      const archived = (digest: string): unknown =>
        objects.get(digest)?.value ??
        localRefuse("archive dependency is absent");
      const { payload, hasReadmission } = parsePayload(archived(payloadDigest));
      const expectedAnchor =
        replayState.priorIndex === null
          ? {
              kind: "activation_origin",
              parent: owner.origin.parentPoint,
              bootstrapStoreDigest: owner.policy.bootstrapStoreDigest,
            }
          : {
              kind: "materialized_history",
              indexDigest: replayState.priorIndex.digest,
              indexSequence: replayState.priorIndex.index.indexSequence,
              retainedSuffixEntries: "64",
            };
      const oldOrigin = exactRecord(archived(payload.originArchiveDigest), [
        "schemaVersion",
        "numericEncoding",
        "facts",
        "policy",
        "bootstrapStore",
      ]);
      if (
        payload.originDigest !==
          localArchiveField(currentPayloadValue, ["originDigest"]) ||
        oldOrigin === null ||
        oldOrigin.schemaVersion !==
          "midgard-watcher-local-user-event-origin-archive-v1" ||
        oldOrigin.numericEncoding !== "exact-decimal-strings" ||
        !same(oldOrigin.policy, owner.policy) ||
        !same(
          localStableOrigin(oldOrigin.facts),
          localArchiveEvidence(localStableOrigin(owner.origin)),
        ) ||
        localArchiveField(oldOrigin.facts, ["originDigest"]) !==
          payload.originDigest ||
        !same(oldOrigin.bootstrapStore, bootstrapStore) ||
        !same(payload.anchor, expectedAnchor)
      )
        return localRefuse(
          "archived activation origin differs from the fresh authenticated activation",
        );
      if (hasReadmission) {
        const priorReadmission = exactRecord(payload.readmission, [
          "kind",
          "previousCheckpointDigest",
          "previousPayloadDigest",
          "archivedOriginDigest",
          "freshOriginDigest",
          "stableSnapshotDigest",
        ]);
        if (
          priorReadmission === null ||
          priorReadmission.kind !== "fresh_authenticated_origin_replay" ||
          !isHex32(priorReadmission.previousCheckpointDigest) ||
          !isHex32(priorReadmission.previousPayloadDigest) ||
          !isHex32(priorReadmission.archivedOriginDigest) ||
          priorReadmission.freshOriginDigest !== payload.originDigest ||
          priorReadmission.stableSnapshotDigest !==
            sha256Canonical(localStableSnapshot(payload.snapshot)) ||
          localArchiveField(
            await readValue(priorReadmission.previousPayloadDigest),
            ["originDigest"],
          ) !== priorReadmission.archivedOriginDigest
        )
          return localRefuse("archived semantic readmission linkage differs");
      }
      const entries = payload.retainedEntries.map(localArchivedEntry);
      if (
        entries.some(
          (entry, index) =>
            BigInt(entry.sequence) !==
            BigInt(entries[0]!.sequence) + BigInt(index),
        ) ||
        entries[0]!.sequence !== (oldRetainedEntries[0]?.sequence ?? "0")
      )
        return localRefuse(
          "archived retained entries are not the exact consecutive suffix",
        );
      if (
        !same(payload.head, entries.at(-1)) ||
        (replayState.priorIndex === null &&
          payload.storeArchiveDigest !== entries.at(-1)!.nextStoreDigest)
      )
        return localRefuse("archived head differs");
      const entryObservations = new Map<string, WatcherUserEventObservation>();
      for (const { value } of objects.values()) {
        const entryArchive = exactRecord(value, ["entry", "observation"]);
        if (entryArchive === null) continue;
        const entry = localArchivedEntry(entryArchive.entry);
        localStableSnapshot(
          localArchiveField(entryArchive.observation, ["snapshot"]),
        );
        const observed = parseObservationStructural(entryArchive.observation);
        if (
          observed === null ||
          observed.observationDigest !== entry.observationDigest ||
          entryObservations.has(entry.entryDigest)
        )
          return localRefuse("archived entry observation differs");
        entryObservations.set(entry.entryDigest, observed);
      }
      const headStore = parseWatcherDurableStore(
        archived(payload.storeArchiveDigest),
      );
      if (storeDigest(headStore) !== payload.storeArchiveDigest)
        return localRefuse("archived head store digest differs");
      for (const entry of entries) {
        if (BigInt(entry.sequence) <= lastProcessedSequence) {
          if (!oldRetainedEntries.some((retained) => same(retained, entry)))
            return localRefuse(
              "archived retained suffix differs from its materialization",
            );
          continue;
        }
        const previous: WatcherLocalUserEventEntry | null = lastProcessedEntry;

        const oldObservation = entryObservations.get(entry.entryDigest);
        if (
          BigInt(entry.sequence) !== lastProcessedSequence + 1n ||
          entry.originDigest !== payload.originDigest ||
          entry.policyDigest !== owner.policy.policyDigest ||
          entry.predecessorEntryDigest !== (previous?.entryDigest ?? null) ||
          entry.sourceStoreDigest !== storeDigest(archivedSourceStore) ||
          BigInt(entry.nextStoreRevision) !==
            BigInt(entry.sourceStoreRevision) + 1n ||
          entry.sourceStoreRevision !== archivedSourceStore.revision ||
          oldObservation === undefined ||
          oldObservation.transitionKind !== "apply_block" ||
          oldObservation.policyDigest !== owner.policy.policyDigest ||
          oldObservation.network !== owner.policy.network ||
          oldObservation.blueprintHash !== owner.policy.blueprintHash ||
          !same(
            oldObservation.deploymentMarker,
            owner.policy.deploymentMarker,
          ) ||
          oldObservation.snapshot.snapshotDigest !== entry.snapshotDigest ||
          oldObservation.sourceDurableStoreDigest !== entry.sourceStoreDigest ||
          oldObservation.durableStoreDigest !== entry.nextStoreDigest ||
          oldObservation.sourceDurableStoreRevision !==
            entry.sourceStoreRevision ||
          oldObservation.durableStoreRevision !== entry.nextStoreRevision ||
          oldObservation.blockHash !== entry.cursor.blockHash ||
          oldObservation.blockNo !== entry.cursor.blockNo ||
          oldObservation.slot !== entry.cursor.slot ||
          (lastProcessedSequence === -1n
            ? entry.predecessorStateDigest !== null
            : entry.predecessorStateDigest === null)
        )
          return localRefuse("archived semantic entry chain differs");
        if (
          previous !== null &&
          (entry.predecessorStateDigest === null ||
            !same(
              localArchiveField(archived(entry.predecessorStateDigest), [
                "head",
              ]),
              previous,
            ))
        )
          return localRefuse("archived predecessor state differs");
        if (replayState.retainedSource !== null) {
          await replayState.retainedSource.close();
          replayState.retainedSource = null;
        }
        const source =
          lastProcessedSequence === -1n
            ? null
            : await replayBlock(entry.cursor);
        if (source !== null) replayState.retainedSource = source;
        const pair =
          source === null
            ? firstPair
            : Object.freeze({
                finality: source.finality,
                observation: source.observation,
                referenceAuthority: source.referenceAuthority,
              });
        const live = localLivePair(owner, pair);
        const oldEvidence = exactRecord(archived(entry.evidenceDigest), [
          "schemaVersion",
          "numericEncoding",
          "witnesses",
          "referenceEvidence",
        ]);
        if (
          oldEvidence === null ||
          oldEvidence.schemaVersion !==
            "midgard-watcher-local-user-event-block-evidence-v1" ||
          oldEvidence.numericEncoding !== "exact-decimal-strings" ||
          !same(live.witness.current.observation.capture.point, entry.cursor) ||
          !same(
            live.witness.current.observation.capture.predecessorPoint,
            entry.parent,
          )
        )
          return localRefuse(
            "fresh replay does not match the archived whole block",
          );
        for (const step of ["first", "current"] as const) {
          if (
            localArchiveField(oldEvidence.witnesses, [
              step,
              "observation",
              "capture",
              "nativeBlock",
              "rawBlockCbor",
            ]) !==
              live.witness.current.observation.capture.nativeBlock
                .rawBlockCbor ||
            !same(
              localArchiveField(oldEvidence.witnesses, [
                step,
                "observation",
                "capture",
                "point",
              ]),
              entry.cursor,
            ) ||
            !same(
              localArchiveField(oldEvidence.witnesses, [
                step,
                "observation",
                "capture",
                "predecessorPoint",
              ]),
              entry.parent,
            )
          )
            return localRefuse(
              "archived original block bytes differ from fresh canonical replay",
            );
        }
        // The fresh W12 capture just corroborated `entry.parent` as this
        // block's canonical predecessor; the archived quiet stretch between
        // the previous entry and that parent lies on the same linear chain.
        if (previous !== null)
          owner.coverage = Object.freeze({
            point: Object.freeze({ ...entry.parent }),
            headEntryDigest: owner.entries.at(-1)!.entryDigest,
          });
        const transition = prepareLocalUserEventTransition(history, pair);
        const fresh = readWatcherLocalUserEventTransition(transition);
        if (
          !same(
            localStableSnapshot(oldObservation.snapshot),
            localStableSnapshot(fresh.snapshot),
          )
        )
          return localRefuse(
            "fresh semantic replay differs from the archived event fold",
          );
        const oldStore = parseWatcherDurableStore(
          archived(entry.nextStoreDigest),
        );
        if (
          storeDigest(oldStore) !== entry.nextStoreDigest ||
          !topologyMatches(oldStore, oldObservation.snapshot)
        )
          return localRefuse("archived event store topology differs");
        const oldNative = localArchiveField(oldEvidence.witnesses, [
          "current",
          "observation",
          "native",
        ]);
        const oldPoint = oldStore.chainPoints.find(
          (point) => point.chainPointId === oldObservation.chainPointId,
        );
        const oldRow = oldStore.l1Observations.find(
          (row) => row.observationId === oldObservation.sourceObservationDigest,
        );
        if (
          oldPoint === undefined ||
          oldRow === undefined ||
          oldRow.chainPointId !== oldPoint.chainPointId ||
          oldPoint.blockHash !== entry.cursor.blockHash ||
          oldPoint.blockNo !== entry.cursor.blockNo ||
          oldPoint.slot !== entry.cursor.slot ||
          oldPoint.chainPointId !==
            localArchiveField(oldNative, ["chainPoint", "chainPointId"]) ||
          oldPoint.depth !==
            localArchiveField(oldNative, ["chainPoint", "depth"]) ||
          oldPoint.providerId !==
            localArchiveField(oldNative, ["provider", "providerId"]) ||
          oldObservation.pointDigest !==
            localArchiveField(oldNative, ["chainPoint", "pointDigest"]) ||
          oldRow.observationId !==
            localArchiveField(oldNative, ["observationDigest"]) ||
          !same(
            localArchiveEvidence(
              JSON.parse(
                Buffer.from(oldRow.payload.cborHex, "hex").toString("utf8"),
              ),
            ),
            oldNative,
          )
        )
          return localRefuse(
            "archived original observation/store binding differs",
          );
        const oldPoints = [...archivedSourceStore.chainPoints, oldPoint];
        const oldJournal = journalWatcherProtocolUtxoTransition({
          sourceStore: archivedSourceStore,
          nextChainPoints: oldPoints,
          spentAtChainPointId: oldPoint.chainPointId,
          nextProtocolUtxos: oldObservation.snapshot.activeEvents.map(
            (event) => ({
              outRef: event.outRef,
              role: protocolRole(event.kind),
              chainPointId:
                archivedSourceStore.protocolUtxos.find(
                  ({ outRef }) => outRef === event.outRef,
                )?.chainPointId ?? oldPoint.chainPointId,
              output: makeWatcherDurablePayload(event.outputCborHex),
            }),
          ),
        });
        const rebuiltOldStore = makeWatcherDurableStore({
          deploymentMarker: owner.policy.deploymentMarker,
          revision: entry.nextStoreRevision,
          records: {
            ...archivedSourceStore,
            chainPoints: oldPoints,
            ...oldJournal,
            l1Observations: [...archivedSourceStore.l1Observations, oldRow],
          },
        });
        if (
          !same(oldStore, rebuiltOldStore) ||
          !same(
            localStableEventStore(oldStore),
            localStableEventStore(fresh.nextStore),
          )
        )
          return localRefuse(
            "archived event journal differs from fresh semantic replay",
          );
        archivedSourceStore = oldStore;
        lastProcessedSequence = BigInt(entry.sequence);
        lastProcessedEntry = entry;
        oldRetainedEntries = [...oldRetainedEntries, entry];
        commitLocalUserEventTransition(history, transition);
      }
      if (
        !same(
          payload.snapshot,
          entryObservations.get(entries.at(-1)!.entryDigest)!.snapshot,
        ) ||
        !same(
          localStableSnapshot(payload.snapshot),
          localStableSnapshot(owner.snapshot),
        )
      )
        return localRefuse(
          "fresh semantic head differs from archived snapshot",
        );
      if (!same(archivedSourceStore, headStore))
        return localRefuse(
          "archived payload materialization differs from the replayed head",
        );
      if (sealedIndex !== null) {
        if (
          sealedIndex.index.lastEntrySequence !==
            lastProcessedEntry!.sequence ||
          !same(
            sealedIndex.index.retainedEntryDigests,
            oldRetainedEntries.slice(-64).map((entry) => entry.entryDigest),
          )
        )
          return localRefuse(
            "archive segment does not seal the exact retained suffix",
          );
        const requiredPoints = new Set(
          oldRetainedEntries.slice(-64).map((entry) => {
            const observation = entryObservations.get(entry.entryDigest);
            if (observation === undefined || observation.chainPointId === null)
              return localRefuse("materialized suffix observation is absent");
            return observation.chainPointId;
          }),
        );
        const retainPoint = (
          blockHash: string,
          slot: string,
          blockNo: string,
        ) => {
          const points = archivedSourceStore.chainPoints.filter(
            (point) =>
              point.blockHash === blockHash &&
              point.slot === slot &&
              point.blockNo === blockNo,
          );
          if (points.length !== 1)
            return localRefuse(
              "materialized event point is not uniquely retained",
            );
          requiredPoints.add(points[0]!.chainPointId);
        };
        for (const event of owner.snapshot.activeEvents)
          retainPoint(
            event.originBlockHash,
            event.originSlot,
            event.originBlockNo,
          );
        for (const event of owner.snapshot.terminalEvents) {
          retainPoint(
            event.originBlockHash,
            event.originSlot,
            event.originBlockNo,
          );
          retainPoint(
            event.terminalBlockHash,
            event.terminalSlot,
            event.terminalBlockNo,
          );
        }
        const materialized = localMaterializedStoreFromPoints(
          archivedSourceStore,
          requiredPoints,
        );
        const archivedMaterialized = parseWatcherDurableStore(
          await readValue(sealedIndex.index.materializedStoreDigest),
        );
        if (!same(materialized, archivedMaterialized))
          return localRefuse(
            "archived materialization is not the exact dependency-preserving projection",
          );
        const pair =
          replayState.retainedSource === null
            ? firstPair
            : replayState.retainedSource;
        await writePrivateObjects(owner.archiveObjects);
        const anchor = await prepareLocalUserEventAnchor(
          history,
          pair,
          archive,
        );
        const freshMaterialized = localAnchors.get(anchor)!.value.nextStore;
        if (
          !same(
            localStableEventStore(materialized),
            localStableEventStore(freshMaterialized),
          )
        )
          return localRefuse(
            "fresh materialization differs from archived semantics",
          );
        await writePrivateObjects(
          readWatcherLocalUserEventAnchor(anchor).archiveObjects,
        );
        commitLocalUserEventAnchor(anchor);
        archivedSourceStore = materialized;
        oldRetainedEntries = oldRetainedEntries.slice(-64);
        replayState.priorIndex = sealedIndex;
      }
    };
    if (rootIndex !== null) {
      for (
        let sequence = 0n;
        sequence <= BigInt(rootIndex.index.indexSequence);
        sequence += 1n
      ) {
        const segment = await findWatcherUserEventArchiveIndex(
          archive,
          rootIndex,
          sequence.toString(),
        );
        if (
          segment.index.previousIndexDigest !==
            replayState.priorIndex?.digest &&
          !(
            replayState.priorIndex === null &&
            segment.index.previousIndexDigest === null
          )
        )
          return localRefuse("archive segment immediate predecessor differs");
        if (
          BigInt(segment.index.firstEntrySequence) !==
          lastProcessedSequence + 1n
        )
          return localRefuse("archive segment entry boundary differs");
        await replaySegment(
          await readClosure(segment.index.sourceArchiveDigests),
          segment.index.sourcePayloadDigest,
          segment,
        );
      }
      if (replayState.priorIndex?.digest !== rootIndex.digest)
        return localRefuse("archive root is not the replayed segment head");
    }
    await replaySegment(currentObjects, previousCheckpoint.payloadDigest, null);
    const replayHead = owner.entries.at(-1)!;
    const replayPayload = owner.archiveObjects.find(
      (object) => object.digest === owner.checkpoint!.payloadDigest,
    )!;
    const replayPayloadFields = exactRecord(
      JSON.parse(Buffer.from(replayPayload.bytesHex, "hex").toString("utf8")),
      [
        "schemaVersion",
        "originArchiveDigest",
        "originDigest",
        "policy",
        "anchor",
        "head",
        "storeArchiveDigest",
        "snapshot",
        "retainedEntries",
        "requiredSemanticResume",
      ],
    );
    if (replayPayloadFields === null)
      return localRefuse("private replay payload differs");
    const readmissionPayload = localArchiveObject({
      ...replayPayloadFields,
      readmission: {
        kind: "fresh_authenticated_origin_replay",
        previousCheckpointDigest: previousCheckpoint.checkpointDigest,
        previousPayloadDigest: previousCheckpoint.payloadDigest,
        archivedOriginDigest: payload.originDigest,
        freshOriginDigest: owner.originDigest,
        stableSnapshotDigest: sha256Canonical(
          localStableSnapshot(owner.snapshot),
        ),
      },
    });
    const archiveObjects = Object.freeze([
      ...new Map([
        ...[...currentObjects.values()].map(
          ({ object }) => [object.digest, object] as const,
        ),
        ...owner.archiveObjects.map(
          (object) => [object.digest, object] as const,
        ),
        [readmissionPayload.digest, readmissionPayload] as const,
      ]).values(),
    ]);
    if (
      archiveObjects.length >
        WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
      archiveObjects.reduce(
        (total, object) => total + object.bytesHex.length / 2,
        0,
      ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
      archiveObjects.reduce(
        (total, object) => total + localArchiveBudgets.get(object)!.nodes,
        0,
      ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
    )
      return localRefuse("semantic readmission archive bound exceeded");
    const nextCheckpoint = makeWatcherUserEventCheckpoint({
      ...previousCheckpoint,
      checkpointSequence: (
        BigInt(previousCheckpoint.checkpointSequence) + 1n
      ).toString(),
      predecessorCheckpointDigest: previousCheckpoint.checkpointDigest,
      payloadDigest: readmissionPayload.digest,
      requiredArchiveDigests: archiveObjects.map(({ digest }) => digest).sort(),
    });
    const refreshed = readWatcherProtectedUserEventCheckpointReceipt(
      await readWatcherProtectedUserEventCheckpoint(runtime),
    );
    if (!same(refreshed.checkpoint, previousCheckpoint))
      return localRefuse("protected head changed during semantic replay");
    const pair =
      replayState.retainedSource === null
        ? firstPair
        : Object.freeze({
            finality: replayState.retainedSource.finality,
            observation: replayState.retainedSource.observation,
            referenceAuthority: replayState.retainedSource.referenceAuthority,
          });
    if (
      !same(
        localLivePair(owner, pair).witness.current.observation.capture.point,
        replayHead.cursor,
      )
    )
      return localRefuse("semantic replay head is no longer live");
    const retained = replayState.retainedSource;
    const readmission = Object.freeze({
      [localReadmissionBrand]: true as const,
    });
    localReadmissions.set(readmission, {
      runtime,
      history,
      pair,
      generation: owner.generation,
      previousCheckpoint,
      nextCheckpoint,
      archiveObjects,
      release: async () => {
        await retained?.close();
      },
      accepted: false,
    });
    replayState.retainedSource = null;
    return readmission;
  } catch (error) {
    closeWatcherLocalUserEventHistory(history);
    await replayState.retainedSource?.close();
    throw error;
  }
};
