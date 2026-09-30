import { admitFraudProofRawL1Point } from "@al-ft/midgard-fault-proofs";

import { type WatcherUserEventArchive } from "../../storage/user-event-checkpoint.js";
import {
  findWatcherUserEventArchiveIndex,
  type WatcherUserEventArchiveIndexRead,
} from ".././user-event-history-archive.js";
import {
  localArchiveObject,
  type LocalHistoryOwner,
  localRefuse,
  localRetainedEvidence,
  type WatcherLocalUserEventEntry,
} from "./local-history.local-history-owner.js";
import {
  evidenceWithinBounds,
  exactRecord,
  isHex32,
  isNatural,
  same,
  sha256Bytes,
  sha256Canonical,
} from "./policy.js";
import { WATCHER_USER_EVENT_INDEXER_BOUNDS } from "./types.js";

const localReadArchivedValue = async (
  archive: WatcherUserEventArchive,
  digest: string,
): Promise<unknown> => {
  if (!isHex32(digest))
    return localRefuse("historical cutoff archive digest differs");
  const bytes = await archive.read(digest);
  if (
    bytes === null ||
    bytes.byteLength >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
    sha256Bytes(bytes) !== digest
  )
    return localRefuse("historical cutoff archive is absent or corrupt");
  let value: unknown;
  try {
    value = JSON.parse(new TextDecoder("utf-8", { fatal: true }).decode(bytes));
  } catch {
    return localRefuse("historical cutoff archive is not JSON");
  }
  if (
    !evidenceWithinBounds(value, { nodes: 0, bytes: 0 }) ||
    localArchiveObject(value).digest !== digest
  )
    return localRefuse("historical cutoff archive encoding or bounds differ");
  return value;
};

/** Parses and validates one sealed segment's retained-entry payload. */
const localArchivedSegmentEntries = async (
  owner: LocalHistoryOwner,
  archive: WatcherUserEventArchive,
  segment: WatcherUserEventArchiveIndexRead,
): Promise<readonly WatcherLocalUserEventEntry[]> => {
  const payload = await localReadArchivedValue(
    archive,
    segment.index.sourcePayloadDigest,
  );
  const entriesValue = localArchiveField(payload, ["retainedEntries"]);
  if (
    !Array.isArray(entriesValue) ||
    entriesValue.length === 0 ||
    entriesValue.length > Number(owner.policy.maximumActiveHistoryEntries) ||
    localArchiveField(payload, ["originDigest"]) !== owner.originDigest ||
    !same(localArchiveField(payload, ["policy"]), owner.policy)
  )
    return localRefuse("historical cutoff segment payload differs");
  const entries = entriesValue.map(localArchivedEntry);
  if (!same(localArchiveField(payload, ["head"]), entries.at(-1)))
    return localRefuse("historical cutoff segment head differs");
  return entries;
};

/**
 * Finds the sealed entry observed at `blockNo`, or null when no event block
 * was observed there. Entries are no longer dense in block numbers (quiet
 * blocks publish nothing), so sealed segments are bisected by the block
 * numbers of their first and last retained entries.
 */
const localArchivedEntryAtBlock = async (
  owner: LocalHistoryOwner,
  archive: WatcherUserEventArchive,
  root: WatcherUserEventArchiveIndexRead,
  blockNo: bigint,
): Promise<Readonly<{
  segment: WatcherUserEventArchiveIndexRead;
  entry: WatcherLocalUserEventEntry;
}> | null> => {
  let first = 0n;
  let last = BigInt(root.index.indexSequence);
  for (let iteration = 0; first <= last && iteration < 65; iteration += 1) {
    const middle = (first + last) / 2n;
    const segment = await findWatcherUserEventArchiveIndex(
      archive,
      root,
      middle.toString(),
    );
    // The sealed payload retains the suffix carried over from the previous
    // segment as well; only this segment's own entry range orders the search.
    const entries = (
      await localArchivedSegmentEntries(owner, archive, segment)
    ).filter(
      (candidate) =>
        BigInt(candidate.sequence) >=
          BigInt(segment.index.firstEntrySequence) &&
        BigInt(candidate.sequence) <= BigInt(segment.index.lastEntrySequence),
    );
    if (entries.length === 0)
      return localRefuse("historical cutoff segment range is absent");
    if (blockNo < BigInt(entries[0]!.cursor.blockNo)) last = middle - 1n;
    else if (blockNo > BigInt(entries.at(-1)!.cursor.blockNo))
      first = middle + 1n;
    else {
      const matches = entries.filter(
        (candidate) => BigInt(candidate.cursor.blockNo) === blockNo,
      );
      if (matches.length > 1)
        return localRefuse("historical cutoff entry is not uniquely archived");
      return matches.length === 0
        ? null
        : Object.freeze({ segment, entry: matches[0]! });
    }
  }
  return null;
};

/**
 * Looks up the accepted event block at a block number: the retained suffix
 * first, then the sealed archive. Null means the block lies inside the
 * published range but was quiet, so nothing was observed there.
 */
export const localEntryAtBlock = async (
  owner: LocalHistoryOwner,
  blockNo: bigint,
  archive: WatcherUserEventArchive,
): Promise<Readonly<{
  entry: WatcherLocalUserEventEntry;
  rawBlockCbor: unknown;
}> | null> => {
  const head = owner.entries.at(-1)!;
  if (
    blockNo < BigInt(owner.origin.block.chainPoint.blockNo) ||
    blockNo > BigInt(head.cursor.blockNo)
  )
    return localRefuse(
      "header cutoff lies outside the published event history",
    );
  const retained = localRetainedEvidence(owner).find(
    ({ entry }) => BigInt(entry.cursor.blockNo) === blockNo,
  );
  if (retained !== undefined)
    return Object.freeze({
      entry: retained.entry,
      rawBlockCbor: retained.rawBlockCbor,
    });
  // Pinned evidence reaches below the retained suffix, so only the suffix's
  // own oldest entry bounds the range where an absent entry means quiet.
  if (blockNo > BigInt(owner.entries[0]!.cursor.blockNo)) return null;
  const root = owner.archiveIndex;
  if (root === null) return null;
  const found = await localArchivedEntryAtBlock(owner, archive, root, blockNo);
  if (found === null) return null;
  const { segment, entry } = found;
  if (!segment.index.sourceArchiveDigests.includes(entry.evidenceDigest))
    return localRefuse(
      "historical cutoff evidence is not in the sealed closure",
    );
  const evidence = await localReadArchivedValue(archive, entry.evidenceDigest);
  if (
    localArchiveField(evidence, ["schemaVersion"]) !==
      "midgard-watcher-local-user-event-block-evidence-v1" ||
    localArchiveField(evidence, ["numericEncoding"]) !== "exact-decimal-strings"
  )
    return localRefuse("historical cutoff evidence framing differs");
  const rawBlockCbor = localArchiveField(evidence, [
    "witnesses",
    "current",
    "observation",
    "capture",
    "nativeBlock",
    "rawBlockCbor",
  ]);
  for (const step of ["first", "current"] as const) {
    if (
      localArchiveField(evidence, [
        "witnesses",
        step,
        "observation",
        "capture",
        "nativeBlock",
        "rawBlockCbor",
      ]) !== rawBlockCbor ||
      !same(
        localArchiveField(evidence, [
          "witnesses",
          step,
          "observation",
          "capture",
          "point",
        ]),
        entry.cursor,
      ) ||
      !same(
        localArchiveField(evidence, [
          "witnesses",
          step,
          "observation",
          "capture",
          "predecessorPoint",
        ]),
        entry.parent,
      )
    )
      return localRefuse("historical cutoff original witness binding differs");
  }
  return Object.freeze({ entry, rawBlockCbor });
};

export const localArchiveField = (
  value: unknown,
  keys: readonly string[],
): unknown => {
  let current = value;
  for (const key of keys) {
    if (
      typeof current !== "object" ||
      current === null ||
      Array.isArray(current)
    )
      return localRefuse("archive field is absent");
    const descriptor = Object.getOwnPropertyDescriptor(current, key);
    if (descriptor === undefined || !("value" in descriptor))
      return localRefuse("archive field is absent");
    current = descriptor.value;
  }
  return current;
};

export const localArchivedEntry = (
  value: unknown,
): WatcherLocalUserEventEntry => {
  const record = exactRecord(value, [
    "schemaVersion",
    "sequence",
    "originDigest",
    "policyDigest",
    "predecessorEntryDigest",
    "predecessorStateDigest",
    "cursor",
    "parent",
    "sourceStoreDigest",
    "nextStoreDigest",
    "sourceStoreRevision",
    "nextStoreRevision",
    "observationDigest",
    "snapshotDigest",
    "evidenceDigest",
    "entryDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== "midgard-watcher-local-user-event-entry-v1" ||
    !isNatural(record.sequence) ||
    record.sequence.length > 20 ||
    !isNatural(record.sourceStoreRevision) ||
    record.sourceStoreRevision.length > 20 ||
    !isNatural(record.nextStoreRevision) ||
    record.nextStoreRevision.length > 20 ||
    !isHex32(record.originDigest) ||
    !isHex32(record.policyDigest) ||
    !(
      record.predecessorEntryDigest === null ||
      isHex32(record.predecessorEntryDigest)
    ) ||
    !(
      record.predecessorStateDigest === null ||
      isHex32(record.predecessorStateDigest)
    ) ||
    !isHex32(record.sourceStoreDigest) ||
    !isHex32(record.nextStoreDigest) ||
    !isHex32(record.observationDigest) ||
    !isHex32(record.snapshotDigest) ||
    !isHex32(record.evidenceDigest) ||
    !isHex32(record.entryDigest)
  )
    return localRefuse("archive entry framing differs");
  const entry = Object.freeze({
    schemaVersion: record.schemaVersion,
    sequence: record.sequence,
    originDigest: record.originDigest,
    policyDigest: record.policyDigest,
    predecessorEntryDigest: record.predecessorEntryDigest,
    predecessorStateDigest: record.predecessorStateDigest,
    cursor: Object.freeze(admitFraudProofRawL1Point(record.cursor)),
    parent: Object.freeze(admitFraudProofRawL1Point(record.parent)),
    sourceStoreDigest: record.sourceStoreDigest,
    nextStoreDigest: record.nextStoreDigest,
    sourceStoreRevision: record.sourceStoreRevision,
    nextStoreRevision: record.nextStoreRevision,
    observationDigest: record.observationDigest,
    snapshotDigest: record.snapshotDigest,
    evidenceDigest: record.evidenceDigest,
    entryDigest: record.entryDigest,
  });
  const { entryDigest, ...fields } = entry;
  if (sha256Canonical(fields) !== entryDigest)
    return localRefuse("archive entry digest differs");
  return entry;
};
