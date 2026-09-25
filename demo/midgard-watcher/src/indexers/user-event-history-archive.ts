/** Content-addressed archive navigation only. These descriptors grant no W12,
 * indexing, publication or dispatch authority. A semantic owner must replay the
 * selected segments and verify their consecutive predecessor links.
 */
import { isProxy } from "node:util/types";

import {
  watcherCanonicalJson,
  watcherSha256CanonicalJson,
} from "../storage/durable-store.js";
import {
  type WatcherUserEventArchive,
  watcherUserEventArchiveDigest,
} from "../storage/user-event-checkpoint.js";

const INDEX_SCHEMA = "midgard-watcher-user-event-archive-index-v1";
const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const UINT64_MAX = (1n << 64n) - 1n;
const INDEX_BYTES = 2 * 1024 * 1024;
const MAXIMUM_DIGESTS = 16_384;

export type WatcherUserEventArchiveIndex = Readonly<{
  schemaVersion: typeof INDEX_SCHEMA;
  indexSequence: string;
  previousIndexDigest: string | null;
  ancestorDigests: readonly string[];
  firstEntrySequence: string;
  lastEntrySequence: string;
  sourcePayloadDigest: string;
  sourceArchiveDigests: readonly string[];
  materializedStoreDigest: string;
  retainedEntryDigests: readonly string[];
}>;
export type WatcherUserEventArchiveIndexRead = Readonly<{
  digest: string;
  index: WatcherUserEventArchiveIndex;
}>;
const refuse = (message: string): never => {
  throw new Error(`Local user-event archive index refused: ${message}`);
};
const natural = (value: unknown): string => {
  if (
    typeof value !== "string" ||
    value.length > 20 ||
    !NATURAL.test(value) ||
    BigInt(value) > UINT64_MAX
  )
    return refuse("sequence is not uint64");
  return value;
};
const digest = (value: unknown): string => {
  if (typeof value !== "string" || !HEX_32.test(value))
    return refuse("digest is invalid");
  return value;
};
const digests = (value: unknown, maximum: number): readonly string[] => {
  if (
    typeof value !== "object" ||
    value === null ||
    isProxy(value) ||
    !Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    value.length > maximum ||
    Reflect.ownKeys(value).length !== value.length + 1
  )
    return refuse("digest list exceeds its bound or is not a dense data array");
  const descriptors = Object.getOwnPropertyDescriptors(value);
  const result: string[] = [];
  for (let index = 0; index < value.length; index += 1) {
    const descriptor = descriptors[index];
    if (
      descriptor === undefined ||
      !("value" in descriptor) ||
      !descriptor.enumerable
    )
      return refuse("digest list is not a dense data array");
    result.push(digest(descriptor.value));
  }
  return Object.freeze(result);
};
const ancestorCount = (sequence: bigint): number =>
  sequence === 0n ? 0 : sequence.toString(2).length;

export const parseWatcherUserEventArchiveIndex = (
  value: unknown,
): WatcherUserEventArchiveIndex => {
  const keys = [
    "schemaVersion",
    "indexSequence",
    "previousIndexDigest",
    "ancestorDigests",
    "firstEntrySequence",
    "lastEntrySequence",
    "sourcePayloadDigest",
    "sourceArchiveDigests",
    "materializedStoreDigest",
    "retainedEntryDigests",
  ];
  if (
    typeof value !== "object" ||
    value === null ||
    isProxy(value) ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== keys.length
  )
    return refuse("descriptor shape differs");
  const descriptors = Object.getOwnPropertyDescriptors(value);
  if (
    keys.some(
      (key) =>
        descriptors[key] === undefined ||
        !("value" in descriptors[key]) ||
        !descriptors[key].enumerable,
    )
  )
    return refuse("descriptor fields differ");
  const field = (key: string): unknown => descriptors[key]!.value;
  if (field("schemaVersion") !== INDEX_SCHEMA) return refuse("schema differs");
  const indexSequence = natural(field("indexSequence"));
  const previousIndexDigest =
    field("previousIndexDigest") === null
      ? null
      : digest(field("previousIndexDigest"));
  const ancestorDigests = digests(field("ancestorDigests"), 64);
  const firstEntrySequence = natural(field("firstEntrySequence"));
  const lastEntrySequence = natural(field("lastEntrySequence"));
  const sourcePayloadDigest = digest(field("sourcePayloadDigest"));
  const sourceArchiveDigests = digests(
    field("sourceArchiveDigests"),
    MAXIMUM_DIGESTS,
  );
  const retainedEntryDigests = digests(field("retainedEntryDigests"), 64);
  if (
    ancestorDigests.length !== ancestorCount(BigInt(indexSequence)) ||
    (indexSequence === "0"
      ? previousIndexDigest !== null || firstEntrySequence !== "0"
      : previousIndexDigest !== ancestorDigests[0]) ||
    BigInt(firstEntrySequence) > BigInt(lastEntrySequence) ||
    !sourceArchiveDigests.includes(sourcePayloadDigest) ||
    sourceArchiveDigests.some(
      (item, index) => index > 0 && item <= sourceArchiveDigests[index - 1]!,
    ) ||
    new Set(retainedEntryDigests).size !== retainedEntryDigests.length ||
    retainedEntryDigests.length !== 64
  )
    return refuse("descriptor continuity or materialization differs");
  const index = Object.freeze({
    schemaVersion: INDEX_SCHEMA,
    indexSequence,
    previousIndexDigest,
    ancestorDigests,
    firstEntrySequence,
    lastEntrySequence,
    sourcePayloadDigest,
    sourceArchiveDigests,
    materializedStoreDigest: digest(field("materializedStoreDigest")),
    retainedEntryDigests,
  });
  if (Buffer.byteLength(watcherCanonicalJson(index), "utf8") > INDEX_BYTES)
    return refuse("descriptor exceeds its byte bound");
  return index;
};

export const readWatcherUserEventArchiveIndex = async (
  archive: WatcherUserEventArchive,
  indexDigest: string,
): Promise<WatcherUserEventArchiveIndexRead> => {
  const expected = digest(indexDigest);
  const bytes = await archive.read(expected);
  if (
    bytes === null ||
    bytes.byteLength > INDEX_BYTES ||
    watcherUserEventArchiveDigest(bytes) !== expected
  )
    return refuse("descriptor is absent or corrupt");
  let value: unknown;
  try {
    value = JSON.parse(new TextDecoder("utf-8", { fatal: true }).decode(bytes));
  } catch {
    return refuse("descriptor is not JSON");
  }
  const index = parseWatcherUserEventArchiveIndex(value);
  if (watcherSha256CanonicalJson(index) !== expected)
    return refuse("descriptor encoding is not canonical");
  return Object.freeze({ digest: expected, index });
};

/** Lookup takes at most 65 archive reads and retains one descriptor at a time.
 * The caller still verifies each segment's exact immediate predecessor while
 * replaying all segments in increasing sequence order; skip links do not waive it.
 */
export const findWatcherUserEventArchiveIndex = async (
  archive: WatcherUserEventArchive,
  root: WatcherUserEventArchiveIndexRead,
  targetSequence: string,
): Promise<WatcherUserEventArchiveIndexRead> => {
  const target = BigInt(natural(targetSequence));
  const rootDigest = digest(root.digest);
  const rootSequence = natural(root.index.indexSequence);
  let current = await readWatcherUserEventArchiveIndex(archive, rootDigest);
  if (
    current.index.indexSequence !== rootSequence ||
    target > BigInt(current.index.indexSequence)
  )
    return refuse("requested sequence exceeds the root");
  for (
    let reads = 0;
    BigInt(current.index.indexSequence) > target;
    reads += 1
  ) {
    if (reads >= 64) return refuse("lookup read bound exceeded");
    const currentSequence = BigInt(current.index.indexSequence);
    const power = (currentSequence - target).toString(2).length - 1;
    const nextDigest = current.index.ancestorDigests[power];
    if (nextDigest === undefined) return refuse("ancestor is absent");
    const next = await readWatcherUserEventArchiveIndex(archive, nextDigest);
    if (
      BigInt(next.index.indexSequence) !==
      currentSequence - (1n << BigInt(power))
    )
      return refuse("ancestor sequence differs");
    current = next;
  }
  return current;
};

export const makeWatcherUserEventArchiveIndex = async (
  archive: WatcherUserEventArchive,
  input: Omit<
    WatcherUserEventArchiveIndex,
    | "schemaVersion"
    | "indexSequence"
    | "previousIndexDigest"
    | "ancestorDigests"
  > &
    Readonly<{
      previous: WatcherUserEventArchiveIndexRead | null;
    }>,
): Promise<WatcherUserEventArchiveIndex> => {
  const {
    previous: suppliedPrevious,
    firstEntrySequence: suppliedFirst,
    lastEntrySequence: suppliedLast,
    sourcePayloadDigest: suppliedPayload,
    sourceArchiveDigests: suppliedArchive,
    materializedStoreDigest: suppliedStore,
    retainedEntryDigests: suppliedRetained,
  } = input;
  const previous =
    suppliedPrevious === null
      ? null
      : Object.freeze({
          digest: digest(suppliedPrevious.digest),
          index: parseWatcherUserEventArchiveIndex(suppliedPrevious.index),
        });
  const fields = Object.freeze({
    firstEntrySequence: natural(suppliedFirst),
    lastEntrySequence: natural(suppliedLast),
    sourcePayloadDigest: digest(suppliedPayload),
    sourceArchiveDigests: digests(suppliedArchive, MAXIMUM_DIGESTS),
    materializedStoreDigest: digest(suppliedStore),
    retainedEntryDigests: digests(suppliedRetained, 64),
  });
  const indexSequence =
    previous === null ? 0n : BigInt(previous.index.indexSequence) + 1n;
  if (indexSequence > UINT64_MAX) return refuse("archive sequence exhausted");
  const ancestorDigests: string[] = [];
  if (previous !== null) {
    const current = await readWatcherUserEventArchiveIndex(
      archive,
      previous.digest,
    );
    if (
      watcherCanonicalJson(current.index) !==
        watcherCanonicalJson(previous.index) ||
      BigInt(fields.firstEntrySequence) !==
        BigInt(current.index.lastEntrySequence) + 1n
    )
      return refuse("previous descriptor or first entry differs");
    ancestorDigests.push(current.digest);
    for (let power = 1; power < ancestorCount(indexSequence); power += 1) {
      const half = await readWatcherUserEventArchiveIndex(
        archive,
        ancestorDigests[power - 1]!,
      );
      if (
        BigInt(half.index.indexSequence) !==
        indexSequence - (1n << BigInt(power - 1))
      )
        return refuse("ancestor half-step differs");
      const ancestor = half.index.ancestorDigests[power - 1];
      if (ancestor === undefined) return refuse("ancestor half-step is absent");
      ancestorDigests.push(ancestor);
    }
  }
  return parseWatcherUserEventArchiveIndex({
    ...fields,
    schemaVersion: INDEX_SCHEMA,
    indexSequence: indexSequence.toString(),
    previousIndexDigest: previous?.digest ?? null,
    ancestorDigests,
  });
};
