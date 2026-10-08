import { Level } from "level";

import {
  type NativeMpfFullIndexCap,
  type NativeMpfFullIndexHealth,
  type NativeMpfOwnerService,
  NativeMpfPromotionIndexCapExceeded,
} from "./protocol.js";
import {
  assertStoredNode,
  encodeStoredNode,
} from "./service.encode-stored-node.js";
import {
  type DecodedPromotionRecord,
  EMPTY_ROOT_HEX,
  FULL_INDEX_HEADER_BYTES,
  FULL_INDEX_MAX_BYTES,
  FULL_INDEX_MAX_RECORDS,
  type StoredValue,
} from "./service.normalize-owner-options.js";

/** A full index's size: what `buildOrReadFullIndex` checks against
 * `FULL_INDEX_MAX_BYTES` (header included) and `FULL_INDEX_MAX_RECORDS`. */
export type FullIndexSize = {
  readonly bytes: number;
  readonly records: number;
};

export const EMPTY_FULL_INDEX_SIZE: FullIndexSize = {
  bytes: FULL_INDEX_HEADER_BYTES,
  records: 0,
};

/** The size of a full index `buildOrReadFullIndex` returned. */
export const fullIndexSizeOf = (fullIndex: Buffer): FullIndexSize => ({
  bytes: fullIndex.length,
  records: fullIndex.readUInt32LE(28),
});

const READ_BATCH = 4_096;

const encodedSize = (hash: string, value: StoredValue | undefined): number => {
  if (value === undefined)
    throw new Error(`Native MPF promotion base closure is missing ${hash}`);
  return encodeStoredNode(hash, value).length;
};

/** Every node of the closure of `candidateRoot`, read from the generated
 * records first and the store otherwise, counted once each. */
const walkCandidate = async (
  db: Level<string, StoredValue>,
  candidateRoot: string,
  generated: ReadonlyMap<string, DecodedPromotionRecord>,
): Promise<FullIndexSize> => {
  let bytes = FULL_INDEX_HEADER_BYTES;
  let records = 0;
  const seen = new Set<string>([candidateRoot]);
  let layer = [candidateRoot];
  while (layer.length > 0) {
    const next: string[] = [];
    const visit = (children: readonly (string | null)[]) => {
      for (const child of children)
        if (child !== null && !seen.has(child)) {
          seen.add(child);
          next.push(child);
        }
    };
    const stored = layer.filter((hash) => !generated.has(hash));
    for (const hash of layer) {
      const record = generated.get(hash);
      if (record === undefined) continue;
      bytes += record.encoded.length;
      records += 1;
      if (record.stored.__kind === "Branch") visit(record.stored.children);
    }
    for (let start = 0; start < stored.length; start += READ_BATCH) {
      const batch = stored.slice(start, start + READ_BATCH);
      const values = await db.getMany(batch);
      batch.forEach((hash, index) => {
        bytes += encodedSize(hash, values[index]);
        records += 1;
        const node = assertStoredNode(values[index], hash);
        if (node.__kind === "Branch") visit(node.children);
      });
    }
    layer = next;
  }
  return { bytes, records };
};

/**
 * The full-index size of `candidateRoot` once promoted from `baseRoot`, whose
 * full index is `base`, given the promotion's generated records (validated:
 * hash-sorted, content-addressed and closed over the store). The candidate's
 * closure is the generated records plus the base subtrees they point at (the
 * frontier). Only the base nodes the candidate drops are read: the walk from
 * the base root stops at every frontier or generated node, since a node's
 * hash fixes its whole subtree, so it reads the replaced paths and no more.
 * The size is the base's, less the dropped nodes, plus the generated records
 * the base does not hold. A frontier node the walk never reaches (only a
 * subtree shared at two positions can do that) falls back to walking the
 * candidate's whole closure, so the size is exact either way.
 */
export const candidateFullIndexSize = async ({
  db,
  baseRoot,
  base,
  candidateRoot,
  records,
}: {
  readonly db: Level<string, StoredValue>;
  readonly baseRoot: string;
  readonly base: FullIndexSize;
  readonly candidateRoot: string;
  readonly records: readonly DecodedPromotionRecord[];
}): Promise<FullIndexSize> => {
  if (candidateRoot === baseRoot) return base;
  if (candidateRoot === EMPTY_ROOT_HEX) return EMPTY_FULL_INDEX_SIZE;
  const generated = new Map(records.map((record) => [record.hashHex, record]));
  const frontier = new Set<string>();
  for (const record of records)
    if (record.stored.__kind === "Branch")
      for (const child of record.stored.children)
        if (child !== null && !generated.has(child)) frontier.add(child);
  const reached = new Set<string>();
  const kept: string[] = [];
  let droppedBytes = 0;
  let droppedRecords = 0;
  const seen = new Set<string>();
  let layer = baseRoot === EMPTY_ROOT_HEX ? [] : [baseRoot];
  while (layer.length > 0) {
    const dropped: string[] = [];
    for (const hash of layer) {
      if (seen.has(hash)) continue;
      seen.add(hash);
      if (frontier.has(hash)) reached.add(hash);
      else if (generated.has(hash)) kept.push(hash);
      else dropped.push(hash);
    }
    const next: string[] = [];
    for (let start = 0; start < dropped.length; start += READ_BATCH) {
      const batch = dropped.slice(start, start + READ_BATCH);
      const values = await db.getMany(batch);
      batch.forEach((hash, index) => {
        droppedBytes += encodedSize(hash, values[index]);
        droppedRecords += 1;
        const node = assertStoredNode(values[index], hash);
        if (node.__kind === "Branch")
          for (const child of node.children)
            if (child !== null) next.push(child);
      });
    }
    layer = next;
  }
  // A generated record the base already holds keeps its subtree, which is
  // the base's: its generated descendants are held too, and its other
  // children are frontier nodes the walk above stopped short of.
  const held = new Set<string>();
  while (kept.length > 0) {
    const hash = kept.pop()!;
    if (held.has(hash)) continue;
    const record = generated.get(hash);
    if (record === undefined) {
      reached.add(hash);
      continue;
    }
    held.add(hash);
    if (record.stored.__kind === "Branch")
      for (const child of record.stored.children)
        if (child !== null) kept.push(child);
  }
  if (reached.size !== frontier.size)
    return walkCandidate(db, candidateRoot, generated);
  let addedBytes = 0;
  let addedRecords = 0;
  for (const record of records)
    if (!held.has(record.hashHex)) {
      addedBytes += record.encoded.length;
      addedRecords += 1;
    }
  return {
    bytes: base.bytes - droppedBytes + addedBytes,
    records: base.records - droppedRecords + addedRecords,
  };
};

/** The refusal of a candidate whose full index is over either cap, or
 * undefined when the next start could load it. */
export const promotionIndexCapBreach = (
  candidateRoot: string,
  size: FullIndexSize,
): NativeMpfPromotionIndexCapExceeded | undefined =>
  size.records > FULL_INDEX_MAX_RECORDS
    ? new NativeMpfPromotionIndexCapExceeded(
        candidateRoot,
        "FULL_INDEX_MAX_RECORDS",
        FULL_INDEX_MAX_RECORDS,
        size.records,
      )
    : size.bytes > FULL_INDEX_MAX_BYTES
      ? new NativeMpfPromotionIndexCapExceeded(
          candidateRoot,
          "FULL_INDEX_MAX_BYTES",
          FULL_INDEX_MAX_BYTES,
          size.bytes,
        )
      : undefined;

/**
 * The fraction of either full-index cap past which readiness warns. It leaves
 * a fifth of each cap: about 100 MiB of index, some 236,000 UTxOs at the ~455
 * index bytes per UTxO measured where the byte cap binds (about 1.18M UTxOs),
 * and 400,000 records, some 296,000 UTxOs at 1.33-1.37 records per UTxO. That
 * is room for many ordinary blocks after the warning and before a promotion is
 * refused, so an operator has time to plan a build whose caps cover the
 * ledger, while a ledger under four fifths of both caps stays quiet. One
 * fraction serves both caps so the two warnings mean the same thing.
 */
export const NATIVE_MPF_FULL_INDEX_WARNING_FRACTION = 0.8;

/** The readiness detail of a live root past the warning fraction of a cap:
 * `native_mpf_full_index_near_cap:<cap>:<observed>:<limit>`. */
export const NATIVE_MPF_FULL_INDEX_NEAR_CAP = "native_mpf_full_index_near_cap";

/** One readiness detail for each cap the live root's full index is past
 * `NATIVE_MPF_FULL_INDEX_WARNING_FRACTION` of. */
export const fullIndexNearCapDetails = (size: FullIndexSize): string[] =>
  (
    [
      ["FULL_INDEX_MAX_RECORDS", size.records, FULL_INDEX_MAX_RECORDS],
      ["FULL_INDEX_MAX_BYTES", size.bytes, FULL_INDEX_MAX_BYTES],
    ] as const satisfies readonly (readonly [
      NativeMpfFullIndexCap,
      number,
      number,
    ])[]
  )
    .filter(
      ([, observed, limit]) =>
        observed > limit * NATIVE_MPF_FULL_INDEX_WARNING_FRACTION,
    )
    .map(
      ([cap, observed, limit]) =>
        `${NATIVE_MPF_FULL_INDEX_NEAR_CAP}:${cap}:${observed.toString()}:${limit.toString()}`,
    );

/** The owner's full-index health, when it reports one (the production owner
 * does; partial test owners may not). */
export const fullIndexHealthOf = (
  owner: NativeMpfOwnerService,
): NativeMpfFullIndexHealth | undefined => {
  const candidate = owner as Partial<{
    fullIndexHealth: () => NativeMpfFullIndexHealth;
  }>;
  return typeof candidate.fullIndexHealth === "function"
    ? candidate.fullIndexHealth()
    : undefined;
};
