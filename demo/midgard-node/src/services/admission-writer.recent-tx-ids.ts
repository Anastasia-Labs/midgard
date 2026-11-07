import type { Status } from "../database/txAdmissions.js";

/** Recently written tx ids the admission path remembers (design D9). */
export const ADMISSION_RECENT_TX_ID_CAPACITY = 1 << 20;

const INITIAL_CAPACITY = 1 << 12;
const ID_BYTES = 32;
const HASH_BYTES = 32;
const STATUSES: readonly Status[] = [
  "queued",
  "validating",
  "accepted",
  "rejected",
];

export type RecentTxId = {
  readonly txFullHash: Buffer;
  /** The admission status when the node last wrote the row. */
  readonly status: Status;
  readonly firstSeenAt: Date;
};

export type RecentTxIds = {
  readonly get: (txId: Buffer) => RecentTxId | undefined;
  readonly remember: (txId: Buffer, entry: RecentTxId) => void;
  readonly size: () => number;
};

/**
 * The last `maxCapacity` tx ids the admission writer wrote, with their full
 * hash, so a duplicate submission is answered without a database read. It is
 * a ring of fixed-width records with an open-addressing index (linear
 * probing, backward-shift deletion) over typed arrays: a million entries hold
 * about 81 MB in a few large buffers instead of a million small objects the
 * collector would walk. The ring starts small and doubles up to
 * `maxCapacity`; once full, each new id evicts the oldest.
 */
export const makeRecentTxIds = (
  maxCapacity: number = ADMISSION_RECENT_TX_ID_CAPACITY,
): RecentTxIds => {
  if (!Number.isSafeInteger(maxCapacity) || maxCapacity <= 0) {
    throw new Error("recent tx id capacity must be a positive integer");
  }
  let capacity = Math.min(INITIAL_CAPACITY, maxCapacity);
  let ids = Buffer.alloc(capacity * ID_BYTES);
  let hashes = Buffer.alloc(capacity * HASH_BYTES);
  let statuses = new Uint8Array(capacity);
  let firstSeen = new Float64Array(capacity);
  // Slot + 1 per index cell; 0 is empty. Twice the ring keeps probes short.
  let index = new Int32Array(indexSizeFor(capacity));
  let mask = index.length - 1;
  let head = 0;
  let count = 0;

  const home = (id: Buffer, offset: number): number =>
    id.readUInt32LE(offset) & mask;

  const sameId = (slot: number, txId: Buffer): boolean =>
    ids.compare(
      txId,
      0,
      ID_BYTES,
      slot * ID_BYTES,
      slot * ID_BYTES + ID_BYTES,
    ) === 0;

  const findCell = (txId: Buffer): number => {
    let cell = home(txId, 0);
    for (;;) {
      const stored = index[cell]!;
      if (stored === 0) return -1;
      if (sameId(stored - 1, txId)) return cell;
      cell = (cell + 1) & mask;
    }
  };

  const insertCell = (slot: number): void => {
    let cell = home(ids, slot * ID_BYTES);
    while (index[cell] !== 0) cell = (cell + 1) & mask;
    index[cell] = slot + 1;
  };

  const deleteCell = (start: number): void => {
    let hole = start;
    let cell = start;
    index[hole] = 0;
    for (;;) {
      cell = (cell + 1) & mask;
      const stored = index[cell]!;
      if (stored === 0) return;
      const preferred = home(ids, (stored - 1) * ID_BYTES);
      // The entry may stay when its home lies cyclically in (hole, cell].
      const stays =
        hole <= cell
          ? hole < preferred && preferred <= cell
          : hole < preferred || preferred <= cell;
      if (stays) continue;
      index[hole] = stored;
      index[cell] = 0;
      hole = cell;
    }
  };

  const slotOfAge = (age: number): number =>
    (head - count + age + capacity) % capacity;

  const grow = (): void => {
    const next = Math.min(capacity * 2, maxCapacity);
    const nextIds = Buffer.alloc(next * ID_BYTES);
    const nextHashes = Buffer.alloc(next * HASH_BYTES);
    const nextStatuses = new Uint8Array(next);
    const nextFirstSeen = new Float64Array(next);
    for (let age = 0; age < count; age += 1) {
      const slot = slotOfAge(age);
      ids.copy(nextIds, age * ID_BYTES, slot * ID_BYTES, (slot + 1) * ID_BYTES);
      hashes.copy(
        nextHashes,
        age * HASH_BYTES,
        slot * HASH_BYTES,
        (slot + 1) * HASH_BYTES,
      );
      nextStatuses[age] = statuses[slot]!;
      nextFirstSeen[age] = firstSeen[slot]!;
    }
    capacity = next;
    ids = nextIds;
    hashes = nextHashes;
    statuses = nextStatuses;
    firstSeen = nextFirstSeen;
    index = new Int32Array(indexSizeFor(capacity));
    mask = index.length - 1;
    head = count % capacity;
    for (let slot = 0; slot < count; slot += 1) insertCell(slot);
  };

  const write = (slot: number, entry: RecentTxId): void => {
    entry.txFullHash.copy(hashes, slot * HASH_BYTES, 0, HASH_BYTES);
    statuses[slot] = Math.max(0, STATUSES.indexOf(entry.status));
    firstSeen[slot] = entry.firstSeenAt.getTime();
  };

  return {
    get: (txId) => {
      if (txId.length !== ID_BYTES) return undefined;
      const cell = findCell(txId);
      if (cell < 0) return undefined;
      const slot = index[cell]! - 1;
      return {
        txFullHash: Buffer.from(
          hashes.subarray(slot * HASH_BYTES, (slot + 1) * HASH_BYTES),
        ),
        status: STATUSES[statuses[slot]!]!,
        firstSeenAt: new Date(firstSeen[slot]!),
      };
    },
    remember: (txId, entry) => {
      if (txId.length !== ID_BYTES || entry.txFullHash.length !== HASH_BYTES) {
        return;
      }
      const existing = findCell(txId);
      if (existing >= 0) {
        write(index[existing]! - 1, entry);
        return;
      }
      if (count === capacity && capacity < maxCapacity) grow();
      if (count === capacity) {
        const oldest = slotOfAge(0);
        deleteCell(
          findCell(ids.subarray(oldest * ID_BYTES, (oldest + 1) * ID_BYTES)),
        );
        count -= 1;
      }
      const slot = head;
      txId.copy(ids, slot * ID_BYTES, 0, ID_BYTES);
      write(slot, entry);
      insertCell(slot);
      head = (head + 1) % capacity;
      count += 1;
    },
    size: () => count,
  };
};

const indexSizeFor = (capacity: number): number => {
  let size = 1;
  while (size < capacity * 2) size *= 2;
  return size;
};
