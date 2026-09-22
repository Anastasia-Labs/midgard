import { DatabaseSync } from "node:sqlite";

import { describe, expect, it } from "vitest";

import {
  createWatcherSqliteBlockProgressStore,
  WATCHER_BLOCK_PROGRESS_CANDIDATE_LIMIT,
  WATCHER_BLOCK_PROGRESS_RETAINED_ROWS,
  type WatcherBlockProgressRecord,
} from "../../src/storage/block-progress-store.js";

const key = Uint8Array.from({ length: 32 }, (_, index) => index + 1);
const otherKey = Uint8Array.from({ length: 32 }, (_, index) => 255 - index);
const hashOf = (blockNo: number): string =>
  blockNo.toString(16).padStart(64, "0");
const record = (
  blockNo: number,
  relevance: WatcherBlockProgressRecord["relevance"] = "quiet",
): WatcherBlockProgressRecord =>
  Object.freeze({
    blockHash: hashOf(blockNo),
    parentBlockHash: hashOf(blockNo - 1),
    blockNo: blockNo.toString(),
    slot: (blockNo * 20).toString(),
    relevance,
  });
const open = (database = new DatabaseSync(":memory:")) => ({
  database,
  store: createWatcherSqliteBlockProgressStore({
    database,
    authenticationKey: key,
  }),
});

describe("authenticated block progress ring", () => {
  it("records a contiguous chain and resumes from its head", () => {
    const { database, store } = open();
    expect(store.readHead()).toBeNull();
    store.record(record(10, "touched"));
    store.record(record(11));
    store.record(record(12));
    expect(store.readHead()).toEqual(record(12));
    expect(
      store.readRange({ afterBlockNo: "10", throughBlockNo: "12" }),
    ).toEqual([record(11), record(12)]);
    // A second handle over the same database reads the same head.
    expect(open(database).store.readHead()).toEqual(record(12));
  });

  it("refuses a record that is not the head's direct child", () => {
    const { store } = open();
    store.record(record(10));
    expect(() => store.record(record(12))).toThrow("direct child");
    expect(() =>
      store.record({ ...record(11), parentBlockHash: hashOf(99) }),
    ).toThrow("direct child");
    expect(() => store.record({ ...record(11), slot: "200" })).toThrow(
      "direct child",
    );
    expect(store.readHead()).toEqual(record(10));
  });

  it("refuses rows written under another key or edited in place", () => {
    const { database, store } = open();
    store.record(record(10));
    store.record(record(11));
    const foreign = createWatcherSqliteBlockProgressStore({
      database,
      authenticationKey: otherKey,
    });
    expect(() => foreign.readHead()).toThrow("authentication");
    database
      .prepare(
        "UPDATE watcher_block_progress_v1 SET relevance = 'touched' WHERE block_no = 11",
      )
      .run();
    expect(() => store.readHead()).toThrow("authentication");
  });

  it("rolls back rows above the fork point and everything for origin", () => {
    const { store } = open();
    for (let blockNo = 10; blockNo <= 15; blockNo += 1)
      store.record(record(blockNo));
    store.rollbackTo({ kind: "point", blockHash: hashOf(12), slot: "240" });
    expect(store.readHead()).toEqual(record(12));
    // Same slot, different hash: the point itself is foreign and goes too.
    store.rollbackTo({ kind: "point", blockHash: hashOf(99), slot: "240" });
    expect(store.readHead()).toEqual(record(11));
    store.rollbackTo({ kind: "origin" });
    expect(store.readHead()).toBeNull();
  });

  it("prunes behind the ring size unless an older retention floor is requested", () => {
    const { store } = open();
    const first = 10;
    const last = first + WATCHER_BLOCK_PROGRESS_RETAINED_ROWS + 5;
    for (let blockNo = first; blockNo <= last; blockNo += 1) {
      store.record(record(blockNo), { retainFromBlockNo: "12" });
    }
    expect(
      store.readRange({ afterBlockNo: "9", throughBlockNo: "13" }),
    ).toEqual([record(12), record(13)]);
    // Without the floor the ring alone decides: rows below head - 2160 go.
    store.record(record(last + 1));
    const ringFloor = last + 1 - WATCHER_BLOCK_PROGRESS_RETAINED_ROWS;
    expect(
      store
        .readRange({ afterBlockNo: "9", throughBlockNo: "20" })
        .map(({ blockNo }) => blockNo),
    ).toEqual(
      Array.from({ length: 21 - ringFloor }, (_, index) =>
        (ringFloor + index).toString(),
      ),
    );
    expect(store.readHead()).toEqual(record(last + 1));
  });

  it("offers dense-then-sparse resume candidates newest first", () => {
    const { store } = open();
    for (let blockNo = 1; blockNo <= 1_500; blockNo += 1)
      store.record(record(blockNo));
    const candidates = store.readCandidates();
    expect(candidates.length).toBeLessThanOrEqual(
      WATCHER_BLOCK_PROGRESS_CANDIDATE_LIMIT,
    );
    expect(candidates[0]).toEqual(record(1_500));
    expect(candidates.slice(0, 8).map(({ blockNo }) => blockNo)).toEqual([
      "1500",
      "1499",
      "1498",
      "1497",
      "1496",
      "1495",
      "1494",
      "1493",
    ]);
    expect(candidates.at(-1)).toEqual(record(1));
    const numbers = candidates.map(({ blockNo }) => Number(blockNo));
    expect([...numbers].sort((a, b) => b - a)).toEqual(numbers);
  });
});
