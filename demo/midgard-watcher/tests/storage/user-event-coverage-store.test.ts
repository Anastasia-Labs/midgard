import { DatabaseSync } from "node:sqlite";

import { h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import {
  assertWatcherUserEventCoverageRecord,
  createInMemoryWatcherUserEventCoverageStore,
  createWatcherSqliteUserEventCoverageStore,
  type WatcherUserEventCoverageRecord,
} from "../../src/storage/user-event-coverage-store.js";

const key = Uint8Array.from({ length: 32 }, (_, index) => index + 1);
const otherKey = Uint8Array.from({ length: 32 }, (_, index) => 200 - index);
const record: WatcherUserEventCoverageRecord = Object.freeze({
  blockHash: h32(0xab),
  blockNo: "1200",
  slot: "24000",
  headEntryDigest: h32(0xcd),
  checkpointDigest: h32(0xef),
});

describe("user-event coverage store", () => {
  it("writes, reads back, advances in place, and clears one checkpoint row", () => {
    const database = new DatabaseSync(":memory:");
    const store = createWatcherSqliteUserEventCoverageStore({
      database,
      authenticationKey: key,
    });
    expect(store.read()).toBeNull();
    store.write(record);
    expect(store.read()).toEqual(record);
    const advanced = {
      ...record,
      blockHash: h32(0xac),
      blockNo: "1201",
      slot: "24020",
    };
    store.write(advanced);
    expect(store.read()).toEqual(advanced);
    expect(
      database
        .prepare("SELECT COUNT(*) AS rows FROM watcher_user_event_coverage_v1")
        .get(),
    ).toMatchObject({ rows: 1 });
    store.clear();
    expect(store.read()).toBeNull();
    const reopened = createWatcherSqliteUserEventCoverageStore({
      database,
      authenticationKey: key,
    });
    reopened.write(record);
    expect(
      createWatcherSqliteUserEventCoverageStore({
        database,
        authenticationKey: key,
      }).read(),
    ).toEqual(record);
  });

  it("refuses a row whose authentication does not verify", () => {
    const database = new DatabaseSync(":memory:");
    createWatcherSqliteUserEventCoverageStore({
      database,
      authenticationKey: key,
    }).write(record);
    expect(() =>
      createWatcherSqliteUserEventCoverageStore({
        database,
        authenticationKey: otherKey,
      }).read(),
    ).toThrow();
    database
      .prepare("UPDATE watcher_user_event_coverage_v1 SET block_no = 1300")
      .run();
    expect(() =>
      createWatcherSqliteUserEventCoverageStore({
        database,
        authenticationKey: key,
      }).read(),
    ).toThrow();
  });

  it("refuses malformed records before writing them", () => {
    const store = createInMemoryWatcherUserEventCoverageStore();
    for (const bad of [
      { ...record, blockHash: "ab" },
      { ...record, blockNo: "01" },
      { ...record, slot: "-1" },
      { ...record, headEntryDigest: "" },
      { ...record, checkpointDigest: "z".repeat(32) },
    ])
      expect(() => assertWatcherUserEventCoverageRecord(bad)).toThrow();
    store.write(record);
    expect(store.read()).toEqual(record);
    store.clear();
    expect(store.read()).toBeNull();
  });
});
