import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it } from "vitest";

import type { WatcherProductionStateQueueObservationV1 } from "../../src/indexers/production-state-queue-observation-v1.js";
import {
  openWatcherSqliteDurableBackendV1,
  unsafeOpenWatcherSqliteDurableBackendForTestV1,
} from "../../src/storage/sqlite-durable-backend-v1.js";

const directories: string[] = [];
const directory = async (): Promise<string> => {
  const value = await mkdtemp("/var/tmp/midgard-watcher-sqlite-");
  directories.push(value);
  return value;
};
const bytes = (value: string): Uint8Array => new TextEncoder().encode(value);
const digest = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");
const hex32 = (value: number): string => value.toString(16).padStart(64, "0");
const stateQueueObservation = (
  value: number,
  previousObservationDigest: string | null,
): WatcherProductionStateQueueObservationV1 =>
  Object.freeze({
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    deploymentIdentityDigest: "11".repeat(32),
    protocolScriptAuthorityDigest: "12".repeat(32),
    stateQueuePolicyId: "13".repeat(28),
    hubOraclePolicyId: "14".repeat(28),
    nativePoint: Object.freeze({
      blockHash: hex32(value),
      parentBlockHash: value === 1 ? null : hex32(value - 1),
      slot: (1_000 + value).toString(),
      blockNo: (100 + value).toString(),
      chainPointId: hex32(10_000 + value),
      finalityDepth: "30",
    }),
    sourceId: "test-source",
    previousObservationDigest,
    checkpoints: Object.freeze([]),
    finalizedQueue: Object.freeze([]),
    finalizedHeaders: Object.freeze([]),
    finalizedCorrectionLock: null,
    correctionLockWitnesses: Object.freeze([]),
    observationDigest: hex32(20_000 + value),
  });

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => await rm(path, { recursive: true })),
  );
});

describe("production watcher SQLite durable backend", () => {
  it("atomically initializes, rejects stale CAS, replaces and survives restart", async () => {
    const path = join(await directory(), "watcher.sqlite");
    const first = bytes("first complete snapshot");
    const second = bytes("second complete snapshot");
    const opened = await openWatcherSqliteDurableBackendV1({ path });
    expect(await opened.backend.read()).toBeNull();
    expect(await opened.backend.compareAndSwap(null, first)).toBe(true);
    expect(await opened.backend.compareAndSwap(null, second)).toBe(false);
    expect(await opened.backend.compareAndSwap("00".repeat(32), second)).toBe(
      false,
    );
    expect(await opened.backend.compareAndSwap(digest(first), second)).toBe(
      true,
    );
    expect(await opened.backend.read()).toEqual(second);
    opened.close();

    const restarted = await openWatcherSqliteDurableBackendV1({ path });
    try {
      expect(await restarted.backend.read()).toEqual(second);
    } finally {
      restarted.close();
    }
  });

  it("detects caller-external database corruption instead of trusting stored hashes", async () => {
    const path = join(await directory(), "watcher.sqlite");
    const opened = await openWatcherSqliteDurableBackendV1({ path });
    const first = bytes("authenticated snapshot");
    expect(await opened.backend.compareAndSwap(null, first)).toBe(true);
    opened.close();

    const hostile = new DatabaseSync(path);
    hostile
      .prepare(
        "UPDATE watcher_durable_snapshot_v1 SET bytes = ? WHERE singleton = 1",
      )
      .run(bytes("substituted snapshot"));
    hostile.close();

    const reopened = await openWatcherSqliteDurableBackendV1({ path });
    try {
      await expect(reopened.backend.read()).rejects.toThrow("digest mismatch");
      await expect(
        reopened.backend.compareAndSwap(digest(first), bytes("next")),
      ).rejects.toThrow("digest mismatch");
    } finally {
      reopened.close();
    }
  });

  it("refuses temporary and symlink-traversing persistence paths", async () => {
    await expect(
      openWatcherSqliteDurableBackendV1({ path: "/tmp/watcher.sqlite" }),
    ).rejects.toThrow("canonical durable path");
  });

  it("persists one exact sparse observation chain and revokes the rolled-back suffix", async () => {
    const path = join(await directory(), "watcher.sqlite");
    const opened = await unsafeOpenWatcherSqliteDurableBackendForTestV1(
      { path },
      () => undefined,
    );
    const first = stateQueueObservation(1, null);
    const second = stateQueueObservation(2, first.observationDigest);
    const third = stateQueueObservation(3, second.observationDigest);
    try {
      await expect(opened.stateQueueObservations.append(first)).resolves.toBe(
        "appended",
      );
      await expect(opened.stateQueueObservations.append(first)).resolves.toBe(
        "unchanged",
      );
      await opened.stateQueueObservations.append(second);
      await opened.stateQueueObservations.append(third);
      expect(await opened.stateQueueObservations.readAll()).toEqual([
        first,
        second,
        third,
      ]);

      await opened.stateQueueObservations.rollbackTo(
        Object.freeze({
          kind: "point",
          blockHash: second.nativePoint.blockHash,
          slot: second.nativePoint.slot,
        }),
      );
      expect(await opened.stateQueueObservations.readAll()).toEqual([
        first,
        second,
      ]);

      await expect(
        opened.stateQueueObservations.append(
          stateQueueObservation(4, first.observationDigest),
        ),
      ).rejects.toThrow("non-successor");
    } finally {
      opened.close();
    }

    const sequenceGap = new DatabaseSync(path);
    sequenceGap
      .prepare(
        "UPDATE sqlite_sequence SET seq = 10000 WHERE name = 'watcher_state_queue_observation_v1'",
      )
      .run();
    sequenceGap.close();

    const restarted = await unsafeOpenWatcherSqliteDurableBackendForTestV1(
      { path },
      () => undefined,
    );
    try {
      const replacement = stateQueueObservation(4, second.observationDigest);
      await restarted.stateQueueObservations.append(replacement);
      expect(await restarted.stateQueueObservations.readAll()).toEqual([
        first,
        second,
        replacement,
      ]);
      await restarted.stateQueueObservations.rollbackTo(
        Object.freeze({ kind: "origin" }),
      );
      expect(await restarted.stateQueueObservations.readAll()).toEqual([]);
    } finally {
      restarted.close();
    }
  });

  it("rejects structural observations and detects persisted sparse-cache substitution", async () => {
    const path = join(await directory(), "watcher.sqlite");
    const production = await openWatcherSqliteDurableBackendV1({ path });
    await expect(
      production.stateQueueObservations.append(stateQueueObservation(1, null)),
    ).rejects.toThrow("was not admitted");
    production.close();

    const seeded = await unsafeOpenWatcherSqliteDurableBackendForTestV1(
      { path },
      () => undefined,
    );
    await seeded.stateQueueObservations.append(stateQueueObservation(1, null));
    seeded.close();

    const hostile = new DatabaseSync(path);
    hostile
      .prepare(
        "UPDATE watcher_state_queue_observation_v1 SET canonical_json = ? WHERE sequence = 1",
      )
      .run("{}");
    hostile.close();

    const reopened = await unsafeOpenWatcherSqliteDurableBackendForTestV1(
      { path },
      () => undefined,
    );
    try {
      await expect(reopened.stateQueueObservations.readAll()).rejects.toThrow(
        "malformed",
      );
    } finally {
      reopened.close();
    }
  });
});
