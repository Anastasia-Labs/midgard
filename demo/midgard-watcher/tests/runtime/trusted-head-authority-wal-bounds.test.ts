import { stat } from "node:fs/promises";
import { DatabaseSync } from "node:sqlite";

import { expect, it } from "vitest";

import { head } from "./trusted-head-authority.policy.js";
import { sqliteScene } from "./trusted-head-authority.sqlite-fixture.js";

it("reuses DB/WAL pages across sustained retirement without long-lived authority read transactions", async () => {
  const scene = await sqliteScene(8);
  let prior = await scene.advance(16),
    maximumWalBytes = 0;
  const fileSize = async (path: string) => {
    try {
      return (await stat(path)).size;
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code === "ENOENT") return 0;
      throw error;
    }
  };
  const before = await fileSize(scene.databasePath);
  try {
    for (let i = 0; i < 256; i++) {
      const next = head(scene.input.policy, 16 + i, "88");
      expect(
        await scene.store.compareAndSwap({
          expectedTrustedHead: prior,
          nextTrustedHead: next,
        }),
      ).toEqual({ committed: true, head: next });
      prior = next;
      maximumWalBytes = Math.max(
        maximumWalBytes,
        await fileSize(scene.databasePath + "-wal"),
      );
    }
    expect(await scene.store.readCurrent()).toEqual(prior);
  } finally {
    scene.store.close();
  }
  const db = new DatabaseSync(scene.databasePath);
  try {
    expect(
      db.prepare("SELECT count(*) AS count FROM authority_records").get()!
        .count,
    ).toBe(8);
    expect(
      db.prepare("PRAGMA freelist_count").get()!.freelist_count,
    ).toBeLessThanOrEqual(8);
  } finally {
    db.close();
  }
  const after = await fileSize(scene.databasePath);
  // Broad physical regression limits for this synthetic small-record domain.
  expect(after).toBeLessThanOrEqual(128 * 1024);
  expect(maximumWalBytes).toBeLessThanOrEqual(512 * 1024);
  console.log(
    "AUTHORITY_WAL_DOMAIN " +
      JSON.stringify({
        liveRecordLimit: 8,
        advances: 256,
        beforeDatabaseBytes: before,
        afterDatabaseBytes: after,
        maximumWalBytes,
      }),
  );
});
