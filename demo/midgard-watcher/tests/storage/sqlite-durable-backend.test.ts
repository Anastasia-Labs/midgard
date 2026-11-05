import { describe, expect, it } from "vitest";

import { openWatcherSqliteDurableBackend } from "../../src/storage/sqlite-durable-backend.js";

describe("production watcher SQLite durable backend", () => {
  it("refuses temporary and symlink-traversing persistence paths", async () => {
    await expect(
      openWatcherSqliteDurableBackend({ path: "/tmp/watcher.sqlite" }),
    ).rejects.toThrow("canonical durable path");
  });
});
