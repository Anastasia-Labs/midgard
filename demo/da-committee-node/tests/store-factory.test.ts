import { describe, expect, it } from "vitest";

import { openWatcherStore } from "../src/store/factory.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { tempDir } from "./helpers.js";

describe("openWatcherStore", () => {
  it("opens the JSON file store for WATCHER_DB_PATH config", async () => {
    const store = await openWatcherStore({
      kind: "file",
      path: await tempDir(),
    });
    expect(store).toBeInstanceOf(JsonFileWatcherStore);
  });

  it("rejects non-Postgres WATCHER_DATABASE_URL values", async () => {
    await expect(
      openWatcherStore({
        kind: "database",
        url: "sqlite:///tmp/watcher.db",
      }),
    ).rejects.toThrow(/postgres/);
  });
});
