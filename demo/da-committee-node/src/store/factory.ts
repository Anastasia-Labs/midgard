import type { LocalStateConfig } from "../config.js";
import { JsonFileWatcherStore, type WatcherStore } from "../store.js";
import { PostgresWatcherStore } from "./postgres.js";

export const openWatcherStore = async (
  localState: LocalStateConfig,
): Promise<WatcherStore> => {
  switch (localState.kind) {
    case "file":
      return JsonFileWatcherStore.open(localState.path);
    case "database":
      return PostgresWatcherStore.open(localState.url);
  }
};
