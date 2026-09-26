import type { LocalStateConfig } from "../config.js";
import { type CommitteeStore, JsonFileCommitteeStore } from "../store.js";
import {
  PostgresCommitteeStore,
  type PostgresCommitteeStoreOptions,
} from "./postgres.js";

export const openCommitteeStore = async (
  localState: LocalStateConfig,
  options: PostgresCommitteeStoreOptions = {},
): Promise<CommitteeStore> => {
  switch (localState.kind) {
    case "file":
      return JsonFileCommitteeStore.open(localState.path);
    case "database":
      return PostgresCommitteeStore.open(localState.url, options);
  }
};
