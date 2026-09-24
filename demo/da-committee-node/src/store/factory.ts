import type { LocalStateConfig } from "../config.js";
import { type CommitteeStore, JsonFileCommitteeStore } from "../store.js";
import { PostgresCommitteeStore } from "./postgres.js";

export const openCommitteeStore = async (
  localState: LocalStateConfig,
): Promise<CommitteeStore> => {
  switch (localState.kind) {
    case "file":
      return JsonFileCommitteeStore.open(localState.path);
    case "database":
      return PostgresCommitteeStore.open(localState.url);
  }
};
