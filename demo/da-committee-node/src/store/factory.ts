import type { LocalStateConfig } from "../config.js";
import type { CommitteeStore } from "../store.js";
import {
  PostgresCommitteeStore,
  type PostgresCommitteeStoreOptions,
} from "./postgres.js";

export const openCommitteeStore = async (
  localState: LocalStateConfig,
  options: PostgresCommitteeStoreOptions = {},
): Promise<CommitteeStore> =>
  PostgresCommitteeStore.open(localState.url, options);
