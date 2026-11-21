import type { LocalStateConfig } from "../config.js";
import {
  PostgresCommitteeStore,
  type PostgresCommitteeStoreOptions,
} from "./postgres.js";

export const openCommitteeStore = async (
  localState: LocalStateConfig,
  options: PostgresCommitteeStoreOptions = {},
): Promise<PostgresCommitteeStore> =>
  PostgresCommitteeStore.open(localState.url, options);
