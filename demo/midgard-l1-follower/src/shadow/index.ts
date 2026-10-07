/**
 * The shadow-diff harness (plan §14): per-block comparison of each role's
 * new projections with the current code's view, the pluggable comparator
 * and plugin interfaces, and the devnet soak runner. Development tooling
 * for the migration; the role comparators are deleted at each cutover.
 */
export {
  reading,
  SHADOW_ROLES,
  type ShadowComparator,
  type ShadowContext,
  type ShadowReading,
  type ShadowRole,
  unavailable,
} from "./comparator.js";
export { compareAll, firstDisagreement, type ShadowResult } from "./compare.js";
export { type DiffEntry, diffValues, type Json, normalise } from "./diff.js";
export {
  type BlockRecord,
  formatSummary,
  Journal,
  JOURNAL_FILE,
  type JournalRecord,
  readJournal,
  type SoakSummary,
  summarise,
} from "./journal.js";
export {
  decodeUtxoAnswer,
  ledgerComparator,
  type LedgerStateReader,
} from "./ledger-comparator.js";
export { isShadowPlugin, type ShadowEnv, type ShadowPlugin } from "./plugin.js";
export {
  type FollowerProjection,
  mergeTrackedSets,
  projectionStoreOptions,
} from "./projection.js";
export {
  rolesWithout,
  runSoak,
  type SoakOptions,
  type SoakStop,
  type SoakStream,
} from "./soak.js";
export {
  readSoakConfig,
  type SoakConfig,
  SoakConfigError,
} from "./soak-config.js";
