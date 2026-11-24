/**
 * The L2 ledger store: UTxO ledgers addressed by their MPF root, with
 * immutable ledger values, over one storage seam with a memory, a SQLite
 * (`./ledger-store/sqlite`) and a LevelDB (in `midgard-node`) adapter.
 * Roots and proofs are byte for byte those of the vendored trie library the
 * on-chain verifier is checked against.
 */
export { EMPTY_LEDGER_ROOT } from "./engine.js";
export {
  LedgerEntryNotCanonical,
  type LedgerMutationRefusalReason,
  LedgerMutationRefused,
  LedgerNodeMissing,
  LedgerNotRetained,
  LedgerOutputMissing,
  LedgerSnapshotRefused,
  LedgerWitnessRefused,
} from "./errors.js";
export type {
  Ledger,
  LedgerMutation,
  LedgerRead,
  LedgerSource,
} from "./ledger.js";
export { createMemoryLedgerStorage } from "./memory.js";
export { ledgerAt, ledgerProof, ledgerReads, ledgerTrie } from "./read.js";
export type {
  LedgerStorage,
  LedgerStorageChange,
  LedgerStorageCounts,
  NodeHash,
  OutRefHex,
  StoredBranch,
  StoredLeaf,
  StoredNode,
} from "./storage.js";
export {
  createLedgerStore,
  type LedgerAdoption,
  type LedgerPersistOptions,
  type LedgerStore,
} from "./store.js";
export {
  ledgerFromWitness,
  type LedgerWitness,
  recordWitness,
  type WitnessBranch,
  type WitnessNode,
} from "./witness.js";
