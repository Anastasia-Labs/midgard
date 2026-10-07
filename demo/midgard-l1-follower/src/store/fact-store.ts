import { outRefKey } from "../codec.js";
import {
  createTemporalRegistry,
  type TemporalRegistry,
  type TemporalTableSpec,
} from "../registry.js";
import { followerMigrations } from "../schema/follower-migrations.js";
import { applyMigrations, type MigrationSet } from "../schema/migrate.js";
import {
  RollbackWith,
  type SqlBackend,
  type SqlTx,
  type TransactionMode,
} from "../sql/backend.js";
import type {
  BlockSummary,
  Cursor,
  Intervention,
  OutRef,
  Point,
  StoredBlock,
  StoredOutput,
  StoredTx,
  TrackedSet,
  View,
} from "../types.js";
import {
  applyBlockIn,
  type ApplyRejection,
  type BlockApplied,
} from "./apply.js";
import type { DerivationHook, RetentionPins, StoreContext } from "./context.js";
import {
  describeViolations,
  type InvariantReport,
  runInvariantChecks,
} from "./invariants.js";
import { pruneIn, type PruneResult } from "./prune.js";
import * as reads from "./reads.js";
import {
  type RewindFault,
  rewindIn,
  type RewindNoop,
  type Rewound,
} from "./rewind.js";
import { StoreIntegrityError } from "./rows.js";
import {
  initializeIn,
  insertSeedOutputsIn,
  loadLiveOutRefs,
  type SeedOutput,
  type SeedResult,
} from "./seed.js";
import { currentViewIn, viewValidIn } from "./view.js";

/** A write that failed and rolled back with nothing changed; retry with backoff. */
export type StoreError = Readonly<{ kind: "error"; error: Error }>;

export type ApplyResult =
  | BlockApplied
  | ApplyRejection
  | Intervention
  | StoreError;
export type RewindResult = Rewound | RewindNoop | Intervention | StoreError;

export type StartResult =
  | Readonly<{
      kind: "ready";
      cursor: Cursor | null;
      liveOutRefs: number;
      migrated: readonly string[];
    }>
  | Intervention;

export type InitializeResult =
  | Readonly<{ kind: "initialized" | "already_initialized"; cursor: Cursor }>
  | Readonly<{ kind: "origin_mismatch"; cursor: Cursor }>;

export type FactStoreOptions = Readonly<{
  /** The security parameter k in blocks (2,160 on mainnet and preprod). */
  securityParameter: number;
  /** The role's static tracked set (§5.2); replace with `setTrackedSet`. */
  trackedSet: TrackedSet;
  /** The role's D-t tables (§7.2). */
  temporalTables?: readonly TemporalTableSpec[];
  /** The role's migrations (D-t tables and others), applied after the follower's. */
  migrations?: readonly MigrationSet[];
  /** The role's S3 derivations, run in each block's transaction in order. */
  derivations?: readonly DerivationHook[];
  /** Columns whose values keep txs (by hash) or blocks (by slot) from pruning. */
  retentionPins?: RetentionPins;
}>;

export type GenerationListener = (
  event: Readonly<{ generation: number; rewound: Rewound }>,
) => void;

export type FactStore = Readonly<{
  dialect: SqlBackend["dialect"];
  registry: TemporalRegistry;
  /** Migrates, runs INV1–INV6 (R5 on failure), loads the tracked-outref set. */
  start(): Promise<StartResult>;
  initialize(
    origin: Readonly<{ point: Point; height: number }>,
  ): Promise<InitializeResult | StoreError>;
  applyBlock(block: BlockSummary): Promise<ApplyResult>;
  rewind(target: Point): Promise<RewindResult>;
  insertSeedOutputs(
    seedSlot: number,
    outputs: readonly SeedOutput[],
  ): Promise<SeedResult | StoreError | null>;
  setTrackedSet(trackedSet: TrackedSet): void;
  trackedSet(): TrackedSet;
  /** Whether an outref is a live tracked row (the in-memory set). */
  isTrackedLive(outRef: OutRef): boolean;
  liveOutRefCount(): number;
  prune(budget?: number): Promise<PruneResult | StoreError>;
  checkInvariants(): Promise<InvariantReport>;
  cursor(): Promise<Cursor | null>;
  currentView(): Promise<View | null>;
  viewValid(view: View): Promise<boolean>;
  onGeneration(listener: GenerationListener): () => void;
  /** A transaction on the store's backend, for reads and guarded writes. */
  transaction<T>(
    mode: TransactionMode,
    run: (tx: SqlTx) => Promise<T>,
  ): Promise<T>;
  liveUtxos(filter: reads.UtxoFilter, at?: Point): Promise<reads.UtxoRead>;
  output(outRef: OutRef): Promise<StoredOutput | null>;
  spenderOf(outRef: OutRef): Promise<reads.Spender>;
  txByHash(hash: Buffer): Promise<StoredTx | null>;
  isCanonical(blockHash: Buffer): Promise<boolean>;
  pointStatus(point: Point): Promise<reads.PointStatus>;
  blockByHash(hash: Buffer): Promise<StoredBlock | null>;
  blockAtHeight(height: number): Promise<StoredBlock | null>;
  blockAtOrBeforeSlot(slot: number): Promise<StoredBlock | null>;
  close(): Promise<void>;
}>;

/** Serialises the store's writers (the sequential writer, rewind, prune, seed). */
class Lane {
  private tail: Promise<unknown> = Promise.resolve();
  run<T>(task: () => Promise<T>): Promise<T> {
    const result = this.tail.then(task, task);
    this.tail = result.catch(() => undefined);
    return result;
  }
}

const asError = (error: unknown): Error =>
  error instanceof Error ? error : new Error(String(error));

const integrity = (detail: string): Intervention => ({
  kind: "intervention",
  reason: "store_integrity",
  detail,
});

/** Internal fault seam for the negative property tests (not exported publicly). */
export const rewindFaults = new WeakMap<FactStore, RewindFault>();

const LOAD_PAGE = 10_000;
const DEFAULT_PRUNE_BUDGET = 5_000;

export const createFactStore = (
  backend: SqlBackend,
  options: FactStoreOptions,
): FactStore => {
  if (
    !Number.isSafeInteger(options.securityParameter) ||
    options.securityParameter < 1
  )
    throw new Error("securityParameter must be a positive integer");
  const registry = createTemporalRegistry(options.temporalTables ?? []);
  const derivations = options.derivations ?? [];
  for (const derivation of derivations)
    for (const table of derivation.writes)
      if (!registry.has(table))
        throw new Error(
          `derivation ${derivation.name} writes unregistered table ${table}`,
        );
  const context: StoreContext = {
    backend,
    dialect: backend.dialect,
    k: options.securityParameter,
    registry,
    derivations,
    pins: options.retentionPins ?? {},
  };
  const dialect = backend.dialect;
  const lane = new Lane();
  const live = new Set<string>();
  const listeners = new Set<GenerationListener>();
  let tracked = options.trackedSet;
  let broken: Intervention | null = null;
  let started = false;
  const notStarted: StoreError = {
    kind: "error",
    error: new Error("the fact store has not been started"),
  };

  const markBroken = (detail: string): Intervention => {
    broken = integrity(detail);
    return broken;
  };
  const read = <T>(run: (tx: SqlTx) => Promise<T>): Promise<T> =>
    backend.transaction("read", run);

  const checkInvariants = (): Promise<InvariantReport> =>
    read((tx) => runInvariantChecks(tx, dialect, registry, "full"));

  const store: FactStore = {
    dialect,
    registry,
    start: () =>
      lane.run(async (): Promise<StartResult> => {
        const { applied } = await applyMigrations(backend, [
          followerMigrations(dialect.name),
          ...(options.migrations ?? []),
        ]);
        const report = await checkInvariants();
        if (!report.ok)
          return markBroken(`at start: ${describeViolations(report)}`);
        live.clear();
        const count = await loadLiveOutRefs(
          (after) =>
            read((tx) =>
              tx.query(
                after === null
                  ? "SELECT tx_hash, output_index FROM l1_outputs WHERE spent_slot IS NULL ORDER BY tx_hash, output_index LIMIT ?"
                  : "SELECT tx_hash, output_index FROM l1_outputs WHERE spent_slot IS NULL AND (tx_hash > ? OR (tx_hash = ? AND output_index > ?)) ORDER BY tx_hash, output_index LIMIT ?",
                after === null
                  ? [LOAD_PAGE]
                  : [after.txHash, after.txHash, after.index, LOAD_PAGE],
              ),
            ),
          (outRef) => live.add(outRefKey(outRef)),
        );
        const cursor = await read((tx) => reads.tipIn(tx, dialect));
        started = true;
        return { kind: "ready", cursor, liveOutRefs: count, migrated: applied };
      }),
    initialize: (origin) =>
      lane.run(async () => {
        try {
          return await backend.transaction("write", (tx) =>
            initializeIn(tx, dialect, origin),
          );
        } catch (error) {
          return { kind: "error", error: asError(error) } as const;
        }
      }),
    applyBlock: (block) =>
      lane.run(async (): Promise<ApplyResult> => {
        if (broken !== null) return broken;
        if (!started) return notStarted;
        try {
          const result = await backend.transaction("write", (tx) =>
            applyBlockIn(tx, context, block, tracked, (key) => live.has(key)),
          );
          if (result.kind === "applied") {
            for (const outRef of result.spent) live.delete(outRefKey(outRef));
            for (const outRef of result.created) live.add(outRefKey(outRef));
          }
          return result;
        } catch (error) {
          if (error instanceof StoreIntegrityError)
            return markBroken(error.message);
          return { kind: "error", error: asError(error) };
        }
      }),
    rewind: (target) =>
      lane.run(async (): Promise<RewindResult> => {
        if (broken !== null) return broken;
        if (!started) return notStarted;
        let result: RewindResult;
        try {
          result = await backend.transaction("write", (tx) =>
            rewindIn(tx, context, target, rewindFaults.get(store) ?? null),
          );
        } catch (error) {
          if (error instanceof RollbackWith) return error.value as Intervention;
          return { kind: "error", error: asError(error) };
        }
        if (result.kind === "intervention") {
          if (result.reason === "store_integrity") broken = result;
          return result;
        }
        if (result.kind !== "rewound") return result;
        for (const outRef of result.deleted) live.delete(outRefKey(outRef));
        for (const outRef of result.unspent) live.add(outRefKey(outRef));
        for (const listener of listeners)
          try {
            listener({ generation: result.generation, rewound: result });
          } catch {
            // A listener's failure never undoes or blocks a committed rewind.
          }
        return result;
      }),
    insertSeedOutputs: (seedSlot, outputs) =>
      lane.run(async () => {
        if (broken !== null)
          return { kind: "error", error: new Error(broken.detail) } as const;
        if (!started) return notStarted;
        try {
          const result = await backend.transaction("write", (tx) =>
            insertSeedOutputsIn(tx, dialect, seedSlot, outputs),
          );
          for (const outRef of result?.inserted ?? [])
            live.add(outRefKey(outRef));
          return result;
        } catch (error) {
          return { kind: "error", error: asError(error) } as const;
        }
      }),
    setTrackedSet: (next) => {
      tracked = next;
    },
    trackedSet: () => tracked,
    isTrackedLive: (outRef) => live.has(outRefKey(outRef)),
    liveOutRefCount: () => live.size,
    prune: (budget = DEFAULT_PRUNE_BUDGET) =>
      lane.run(async () => {
        try {
          return await backend.transaction("write", (tx) =>
            pruneIn(tx, context, budget),
          );
        } catch (error) {
          return { kind: "error", error: asError(error) } as const;
        }
      }),
    checkInvariants,
    cursor: () => read((tx) => reads.tipIn(tx, dialect)),
    currentView: () => read((tx) => currentViewIn(tx, dialect)),
    // FOR SHARE needs a read-write transaction on Postgres.
    viewValid: (view) =>
      backend.transaction("write", (tx) => viewValidIn(tx, dialect, view)),
    onGeneration: (listener) => {
      listeners.add(listener);
      return () => listeners.delete(listener);
    },
    transaction: (mode, run) => backend.transaction(mode, run),
    liveUtxos: (filter, at) =>
      read((tx) => reads.liveUtxosIn(tx, dialect, filter, at)),
    output: (outRef) => read((tx) => reads.outputIn(tx, dialect, outRef)),
    spenderOf: (outRef) => read((tx) => reads.spenderOfIn(tx, outRef)),
    txByHash: (hash) => read((tx) => reads.txByHashIn(tx, dialect, hash)),
    isCanonical: (hash) => read((tx) => reads.isCanonicalIn(tx, hash)),
    pointStatus: (point) =>
      read((tx) => reads.pointStatusIn(tx, dialect, point)),
    blockByHash: (hash) => read((tx) => reads.blockByHashIn(tx, hash)),
    blockAtHeight: (height) => read((tx) => reads.blockAtHeightIn(tx, height)),
    blockAtOrBeforeSlot: (slot) =>
      read((tx) => reads.blockAtOrBeforeSlotIn(tx, slot)),
    close: () => lane.run(() => backend.close()),
  };
  return store;
};
