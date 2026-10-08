/**
 * The landed-block rebase through the history owner (plan §7.3, N3), with
 * a real owner over recorded emulator history and a modelled native MPF
 * owner: a processed foreign block waits unapplied until the landed-block
 * port's `requestRebase` asks the owner to reconcile; the owner's reconcile
 * reads the rebase as pending (`landedBlockRebaseDisposition`), its
 * pending-reconciliation preparation runs `prepareLandedBlockRebase`, and
 * the next reconcile finds nothing pending, with the owner ready again.
 */
import { randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Ref, Schedule } from "effect";
import { expect, it, vi } from "vitest";

import { repairUnpublishedHistoryLedger } from "../src/database/eventHistoryLedgerRepair.js";
import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import { nodeLandedBlockPorts } from "../src/landed-blocks/node-ports.js";
import {
  landedBlockRebaseDisposition,
  prepareLandedBlockRebase,
} from "../src/landed-blocks/rebase.js";
import { rebasePlan } from "../src/landed-blocks/rebase-target.js";
import {
  Frontier,
  insertRow,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import { makeEventHistoryOwner } from "../src/services/event-history-owner.js";
import {
  runHistoryProducer,
  withHistoryWrite,
} from "../src/services/event-history-producer.js";
import { Globals } from "../src/services/globals.js";
import { ingestAtFollowerView } from "../src/services/l1-follower.recovery.js";
import { Lucid } from "../src/services/lucid.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/protocol.js";
import {
  makeRecordedHistoryTransport,
  openHistorySourceOwnerLifecycle,
} from "./helpers/history-source-owner-emulator.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const entry = (label: string, lovelace: bigint): Ledger.MinimalEntry => ({
  outref: makeOutRefCbor(simDigest(`owner-rebase:${label}`), 0),
  output: simOutput(lovelace),
});

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");
const root = (n: number) => n.toString(16).padStart(2, "0").repeat(32);

const E0 = entry("e0", 2_000_000n);
const E1 = entry("e1", 3_000_000n);
const FRONTIER = "f0".repeat(28);
const BLOCK = "b1".repeat(28);
const R0 = root(0x10);
const R1 = root(0x11);

/** The modelled native MPF owner: the processed block's delta reaches `R1`. */
const nativeOwner = (native: { durableRoot: string }) =>
  ({
    diagnostics: async () => ({ durableRoot: native.durableRoot }),
    restoreCanonicalRoot: async ({ targetRoot }: { targetRoot: string }) => {
      native.durableRoot = targetRoot;
    },
    fork: async (base: string) => ({ base }),
    applyEvents: async () => ({ candidateRoot: R1 }),
    promote: async () => {
      native.durableRoot = R1;
    },
    discard: async () => undefined,
  }) as unknown as NativeMpfOwnerService;

it("runs a requested landed-block rebase through the owner's reconcile and preparation", async () => {
  const recorded = await openHistorySourceOwnerLifecycle();
  await recorded.observer.flush();
  recorded.observer.restore();
  vi.useRealTimers();
  const transport = makeRecordedHistoryTransport(recorded);
  const { globals } = recorded;
  const native = { durableRoot: R0 };
  await Effect.runPromise(
    Ref.set(globals.NATIVE_MPF_OWNER, nativeOwner(native)),
  );
  const prepared: string[] = [];
  try {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.scoped(
          Effect.gen(function* () {
            yield* withHistoryWrite(resetApplicationTables);
            const cache = yield* makeMempoolLedgerCacheService(
              globals,
              Effect.succeed([]),
            );
            const owner = yield* makeEventHistoryOwner({
              binding: recorded.binding,
              histories: SDK.requireEventHistoryContracts(
                recorded.fixture.contracts,
              ),
              ownerToken: randomUUID(),
              cache,
              slotToUnixTime: recorded.fixture.operatorLucid.slotToUnixTime,
              transport: transport.options,
              heartbeatIntervalMs: 100,
              leaseDurationMs: 60_000,
              rollbackHorizon: 2160,
              retainedPointLimit: 16,
              maximumReceiptBytes: 16 * 1024 * 1024,
              // The node's wiring (event-history-runtime.ts): the rebase's
              // disposition first, then the follower-view ingest.
              reconcile: (change) =>
                Effect.gen(function* () {
                  const rebase = yield* landedBlockRebaseDisposition;
                  if (rebase !== undefined) return rebase;
                  return yield* ingestAtFollowerView({
                    change,
                    repair: repairUnpublishedHistoryLedger(change),
                    network: "Preprod",
                    slotToUnixTime:
                      recorded.fixture.operatorLucid.slotToUnixTime,
                  }).pipe(Effect.provideService(Globals, globals));
                }),
              preparePendingReconciliation: (checkpoint, preparation) =>
                Effect.sync(() => prepared.push(checkpoint.head.id)).pipe(
                  Effect.zipRight(prepareLandedBlockRebase(preparation)),
                  Effect.provideService(Globals, globals),
                  // Reached only to start a native owner; the modelled one
                  // is running.
                  Effect.provideService(Lucid, {} as Lucid),
                  Effect.provideService(
                    MidgardContracts,
                    {} as MidgardContracts,
                  ),
                ),
            });
            yield* owner.awaitReady.pipe(Effect.timeout("30 seconds"));
            yield* Ref.set(globals.EVENT_HISTORY_OWNER, owner);

            // `confirmed_ledger` at the frontier holds `E0`; the processed
            // foreign block spends it for `E1` and waits for the rebase.
            yield* runHistoryProducer(
              Effect.gen(function* () {
                yield* ConfirmedLedgerDB.insertMultiple([
                  ...(yield* ledgerRows([E0], new Map())),
                ]);
                yield* Frontier.upsert({ headerHash: FRONTIER, utxosRoot: R0 });
                yield* insertRow({
                  headerHash: BLOCK,
                  parentHeaderHash: FRONTIER,
                  parentUtxosRoot: R0,
                  utxosRoot: R1,
                  kind: "foreign",
                  state: "processed",
                  applied: false,
                  spent: [E0.outref],
                  produced: [E1],
                  depositIds: [],
                  withdrawals: [],
                  forcedIds: [],
                  txIds: [],
                });
              }),
            ).pipe(Effect.provideService(Globals, globals));
            expect((yield* rebasePlan).kind).toBe("ready");
            // Nothing runs it unasked.
            yield* Effect.sleep("500 millis");
            expect((yield* retrieveRows).map((row) => row.applied)).toEqual([
              false,
            ]);
            expect(prepared).toEqual([]);

            const blocked = yield* nodeLandedBlockPorts(
              {} as Parameters<typeof nodeLandedBlockPorts>[0],
              {} as Parameters<typeof nodeLandedBlockPorts>[1],
            )
              .requestRebase("a processed landed block waits for the rebase")
              .pipe(Effect.provideService(Globals, globals));
            expect(blocked).toBeUndefined();

            yield* Effect.gen(function* () {
              const applied = (yield* retrieveRows).map((row) => row.applied);
              const status = yield* owner.reconciliationStatus;
              const { ready } = yield* owner.frontier;
              if (applied.join() !== "true" || status !== undefined || !ready)
                return yield* Effect.fail(
                  new Error(
                    `rebase not settled: ${JSON.stringify({ applied, status, ready })}`,
                  ),
                );
            }).pipe(
              Effect.retry(Schedule.spaced("20 millis")),
              Effect.timeout("20 seconds"),
            );
            expect(prepared.length).toBeGreaterThan(0);
            expect(native.durableRoot).toBe(R1);
            expect(yield* Ref.get(globals.LANDED_BLOCK_REBASE_FAILURE)).toBe(
              undefined,
            );
            expect(yield* landedBlockRebaseDisposition).toBeUndefined();
            const sql = yield* SqlClient.SqlClient;
            const working = (yield* sql<{
              outref: Buffer;
            }>`SELECT outref FROM mempool_ledger`).map((row) =>
              hex(row.outref),
            );
            expect(working).toContain(hex(E1.outref));
            expect(working).not.toContain(hex(E0.outref));
            yield* Ref.set(globals.EVENT_HISTORY_OWNER, undefined);
            yield* owner.close;
          }),
        ),
      ),
    );
  } finally {
    transport.close();
  }
}, 120_000);
