import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_ID,
} from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";
import { beforeEach, expect, it } from "vitest";

import * as Deposits from "../../src/database/deposits.js";
import * as Authority from "../../src/database/eventHistoryAuthority.js";
import * as Journal from "../../src/database/eventHistoryJournal.js";
import * as ForeignTips from "../../src/database/foreignTipReconciliations.js";
import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import * as Withdrawals from "../../src/database/withdrawals.js";
import { retentionSweepAction } from "../../src/fibers/retention-sweeper.js";
import { historyIncarnationEntry } from "../../src/l1-event-history-entries.js";
import type { EventHistorySourceBinding } from "../../src/l1-event-history-source.js";
import { historyOwnerCoverage } from "../../src/services/event-history-owner.coverage.js";
import { HistoryProducer } from "../../src/services/event-history-producer.js";
import { Globals } from "../../src/services/globals.js";
import {
  ContractDeploymentIdentity,
  NodeConfig,
} from "../../src/services/index.js";
import { registerForeignTipHorizonTests } from "./foreign-tip-retention-horizon.js";
import { registerForeignTipPagingTests } from "./foreign-tip-retention-paging.js";
import { retainEverything } from "./history-journal-retention.js";

export const registerForeignTipRetentionTests = (input: {
  start: () => Promise<{
    token: Authority.Token;
    checkpoint: Journal.Checkpoint;
  }>;
  run: <A, E>(work: Effect.Effect<A, E, SqlClient.SqlClient>) => Promise<A>;
  binding: () => EventHistorySourceBinding;
  prepare: (
    checkpoint: Journal.Checkpoint,
    n: number,
  ) => Promise<ReturnType<typeof Journal.prepareAppend>>;
  admit: (
    checkpoint: Journal.Checkpoint,
    n: number,
    kind: "deposit" | "withdrawal",
  ) => Promise<ReturnType<typeof Journal.prepareAppend>>;
}) => {
  const { start, run, prepare, admit } = input;
  const read = async () => {
    const value = await run(Journal.load(binding));
    if (value === null) throw new Error("Missing test checkpoint");
    return value;
  };
  let binding: EventHistorySourceBinding;
  beforeEach(() => {
    binding = input.binding();
  });
  const hash = (n: number) => n.toString(16).padStart(64, "0");
  const foreignRetentionFixture = async (
    lateKind?: "deposit" | "withdrawal",
  ) => {
    const started = await start();
    const token = started.token;
    let checkpoint = started.checkpoint;
    await run(ForeignTips.clear);
    if (lateKind !== undefined) {
      await run(
        Authority.withRecovery(
          token,
          Journal.append(
            binding,
            await admit(checkpoint, 2, lateKind),
            ({ after }) =>
              Effect.gen(function* () {
                const converted = yield* historyIncarnationEntry(
                  after.incarnations[0]!,
                  "Preprod",
                );
                if (converted.kind === "deposit")
                  yield* Deposits.insertEntries([converted.entry]);
                else yield* Withdrawals.insertEntries([converted.entry]);
              }),
            retainEverything,
          ),
        ),
      );
      checkpoint = await read();
      for (const n of [3, 4]) {
        const prepared = await prepare(checkpoint, n);
        await run(
          Authority.withRecovery(
            token,
            Journal.append(binding, prepared, () => Effect.void, {
              tipHeight: prepared.block.point.height,
              horizon: 0,
              holdSlot: undefined,
            }),
          ),
        );
        checkpoint = await read();
      }
    }
    await run(
      Authority.publishReady(token, {
        point: checkpoint.head,
        snapshotDigest: checkpoint.capture.snapshotDigest,
      }),
    );
    const header: SDK.Header = {
      prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
      validationTraceCount: 0n,
      startTime: 1n,
      endTime: 2n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: "21".repeat(28),
      operatorVkey: "22".repeat(28),
      protocolVersion: 1n,
    };

    const putEffect = (
      n: number,
      options: {
        resolved?: boolean;
        manifest?: string;
        start?: number;
        end?: number;
        header?: Partial<SDK.Header>;
      } = {},
    ) =>
      Effect.gen(function* () {
        const selected = {
          ...header,
          ...options.header,
          operatorVkey: n.toString(16).padStart(56, "0"),
          startTime: BigInt(options.start ?? 1),
          endTime: BigInt(options.end ?? 2),
        };
        const foreignHeaderHash = yield* SDK.hashBlockHeader(selected);
        const marker = makeDeploymentMarker(
          options.manifest ?? binding.manifestId,
        );
        yield* ForeignTips.recordMismatch({
          foreignHeaderHash,
          replacedBaseHeaderHash: "23".repeat(28),
          foreignHeader: selected,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          deploymentMarker: marker,
        });
        if (options.resolved !== false)
          yield* ForeignTips.markResolved({
            foreignHeaderHash,
            deploymentMarker: marker,
            evidence: { kind: ForeignTips.EvidenceKind.VerifiedEmpty },
          });
        return foreignHeaderHash;
      });
    const put = (n: number, options?: Parameters<typeof putEffect>[1]) =>
      run(putEffect(n, options));
    const coverage = historyOwnerCoverage(checkpoint, (slot) => slot * 10_000);
    const sweep = async (
      options: {
        view?:
          | "missing"
          | { confirmedHeadHash: Buffer; liveQueueHeaderHashes: Buffer[] };
        coverage?: typeof coverage | "missing";
        cutoff?: number;
        /** The node's tx-order ingestion watermark; the sweep time by
         * default, `"unset"` before any tx-order reconcile succeeded. */
        txOrdersIngestedThrough?: number | "unset";
        /** A node whose contract bundle carries no deployment manifest. */
        manifestless?: boolean;
      } = {},
    ) => {
      const sweptAt = new Date(
        MIDGARD_RETENTION_WINDOW.requiredRetentionMs +
          (options.cutoff ?? 2_000_000),
      );
      // History ingested past the horizon unless a test says otherwise, so
      // only the pins under test keep a row.
      const selectedCoverage =
        options.coverage === "missing"
          ? { ...coverage, retention: undefined }
          : (options.coverage ?? {
              ...coverage,
              includedThroughMs:
                MIDGARD_RETENTION_WINDOW.requiredRetentionMs + 1_000_001,
            });
      const globals = {
        TX_ORDERS_INGESTED_THROUGH_MS: await Effect.runPromise(
          Ref.make<number | undefined>(
            options.txOrdersIngestedThrough === "unset"
              ? undefined
              : (options.txOrdersIngestedThrough ?? sweptAt.getTime()),
          ),
        ),
        EVENT_HISTORY_OWNER: await Effect.runPromise(
          Ref.make({
            runProducer: (
              work: (
                value: Authority.Token,
                assert: Effect.Effect<void>,
                prefix: typeof selectedCoverage,
              ) => Effect.Effect<unknown, unknown, SqlClient.SqlClient>,
            ) => work(token, Effect.void, selectedCoverage),
          }),
        ),
      };
      await run(
        retentionSweepAction(
          options.view === "missing"
            ? undefined
            : (options.view ?? {
                confirmedHeadHash: Buffer.alloc(28, 1),
                liveQueueHeaderHashes: [],
              }),
          sweptAt,
        ).pipe(
          Effect.provideService(NodeConfig, { RETENTION_DAYS: 0 } as never),
          Effect.provideService(ContractDeploymentIdentity, {
            manifestId: options.manifestless ? undefined : binding.manifestId,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          } as never),
          Effect.provideService(Globals, globals as never),
        ),
      );
    };
    const exists = async (key: string) =>
      Option.isSome(await run(ForeignTips.retrieveByForeignHeaderHash(key)));
    return {
      put,
      putEffect,
      sweep,
      exists,
      token,
      checkpoint,
      coverage,
      withPermit: <A, E>(work: Effect.Effect<A, E, SqlClient.SqlClient>) =>
        work.pipe(Effect.provideService(HistoryProducer, { token, coverage })),
    };
  };

  it("prunes resolved foreign evidence only beyond the authenticated rollback and retention windows", async () => {
    const f = await foreignRetentionFixture();
    const key = await f.put(1);
    await f.sweep();
    expect(
      await f.exists(key),
      "resolved foreign evidence outside both authenticated windows should be removed",
    ).toBe(false);
  });

  it("prunes resolved evidence of every deployment of the profile on a node without a manifest", async () => {
    const f = await foreignRetentionFixture();
    const own = await f.put(1);
    const other = await f.put(2, { manifest: hash(999) });
    await f.sweep({ manifestless: true });
    expect(await f.exists(own)).toBe(false);
    expect(await f.exists(other)).toBe(false);
  });

  // Awaiting and other-deployment rows share the ingestion cutoff with
  // resolved rows; `registerForeignTipHorizonTests` covers what else keeps
  // them.
  it.each([
    "confirmed head",
    "live queue",
    "retention equality",
    "rollback equality",
    "missing view",
    "missing coverage",
    "stale anchor",
    "recovery plan",
  ] as const)("retains foreign evidence pinned by %s", async (pin) => {
    const f = await foreignRetentionFixture();
    const key = await f.put(1, {
      end:
        pin === "rollback equality"
          ? f.coverage.retention.includedThroughMs
          : 2,
    });
    if (pin === "recovery plan")
      await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`INSERT INTO event_history_recovery_plans (recovery_id, binding_digest, manifest_id, header_hash,
        intent, evidence_digest, checkpoint_revision, head_hash, snapshot_digest, owner_generation, state)
        VALUES (${Buffer.from(hash(980), "hex")}, ${Buffer.from(binding.digest, "hex")}, ${Buffer.from(binding.manifestId, "hex")},
          ${Buffer.from(key, "hex")}, 'test-foreign-retention-pin', ${Buffer.from(hash(981), "hex")}, ${f.checkpoint.revision},
          ${Buffer.from(f.checkpoint.head.id, "hex")}, ${Buffer.from(f.checkpoint.capture.snapshotDigest, "hex")}, ${f.token.generation}, 'applied')`;
        }),
      );
    await f.sweep({
      view:
        pin === "missing view"
          ? "missing"
          : pin === "confirmed head"
            ? {
                confirmedHeadHash: Buffer.from(key, "hex"),
                liveQueueHeaderHashes: [],
              }
            : pin === "live queue"
              ? {
                  confirmedHeadHash: Buffer.alloc(28, 1),
                  liveQueueHeaderHashes: [Buffer.from(key, "hex")],
                }
              : undefined,
      cutoff: pin === "retention equality" ? 2 : undefined,
      coverage:
        pin === "missing coverage"
          ? "missing"
          : pin === "stale anchor"
            ? {
                ...f.coverage,
                retention: {
                  ...f.coverage.retention,
                  anchor: { ...f.coverage.retention.anchor, id: hash(980) },
                },
              }
            : undefined,
    });
    expect(await f.exists(key)).toBe(true);
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM event_history_recovery_plans WHERE recovery_id = ${Buffer.from(hash(980), "hex")}`;
      }),
    );
  });

  it("retains resolved foreign evidence for late indexed and unfinalized deposit events", async () => {
    const f = await foreignRetentionFixture("deposit");
    const key = await f.put(1, { start: 999_999, end: 1_000_000 });
    await f.sweep();
    expect(await f.exists(key)).toBe(true);
    // The foreign consumer may have projected the late event, but local header
    // finalization still needs that evidence. Consumed is the terminal deposit state.
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE deposits_utxos SET status = 'projected', projected_header_hash = ${Buffer.from(key, "hex")}`;
      }),
    );
    await f.sweep();
    expect(await f.exists(key)).toBe(true);
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE deposits_utxos SET status = 'consumed'`;
      }),
    );
    await f.sweep();
    expect(await f.exists(key)).toBe(false);
  });

  registerForeignTipPagingTests(
    foreignRetentionFixture,
    run,
    () => binding.manifestId,
  );
  registerForeignTipHorizonTests(foreignRetentionFixture, run, hash);

  it("retains foreign evidence while a pending journal depends on its base tail", async () => {
    const f = await foreignRetentionFixture();
    const key = await f.put(1);
    const journalHash = Buffer.alloc(28, 0x91);
    const roots = {
      utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    };
    await run(
      f.withPermit(
        Pending.preparePendingSubmission({
          headerHash: journalHash,
          headerCbor: Buffer.from("d87980", "hex"),
          preparedTxHash: Buffer.alloc(32, 0x92),
          blockEndTime: new Date(3),
          metadata: {
            deploymentMarker: makeDeploymentMarker(binding.manifestId),
            consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
            stateQueueLeaseToken: "foreign-retention-test",
            baseSnapshotId: "foreign-retention-test",
            baseTailOutRef: "foreign-retention-test#0",
            baseTailHeaderHash: Buffer.from(key, "hex"),
            baseTailDatumCbor: "d87980",
            baseRoots: roots,
            blockStartTime: new Date(2),
            expectedRoots: {
              ...roots,
              transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            },
            expectedCounts: {
              withdrawalCount: 0n,
              forcedTransactionCount: 0n,
              l2TransactionCount: 0n,
              depositCount: 0n,
              totalEventCount: 0n,
              transitionStepCount: 0n,
              validationTraceCount: 0n,
            },
          },
          depositEventIds: [],
          depositEntries: [],
          forcedTransactionEventIds: [],
          forcedTransactionEntries: [],
          withdrawalEventIds: [],
          withdrawalEntries: [],
          mempoolTxIds: [],
          mempoolTxs: [],
          mempoolTxSourceTable: "none",
          transitionTraceMembers: [],
          eventToStepMembers: [],
          validationTraceMembers: [],
          validationTraceWitnessMembers: [],
          ledgerDelta: { spent: [], produced: [] },
        }),
      ),
    );
    try {
      await f.sweep();
      expect(await f.exists(key)).toBe(true);
      await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE pending_block_finalizations SET status = 'finalized' WHERE header_hash = ${journalHash}`;
        }),
      );
      await f.sweep();
      expect(await f.exists(key)).toBe(false);
    } finally {
      await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`DELETE FROM pending_block_finalizations WHERE header_hash = ${journalHash}`;
        }),
      );
    }
  });

  it("retains foreign evidence for invalid withdrawals still needing finalization", async () => {
    const f = await foreignRetentionFixture("withdrawal");
    const key = await f.put(1, { start: 999_999, end: 1_000_000 });
    await f.sweep();
    expect(await f.exists(key)).toBe(true);
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE withdrawal_utxos SET status = 'projected', settlement_event_info = raw_event_info,
      validity = 'IncorrectWithdrawalSignature', projected_header_hash = ${Buffer.from(key, "hex")}`;
      }),
    );
    await f.sweep();
    expect(await f.exists(key)).toBe(true);
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE withdrawal_utxos SET status = 'finalized'`;
      }),
    );
    await f.sweep();
    expect(await f.exists(key)).toBe(false);
  });
};
