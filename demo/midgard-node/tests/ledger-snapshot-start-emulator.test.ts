/**
 * A node that starts with an empty database and an empty native ledger
 * store, on an actual deployment on the emulator, through the production
 * lifecycle: it takes in the ledger at the landed queue's confirmed header
 * (the empty ledger while nothing was merged, a snapshot peer's ledger
 * after a merge), then folds the queue's blocks above that header as
 * foreign blocks and commits on the tail. An L1 rollback of the merge it
 * took its ledger at holds it until the same merge relands. A deposit
 * included above that header gets a row and is included by the node's next
 * block; one included at or below it is in the taken-in ledger and is
 * projected no second time.
 *
 * Each fresh start wipes the node's database and native store after the
 * previous generation stopped. The DA payloads of the blocks it must fold
 * are put back as the public DA transport would serve them: this fixture's
 * DA transport is not wired. The snapshot peer serves, through the node's
 * own `ledger-snapshot` handler, a ledger store filled from the previous
 * generation's `confirmed_ledger`.
 */
import "./helpers/follower-emulator-installed.js";

import { rm } from "node:fs/promises";

import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { SqlClient, type Statement } from "@effect/sql";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { createDaLibp2pRetainedPayloadRequestHandlers } from "../src/da/libp2p-producer.create-da-libp2p-retained-payload-request-handlers.js";
import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import * as SnapshotAnchor from "../src/database/snapshot-anchor.js";
import { CONFIRMED_LEDGER_BEHIND } from "../src/landed-blocks/holds.js";
import { Frontier } from "../src/landed-blocks/store.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import {
  LEDGER_SNAPSHOT_PENDING,
  type LedgerSnapshotPeerSource,
} from "../src/services/ledger-snapshot-start.js";
import {
  callRetainedPayloadHandler,
  DEPLOYMENT,
  parseManifestFixture,
} from "./da-payload-libp2p-producer.manifest-fixture.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  attestQueuedStateQueueHeader,
  Database,
  emulatorStateQueueSnapshot,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  mergeMaturityWindow,
  runCommitWorkerUntilSubmitted,
  runMergeUntilMerged,
  SDK,
} from "./deposit-flow-emulator-shared.js";
import {
  admitTransfer,
  buildDepositorTransfer,
  depositorL2Utxos,
} from "./helpers/emulator-l2-transfer.js";
import { emulatorState } from "./helpers/emulator-snapshot.js";
import {
  openProductionLifecycle,
  type ProductionLifecycle,
} from "./helpers/production-lifecycle.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

type Node = Pick<
  ProductionLifecycle,
  | "fixture"
  | "lucidService"
  | "globals"
  | "production"
  | "synchronize"
  | "command"
  | "applyLanded"
  | "unappliedHolds"
  | "landedHold"
  | "driveOnce"
  | "close"
>;

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

/** A ledger table's (outref, output) pairs, in outref order. */
const pairsOf = (table: "confirmed_ledger" | "mempool_ledger") =>
  Effect.flatMap(SqlClient.SqlClient, (sql) =>
    sql<{
      outref: Buffer;
      output: Buffer;
    }>`SELECT outref, output FROM ${sql(table)} ORDER BY outref`.pipe(
      Effect.map((rows) =>
        rows.map((row) => [hex(row.outref), hex(row.output)]),
      ),
    ),
  );

/** What the node holds of its ledger. */
const ledgerState = (node: Node) =>
  node.command(
    Effect.gen(function* () {
      return {
        confirmed: yield* pairsOf("confirmed_ledger"),
        mempool: yield* pairsOf("mempool_ledger"),
        frontier: yield* Frontier.retrieve,
        anchor: yield* SnapshotAnchor.retrieve,
      };
    }),
  );

/** The node's retained DA payload rows. */
const retainedPayloads = (node: Node) =>
  node.command(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) =>
        sql<Record<string, Statement.Primitive>>`SELECT * FROM da_payloads`,
    ),
  );

const runDatabase = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(effect));

/** The node's deposit rows of the deposit admitted by the L1 tx `txHash`. */
const depositRows = (node: Node, txHash: string) =>
  node.command(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{
        ledger_output: Buffer;
        projected_header_hash: Buffer | null;
      }>`SELECT ledger_output, projected_header_hash FROM deposits_utxos
        WHERE deposit_l1_tx_hash = ${Buffer.from(txHash, "hex")}`,
    ),
  );

it("a fresh node starts from the empty ledger before any merge and from a peer's ledger after one, folds the blocks above it and commits on the tail", async () => {
  const lifecycle = await openProductionLifecycle();
  const { fixture } = lifecycle;
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  const histories = SDK.requireEventHistoryContracts(fixture.contracts);
  const ledgerPath = lifecycle.production.nodeConfig.LEDGER_MPF_DB_PATH;
  let node: Node = lifecycle;
  const queue = () =>
    Effect.runPromise(
      emulatorStateQueueSnapshot(
        fixture.operatorLucid,
        fixture.contracts.stateQueue,
        "manual_status",
      ).pipe(Effect.provide(Database.layer)),
    );
  /** The deposits live on L1. */
  const liveDeposits = () =>
    Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        fixture.operatorLucid,
        SDK.eventHistoryDeploymentFromContracts(histories.deposit),
      ),
    );
  /** Admits a deposit of `lovelace`: its L1 tx hash and event id. */
  const deposit = async (lovelace: bigint) => {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService: node.lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
    await node.synchronize();
    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
        l2Address: address,
        l2Datum: null,
        lovelace,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
      }),
    );
    const signed = await built.tx.sign.withWallet().complete();
    const txHash = await signed.submit();
    expect(await wallet.awaitTx(txHash)).toBe(true);
    await node.synchronize();
    const admitted = await liveDeposits();
    const latest = admitted.reduce((at, utxo) =>
      utxo.facts.inclusion_time > at.facts.inclusion_time ? utxo : at,
    );
    await lifecycle.deployment.chain.awaitLedgerTime(
      Number(latest.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await node.synchronize();
    return { txHash, id: latest.idCbor.toString("hex") };
  };
  /** The node commits a block and takes its commit in. */
  const commit = async () => {
    const submitted = await runCommitWorkerUntilSubmitted({
      fixture,
      lucidService: node.lucidService,
      latestBlock: await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      ),
      nodeConfig: node.production.nodeConfig,
      production: { ...node.production, globals: node.globals },
    });
    expect(await fixture.operatorLucid.awaitTx(submitted.submittedTxHash)).toBe(
      true,
    );
    await node.applyLanded(submitted.submittedHeaderHash);
    return submitted.submittedHeaderHash;
  };
  /** The node attests and merges the queue's one block. */
  const merge = async () => {
    const before = await queue();
    expect(before.blockCount).toBe(1);
    await attestQueuedStateQueueHeader({
      fixture,
      lucidService: node.lucidService,
      globals: node.globals,
      headerHash: before.tailCommitBase.headerHash,
    });
    await advanceEmulatorPastUnixTime(
      fixture,
      mergeMaturityWindow(
        fixture.operatorLucid,
        before.tailCommitBase.blockEndTimeMs,
      ).readyAfterUnixTime,
    );
    vi.setSystemTime(fixture.emulator.now());
    await runMergeUntilMerged({
      fixture,
      lucidService: node.lucidService,
      globals: node.globals,
      production: node.production,
    });
  };
  /** Stops the node and starts it again with an empty database and native
   * store, and the DA payloads `payloads` as the DA transport serves them. */
  const startFresh = async (
    payloads: readonly Record<string, Statement.Primitive>[],
    ledgerSnapshotPeers?: LedgerSnapshotPeerSource,
  ) => {
    node = await lifecycle.restartRuntime({
      afterStop: async () => {
        await runDatabase(
          Effect.gen(function* () {
            yield* resetApplicationTables;
            const sql = yield* SqlClient.SqlClient;
            if (payloads.length > 0)
              yield* sql`INSERT INTO da_payloads ${sql.insert([...payloads])}`;
          }),
        );
        await rm(ledgerPath, { recursive: true, force: true });
      },
      ledgerSnapshotPeers,
    });
  };
  const ownStatus = (headerHash: string) =>
    node.command(
      PendingBlockFinalizationsDB.retrieveStatusByHeaderHash(
        Buffer.from(headerHash, "hex"),
      ),
    );

  try {
    // Block 1, committed and not merged.
    const belowH = await deposit(12_000_000n);
    await commit();
    const unmerged = await ledgerState(node);
    expect(unmerged.mempool).toHaveLength(1);
    const unmergedQueue = await queue();
    expect(unmergedQueue.blockCount).toBe(1);

    // A fresh node on the unmerged deployment starts from the empty ledger,
    // asking no peer, and folds block 1.
    await startFresh(await retainedPayloads(node));
    expect(await ledgerState(node)).toEqual({
      confirmed: [],
      mempool: unmerged.mempool,
      frontier: unmerged.frontier,
      anchor: {
        headerHash: unmergedQueue.root.headerHash,
        utxosRoot: EMPTY_LEDGER_ROOT,
        endTimeMs: expect.any(BigInt),
      },
    });

    // Block 1 merged, block 2 committed on it and not merged.
    const beforeMerge = emulatorState(fixture.emulator);
    await merge();
    const merged = await queue();
    expect(merged.blockCount).toBe(0);
    await deposit(13_000_000n);
    await commit();
    const tail = await ledgerState(node);
    expect(tail.confirmed).toHaveLength(1);
    expect(tail.mempool).toHaveLength(2);
    const withTail = emulatorState(fixture.emulator);

    // The snapshot peer holds the merged ledger, and does not serve it the
    // first time it is asked.
    const peerStore = createLedgerStore(createMemoryLedgerStorage());
    const peerLedger = await (
      await peerStore.at(EMPTY_LEDGER_ROOT)
    ).apply(
      tail.confirmed.map(([outRef, output]) => ({
        type: "insert" as const,
        outRef: Buffer.from(outRef!, "hex"),
        output: Buffer.from(output!, "hex"),
      })),
    );
    await peerStore.persist(peerLedger);
    const R = merged.tailCommitBase.roots.utxosRoot;
    expect(peerLedger.root).toBe(R);
    let asked = 0;
    // The frontier when the peer is asked again, after the run that held.
    let frontierWhenServed: unknown = "not asked again";
    const handlers = createDaLibp2pRetainedPayloadRequestHandlers({
      manifest: parseManifestFixture(),
      retrieveByHeaderHash: async () => undefined,
      ledgerEntries: async (utxosRoot, after) => {
        if (asked++ === 0) return undefined;
        if (after === undefined)
          frontierWhenServed = await Effect.runPromise(
            provideDatabaseLayers(Frontier.retrieve),
          );
        return (await peerStore.at(utxosRoot)).entries(
          after === undefined ? {} : { after },
        );
      },
    });
    await startFresh(await retainedPayloads(node), {
      deploymentFingerprint: DEPLOYMENT,
      peers: [
        {
          identity: "snapshot-peer",
          request: async (requestCbor: Buffer) =>
            callRetainedPayloadHandler({
              handlers,
              protocol: DaRequestResponseProtocol.ledgerSnapshot,
              request: requestCbor,
            }),
        },
      ],
      excluded: new Set(),
    });
    // While the snapshot was pending the node held only that: landed
    // processing did not run, and no frontier was set before the install.
    expect(node.unappliedHolds().map((hold) => hold.reason)).toEqual([
      LEDGER_SNAPSHOT_PENDING,
    ]);
    expect(frontierWhenServed).toBeUndefined();
    expect(await ledgerState(node)).toEqual({
      confirmed: tail.confirmed,
      mempool: tail.mempool,
      frontier: tail.frontier,
      anchor: {
        headerHash: merged.root.headerHash,
        utxosRoot: R,
        endTimeMs: BigInt(merged.tailCommitBase.blockEndTimeMs),
      },
    });

    // The block-1 deposit, included at or below H and still live on L1, is
    // in the taken-in ledger and has no row: it is projected no second time.
    expect(
      (await liveDeposits()).some(
        (utxo) => utxo.idCbor.toString("hex") === belowH.id,
      ),
    ).toBe(true);
    expect(await depositRows(node, belowH.txHash)).toEqual([]);
    const [belowHOutput] = unmerged.mempool;
    expect(
      (await ledgerState(node)).mempool.filter(
        ([outRef]) => outRef === belowHOutput![0],
      ),
    ).toEqual([belowHOutput]);

    // L1 rolls the anchor's merge back (and block 2 with it): the node holds
    // confirmed_ledger_behind until the same merge relands.
    const installed = await ledgerState(node);
    await lifecycle.rollBackTo(beforeMerge, { synchronize: false });
    const rolledBack = await node.driveOnce();
    expect(rolledBack.holds.map((hold) => hold.reason)).toContain(
      CONFIRMED_LEDGER_BEHIND,
    );
    await lifecycle.rollBackTo(withTail);
    expect(node.landedHold()).toBeUndefined();
    expect(await ledgerState(node)).toEqual(installed);

    // A deposit included above H gets a row and is projected into the
    // working ledger once; the fresh node commits block 3 on block 2, which
    // it folded, including it.
    const aboveH = await deposit(14_000_000n);
    const [aboveHRow, ...moreRows] = await depositRows(node, aboveH.txHash);
    expect(moreRows).toEqual([]);
    const third = await commit();
    expect(await ownStatus(third)).toBe(
      PendingBlockFinalizationsDB.OwnBlockStatus.Applied,
    );
    const afterThird = (await ledgerState(node)).mempool;
    expect(afterThird).toHaveLength(3);
    expect(
      afterThird.filter(
        ([outRef]) => !installed.mempool.some(([held]) => held === outRef),
      ),
    ).toEqual([[expect.any(String), hex(aboveHRow!.ledger_output)]]);
    expect(
      hex((await depositRows(node, aboveH.txHash))[0]!.projected_header_hash!),
    ).toBe(third);

    // The block-1 deposit's output is spendable exactly once.
    const [spendable, ...otherSpendable] = (
      await depositorL2Utxos(node)
    ).filter((utxo) => hex(utxo.outrefCbor) === belowHOutput![0]);
    expect(otherSpendable).toEqual([]);
    const spend = async (lovelace: bigint) =>
      admitTransfer(
        node,
        await buildDepositorTransfer(node, [spendable!], lovelace),
      );
    expect(await spend(5_000_000n)).toBe("accepted");
    expect(await spend(6_000_000n)).toBe("rejected");
  } finally {
    await node.close();
    vi.useRealTimers();
  }
});
