import { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  createWalletSeeder,
  type FactStore,
  followChain,
  type FollowerProjection,
  type FollowStatus,
  openSqliteFactStore,
  type OriginConfig,
  projectionStoreOptions,
  WALLET_SEED_PENDING,
  type WalletSeedStatus,
} from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  type EventProjectionConfig,
  EVENTS_TABLE,
} from "@al-ft/midgard-l1-follower/events";
import { L1FollowerProvider } from "@al-ft/midgard-l1-follower/provider";
import { getAddressDetails } from "@lucid-evolution/lucid";

import { eventRefusalDegradationsIn } from "./event-refusals.js";
import {
  watcherProjection,
  type WatcherProjectionDeployment,
  watcherUnitHistoryPolicies,
} from "./projection.js";
import {
  createWatcherProofRetention,
  type WatcherProofRetention,
} from "./proof-retention.js";
import { createFollowerRawReads } from "./raw-reads.js";
import {
  ledgerOutputsFromTransport,
  ledgerOutputsQueryFromTransport,
} from "./raw-reads.ledger.js";
import type { FollowerRawReads } from "./raw-reads.types.js";
import { WATCHER_PROOF_PIN_EVENTS_TABLE } from "./tables.js";
import {
  createTxInputsResolver,
  type WatcherL1Degradation,
} from "./tx-inputs.js";
import { readWatcherQueueView } from "./view.js";

/**
 * The watcher's L1 chain follower (ticket W1): one SQLite fact store with
 * the watcher projection, one node transport, the follow loop, and the
 * seed of the watcher's own wallets (prover and availability). Every L1 read the watcher makes goes through it.
 *
 * Liveness: nothing here throws after construction. A condition the
 * follower cannot clear on its own stops the loop with an intervention and
 * the process stays up; every reason the watcher is not ready is in
 * `readiness()` with a detail.
 */

/** The configured L1 origin is missing: the follower has no start point. */
export const L1_ORIGIN_NOT_CONFIGURED = "l1_origin_not_configured";

export type WatcherFollowerReadiness = Readonly<{
  reason: string;
  detail: string;
}>;

export type WatcherFollowerRuntime = Readonly<{
  store: FactStore;
  transport: L1NodeTransport;
  rawReads: FollowerRawReads;
  provider: L1FollowerProvider;
  /** The pins that hold open proof objectives' history past k (E1 ruling). */
  proofRetention: WatcherProofRetention;
  /** The latest follow status; null before the loop's first status. */
  status(): FollowStatus | null;
  /** Every reason the follower holds the watcher unready; empty when ready. */
  readiness(): Promise<readonly WatcherFollowerReadiness[]>;
  /** Named conditions for status and metrics that never fail readiness. */
  degradations(): Promise<readonly WatcherL1Degradation[]>;
  /** Hears every status change (a new block, a rewind, a wait). */
  onChange(listener: (status: FollowStatus) => void): () => void;
  /** Settles with the final status once the loop stops (abort or intervention). */
  done: Promise<FollowStatus | null>;
  close(): Promise<void>;
}>;

export type WatcherFollowerRuntimeInput = Readonly<{
  deployment: WatcherProjectionDeployment;
  /** The fact store's file (`:memory:` in tests). */
  storePath: string;
  /** The manifest's automaticRecoveryMaxDepth. */
  automaticRecoveryMaxDepth: number;
  /** Null when the operator configured no L1 origin. */
  origin: OriginConfig | null;
  node: Readonly<{
    binaryPath: string;
    socketPath: string;
    networkMagic: number;
    /** The watcher's `l1.requestTimeoutMs`: each node request's bound. */
    requestTimeoutMs: number;
  }>;
  /** The watcher's own wallets (bech32): tracked and seeded. */
  walletAddresses: readonly string[];
  /**
   * The deployment's deposit and withdrawal event lists: the shared event
   * projection (`@al-ft/midgard-l1-follower/events`) the user-event reads
   * use. Absent: the store records no event lists.
   */
  eventProjection?: EventProjectionConfig;
  log?: (line: string) => void;
  /** Test seam: a transport other than the node sidecar. */
  unsafeTransportForTest?: L1NodeTransport;
}>;

/** The `hubOracleOneShot` outref of the manifest (`txHash#index`). */
export const parseHubOracleOneShot = (
  outRef: string,
): OriginConfig["hubOracleOneShot"] => {
  const match = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u.exec(outRef);
  if (match === null)
    throw new Error(`hubOracleOneShot outref is malformed: ${outRef}`);
  return { txHash: Buffer.from(match[1]!, "hex"), index: Number(match[2]) };
};

/**
 * The store keeps every fact the deepest read needs: recovery reads sit
 * automaticRecoveryMaxDepth + 2 blocks below the tip (one block of slack
 * for the predecessor of the deepest boundary).
 */
export const watcherSecurityParameter = (
  automaticRecoveryMaxDepth: number,
): number => automaticRecoveryMaxDepth + 2;

/**
 * The shared event projection, with its events held past k while an open
 * proof objective reads them: a row of `WATCHER_PROOF_PIN_EVENTS_TABLE`
 * (written by `WatcherProofRetention.holdEvents`) pins the event's row. The
 * pin matches the key alone, so an event of the other kind with the same
 * key is held too: harmless over-retention.
 */
export const watcherEventProjection = (
  events: EventProjectionConfig,
): FollowerProjection => {
  const projection = eventProjection(events);
  return {
    ...projection,
    temporalTables: (projection.temporalTables ?? []).map((table) =>
      table.name === EVENTS_TABLE
        ? {
            ...table,
            pinnedBy: [
              ...(table.pinnedBy ?? []),
              {
                column: "event_key",
                table: WATCHER_PROOF_PIN_EVENTS_TABLE,
                tableColumn: "event_key",
              },
            ],
          }
        : table,
    ),
  };
};

/**
 * The projections the watcher's store runs: the state queue, and the
 * deposit and withdrawal event lists when the deployment names them.
 */
export const watcherFollowerProjections = (
  deployment: WatcherProjectionDeployment,
  events: EventProjectionConfig | undefined,
): FollowerProjection[] =>
  events === undefined
    ? [watcherProjection(deployment)]
    : [watcherProjection(deployment), watcherEventProjection(events)];

export const openWatcherFollowerRuntime = (
  input: WatcherFollowerRuntimeInput,
): WatcherFollowerRuntime => {
  const log = input.log ?? (() => undefined);
  const wallets = input.walletAddresses.map((address) =>
    Buffer.from(getAddressDetails(address).address.hex, "hex"),
  );
  const transport =
    input.unsafeTransportForTest ??
    new L1NodeTransport({
      binaryPath: input.node.binaryPath,
      socketPath: input.node.socketPath,
      networkMagic: input.node.networkMagic,
      requestTimeoutMs: input.node.requestTimeoutMs,
      onDiagnostic: (line) => log(`L1 node transport: ${line}`),
    });
  const store = openSqliteFactStore({
    ...projectionStoreOptions(
      watcherFollowerProjections(input.deployment, input.eventProjection),
      {
        securityParameter: watcherSecurityParameter(
          input.automaticRecoveryMaxDepth,
        ),
        trackedSet: {
          addresses: new Set(),
          paymentCredentials: new Set(),
          policies: new Set(),
        },
      },
      "sqlite",
    ),
    // Seeded (§5.3 step 4), never in the tracked-set record.
    wallets,
    path: input.storePath,
  });
  const unitHistoryPolicies = watcherUnitHistoryPolicies(input.deployment);
  const rawReads = createFollowerRawReads(store, {
    stateQueuePolicyId: input.deployment.stateQueueMint,
    unitHistoryPolicies,
    ledgerOutputsAt: ledgerOutputsFromTransport(transport),
  });
  const provider = new L1FollowerProvider({ store, transport });
  // The node's slot configuration, read once it answers (`l1_node_behind`).
  let slotConfig: ReturnType<L1FollowerProvider["slotConfig"]> | undefined;
  const slotTime = async (slot: number): Promise<number> => {
    slotConfig ??= provider.slotConfig().catch((error: unknown) => {
      slotConfig = undefined;
      throw error;
    });
    const { zeroTime, zeroSlot, slotLength } = await slotConfig;
    return zeroTime + (slot - zeroSlot) * slotLength;
  };
  const proofRetention = createWatcherProofRetention(store, {
    unitHistoryPolicies,
    stateQueuePolicyId: input.deployment.stateQueueMint,
  });
  // Resolves recorded txs' inputs at ingest, while the node still serves
  // the predecessor's ledger state (E1 ruling, facet 2).
  const txInputs = createTxInputsResolver({
    store,
    ledger: ledgerOutputsQueryFromTransport(transport),
    log: (line) => log(`L1 follower: ${line}`),
  });
  const seeder = createWalletSeeder({
    store,
    ledger: transport,
    wallets,
  });
  const listeners = new Set<(status: FollowStatus) => void>();
  const abort = new AbortController();
  let latest: FollowStatus | null = null;
  let seed: WalletSeedStatus = {
    kind: "pending",
    reason: "not_initialized",
    detail: "the follower has not applied a block yet",
    wallets,
  };
  let seeding: Promise<void> | null = null;
  let seedAgain = false;

  /** Seeds the owed wallet at the cursor; one step at a time, coalesced. */
  const stepSeed = (): void => {
    if (seeding !== null) {
      seedAgain = true;
      return;
    }
    if (seeder.ready()) {
      seed = { kind: "ready" };
      return;
    }
    seeding = (async () => {
      try {
        seed = await seeder.step();
      } catch (error) {
        seed = {
          kind: "pending",
          reason: "ledger_unavailable",
          detail: error instanceof Error ? error.message : String(error),
          wallets: seeder.owed(),
        };
      } finally {
        seeding = null;
      }
      if (seedAgain && !abort.signal.aborted) {
        seedAgain = false;
        stepSeed();
      }
    })();
  };
  // A rewind below the seed point owes the seed again (the seeder hears it).
  const unsubscribeRewind = store.onGeneration(() => {
    if (!seeder.ready()) stepSeed();
    txInputs.trigger();
  });

  const done: Promise<FollowStatus | null> =
    input.origin === null
      ? Promise.resolve(null)
      : followChain({
          store,
          transport,
          origin: input.origin,
          signal: abort.signal,
          log: (line) => log(`L1 follower: ${line}`),
          // `l1_node_behind` at the follower's default bound; it clears
          // when the node catches up.
          nodeBehind: { slotTime },
          onStatus: (status) => {
            latest = status;
            if (status.cursor !== null && !seeder.ready()) stepSeed();
            if (status.cursor !== null) txInputs.trigger();
            for (const listener of listeners) {
              try {
                listener(status);
              } catch (error) {
                log(
                  `L1 follower listener failed: ${error instanceof Error ? error.message : String(error)}`,
                );
              }
            }
          },
        }).catch((error: unknown) => {
          // followChain never throws; a throw here is a defect, kept visible.
          log(
            `L1 follower loop failed: ${error instanceof Error ? error.message : String(error)}`,
          );
          return latest;
        });

  const readiness = async (): Promise<readonly WatcherFollowerReadiness[]> => {
    if (input.origin === null)
      return [
        {
          reason: L1_ORIGIN_NOT_CONFIGURED,
          detail:
            "l1.origin is not set: the follower needs the point immediately before the prepareHubOracleNonce block",
        },
      ];
    const reasons: WatcherFollowerReadiness[] = [];
    if (latest === null)
      reasons.push({
        reason: "l1_follower_catching_up",
        detail: "the follower has not reported a status yet",
      });
    else
      for (const entry of latest.readiness)
        reasons.push({ reason: entry.reason, detail: entry.detail });
    if (seed.kind === "pending" && !seeder.ready())
      reasons.push({
        reason: WALLET_SEED_PENDING,
        detail: `${seed.reason}: ${seed.detail}`,
      });
    // A user event an open proof reads, pruned before a hold held it.
    reasons.push(...proofRetention.readiness());
    try {
      reasons.push(...(await txInputs.assess()).readiness);
      const cursor = await store.cursor();
      if (cursor !== null) {
        const view = await store.transaction("read", (tx) =>
          readWatcherQueueView(tx, cursor.point.slot),
        );
        if (!view.healthy)
          reasons.push({ reason: view.reason, detail: view.detail });
      }
    } catch (error) {
      reasons.push({
        reason: "l1_follower_store_unreadable",
        detail: error instanceof Error ? error.message : String(error),
      });
    }
    return reasons;
  };

  let closing: Promise<void> | null = null;
  return Object.freeze({
    store,
    transport,
    rawReads,
    provider,
    proofRetention,
    status: () => latest,
    readiness,
    degradations: async () => {
      const inputs = await txInputs.assess().then(
        ({ degradations }) => degradations,
        () => [],
      );
      // The refusals table exists only with the event projection; a failed
      // read of it never hides the other degradations.
      const refusals =
        input.eventProjection === undefined
          ? []
          : await store
              .transaction("read", eventRefusalDegradationsIn)
              .catch(() => []);
      return [...inputs, ...proofRetention.degradations(), ...refusals];
    },
    onChange: (listener: (status: FollowStatus) => void) => {
      listeners.add(listener);
      return () => listeners.delete(listener);
    },
    done,
    close: () => {
      closing ??= (async () => {
        abort.abort();
        await done.catch(() => undefined);
        await seeding?.catch(() => undefined);
        await txInputs.close();
        unsubscribeRewind();
        seeder.close();
        listeners.clear();
        await store.close().catch(() => undefined);
        await transport.close().catch(() => undefined);
      })();
      return closing;
    },
  });
};
