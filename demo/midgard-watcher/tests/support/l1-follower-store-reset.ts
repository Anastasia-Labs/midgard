/**
 * Shared fixture of the watcher's store-reset tests
 * (`tests/l1-follower/tracked-set-reset*.test.ts`): the simulated protocol
 * chain, a scripted node, and a decision driver over stub collaborators
 * that count what it asks of them.
 */

import type {
  ChainSyncEvent,
  ChainSyncStream,
  L1NodeTransport,
} from "@al-ft/l1-node-transport";
import {
  decodeBlock,
  type FactStore,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  encodeUtxoAnswer,
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  simUniverse,
  type SimUtxo,
} from "@al-ft/midgard-l1-follower/testing";
import { expect } from "vitest";

import { watcherSecurityParameter } from "../../src/l1-follower/follower-runtime.js";
import type { WatcherObservationAuthority } from "../../src/l1-follower/observation.js";
import { watcherProjection } from "../../src/l1-follower/projection.js";
import { createWatcherDecisionDriver } from "../../src/runtime/watcher-runtime.decision-driver.js";
import {
  commitTx,
  initTx,
  queueState,
  SIM_HUB_ORACLE_ONE_SHOT,
  SIM_WATCHER_DEPLOYMENT,
} from "./l1-follower-state-queue-traffic.js";

export const D = SIM_WATCHER_DEPLOYMENT;
export const RECOVERY_DEPTH = 4;
export const K = watcherSecurityParameter(RECOVERY_DEPTH);
export const RELEASE_DEPTH = 2;
export const SOURCE_ID = "tracked-set-reset";
export const ONE_SHOT: SimUtxo = {
  outRef: SIM_HUB_ORACLE_ONE_SHOT,
  output: { address: simUniverse().untrackedAddress, lovelace: 5_000_000n },
};
export const AUTHORITY: WatcherObservationAuthority = {
  authorityDigest: "a1".repeat(32),
  deploymentFingerprint: "a2".repeat(32),
  protocolScriptHashes: {
    hubOracleMint: D.hubOracleMint,
    stateQueueSpend: D.stateQueueSpend,
    stateQueueMint: D.stateQueueMint,
    correctionLockSpend: D.correctionLockSpend,
    fraudProofSpend: D.fraudProofSpend,
    fraudProofMint: D.fraudProofMint,
    referenceScriptAuthMint: "b1".repeat(28),
    availabilityChallengeSpend: D.availabilityChallengeSpend,
    availabilityChallengeMint: D.availabilityChallengeMint,
    daBondPoolSpend: D.daBondPoolSpend,
    daBondPoolMint: "b2".repeat(28),
    daAttestationMint: D.daAttestationMint,
    availabilityChallengeOpenWithdraw: "b3".repeat(28),
    availabilityChallengeSettleWithdraw: "b4".repeat(28),
    availabilityChallengeCloseWithdraw: "b5".repeat(28),
    availabilityChallengeTimeoutWithdraw: "b6".repeat(28),
  },
};

export const WALLET =
  "addr_test1qzzmyy6upfeafmd3gpm50mqqyd3atfhwep6wj27cwsgvrrde3c53zgrk73cgkyjt85cy0ngflze0gc3pnajj3qzws5cqj0qg49";

export const until = async (
  what: string,
  holds: () => boolean,
  ms = 20_000,
) => {
  const deadline = Date.now() + ms;
  while (!holds()) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
};

export const openStore = (path: string) =>
  openSqliteFactStore({
    ...simStoreOptions([watcherProjection(D)], K, "sqlite"),
    path,
  });

/** A store at the start that drops its tracked-set record: the next start resets it. */
export const dropRecord = (store: FactStore) =>
  store.transaction("write", (tx) =>
    tx.query("DELETE FROM l1_follower_tracked_set"),
  );

/** The protocol init, then `blocks` blocks, a header commit in the first. */
export const chainEvents = (blocks: number) => {
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const events: ChainSyncEvent[] = [chain.forward([initTx(D)]).event];
  for (let i = 0; i < blocks; i += 1) {
    const state = i === 0 ? queueState(chain, D) : null;
    events.push(
      chain.forward(state === null ? [] : [commitTx(state, D)]).event,
    );
  }
  return events;
};

export const applyAll = async (
  store: FactStore,
  events: readonly ChainSyncEvent[],
) => {
  for (const event of events) {
    if (event.kind !== "roll_forward") throw new Error("forward events only");
    expect((await store.applyBlock(decodeBlock(event.block))).kind).toBe(
      "applied",
    );
  }
};

/** Stub bridge and availability counting what the driver asks of them. */
export const collaborators = () => {
  const seen = {
    bridgeInvalidations: 0,
    availabilityInvalidations: 0,
    recoveryPreparations: 0,
    dispatched: [] as string[],
  };
  const bridge = {
    prepareForRecovery: (observation: { observationDigest: string }) => {
      seen.recoveryPreparations += 1;
      return Promise.resolve({
        observationDigest: observation.observationDigest,
        decisionDigests: [],
        target: null,
      });
    },
    recoverExisting: () => Promise.resolve(0),
    reconcileAndDispatch: (observation: { observationDigest: string }) => {
      seen.dispatched.push(observation.observationDigest);
      return Promise.resolve({
        observationDigest: observation.observationDigest,
        decisionDigests: [],
        target: null,
      });
    },
    invalidateForRollback: () => {
      seen.bridgeInvalidations += 1;
    },
  };
  const availability = {
    reconcile: () => Promise.resolve(),
    invalidateForRollback: () => {
      seen.availabilityInvalidations += 1;
    },
  };
  return { seen, bridge, availability };
};

export type Collaborators = ReturnType<typeof collaborators>;

/** A driver over `store` and the stub collaborators; `rewound` hears `onRewind`. */
export const driverOver = (
  store: FactStore,
  c: Collaborators,
  rewound: number[],
) =>
  createWatcherDecisionDriver(
    {
      store,
      onFollowerChange: () => () => undefined,
      authority: AUTHORITY,
      sourceId: SOURCE_ID,
      releaseDepth: RELEASE_DEPTH,
      bridge: c.bridge as never,
      availability: c.availability as never,
      onRewind: (generation) => rewound.push(generation),
      retryDelayMs: 10,
    },
    { atTip: () => true, started: () => true },
  );

/** One pass to a decision, then the driver closes (the process stops). */
export const decideOnce = async (
  store: FactStore,
  c: Collaborators,
  rewound: number[],
) => {
  const driver = driverOver(store, c, rewound);
  const preparations = c.seen.recoveryPreparations;
  driver.wake();
  await until(
    "a decision",
    () =>
      c.seen.recoveryPreparations > preparations &&
      driver.readiness().length === 0,
  );
  await driver.idle();
  const status = driver.status();
  await driver.close();
  return status;
};

/** A node that serves `events` from `state.acked` on; any ledger read answers the one-shot. */
export const scriptedTransport = (
  events: readonly ChainSyncEvent[],
  state: { acked: number },
): L1NodeTransport =>
  ({
    openChainSync: (): ChainSyncStream => {
      let position = state.acked;
      let closed = false;
      return {
        opened: Promise.resolve(),
        next: async () => {
          for (;;) {
            if (closed) return undefined;
            if (position < events.length) return events[(position += 1) - 1];
            await new Promise((resolve) => setTimeout(resolve, 2));
          }
        },
        ack: (seq: bigint) => {
          const index = events.findIndex((event) => event.seq === seq);
          if (index >= 0) state.acked = Math.max(state.acked, index + 1);
        },
        close: () => {
          closed = true;
          return Promise.resolve();
        },
      } as unknown as ChainSyncStream;
    },
    withLedgerState: (
      _at: unknown,
      use: (session: { query: () => Promise<Uint8Array> }) => unknown,
    ) => use({ query: () => Promise.resolve(encodeUtxoAnswer([ONE_SHOT])) }),
    // The scripted node is always reachable: no readiness change to report.
    readiness: { ready: true, nodeToClientVersion: 32784 },
    onReadiness: (): (() => void) => () => undefined,
    close: () => Promise.resolve(),
  }) as unknown as L1NodeTransport;
