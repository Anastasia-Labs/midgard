/**
 * The watcher's follower runtime over a node whose ledger side hangs. The
 * wallet seed holds the store's writer lane on a ledger read with no request
 * deadline, so a block that arrives meanwhile waits behind it and the
 * follow loop is parked in that block's write:
 *
 * - readiness answers at once and names the seed read in flight, with its
 *   point, and the node-behind check's lag as unknown (its slot
 *   configuration read is queued on the same node);
 * - a shutdown completes without waiting for the read: closing the node
 *   connection ends it, so the block's write and the loop can finish.
 *
 * The node is a fake at the transport boundary: ledger states serialize
 * like the real transport's, no ledger query is answered, and a closed
 * transport fails every pending read, as a sidecar exit does.
 */
import type {
  ChainSyncEvent,
  ChainSyncStream,
  L1NodeTransport,
} from "@al-ft/l1-node-transport";
import { SidecarExitedError } from "@al-ft/l1-node-transport";
import { fakeL1NodeTransport } from "@al-ft/l1-node-transport/testing";
import {
  FOLLOWER_NODE_BEHIND,
  WALLET_SEED_PENDING,
} from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { afterEach, describe, expect, it } from "vitest";

import { openWatcherFollowerRuntime } from "../../src/l1-follower/follower-runtime.js";
import { SIM_HUB_ORACLE_ONE_SHOT } from "../support/l1-follower-state-queue-traffic.js";
import {
  chainEvents,
  D,
  RECOVERY_DEPTH,
  until,
  WALLET,
} from "../support/l1-follower-store-reset.js";

const opened: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of opened.splice(0).reverse()) await close();
});

/** Settles with `promise`, or with "still waiting" after `ms` (a test-side bound only). */
const within = <T>(ms: number, promise: Promise<T>) =>
  Promise.race([
    promise,
    new Promise<"still waiting">((resolve) => {
      setTimeout(() => resolve("still waiting"), ms).unref();
    }),
  ]);

/**
 * A node that serves chain-sync but never answers a ledger query until it
 * is closed (a hung ledger side). It serves the first block at once and the
 * rest once `deliverRest` is called.
 */
const hungLedgerNode = (events: readonly ChainSyncEvent[]) => {
  const seen = { queries: 0, delivered: 0 };
  let rest = false;
  let closed = false;
  const pending = new Set<(error: Error) => void>();
  const closedError = () =>
    new SidecarExitedError({
      code: 0,
      signal: null,
      fatal: null,
      diagnostics: "",
    });
  const query = (): Promise<Uint8Array> => {
    if (closed) return Promise.reject(closedError());
    seen.queries += 1;
    return new Promise((_, reject) => pending.add(reject));
  };
  let ledgerLock: Promise<unknown> = Promise.resolve();
  const transport: L1NodeTransport = fakeL1NodeTransport({
    openChainSync: (): ChainSyncStream => {
      let position = 0;
      let streamClosed = false;
      return {
        opened: Promise.resolve(),
        next: async () => {
          for (;;) {
            if (streamClosed || closed) return undefined;
            if (position < (rest ? events.length : 1)) {
              seen.delivered += 1;
              return events[(position += 1) - 1];
            }
            await new Promise((resolve) => setTimeout(resolve, 2));
          }
        },
        ack: () => undefined,
        close: () => {
          streamClosed = true;
          return Promise.resolve();
        },
      } as unknown as ChainSyncStream;
    },
    // One ledger state at a time, as the real transport holds its lock.
    withLedgerState: (_at, use) => {
      const run = ledgerLock.then(() =>
        closed ? Promise.reject(closedError()) : use({ query }),
      );
      ledgerLock = run.catch(() => undefined);
      return run;
    },
    readiness: { ready: true, nodeToClientVersion: 32784 },
    onReadiness: () => () => undefined,
    // As a sidecar exit: every pending and later ledger read fails.
    close: () => {
      closed = true;
      for (const reject of pending) reject(closedError());
      pending.clear();
      return Promise.resolve();
    },
  });
  return {
    transport,
    seen,
    deliverRest: () => {
      rest = true;
    },
  };
};

type Reasons = readonly Readonly<{ reason: string; detail: string }>[];
const detailOf = (reasons: Reasons, reason: string) =>
  reasons.find((entry) => entry.reason === reason)?.detail;

describe("the watcher's follower runtime with a hung ledger side", () => {
  it("names the seed read in flight and the unknown node lag, and shuts down without waiting for the read, while a block waits behind it", async () => {
    // Every event carries the final tip, as a lagging follower sees it: the
    // first block is not at the tip, so the loop reads the next one.
    const chain = chainEvents(3);
    const events = chain.map((event) => ({
      ...event,
      tip: chain[chain.length - 1]!.tip,
    }));
    const node = hungLedgerNode(events);
    const follower = openWatcherFollowerRuntime({
      deployment: D,
      storePath: ":memory:",
      automaticRecoveryMaxDepth: RECOVERY_DEPTH,
      origin: {
        origin: SIM_ORIGIN.point,
        hubOracleOneShot: SIM_HUB_ORACLE_ONE_SHOT,
      },
      node: {
        binaryPath: "unused",
        socketPath: "unused",
        networkMagic: 42,
        requestTimeoutMs: 1_000,
      },
      walletAddresses: [WALLET],
      unsafeTransportForTest: node.transport,
    });
    let closing: Promise<void> | undefined;
    opened.push(async () => {
      closing ??= follower.close();
      // A shutdown that waited for the read is released here, so a failing
      // run still ends.
      await node.transport.close();
      await closing;
    });

    // The first block gives the store a cursor; the seed then waits on the
    // node, holding the store's writer lane.
    let reasons: Reasons = [];
    const deadline = Date.now() + 20_000;
    while (
      !(detailOf(reasons, WALLET_SEED_PENDING) ?? "").startsWith("in_flight")
    ) {
      if (Date.now() > deadline)
        throw new Error(
          `no seed in flight; readiness ${JSON.stringify(reasons)}`,
        );
      await new Promise((resolve) => setTimeout(resolve, 5));
      reasons = await follower.readiness();
    }
    const cursor = (await follower.store.cursor())!.point;
    expect(detailOf(reasons, WALLET_SEED_PENDING)).toContain(
      `in_flight: seed read in flight at slot ${cursor.slot} (block ${cursor.hash.toString("hex")}) since `,
    );

    // The next block arrives: its write waits behind the seed, and the
    // loop, parked in it, reads no further block.
    node.deliverRest();
    await until(
      "a block delivered behind the seed",
      () => node.seen.delivered === 2,
    );
    expect(follower.status()?.events).toBe(1);

    // Readiness answers at once, and the node-behind check, whose slot
    // configuration read is queued on the hung node, says the lag is
    // unknown instead of waiting for it.
    const now = await within(2_000, follower.readiness());
    expect(now).not.toBe("still waiting");
    expect(detailOf(now as Reasons, FOLLOWER_NODE_BEHIND)).toContain(
      "lag behind wall-clock time unknown",
    );
    expect(detailOf(now as Reasons, WALLET_SEED_PENDING)).toContain(
      "in_flight",
    );

    closing = follower.close();
    expect(
      await within(
        2_000,
        closing.then(() => "closed"),
      ),
    ).toBe("closed");
  });
});
