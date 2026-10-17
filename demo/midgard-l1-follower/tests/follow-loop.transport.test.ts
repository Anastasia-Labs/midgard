import { mkdtemp, realpath, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  type ChainSyncEvent,
  type ChainSyncStream,
  L1NodeTransport,
} from "@al-ft/l1-node-transport";
import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  type FactStore,
  FOLLOW_CREDIT_POLICY,
  followChain,
  FOLLOWER_NODE_UNAVAILABLE,
  FOLLOWER_WAITING,
  type FollowStatus,
  intersectionPoints,
  openPostgresFactStore,
  openSqliteFactStore,
  transportPoint,
} from "../src/index.js";
import {
  buildForkSteps,
  forkCorpus,
  simStoreOptions,
} from "../src/testing/index.js";
import { type FakeBlock, fakeChains, rawBlock } from "./support/fake-chain.js";
import {
  appliedAll,
  follow,
  script,
  simOrigin,
} from "./support/follow-loop.js";
import { FIXTURE_PROJECTION, SIM_K } from "./support/fork-sim.js";
import { testDatabases } from "./support/postgres.js";

const databases = testDatabases();
const fakes = fakeChains();

afterEach(async () => {
  await fakes.cleanup();
});
afterAll(async () => {
  await databases.dropAll();
});

const pause = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

const stores: Readonly<
  Record<"sqlite" | "postgres", () => Promise<FactStore>>
> = {
  sqlite: () =>
    Promise.resolve(
      openSqliteFactStore({
        ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "sqlite"),
        path: ":memory:",
      }),
    ),
  postgres: async () =>
    openPostgresFactStore({
      ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "postgres"),
      connection: { connectionString: (await databases.create()).url },
    }),
};

/** The roll-forwards among `events`, as the fake chain handler serves them. */
const fakeBlocks = (events: readonly ChainSyncEvent[]): FakeBlock[] => {
  const blocks: FakeBlock[] = [];
  for (const event of events)
    if (event.kind === "roll_forward" && event.point.kind === "point")
      blocks.push({
        slot: Number(event.point.slot),
        hash: event.point.hash,
        blockNo: Number(event.blockNo),
        prevHash: event.prevHash,
        blockType: event.blockType,
        block: Buffer.from(event.block).toString("hex"),
      });
  return blocks;
};

const simBase = () => {
  const origin = simOrigin().origin;
  return { slot: origin.slot, hash: origin.hash.toString("hex") };
};

/**
 * A simulated chain with a rollback deeper than one block: the events up to
 * the rollback (the old branch) and the node's chain after it (the old
 * branch's surviving prefix, then the new branch).
 */
const orphanedCursorChain = () => {
  for (const entry of forkCorpus(SIM_K)) {
    const events = buildForkSteps(entry.scenario, [
      FIXTURE_PROJECTION,
    ]).steps.map((step) => step.event);
    const r = events.findIndex((event) => event.kind === "roll_backward");
    if (r <= 1 || !events.slice(r + 1).some((e) => e.kind === "roll_forward"))
      continue;
    const rollback = events[r]!;
    const prefix = events.slice(0, r);
    const kept = prefix.findIndex(
      (event) =>
        event.point.kind === "point" &&
        rollback.point.kind === "point" &&
        event.point.hash === rollback.point.hash,
    );
    if (kept >= prefix.length - 1) continue;
    const branch: ChainSyncEvent[] = [];
    for (const event of events.slice(r + 1)) {
      if (event.kind !== "roll_forward") break;
      branch.push(event);
    }
    return {
      prefix,
      node: fakeBlocks([...prefix.slice(0, kept + 1), ...branch]),
    };
  }
  throw new Error("no corpus scenario rolls back below the cursor");
};

describe.each(["sqlite", "postgres"] as const)(
  "followChain over the transport (%s)",
  (dialect) => {
    it("resumes from a cursor whose block left the chain: rewinds and follows the node's chain", async () => {
      const { prefix, node } = orphanedCursorChain();
      const store = await stores[dialect]();
      try {
        // The follower applied the old branch, then stopped.
        const old = script(prefix);
        await follow({ store, script: old, until: appliedAll(old) });
        const cursor = (await store.cursor())!;
        const points = await intersectionPoints(store);
        // The consumer's position is the first point it asks for.
        expect(points[0]).toEqual(
          transportPoint({
            slot: cursor.point.slot,
            hash: cursor.point.hash,
          }),
        );
        expect(
          node.some(
            (block) => block.hash === cursor.point.hash.toString("hex"),
          ),
        ).toBe(false);

        const transport = await fakes.transport(simBase(), node);
        const tip = node.at(-1)!;
        const abort = new AbortController();
        const statuses: FollowStatus[] = [];
        const running = followChain({
          store,
          transport,
          origin: simOrigin(),
          signal: abort.signal,
          backoffMs: { initial: 5, max: 20 },
          stuckAfter: 3,
          onStatus: (status) => {
            statuses.push(status);
          },
        });
        const deadline = Date.now() + 10_000;
        let reached = false;
        while (!reached && Date.now() < deadline) {
          const at = await store.cursor().catch(() => null);
          reached = at?.point.hash.toString("hex") === tip.hash;
          if (!reached) await pause(20);
        }
        abort.abort();
        await running;
        const last = statuses.at(-1)!;
        expect(
          reached,
          JSON.stringify({ stuck: last.stuck, waiting: last.waiting }),
        ).toBe(true);
        expect(statuses.some((status) => status.stuck !== null)).toBe(false);
      } finally {
        await store.close();
      }
    });

    it("holds l1_node_unavailable while the node is lost, and clears it once the node is back", async () => {
      const events = buildForkSteps(forkCorpus(SIM_K)[0]!.scenario, [
        FIXTURE_PROJECTION,
      ]).steps.map((step) => step.event);
      const chain = fakeBlocks(
        events.slice(
          0,
          events.findIndex((event) => event.kind === "roll_backward"),
        ),
      );
      const directory = await realpath(
        await mkdtemp(join(tmpdir(), "l1-follower-node-loss-")),
      );
      const backPath = join(directory, "back");
      const transport = new L1NodeTransport({
        binaryPath: await writeFakeSidecar({
          path: join(directory, "fake-sidecar"),
          handlerModule: fileURLToPath(
            new URL("./fixtures/node-loss-handler.mjs", import.meta.url),
          ),
          options: {
            base: simBase(),
            blocks: chain,
            lostPath: join(directory, "lost"),
            backPath,
          },
        }),
        socketPath: join(directory, "node.socket"),
        networkMagic: 42,
        requestTimeoutMs: 10_000,
        restartDelayMs: { initial: 20, max: 100 },
      });
      const store = await stores[dialect]();
      const abort = new AbortController();
      let status: FollowStatus | undefined;
      const statuses: FollowStatus[] = [];
      const running = followChain({
        store,
        transport,
        origin: simOrigin(),
        signal: abort.signal,
        backoffMs: { initial: 5, max: 20 },
        onStatus: (next) => {
          status = next;
          statuses.push(next);
        },
      });
      const until = async (what: string, holds: () => boolean) => {
        const deadline = Date.now() + 10_000;
        while (!holds()) {
          if (Date.now() > deadline)
            throw new Error(
              `timed out waiting for ${what}; ${JSON.stringify(status?.readiness)}`,
            );
          await pause(10);
        }
      };
      const unavailable = () =>
        status?.readiness.find(
          (reason) => reason.reason === FOLLOWER_NODE_UNAVAILABLE,
        );
      try {
        await until("the cursor at the tip", () => status?.atTip === true);
        await until(
          "the lost node",
          () => unavailable()?.detail.startsWith("node_unreachable: ") === true,
        );
        expect(status!.atTip).toBe(false);
        expect(status!.node).toMatchObject({ reason: "node_unreachable" });
        expect(
          statuses.some(
            (seen) =>
              seen.atTip &&
              seen.readiness.some(
                (reason) => reason.reason === FOLLOWER_NODE_UNAVAILABLE,
              ),
          ),
        ).toBe(false);
        await writeFile(backPath, "back");
        await until("the node back", () => unavailable() === undefined);
        expect(status!.node).toBeNull();
      } finally {
        abort.abort();
        await running;
        await transport.close();
        await store.close();
        await rm(directory, { recursive: true, force: true });
      }
    });
  },
);

describe("followChain's chain-sync credit", () => {
  it("opens with a deep window while behind the node's tip, and narrows it to one block at the tip", async () => {
    const origin = simOrigin().origin;
    const blocks: FakeBlock[] = [];
    for (let i = 1; i <= 30; i += 1)
      blocks.push(
        rawBlock(
          origin.slot + i,
          1_000 + i,
          blocks.at(-1)?.hash ?? origin.hash.toString("hex"),
          [],
        ),
      );
    const transport = await fakes.transport(simBase(), blocks);
    const streams: ChainSyncStream[] = [];
    /** Whether each stream opened with its catch-up window. */
    const openedCatchingUp: boolean[] = [];
    const store = await stores.sqlite();
    const abort = new AbortController();
    let status: FollowStatus | undefined;
    const running = followChain({
      store,
      transport: {
        readiness: transport.readiness,
        onReadiness: (listener) => transport.onReadiness(listener),
        openChainSync: (options) => {
          const stream = transport.openChainSync(options);
          streams.push(stream);
          openedCatchingUp.push(stream.catchingUp);
          return stream;
        },
      },
      origin: simOrigin(),
      signal: abort.signal,
      backoffMs: { initial: 5, max: 20 },
      onStatus: (next) => {
        status = next;
      },
    });
    try {
      const deadline = Date.now() + 10_000;
      while (status?.atTip !== true || status.cursor?.height !== 1_030) {
        if (Date.now() > deadline)
          throw new Error(`timed out; ${JSON.stringify(status?.readiness)}`);
        await pause(10);
      }
      expect(streams.length).toBeGreaterThan(0);
      for (const stream of streams)
        expect(stream.options.credit).toEqual({
          catchUpWindow: 50,
          tipWindow: 1,
          catchUpDistance: 10n,
        });
      expect(FOLLOW_CREDIT_POLICY).toEqual(streams[0]!.options.credit);
      // 30 blocks behind at open: the deep window. At the tip: one block.
      expect(openedCatchingUp.every((catchingUp) => catchingUp)).toBe(true);
      expect(streams.at(-1)!.catchingUp).toBe(false);
    } finally {
      abort.abort();
      await running;
      await store.close();
    }
  });
});

describe("followChain over a stream that keeps failing and reopening", () => {
  it("reports a reopen loop as a wait on the stream, and clears it at the next applied event", async () => {
    const origin = simOrigin().origin;
    const blocks: FakeBlock[] = [];
    for (let i = 1; i <= 8; i += 1)
      blocks.push(
        rawBlock(
          origin.slot + i,
          1_000 + i,
          blocks.at(-1)?.hash ?? origin.hash.toString("hex"),
          [],
        ),
      );
    // The first open serves 3 blocks and fails; the next 2 fail at once.
    const transport = await fakes.transport(simBase(), blocks, {
      failingOpens: 2,
      servedBeforeFailing: 3,
    });
    const store = await stores.sqlite();
    const abort = new AbortController();
    const statuses: FollowStatus[] = [];
    const logged: string[] = [];
    const running = followChain({
      store,
      transport,
      origin: simOrigin(),
      signal: abort.signal,
      backoffMs: { initial: 5, max: 20 },
      log: (line) => logged.push(line),
      onStatus: (next) => {
        statuses.push(next);
      },
    });
    const reopenLoop = (status: FollowStatus): boolean =>
      status.waiting?.cause === "stream" &&
      status.readiness.some(
        (entry) =>
          entry.reason === FOLLOWER_WAITING &&
          entry.detail.includes("3 times in a row"),
      );
    try {
      const deadline = Date.now() + 15_000;
      while (statuses.at(-1)?.cursor?.height !== 1_008) {
        if (Date.now() > deadline)
          throw new Error(
            `timed out; ${JSON.stringify(statuses.at(-1)?.readiness)}`,
          );
        await pause(20);
      }
      const loop = statuses.findIndex(reopenLoop);
      expect(loop).toBeGreaterThan(-1);
      // Held at the cursor of the third block while the stream reopened.
      expect(statuses[loop]!.cursor?.height).toBe(1_003);
      // Two failures in a row were logged, not yet a wait.
      expect(
        statuses
          .slice(0, loop)
          .some((status) => status.waiting?.cause === "stream"),
      ).toBe(false);
      expect(
        logged.filter((line) => line.startsWith("chain-sync stream failed")),
      ).toHaveLength(3);
      const last = statuses.at(-1)!;
      expect(last.waiting).toBeNull();
      expect(
        last.readiness.some((entry) => entry.reason === FOLLOWER_WAITING),
      ).toBe(false);
    } finally {
      abort.abort();
      await running;
      await store.close();
    }
  });
});
