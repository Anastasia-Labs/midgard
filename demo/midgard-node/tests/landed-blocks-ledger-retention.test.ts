/**
 * The landed-block hook's native MPF retention: after processing the queue
 * it read, the hook has the native owner keep the ledgers of the queue
 * blocks inside the retention window, the processed tip's, the frontier's
 * and the L1-confirmed one, and a retention that fails holds nothing.
 */
import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import type { LandedStateQueue } from "../src/l1-state-queue/index.js";
import { landedBlockHook } from "../src/landed-blocks/hook.js";
import { retainLandedLedgers } from "../src/landed-blocks/ledger-retention.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import {
  Frontier,
  insertRow,
  type LandedBlockRow,
} from "../src/landed-blocks/store.js";
import { Lucid } from "../src/services/lucid.js";
import { installEmptyLedger } from "./helpers/ledger-install.js";
import {
  NODE_PREFIX,
  nodeDatum,
  SIM_QUEUE_CONFIG,
} from "./helpers/state-queue-sim.fixtures.js";
import {
  block,
  confirmedAt,
  element,
  inNode,
  queueOf,
} from "./landed-blocks-own.fixture.js";
import { provideDatabaseLayers } from "./utils.js";

const root = (byte: number) => byte.toString(16).padStart(2, "0").repeat(32);
const header = (byte: number) => byte.toString(16).padStart(2, "0").repeat(28);

/** The blocks' roots. */
const P0 = root(0x10);
const P1 = root(0x11);
const P2 = root(0x12);
const P3 = root(0x13);
/** The queue root's confirmed root. */
const Q = root(0x20);
/** The processed rows' roots, and the frontier's. */
const S1 = root(0x31);
const S2 = root(0x32);
const S3 = root(0x33);
const F = root(0x40);

/**
 * Queue blocks O, M, B chained P0 -> P1 -> P2 -> P3, one second apart. O
 * and M were merged: only the follower's history still holds them. The
 * root's confirmed state has root Q.
 */
const chain = () => {
  const o = block(
    1,
    { hash: SDK.GENESIS_HEADER_HASH, root: P0, endTime: 0n },
    P1,
  );
  const m = block(2, { hash: o.hash, root: P1, endTime: o.header.endTime }, P2);
  const b = block(3, { hash: m.hash, root: P2, endTime: m.header.endTime }, P3);
  const queue = queueOf(confirmedAt("genesis", Q), [b]);
  const merged = [o, m].map((node, index) =>
    element(
      10 + index,
      nodeDatum(node.header, "Unattested", null),
      NODE_PREFIX + node.hash,
      node.hash,
      node.header.endTime,
    ),
  );
  return { o, queue, history: [...merged, queue.root!, ...queue.nodes] };
};

const row = (
  headerHash: string,
  parentHeaderHash: string,
  utxosRoot: string,
  state: LandedBlockRow["state"],
): LandedBlockRow => ({
  headerHash,
  parentHeaderHash,
  parentUtxosRoot: P0,
  utxosRoot,
  kind: "foreign",
  state,
  applied: true,
  spent: [],
  produced: [],
  depositIds: [],
  withdrawals: [],
  forcedIds: [],
  txIds: [],
});

/** Processed S1 then S2 on it, a removed S3, and the frontier at F. */
const seedRows = Effect.gen(function* () {
  yield* insertRow(row(header(1), header(0), S1, "processed"));
  yield* insertRow(row(header(2), header(1), S2, "processed"));
  yield* insertRow(row(header(3), header(9), S3, "removed"));
  yield* Frontier.upsert({ headerHash: header(0), utxosRoot: F });
});

/** Runs `retainLandedLedgers` with `retain` as the owner's, its L1 time at
 * the view `tipMs`. */
const retainWith = (
  history: ReturnType<typeof chain>["history"],
  queue: LandedStateQueue,
  tipMs: number,
  retain: (
    roots: ReadonlySet<string>,
  ) => Promise<{ notRetained: ReadonlySet<string> }>,
) =>
  inNode(
    Effect.gen(function* () {
      yield* seedRows;
      yield* retainLandedLedgers(
        () => Effect.succeed(history),
        Ref.get(
          Ref.unsafeMake<{ retain: typeof retain } | undefined>({ retain }),
        ),
      )(queue);
    }).pipe(
      Effect.provideService(Lucid, {
        api: {
          slotToUnixTime: (slot: number) => {
            expect(slot).toBe(queue.view.point.slot);
            return tipMs;
          },
        },
      } as unknown as Lucid),
    ),
  );

describe("landed-block ledger retention", () => {
  it("keeps the window blocks' roots, the processed tip's, the frontier's and the confirmed root", async () => {
    const { o, queue, history } = chain();
    const kept: ReadonlySet<string>[] = [];
    // O has just left the window; M, merged, is still in it.
    const tipMs =
      Number(o.header.endTime) + MIDGARD_RETENTION_WINDOW.ledgerRetentionMs;
    await retainWith(history, queue, tipMs, async (roots) => {
      kept.push(roots);
      return { notRetained: new Set() };
    });
    expect(kept).toEqual([new Set([P1, P2, P3, Q, S2, F])]);
  });

  it("holds nothing when the owner refuses the retain", async () => {
    const { o, queue, history } = chain();
    await expect(
      retainWith(history, queue, Number(o.header.endTime), () =>
        Promise.reject(new Error("Native MPF owner service is closed")),
      ),
    ).resolves.toBeUndefined();
  });

  it("reads nothing while no native owner runs", async () => {
    const { queue } = chain();
    let historyReads = 0;
    await inNode(
      retainLandedLedgers(
        () =>
          Effect.sync(() => {
            historyReads += 1;
            return [];
          }),
        Ref.get(Ref.unsafeMake<undefined>(undefined)),
      )(queue).pipe(
        Effect.provideService(Lucid, {
          api: { slotToUnixTime: () => 0 },
        } as unknown as Lucid),
      ),
    );
    expect(historyReads).toBe(0);
  });

  it("runs after the hook processed the queue it read", async () => {
    const queue = { ...chain().queue, root: null } as LandedStateQueue;
    await inNode(installEmptyLedger());
    const retained: LandedStateQueue[] = [];
    const hook = landedBlockHook({
      store: {
        dialect: "postgres",
        transaction: async () => ({ kind: "ok", queue }),
        cursor: async () => null,
      } as never,
      config: SIM_QUEUE_CONFIG,
      // A queue without a root is processed without a port.
      ports: {
        retainLedgers: (read: LandedStateQueue) =>
          Effect.sync(() => void retained.push(read)),
      } as unknown as LandedBlockPorts<never>,
      run: (effect) =>
        Effect.runPromise(
          provideDatabaseLayers(effect as Effect.Effect<never, never, never>),
        ),
    });
    expect(
      await hook({ kind: "unchanged", view: queue.view } as never),
    ).toBeUndefined();
    expect(retained).toEqual([queue]);
  });
});
