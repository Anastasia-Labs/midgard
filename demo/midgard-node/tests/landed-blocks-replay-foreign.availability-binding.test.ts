/**
 * The foreign replayer against the availability commitment its block's node
 * carries (`SDK.daPayloadAvailabilityBinding`). The header does not commit
 * the envelope's bytes (a body can be served recompressed, or tampered), so
 * a peer can serve the canonical header over other bytes. Once the node carries a commitment, a fetched or retained body that
 * does not rebuild it is the next peer's to serve, within the same fetch.
 * While the node is Unattested no replay refusal is final: it is `missing`,
 * and the next fetch asks the next peer. A copy imported then is retained
 * only once a pass has checked it against the commitment the node carries
 * later; the block's verdict stays as it was.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Clock, Effect, Option } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { FOREIGN_DA_RETRY_MAX_MS } from "../src/da/foreign-retained-da.js";
import type { LandedStateQueue } from "../src/l1-state-queue/index.js";
import { landedBlockHook } from "../src/landed-blocks/hook.js";
import { nodeLandedBlockPorts } from "../src/landed-blocks/node-ports.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import { foreignBlockReplayer } from "../src/landed-blocks/replay-foreign.js";
import { insertRow, retrieveRows } from "../src/landed-blocks/store.js";
import {
  FORCED_CONFIG,
  nativeTransactionCbor,
} from "./helpers/forced-orders-chain.js";
import { db } from "./helpers/forced-orders-node-store.js";
import {
  admissionTx,
  eventOrder,
  EVENTS_CONFIG,
} from "./helpers/l1-events-chain.js";
import { SIM_QUEUE_CONFIG } from "./helpers/state-queue-sim.fixtures.js";
import {
  attested,
  AVAILABILITY,
  published,
  recompressed,
} from "./landed-blocks-replay-foreign.availability.js";
import {
  clock,
  emptyBlock,
  envelope,
  FIXTURE_ORDER_ID,
  fixtureOrderTx,
  followChain,
  forcedBlock,
  inNode,
  inputFor,
  nonceRef,
  retain,
  retained,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import { serveDa } from "./landed-blocks-replay-foreign.transport.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];
const time = { ms: 1_000_000 };
const clockAt: Clock.Clock = {
  [Clock.ClockTypeId]: Clock.ClockTypeId,
  unsafeCurrentTimeMillis: () => time.ms,
  currentTimeMillis: Effect.sync(() => time.ms),
  unsafeCurrentTimeNanos: () => BigInt(time.ms) * 1_000_000n,
  currentTimeNanos: Effect.sync(() => BigInt(time.ms) * 1_000_000n),
  sleep: () => Effect.void,
};

beforeEach(async () => {
  clock.offsetMs = 0;
  time.ms = 1_000_000;
  await db(resetApplicationTables);
});
afterEach(async () => {
  vi.restoreAllMocks();
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

/**
 * A followed chain (with the forced fixture's order admitted unless told
 * otherwise) and one replayer over it, kept across runs as the follower
 * keeps one.
 */
const setup = async (options: { forcedOrder?: boolean } = {}) => {
  const followed = await followChain();
  opened.push(followed.store);
  if (options.forcedOrder !== false)
    await followed.chain.forward([
      await fixtureOrderTx(followed.chain, FIXTURE_ORDER_ID),
    ]);
  const replayer = foreignBlockReplayer({
    store: followed.store,
    events: EVENTS_CONFIG,
    forcedOrders: FORCED_CONFIG,
    availability: () => AVAILABILITY,
  });
  const run = async (
    block: SDK.DaPayload,
    status: SDK.DaAvailabilityStateQueueStatus,
  ) =>
    inNode(
      replayer
        .replay(await inputFor(followed.store, block, status))
        .pipe(Effect.withClock(clockAt)),
    );
  const bind = (queue: LandedStateQueue) =>
    inNode(replayer.bind(queue).pipe(Effect.withClock(clockAt)));
  return { ...followed, run, bind };
};

/** `block`'s body with `alter` applied, under the block's own header. */
const tampered = (
  block: SDK.DaPayload,
  alter: Partial<SDK.DaPayload["block_body"]>,
): SDK.DaPayload => ({
  ...block,
  block_body: { ...block.block_body, ...alter },
});

/** The forced block without its validation descriptor (counted 0). */
const undescribed = (block: SDK.DaPayload) =>
  tampered(block, {
    validation_traces: [],
    counts: { ...block.block_body.counts, validationTraceCount: 0n },
  });

/** The forced block's committed entry over another transaction's preimage. */
const otherPreimage = (block: SDK.DaPayload) => {
  const [[key]] = block.block_body.forced_transaction_preimages as [
    SDK.DaPayloadEntry,
  ];
  return tampered(block, {
    forced_transaction_preimages: [
      [key, nativeTransactionCbor([0x21]).toString("hex")],
    ],
  });
};

const retainedBytes = async (block: SDK.DaPayload) => {
  const row = await retained(block);
  return Option.isSome(row) ? row.value.payload_cbor : undefined;
};

/** The landed queue at one node: `block`'s, carrying `status`. */
const queueOf = (
  block: SDK.DaPayload,
  status: SDK.DaAvailabilityStateQueueStatus,
) => {
  const { header, header_hash: headerHash } = block.block_body;
  const data = Data.castTo(
    {
      header,
      block_time: header.endTime,
      da_attestation: status,
      proven_fraud: null,
    },
    SDK.StateQueueNode,
  );
  // Only the fields the binding pass reads.
  return {
    nodes: [
      {
        headerHash,
        daStatus: JSON.stringify(status),
        element: { datum: { data } },
      },
    ],
  } as unknown as LandedStateQueue;
};

/** Records `block` as a processed foreign landed block. */
const landed = (block: SDK.DaPayload) => {
  const { header, header_hash: headerHash } = block.block_body;
  return inNode(
    insertRow({
      headerHash,
      parentHeaderHash: header.prevHeaderHash,
      parentUtxosRoot: header.prevUtxosRoot,
      utxosRoot: header.utxosRoot,
      kind: "foreign",
      state: "processed",
      applied: false,
      spent: [],
      produced: [],
      depositIds: [],
      withdrawals: [],
      forcedIds: [],
      txIds: [],
    }),
  );
};

describe("foreign replay against a node carrying a commitment", () => {
  it("fetches past a peer whose body differs from the attested commitment, in the same fetch", async () => {
    const { run } = await setup();
    const honest = await forcedBlock();
    const requests = serveDa(
      (peer) => (peer === 0 ? undescribed(honest) : honest),
      2,
    );
    expect(await run(honest, await attested(honest))).toMatchObject({
      kind: "replayed",
    });
    expect(requests).toEqual([0, 1]);
    expect(await retainedBytes(honest)).toEqual(await envelope(honest));
  });

  it("checks a Published node against its terminal commitment", async () => {
    const { run } = await setup();
    const honest = await forcedBlock();
    const requests = serveDa(
      (peer) => (peer === 0 ? undescribed(honest) : honest),
      2,
    );
    expect(await run(honest, await published(honest))).toMatchObject({
      kind: "replayed",
    });
    expect(requests).toEqual([0, 1]);
    expect(await retainedBytes(honest)).toEqual(await envelope(honest));
  });

  it("refuses a body the commitment binds that does not replay", async () => {
    const { run } = await setup();
    const honest = await forcedBlock();
    for (const [variant, detail] of [
      [undescribed(honest), /lacks its validation descriptor/u],
      [otherPreimage(honest), /forced transaction differs from its order/u],
    ] as const) {
      serveDa(() => variant, 2);
      expect(await run(honest, await attested(variant))).toMatchObject({
        kind: "invalid",
        detail: expect.stringMatching(detail),
      });
    }
  });

  it("drops a retained copy that differs from the commitment and fetches a bound one", async () => {
    const { run } = await setup();
    const honest = await forcedBlock();
    await retain(await retainedRow(otherPreimage(honest)));
    serveDa(() => honest);
    expect(await run(honest, await attested(honest))).toMatchObject({
      kind: "replayed",
    });
    expect(await retainedBytes(honest)).toEqual(await envelope(honest));
  });

  it("stays missing while every peer serves other bytes, backs off, and imports once one serves the bound body", async () => {
    const { run } = await setup();
    const honest = await forcedBlock();
    const status = await attested(honest);
    const requests = serveDa(() => undescribed(honest), 2);
    expect(await run(honest, status)).toMatchObject({
      kind: "missing",
      detail: expect.stringContaining("unavailable or unauthenticated"),
    });
    expect(requests).toEqual([0, 1]);
    expect(await run(honest, status)).toMatchObject({
      kind: "missing",
      detail: expect.stringContaining("backs off"),
    });
    expect(requests).toEqual([0, 1]);
    time.ms += FOREIGN_DA_RETRY_MAX_MS;
    serveDa((peer) => (peer === 0 ? undescribed(honest) : honest), 2);
    expect(await run(honest, status)).toMatchObject({ kind: "replayed" });
  });
});

describe("foreign replay against an Unattested node", () => {
  it("names a copy whose uncommitted section does not replay missing, and the next fetch asks the next peer", async () => {
    const { run } = await setup();
    const honest = await forcedBlock();
    const requests = serveDa(
      (peer) => (peer === 0 ? undescribed(honest) : honest),
      2,
    );
    expect(await run(honest, "Unattested")).toMatchObject({
      kind: "missing",
      detail: expect.stringMatching(
        /^the block's payload is not bound to an attested commitment yet.*lacks its validation descriptor/u,
      ),
    });
    expect(requests).toEqual([0]);
    expect(await run(honest, "Unattested")).toMatchObject({
      kind: "replayed",
    });
    expect(requests).toEqual([0, 1]);
    // Imported unbound: nothing is retained before a commitment binds it.
    expect(await retainedBytes(honest)).toBeUndefined();
  });

  it("names a copy failing a committed check through an uncommitted input missing, never invalid", async () => {
    const { run } = await setup();
    const honest = await forcedBlock();
    const requests = serveDa(() => otherPreimage(honest), 2);
    for (let attempt = 0; attempt < 3; attempt++)
      expect(await run(honest, "Unattested")).toMatchObject({
        kind: "missing",
      });
    expect(requests).toEqual([0, 1, 0]);
  });

  it("names a fraudulent block missing while Unattested and invalid once attested", async () => {
    const { chain, run } = await setup({ forcedOrder: false });
    const fraud = await emptyBlock();
    await chain.forward([
      admissionTx(
        eventOrder("deposit", nonceRef(), { inclusionTime: 2n }),
        chain.chain.nonce(),
      ),
    ]);
    const requests = serveDa(() => fraud, 2);
    expect(await run(fraud, "Unattested")).toMatchObject({
      kind: "missing",
      detail: expect.stringContaining("leaves out in-window deposit"),
    });
    expect(await run(fraud, await attested(fraud))).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("leaves out in-window deposit"),
    });
    expect(requests).toEqual([0, 1]);
  });
});

describe("binding a copy imported Unattested", () => {
  it("imports a copy whose uncommitted bytes differ, then retains the bound body in its place once attested", async () => {
    const { run, bind } = await setup();
    const honest = await forcedBlock();
    const requests = serveDa(
      (peer) => (peer === 0 ? recompressed(honest) : honest),
      2,
    );
    expect(await run(honest, "Unattested")).toMatchObject({
      kind: "replayed",
    });
    expect(requests).toEqual([0]);
    expect(await retainedBytes(honest)).toBeUndefined();
    await landed(honest);
    const before = await inNode(retrieveRows);
    await bind(queueOf(honest, await attested(honest)));
    expect(await retainedBytes(honest)).toEqual(await envelope(honest));
    expect(requests).toEqual([0, 0, 1]);
    // The verdict stands: the landed row is as it was.
    expect(await inNode(retrieveRows)).toEqual(before);
  });

  it("retains the held copy as it is when the commitment binds it", async () => {
    const { run, bind } = await setup();
    const honest = await forcedBlock();
    const variant = recompressed(honest);
    const requests = serveDa(() => variant, 2);
    expect(await run(honest, "Unattested")).toMatchObject({
      kind: "replayed",
    });
    await landed(honest);
    await bind(queueOf(honest, await attested(variant)));
    expect(await retainedBytes(honest)).toEqual(await envelope(variant));
    expect(requests).toEqual([0]);
  });

  it("drops a retained copy the later commitment does not bind", async () => {
    const { bind } = await setup();
    const honest = await forcedBlock();
    await retain(await retainedRow(recompressed(honest)));
    await landed(honest);
    serveDa(() => undefined, 2);
    await bind(queueOf(honest, await attested(honest)));
    expect(await retainedBytes(honest)).toBeUndefined();
    serveDa(() => honest, 2);
    time.ms += FOREIGN_DA_RETRY_MAX_MS;
    await bind(queueOf(honest, await attested(honest)));
    expect(await retainedBytes(honest)).toEqual(await envelope(honest));
  });
});

describe("the node's binding pass in the driver run", () => {
  it("is the node ports' pass over the copies their replay imported unbound", async () => {
    const { store } = await setup();
    const honest = await forcedBlock();
    const ports = nodeLandedBlockPorts(
      store,
      {
        projection: EVENTS_CONFIG,
        forcedOrders: FORCED_CONFIG,
        stateQueue: SIM_QUEUE_CONFIG,
      },
      () => Effect.succeed(undefined),
      () => AVAILABILITY,
    );
    serveDa((peer) => (peer === 0 ? recompressed(honest) : honest), 2);
    expect(
      await inNode(ports.replay(await inputFor(store, honest, "Unattested"))),
    ).toMatchObject({ kind: "replayed" });
    await landed(honest);
    await inNode(ports.bindRetained(queueOf(honest, await attested(honest))));
    expect(await retainedBytes(honest)).toEqual(await envelope(honest));
  });

  it("runs after the landed hook processed the queue it read", async () => {
    const queue = {
      ...queueOf(await forcedBlock(), "Unattested"),
      healthy: true,
      root: null,
    } as LandedStateQueue;
    const bound: LandedStateQueue[] = [];
    const hook = landedBlockHook({
      store: {
        dialect: "postgres",
        transaction: async () => ({ kind: "ok", queue }),
        cursor: async () => null,
      } as never,
      config: SIM_QUEUE_CONFIG,
      // A queue without a root is processed without a port.
      ports: {
        bindRetained: (read: LandedStateQueue) =>
          Effect.sync(() => void bound.push(read)),
      } as unknown as LandedBlockPorts<never>,
      run: (effect) => Effect.runPromise(effect as Effect.Effect<never>),
    });
    expect(
      await hook({ kind: "unchanged", view: queue.view } as never),
    ).toBeUndefined();
    expect(bound).toEqual([queue]);
  });
});
