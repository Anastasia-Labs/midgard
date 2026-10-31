/**
 * The node's follower origin against the deployment's recorded origin T (the
 * manifest's `l1Origin`, the local node tip the deployer recorded before it
 * signed the hub-oracle nonce). A store at T holds every output created after
 * T, those before the nonce's block included, so a block naming a deposit
 * read from such an output replays. A store whose origin is after T is
 * flagged R3 (`origin_after_protocol_init`) even though it sees the
 * protocol-init spend.
 *
 * The bench's slot configuration maps slot `s` to `s * 1000` ms.
 */
import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import {
  type FactStore,
  type OutRef,
  protocolInitStatus,
} from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { db } from "./helpers/forced-orders-node-store.js";
import {
  admissionTx,
  ELSEWHERE,
  eventOrder,
} from "./helpers/l1-events-chain.js";
import type { ChainDriver } from "./helpers/l1-events-store.js";
import { nodeFollowerPlanFor } from "./helpers/node-follower-plan.js";
import {
  clock,
  depositBlock,
  depositsAt,
  followChain,
  nonceRef,
} from "./landed-blocks-replay-foreign.fixture.js";
import {
  reach,
  replayAt,
  SLOT_MS,
  startFor,
  windowed,
} from "./landed-blocks-replay-foreign.window.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];

beforeEach(async () => {
  clock.offsetMs = 0;
  await db(resetApplicationTables);
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

/** The follower store's origin, in the manifest's form. */
const STORE_ORIGIN: L1Origin = {
  slot: SIM_ORIGIN.point.slot,
  blockHash: SIM_ORIGIN.point.hash.toString("hex"),
};

/**
 * A store at the simulated origin, where a retention output for an external
 * deposit is paid, then the prepare block creates the hub-oracle one-shot
 * and the protocol-init block spends it, then the deposit is admitted
 * reading its payload from that output (`seen`) or from one outside the
 * store's facts.
 */
const deployedChain = async (seen: boolean) => {
  const { store, chain } = await followChain();
  opened.push(store);
  const start = startFor(SIM_ORIGIN.point.slot);
  const nonce = nonceRef();
  const order = eventOrder("deposit", nonce, {
    external: true,
    inclusionTime: start + SLOT_MS,
  });
  let retained: OutRef = { txHash: Buffer.alloc(32, 0xab), index: 0 };
  if (seen) {
    const [hash] = await chain.forward([
      {
        inputs: [nonceRef()],
        outputs: [order.retained!],
        nonce: chain.chain.nonce(),
      },
    ]);
    retained = { txHash: hash!, index: 0 };
  }
  const hubOracleOneShot = await prepareAndInit(store, chain);
  await chain.forward([admissionTx(order, chain.chain.nonce(), retained)]);
  return { store, chain, start, hubOracleOneShot };
};

/** The prepare block's one-shot output, and the protocol-init tx spending it. */
const prepareAndInit = async (
  store: FactStore,
  chain: ChainDriver,
): Promise<OutRef> => {
  const [prepare] = await chain.forward([
    {
      inputs: [nonceRef()],
      outputs: [{ address: ELSEWHERE, lovelace: 2_000_000n }],
      nonce: chain.chain.nonce(),
    },
  ]);
  const oneShot = { txHash: prepare!, index: 0 };
  store.watchProtocolInit(oneShot);
  await chain.forward([
    {
      inputs: [oneShot],
      outputs: [{ address: ELSEWHERE, lovelace: 2_000_000n }],
      nonce: chain.chain.nonce(),
    },
  ]);
  return oneShot;
};

/** The node's follower origin check, from its plan, at the store's tip. */
const originCheck = async (
  store: FactStore,
  input: Readonly<{
    l1Origin: L1Origin | null;
    recordedOrigin: L1Origin | undefined;
    hubOracleOneShot: OutRef;
  }>,
) => {
  const plan = await Effect.runPromise(nodeFollowerPlanFor(input));
  if (plan.kind !== "run")
    throw new Error(`the node's follower does not run: ${plan.detail}`);
  const cursor = (await store.cursor())!;
  expect(plan.origin.origin).toEqual(cursor.origin);
  return protocolInitStatus(store, plan.origin, cursor.point);
};

describe("a node follower store at the deployment's recorded origin", () => {
  it("N-a: replays a block naming a deposit whose retention output was paid after T and before the prepare block", async () => {
    const { store, start, hubOracleOneShot } = await deployedChain(true);
    // No L1_ORIGIN: the node starts at the manifest's T.
    expect(
      await originCheck(store, {
        l1Origin: null,
        recordedOrigin: STORE_ORIGIN,
        hubOracleOneShot,
      }),
    ).toMatchObject({ kind: "seen" });
    const [known] = await depositsAt(store);
    const block = await windowed(await depositBlock(known!.entry), start);
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({ kind: "replayed" });
  });

  it("N-b: flags R3 for a store whose origin is after T, though it sees the protocol-init spend", async () => {
    const { store, hubOracleOneShot } = await deployedChain(false);
    const recordedOrigin: L1Origin = {
      slot: STORE_ORIGIN.slot - 1,
      blockHash: "9f".repeat(32),
    };
    // Without the manifest's T, the same store passes the spend check.
    expect(
      await originCheck(store, {
        l1Origin: STORE_ORIGIN,
        recordedOrigin: undefined,
        hubOracleOneShot,
      }),
    ).toMatchObject({ kind: "seen" });
    expect(
      await originCheck(store, {
        l1Origin: STORE_ORIGIN,
        recordedOrigin,
        hubOracleOneShot,
      }),
    ).toEqual({
      kind: "intervention",
      reason: "origin_after_protocol_init",
      detail: expect.stringContaining(
        `after the deployment's recorded l1Origin ${recordedOrigin.slot.toString()}.${recordedOrigin.blockHash}`,
      ) as unknown,
    });
  });
});
