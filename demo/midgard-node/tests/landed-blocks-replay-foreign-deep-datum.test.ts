/**
 * The production foreign replayer on the follower's facts when an in-window
 * event's payload is unusual. An event ingestion leaves out as undecodable
 * is left out of the window the same way: a block without it replays, and
 * one naming it is `invalid`. An honest withdrawal whose L1 datum is deeply
 * nested, or nested to the admission bound, replays.
 */
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import type { FactStore, OutRef } from "@al-ft/midgard-l1-follower";
import { eventsAt } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import {
  ADMISSION_PAYLOAD_NODES,
  DEEP_PLUTUS_DATA_CASES,
  deepPlutusDataCbor,
  depthAtAdmissionBound,
  plutusDataNodeCount,
  replaceCborItemOnce,
} from "@al-ft/midgard-test-support/deep-plutus-data";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { reconcileFollowerEvents } from "../src/database/follower-events.js";
import { WithdrawalsDB } from "../src/database/index.js";
import { EVENT_UNDECODABLE } from "../src/l1-events/driver.js";
import { eventKeyCbor } from "../src/mpf/trace-events.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import {
  FollowerWriteFixture,
  withFollowerWrite,
} from "../src/services/follower-write-gate.js";
import { countsFromLengths } from "./da-payload.record.js";
import { rebind } from "./foreign-block-import.fixture.js";
import { db } from "./helpers/forced-orders-node-store.js";
import { admissionTx, eventOrder, listOf } from "./helpers/l1-events-chain.js";
import {
  clock,
  depositBlock,
  depositsAt,
  emptyBlock,
  followChain,
  idKey,
  inNode,
  inputFor,
  naming,
  NETWORK,
  nonceRef,
  replayerFor,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];

const follow = async () => {
  const followed = await followChain();
  opened.push(followed.store);
  return followed;
};

beforeEach(async () => {
  clock.offsetMs = 0;
  await db(resetApplicationTables);
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

/** Retains `payload` and replays it with `replayer` at the store's view. */
const replayedBy = async (
  replayer: ReturnType<typeof replayerFor>,
  store: FactStore,
  payload: SDK.DaPayload,
) => {
  await retain(await retainedRow(payload));
  return inNode(replayer(await inputFor(store, payload)));
};

/** Local ingestion of the deposits the view knows: what it refuses. */
const ingestedDeposits = async (store: FactStore) => {
  const view = (await store.currentView())!;
  const read = await eventsAt(store, listOf("deposit"), view.point);
  if (read.kind !== "ok") throw new Error(`eventsAt: ${read.kind}`);
  const outcome = await inNode(
    withFollowerWrite(
      reconcileFollowerEvents(
        { view, events: read.value },
        {
          network: NETWORK,
          slotToUnixTime: (slot) => slot * 1000,
          cutoffMs: 0,
        },
      ),
    ).pipe(Effect.provideService(FollowerWriteFixture, true)),
  );
  if (outcome.kind !== "applied") throw new Error(`outcome ${outcome.kind}`);
  return { events: read.value, ingestion: outcome.ingestion };
};

// The helper's withdrawal Order datum: facts [3, 0], inline payload [2, 0],
// event info [0, 1], body [0].
const PAYLOAD_IN_ORDER = [3, 0, 2, 0];
const BODY_IN_ORDER = [...PAYLOAD_IN_ORDER, 0, 1, 0];

/** An in-window withdrawal Order whose L1 datum is `InlineDatum { datum }`. */
const withdrawalOrder = (datum: string) => {
  const order = eventOrder("withdrawal", nonceRef(), { inclusionTime: 2n });
  const orderDatum = order.order.datum!.toString("hex");
  const body = plutusConstrFieldCbor(orderDatum, BODY_IN_ORDER);
  const deepBody = replaceCborItemOnce(
    body,
    plutusConstrFieldCbor(body, [4]),
    `d87b81${datum}`,
  );
  return {
    ...order,
    order: {
      ...order.order,
      datum: Buffer.from(
        replaceCborItemOnce(orderDatum, body, deepBody),
        "hex",
      ),
    },
  };
};

/**
 * The honest block on an empty parent naming the view's one withdrawal:
 * with no ledger output it settles as `NonExistentWithdrawalUtxo` and keeps
 * the ledger empty.
 */
const withdrawalBlock = async (store: FactStore, nonce: OutRef) => {
  const view = (await store.currentView())!;
  const read = await eventsAt(store, listOf("withdrawal"), view.point);
  if (read.kind !== "ok") throw new Error(`eventsAt: ${read.kind}`);
  const [event] = read.value;
  if (event === undefined) throw new Error("the view holds no withdrawal");
  const info = aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(event.payloadCbor, [0, 1]),
  );
  // The event's l2_outref is its id.
  const ledgerOutRef = await Effect.runPromise(
    WithdrawalsDB.toLedgerOutRef({
      [WithdrawalsDB.Columns.L2_OUTREF]: Buffer.from(event.idCbor, "hex"),
    }),
  );
  const { settlementEventInfo } = await Effect.runPromise(
    SDK.classifyWithdrawalFromLedger({
      // Body field 1, a 28-byte string: its bytes follow the `581c` head.
      l2Owner: plutusConstrFieldCbor(info, [0, 1]).slice(4),
      l2ValueCbor: plutusConstrFieldCbor(info, [0, 2]),
      eventInfoCbor: info,
      ledgerOutRef,
      ledgerOutput: null,
    }),
  );
  const settlement = settlementEventInfo.toString("hex");
  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: {
      withdrawal_id: {
        transactionId: nonce.txHash.toString("hex"),
        outputIndex: BigInt(nonce.index),
      },
    },
  };
  const base = await emptyBlock();
  const counts = countsFromLengths({ withdrawals: 1 });
  const block = await rebind({
    ...base,
    block_body: {
      ...base.block_body,
      header: { ...base.block_body.header, ...counts },
      counts,
      withdrawals: [[event.idCbor, settlement]],
      transition_trace: [
        [
          encodeTransitionIntegerCbor(0n).toString("hex"),
          encodeTransitionStepCbor({
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "Withdrawal",
            pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
            post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          }).toString("hex"),
        ],
      ],
      event_to_step: [
        [
          (await Effect.runPromise(eventKeyCbor(eventKey))).toString("hex"),
          encodeEventToStepValueCbor({
            step_index: 0n,
            phase: "Withdrawal",
          }).toString("hex"),
        ],
      ],
    },
  });
  return { block, idCbor: event.idCbor, settlement };
};

describe("foreign replay of an in-window event ingestion leaves out", () => {
  it.each([
    { name: "a foreign L2 network id", options: { l2NetworkId: 7n } },
    {
      name: "a pointer L2 stake credential",
      options: {
        l2Address: {
          paymentCredential: { PublicKeyCredential: ["cd".repeat(28)] },
          stakeCredential: {
            Pointer: {
              slotNumber: 1n,
              transactionIndex: 2n,
              certificateIndex: 3n,
            },
          },
        },
      },
    },
  ] satisfies readonly {
    name: string;
    options: Parameters<typeof eventOrder>[2];
  }[])(
    "judges a block without the deposit with $name, as ingestion does",
    async ({ options }) => {
      const { store, chain } = await follow();
      const left = eventOrder("deposit", nonceRef(), {
        ...options,
        inclusionTime: 2n,
      });
      const kept = eventOrder("deposit", nonceRef(), { inclusionTime: 2n });
      await chain.forward([admissionTx(left, chain.chain.nonce())]);
      await chain.forward([admissionTx(kept, chain.chain.nonce())]);
      const leftId = idKey(left.nonce);
      const { events, ingestion } = await ingestedDeposits(store);
      expect(ingestion).toMatchObject({ inserted: 1 });
      expect(ingestion.refused).toMatchObject([
        { kind: "deposit", idCbor: leftId, reason: EVENT_UNDECODABLE },
      ]);
      const [read, ...more] = await depositsAt(store);
      expect(more).toEqual([]);
      expect(read!.entry.idCbor).toBe(idKey(kept.nonce));
      // The block ingestion would build leaves the deposit out, and replays.
      const block = await depositBlock(read!.entry);
      const replayer = replayerFor(store);
      expect(await replayedBy(replayer, store, block)).toMatchObject({
        kind: "replayed",
        depositIds: [Buffer.from(idKey(kept.nonce), "hex")],
      });
      // Ingestion never includes it, so a block naming it is invalid.
      const event = events.find(({ idCbor }) => idCbor === leftId)!;
      const info = aikenSerialisedPlutusDataCborPreservingMapOrder(
        plutusConstrFieldCbor(event.payloadCbor, [0, 1]),
      );
      const named = await naming(block, {
        deposits: [...block.block_body.deposits, [leftId, info]],
      });
      expect(await replayedBy(replayer, store, named)).toEqual({
        kind: "invalid",
        detail: `deposit ${leftId} is not a user event the node reads: ${ingestion.refused[0]!.detail}`,
      });
    },
  );
});

/** Replays the honest block naming an in-window withdrawal `order`. */
const replaysWithdrawal = async (order: ReturnType<typeof withdrawalOrder>) => {
  const { store, chain } = await follow();
  await chain.forward([admissionTx(order, chain.chain.nonce())]);
  const { block, idCbor, settlement } = await withdrawalBlock(
    store,
    order.nonce,
  );
  expect(await replayedBy(replayerFor(store), store, block)).toMatchObject({
    kind: "replayed",
    root: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawals: [
      { id: idCbor, validity: "NonExistentWithdrawalUtxo", settlement },
    ],
  });
};

const payloadOf = (order: ReturnType<typeof withdrawalOrder>) =>
  plutusConstrFieldCbor(order.order.datum!.toString("hex"), PAYLOAD_IN_ORDER);

describe("foreign replay of a deeply nested withdrawal datum", () => {
  it.each([
    { name: "a shallow datum", datum: "00" },
    ...DEEP_PLUTUS_DATA_CASES.map((deep) => ({
      name: `a ${deep.shape} datum ${deep.depth} deep (${deep.bound})`,
      datum: deepPlutusDataCbor(deep),
    })),
  ])("replays the block naming a withdrawal with $name", ({ datum }) =>
    replaysWithdrawal(withdrawalOrder(datum)),
  );

  // Guard rows: these hold with a recursive decoder as well.
  it.each(["list", "constr"] as const)(
    "replays the block naming a withdrawal whose %s datum is nested to the admission bound",
    async (shape) => {
      const depth = depthAtAdmissionBound(payloadOf(withdrawalOrder("00")));
      const order = withdrawalOrder(deepPlutusDataCbor({ shape, depth }));
      expect(plutusDataNodeCount(payloadOf(order))).toBe(
        ADMISSION_PAYLOAD_NODES,
      );
      await replaysWithdrawal(order);
    },
  );
});
