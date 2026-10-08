/**
 * The production foreign replayer's test bench (N3): a follower store with
 * the event and forced-order projections in the node's database, a
 * simulated chain that admits events and orders into it, real DA payloads
 * (an empty block, an honest deposit block, an honest forced-transaction
 * block) and the node services the replayer runs under.
 */
import { outRefToCbor } from "@al-ft/lucid-midgard";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import type {
  FactStore,
  FactStoreOptions,
  OutRef,
  TrackedSet,
} from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventTrackedSet,
} from "@al-ft/midgard-l1-follower/events";
import { eventsAt } from "@al-ft/midgard-l1-follower/events";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { vi } from "vitest";

import { foreignRetainedDaInsert } from "../src/da/foreign-retained-da.js";
import { DaPayloadsDB } from "../src/database/index.js";
import {
  forcedOrderProjection,
  forcedOrderTrackedSet,
} from "../src/forced-orders/index.js";
import { userEventEntry } from "../src/l1-events/entries.js";
import type { ReplayInput } from "../src/landed-blocks/replay.js";
import { replayForeignBlock } from "../src/landed-blocks/replay-foreign.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { eventKeyCbor } from "../src/mpf/trace-events.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import { NodeConfig } from "../src/services/config.js";
import type { Database } from "../src/services/database.js";
import * as Producer from "../src/services/event-history-producer.js";
import { withHistoryWrite } from "../src/services/event-history-producer.js";
import { Globals } from "../src/services/globals.js";
import { Lucid } from "../src/services/lucid.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import { countsFromLengths, headerFor } from "./da-payload.record.js";
import { fixture, rebind } from "./foreign-block-import.fixture.js";
import {
  FORCED_CONFIG,
  inlineOrderMaterial,
  orderTx,
} from "./helpers/forced-orders-chain.js";
import { openNodeFollowerStore } from "./helpers/forced-orders-node-store.js";
import { EVENTS_CONFIG, listOf } from "./helpers/l1-events-chain.js";
import { ChainDriver } from "./helpers/l1-events-store.js";
import { provideDatabaseLayers } from "./utils.js";

export const K = 2;
export const NETWORK = "Preprod" as const;

const union = (...sets: TrackedSet[]): TrackedSet => ({
  addresses: new Set(sets.flatMap((set) => [...set.addresses])),
  paymentCredentials: new Set(
    sets.flatMap((set) => [...set.paymentCredentials]),
  ),
  policies: new Set(sets.flatMap((set) => [...set.policies])),
});

/** A follower store in the node database with both projections, and its chain. */
export const followChain = async (extra: Partial<FactStoreOptions> = {}) => {
  const store = await openNodeFollowerStore(
    [eventProjection(EVENTS_CONFIG), forcedOrderProjection(FORCED_CONFIG)],
    K,
    extra,
  );
  const chain = new ChainDriver(
    store,
    union(eventTrackedSet(EVENTS_CONFIG), forcedOrderTrackedSet(FORCED_CONFIG)),
  );
  await chain.init();
  return { store, chain };
};

let nonces = 0;
/** A fresh outside input (an event's or an order's id). */
export const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xd7);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: 0 };
};

/** The id the forced fixture's block names (`fixture()`'s default order). */
export const FIXTURE_ORDER_ID: OutRef = {
  txHash: Buffer.alloc(32, 0x5a),
  index: 0,
};

/** The honest forced-transaction block (`foreign-block-import.fixture.ts`). */
export const forcedBlock = () => fixture();

/** An order tx admitting the fixture's forced transaction under `id`. */
export const fixtureOrderTx = async (
  chain: ChainDriver,
  id: OutRef,
  inclusionTime = 2n,
): Promise<SimTx> => {
  const block = await forcedBlock();
  const cbor = Buffer.from(
    block.block_body.forced_transaction_preimages[0]![1],
    "hex",
  );
  const { material, inline } = inlineOrderMaterial(cbor);
  return orderTx({
    material,
    nonceInput: id,
    carriage: [],
    inline,
    inclusionTime,
    nonce: chain.chain.nonce(),
  });
};

/** The order-id key a block body names an order (or event) by. */
export const idKey = (outRef: OutRef): string =>
  Data.to(
    {
      transactionId: outRef.txHash.toString("hex"),
      outputIndex: BigInt(outRef.index),
    },
    SDK.OutputReference,
  );

const emptyBody = (): SDK.DaPayload["block_body"] => ({
  header_hash: "00".repeat(28),
  header: headerFor(
    Object.fromEntries(
      [
        "utxosRoot",
        "withdrawalsRoot",
        "forcedTransactionsRoot",
        "transactionsRoot",
        "depositsRoot",
        "transitionTraceRoot",
        "eventToStepRoot",
        "validationTracesRoot",
      ].map((key) => [key, SDK.EMPTY_MERKLE_TREE_ROOT]),
    ) as Parameters<typeof headerFor>[0],
    countsFromLengths({}),
  ),
  utxos: [],
  deposits: [],
  withdrawals: [],
  transactions: [],
  transaction_preimages: [],
  cek_program_material: [],
  forced_transactions: [],
  forced_transaction_preimages: [],
  transition_trace: [],
  event_to_step: [],
  validation_traces: [],
  validation_trace_witnesses: [],
  counts: countsFromLengths({}),
});

/** A block with no events: honest on an empty parent. */
export const emptyBlock = () =>
  rebind({ version: SDK.DA_PAYLOAD_VERSION, block_body: emptyBody() });

/** `payload` with its body's event lists replaced (roots rebound). */
export const naming = (
  payload: SDK.DaPayload,
  lists: Partial<
    Pick<
      SDK.DaPayload["block_body"],
      "deposits" | "withdrawals" | "forced_transactions"
    >
  >,
) => rebind({ ...payload, block_body: { ...payload.block_body, ...lists } });

/** `payload` with `list`'s first entry named again. No root can commit a
 * list with a repeated key, so the header keeps the honest roots: the
 * replayer's set check runs before any root check. */
export const repeating = (
  payload: SDK.DaPayload,
  list: "deposits" | "withdrawals" | "forced_transactions",
): SDK.DaPayload => ({
  ...payload,
  block_body: {
    ...payload.block_body,
    [list]: [...payload.block_body[list], payload.block_body[list][0]!],
  },
});

/** The deposits the view knows, decoded the way the replayer decodes them. */
export const depositsAt = async (store: FactStore) => {
  const view = (await store.currentView())!;
  const read = await eventsAt(store, listOf("deposit"), view.point);
  if (read.kind !== "ok") throw new Error(`eventsAt: ${read.kind}`);
  return read.value.map((event) => {
    const decoded = userEventEntry(event, NETWORK);
    if (decoded.kind !== "deposit") throw new Error("not a deposit");
    return { event, entry: decoded.entry };
  });
};

/** The honest block that inserts one deposit on an empty parent. */
export const depositBlock = async (entry: {
  readonly idCbor: string;
  readonly infoCbor: string;
  readonly ledgerOutput: string;
}) => {
  const id = Data.from(entry.idCbor, SDK.OutputReference);
  const key = outRefToCbor({
    txHash: id.transactionId,
    outputIndex: Number(id.outputIndex),
  });
  const output = Buffer.from(entry.ledgerOutput, "hex");
  const root = await Effect.runPromise(
    computeLedgerMpfRootFromLedgerEntries([{ outref: key, output }]),
  );
  const eventKey: SDK.EventKey = { DepositEventKey: { deposit_id: id } };
  const eventCbor = (await Effect.runPromise(eventKeyCbor(eventKey))).toString(
    "hex",
  );
  const body = emptyBody();
  const counts = countsFromLengths({ deposits: 1 });
  return rebind({
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      ...body,
      header: { ...body.header, ...counts },
      counts,
      utxos: [[key.toString("hex"), entry.ledgerOutput]],
      deposits: [[entry.idCbor, entry.infoCbor]],
      transition_trace: [
        [
          encodeTransitionIntegerCbor(0n).toString("hex"),
          encodeTransitionStepCbor({
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "Deposit",
            pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
            post_utxos_root: root,
          }).toString("hex"),
        ],
      ],
      event_to_step: [
        [
          eventCbor,
          encodeEventToStepValueCbor({
            step_index: 0n,
            phase: "Deposit",
          }).toString("hex"),
        ],
      ],
    },
  });
};

export const envelope = (payload: SDK.DaPayload) =>
  wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" });

/** The DA row the node retains for `payload`, optionally altered. */
export const retainedRow = async (
  payload: SDK.DaPayload,
  alter: (row: DaPayloadsDB.InsertInput) => DaPayloadsDB.InsertInput = (row) =>
    row,
) =>
  alter(
    foreignRetainedDaInsert(
      payload.block_body.header_hash,
      payload.block_body.header,
      await envelope(payload),
    ),
  );

export const MANIFEST_ID = "de".repeat(32);

export const identity = ContractDeploymentIdentity.make({
  kind: "manifest",
  manifestId: MANIFEST_ID,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  deploymentMarker: makeDeploymentMarker(MANIFEST_ID),
});

/** The L1 clock the replayer reads: POSIX ms of a slot, shifted by `offsetMs`. */
export const clock = { offsetMs: 0 };

const lucid = {
  api: {
    slotToUnixTime: (slot: number) => clock.offsetMs + slot * 1_000,
    config: () => ({
      slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1_000 },
    }),
  },
} as never;

/** Runs a node effect with the replayer's services (zero fees, `NETWORK`). */
export const inNode = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    Database | NodeConfig | Lucid | ContractDeploymentIdentity | Globals
  >,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.flatMap(NodeConfig, (config) =>
        effect.pipe(
          Effect.provideService(NodeConfig, {
            ...config,
            NETWORK,
            MIN_FEE_A: 0n,
            MIN_FEE_B: 0n,
          }),
        ),
      ).pipe(
        Effect.provideService(Lucid, lucid),
        Effect.provideService(ContractDeploymentIdentity, identity),
        Effect.provide(Globals.Default),
      ),
    ),
  );

/** The replay input for `payload` on an empty parent at the store's view. */
export const inputFor = async (
  store: FactStore,
  payload: SDK.DaPayload,
): Promise<ReplayInput> => {
  const header = payload.block_body.header;
  return {
    headerHash: payload.block_body.header_hash,
    header,
    parentHeaderHash: header.prevHeaderHash,
    parentUtxosRoot: header.prevUtxosRoot,
    parentEntries: [],
    view: (await store.currentView())!,
  };
};

/** The production replayer over `store`; the follower keeps one across runs. */
export const replayerFor = (store: FactStore) =>
  replayForeignBlock({
    store,
    events: EVENTS_CONFIG,
    forcedOrders: FORCED_CONFIG,
  });

/** Replays `payload` with a fresh production replayer at the store's view. */
export const replay = async (store: FactStore, payload: SDK.DaPayload) =>
  inNode(replayerFor(store)(await inputFor(store, payload)));

/** The node's event-history owner is out of scope here: its producer
 * registration runs the work as is, so history writes (the replayer keeping
 * a fetched payload) go through the explicit test-fixture gate. */
export const unownedHistory = () =>
  vi
    .spyOn(Producer, "runHistoryProducer")
    .mockImplementation((work) => work as never);

/** Retains `row` through the history-write gate. */
export const retain = (row: DaPayloadsDB.InsertInput) =>
  inNode(withHistoryWrite(DaPayloadsDB.upsertAvailable(row)));

export const retained = (payload: SDK.DaPayload) =>
  inNode(
    DaPayloadsDB.retrieveByHeaderHash(
      Buffer.from(payload.block_body.header_hash, "hex"),
    ),
  );
