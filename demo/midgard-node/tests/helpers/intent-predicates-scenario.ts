/**
 * The node's §8.4 family predicates (`nodeFamilyPredicate`) over a follower
 * store in the node's own test database, so a predicate reads the node
 * tables (block journal, event rows) beside the follower's facts:
 *
 * - a simulated chain carrying the landed state queue (root, one node
 *   with header `first`, plain outputs at the queue address for intents
 *   to spend) and an operator directory (three lists, the scheduler, the
 *   hub oracle) of a real deployment;
 * - `record` journals a simulated transaction; `verdict` derives its
 *   journaled state and runs the production predicate on it.
 */
import {
  deriveIntentStatusIn,
  type FactStore,
  intentJournalProjection,
  type OutRef,
  recordIntentIn,
  type TrackedSet,
} from "@al-ft/midgard-l1-follower";
import { decodeTransaction } from "@al-ft/midgard-l1-follower";
import {
  encodeSimTx,
  SIM_ORIGIN,
  type SimTx,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";

import {
  operatorSetProjection,
  operatorSetTrackedSet,
} from "../../src/l1-operator-set/index.js";
import {
  stateQueueProjection,
  stateQueueTrackedSet,
} from "../../src/l1-state-queue/index.js";
import {
  nodeFamilyPredicate,
  type NodeFamilyPredicateDeps,
} from "../../src/services/l1-follower.intent-predicates.js";
import { resetApplicationTables } from "../utils.js";
import { db, openNodeFollowerStore } from "./forced-orders-node-store.js";
import { ChainDriver } from "./l1-events-store.js";
import {
  operatorKey,
  OperatorSetChain,
  type OperatorSetChainFixture,
  txOf,
  type TxParts,
} from "./operator-set-chain.js";
import {
  GENESIS_HASH,
  NODE_PREFIX,
  nodeDatum,
  QUEUE_ADDRESS,
  queueOutput,
  ROOT_ASSET,
  rootDatum,
  SIM_QUEUE_CONFIG,
  simHeader,
} from "./state-queue-sim.fixtures.js";

export const PREDICATE_K = 6;
export const OWN = operatorKey(0x50);
export const FOREIGN = operatorKey(0x20);
const SPARES = 8;

const union = (...sets: readonly TrackedSet[]): TrackedSet => ({
  addresses: new Set(sets.flatMap((set) => [...set.addresses])),
  paymentCredentials: new Set(
    sets.flatMap((set) => [...set.paymentCredentials]),
  ),
  policies: new Set(sets.flatMap((set) => [...set.policies])),
});

export const outRefText = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

export type PredicateScenario = Awaited<
  ReturnType<typeof openPredicateScenario>
>;

export const openPredicateScenario = async (
  fixture: OperatorSetChainFixture,
  opened: FactStore[],
) => {
  await db(resetApplicationTables);
  const store = await openNodeFollowerStore(
    [
      stateQueueProjection(SIM_QUEUE_CONFIG),
      operatorSetProjection(fixture.config),
      intentJournalProjection,
    ],
    PREDICATE_K,
  );
  opened.push(store);
  const driver = new ChainDriver(
    store,
    union(
      stateQueueTrackedSet(SIM_QUEUE_CONFIG),
      operatorSetTrackedSet(fixture.config),
    ),
  );
  await driver.init();
  const nonce = () => driver.chain.nonce();
  const lists = new OperatorSetChain(fixture);
  const live = () => driver.chain.live();
  const land = (...parts: TxParts[]) => driver.forward([txOf(parts, nonce())]);
  await land(lists.genesis(driver.chain.outsideInput()));

  const plain = { address: QUEUE_ADDRESS, lovelace: 3_000_000n };
  const [rootTx] = await driver.forward([
    {
      inputs: [driver.chain.outsideInput()],
      outputs: [
        queueOutput(ROOT_ASSET, rootDatum(GENESIS_HASH, null)),
        ...Array.from({ length: SPARES }, () => plain),
      ],
      nonce: nonce(),
    },
  ]);
  const first = simHeader(1, GENESIS_HASH);
  const firstHash = SDK.stateQueueHeaderHash(first);
  const [queueTx] = await driver.forward([
    {
      inputs: [{ txHash: rootTx!, index: 0 }],
      outputs: [
        queueOutput(ROOT_ASSET, rootDatum(GENESIS_HASH, firstHash)),
        queueOutput(
          NODE_PREFIX + firstHash,
          nodeDatum(first, "Unattested", null),
        ),
      ],
      nonce: nonce(),
    },
  ]);
  const root: OutRef = { txHash: queueTx!, index: 0 };
  const head: OutRef = { txHash: queueTx!, index: 1 };
  const spare = (i: number): OutRef => ({ txHash: rootTx!, index: 1 + i });
  /** An own transaction spending `inputs` back to the queue address. */
  const spend = (
    inputs: readonly OutRef[],
    extra: Partial<SimTx> = {},
  ): SimTx => ({
    inputs,
    outputs: [plain],
    nonce: nonce(),
    ...extra,
  });

  const record = async (
    family: string,
    workflowKey: string,
    tx: SimTx,
    contentRef: Buffer | null,
  ): Promise<Buffer> => {
    const txCbor = encodeSimTx(tx);
    const result = await store.transaction("write", (sqlTx) =>
      recordIntentIn(sqlTx, store.dialect, {
        family,
        workflowKey,
        txCbor,
        isOwnOutput: () => false,
        contentRef,
      }),
    );
    if (result.kind !== "recorded") throw new Error(`record: ${result.kind}`);
    return decodeTransaction(txCbor).hash;
  };

  const deps = (
    overrides: Partial<NodeFamilyPredicateDeps> = {},
  ): NodeFamilyPredicateDeps => ({
    store,
    stateQueue: SIM_QUEUE_CONFIG,
    operatorSet: { config: fixture.config, ownKey: OWN },
    // POSIX time counts from the simulated origin, so a shift starting at
    // 0 covers the scenario's first hour.
    slotToPosixMs: (slot) => (slot - SIM_ORIGIN.point.slot) * 1000,
    horizonLagBlocks: 2,
    ...overrides,
  });

  /** The production predicate's verdict on the journaled `txHash`. */
  const verdict = async (
    txHash: Buffer,
    overrides: Partial<NodeFamilyPredicateDeps> = {},
  ): Promise<boolean> => {
    const { state } = await store.transaction("read", (tx) =>
      deriveIntentStatusIn(tx, store.dialect, txHash),
    );
    if (state === null) throw new Error("not journaled");
    return nodeFamilyPredicate(deps(overrides))(state);
  };

  /** Runs SQL against the node database through the store. */
  const sql = (text: string, params: readonly unknown[] = []) =>
    store.transaction("write", (tx) => tx.query(text, params as never));

  return {
    store,
    driver,
    lists,
    live,
    land,
    nonce,
    first,
    firstHash,
    root,
    head,
    spare,
    spend,
    record,
    verdict,
    sql,
  };
};
