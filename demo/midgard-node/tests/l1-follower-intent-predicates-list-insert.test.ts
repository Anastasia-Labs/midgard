/**
 * The list-insert family (`list_insert`) on the node intent journal over the
 * follower's facts, in the node's own test database, for both in-node
 * paths: the genesis deposit (`list_insert:deposit:<event key>`) and the
 * atomic protocol initialization (`list_insert:protocol_init:<nonce>`).
 *
 * - Each is recorded through the production journal and resent by S6 while
 *   its §8.4 predicate reads its target unreached: the deposit's event key
 *   absent from the follower's key set, no live output carrying the
 *   state-queue policy.
 * - Once a landed transaction that spends none of its inputs reaches the
 *   target (admits the event key, creates the queue root), the predicate is
 *   false: S6 abandons the intent and never sends it again.
 * - A record the journal refuses raises the family's named `/readyz` hold,
 *   which the family's next successful record clears.
 */
import {
  decodeTransaction,
  type FactStore,
  intentJournalProjection,
  type OutRef,
  type TrackedSet,
} from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventTrackedSet,
} from "@al-ft/midgard-l1-follower/events";
import { encodeSimTx, type SimTx } from "@al-ft/midgard-l1-follower/testing";
import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  stateQueueProjection,
  stateQueueTrackedSet,
} from "../src/l1-state-queue/index.js";
import {
  INTENT_INPUT_UNTRACKED,
  intentJournalOver,
  type IntentJournalService,
  journaledIntent,
} from "../src/services/intent-journal.js";
import { nodeFamilyPredicate } from "../src/services/l1-follower.intent-predicates.js";
import { createNodeIntentStage } from "../src/services/l1-follower.intents.js";
import {
  db,
  openNodeFollowerStore,
} from "./helpers/forced-orders-node-store.js";
import {
  admissionTx,
  eventKeyOf,
  eventOrder,
  EVENTS_CONFIG,
} from "./helpers/l1-events-chain.js";
import { ChainDriver } from "./helpers/l1-events-store.js";
import {
  GENESIS_HASH,
  QUEUE_ADDRESS,
  queueOutput,
  ROOT_ASSET,
  rootDatum,
  SIM_QUEUE_CONFIG,
} from "./helpers/state-queue-sim.fixtures.js";
import { resetApplicationTables } from "./utils.js";

const K = 6;
const SPARES = 6;
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

const union = (...sets: readonly TrackedSet[]): TrackedSet => ({
  addresses: new Set(sets.flatMap((set) => [...set.addresses])),
  paymentCredentials: new Set(
    sets.flatMap((set) => [...set.paymentCredentials]),
  ),
  policies: new Set(sets.flatMap((set) => [...set.policies])),
});

const outRefText = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

/**
 * A follower store with the state-queue and event projections and the
 * intent journal, `SPARES` tracked plain outputs, the production journal
 * over the node database, and S6 with a transport that records each send.
 */
const open = async () => {
  await db(resetApplicationTables);
  const store = await openNodeFollowerStore(
    [
      stateQueueProjection(SIM_QUEUE_CONFIG),
      eventProjection(EVENTS_CONFIG),
      intentJournalProjection,
    ],
    K,
  );
  opened.push(store);
  const driver = new ChainDriver(
    store,
    union(
      stateQueueTrackedSet(SIM_QUEUE_CONFIG),
      eventTrackedSet(EVENTS_CONFIG),
    ),
  );
  await driver.init();
  const plain = { address: QUEUE_ADDRESS, lovelace: 3_000_000n };
  const [funding] = await driver.forward([
    {
      inputs: [driver.chain.outsideInput()],
      outputs: Array.from({ length: SPARES }, () => plain),
      nonce: driver.chain.nonce(),
    },
  ]);
  const spare = (i: number): OutRef => ({ txHash: funding!, index: i });
  const spend = (inputs: readonly OutRef[]): SimTx => ({
    inputs,
    outputs: [plain],
    nonce: driver.chain.nonce(),
  });
  const sent: string[] = [];
  const stage = createNodeIntentStage({
    store,
    transport: {
      hasTx: () => Promise.resolve(false),
      submit: (bytes) => {
        sent.push(Buffer.from(bytes).toString("hex"));
        return Promise.resolve({ accepted: true } as const);
      },
      withLedgerState: () => Promise.reject(new Error("no ledger state")),
    },
    securityParameter: K,
    seededAddresses: [],
    wanted: nodeFamilyPredicate({
      store,
      stateQueue: SIM_QUEUE_CONFIG,
      operatorSet: null,
      slotToPosixMs: (slot) => slot * 1000,
      horizonLagBlocks: 0,
    }),
    log: () => undefined,
  });
  return { store, driver, spare, spend, sent, stage };
};

/** Runs `body` with the production journal and a second one over the node database. */
const withJournals = <A>(
  body: (
    journals: Readonly<{
      worker: IntentJournalService;
      main: IntentJournalService;
    }>,
  ) => Promise<A>,
): Promise<A> =>
  db(
    Effect.flatMap(SqlClient.SqlClient, (sql) =>
      Effect.promise(() =>
        body({
          worker: intentJournalOver(sql, () => false),
          main: intentJournalOver(sql, () => false),
        }),
      ),
    ),
  );

const bytesOf = (tx: SimTx) => {
  const cbor = Buffer.from(encodeSimTx(tx));
  return {
    cbor: cbor.toString("hex"),
    txHash: decodeTransaction(cbor).hash.toString("hex"),
  };
};

/** Records `tx` as a list insert under `workflowKey` through the journal. */
const record = (
  journal: IntentJournalService,
  workflowKey: string,
  tx: SimTx,
) => {
  const { cbor, txHash } = bytesOf(tx);
  return Effect.runPromise(
    Effect.either(
      Effect.flatMap(journal.openPlan, (plan) =>
        journal.record(
          journaledIntent("list_insert", workflowKey, plan),
          cbor,
          txHash,
          { kind: "record_only" },
        ),
      ),
    ),
  );
};

type Scenario = Awaited<ReturnType<typeof open>>;

/**
 * The two in-node list inserts: the workflow key of the intent spending
 * `nonce`, and a landed transaction spending `other` (none of the intent's
 * inputs) that reaches the insert's target.
 */
const PATHS = [
  {
    path: "the genesis deposit",
    key: (nonce: OutRef) => `list_insert:deposit:${eventKeyOf(nonce)}`,
    reach: (s: Scenario, nonce: OutRef, other: OutRef): SimTx => ({
      ...admissionTx(eventOrder("deposit", nonce), s.driver.chain.nonce()),
      inputs: [other],
    }),
  },
  {
    path: "the protocol initialization",
    key: (nonce: OutRef) => `list_insert:protocol_init:${outRefText(nonce)}`,
    reach: (s: Scenario, _nonce: OutRef, other: OutRef): SimTx => ({
      inputs: [other],
      outputs: [queueOutput(ROOT_ASSET, rootDatum(GENESIS_HASH, null))],
      nonce: s.driver.chain.nonce(),
    }),
  },
] as const;

describe.each(PATHS)("the list insert of $path", ({ key, reach }) => {
  it("is journaled and resent while the facts show its target unreached, and abandoned unsent once a landed tx that spends none of its inputs reaches it", async () => {
    const s = await open();
    const nonce = s.spare(0);
    const intent = s.spend([nonce]);
    const { cbor } = bytesOf(intent);
    await withJournals(async ({ worker }) => {
      const recorded = await record(worker, key(nonce), intent);
      expect(Either.isRight(recorded) && recorded.right.kind).toBe("recorded");
    });

    // The first send was lost: S6 sends the journaled bytes.
    expect(await s.stage.run()).toEqual([]);
    const txHash = Buffer.from(bytesOf(intent).txHash, "hex");
    expect(s.stage.lastReport()!.entry(txHash)).toMatchObject({
      action: "resubmit",
      status: { kind: "live", inputsAvailable: true },
      intent: { family: "list_insert", workflowKey: key(nonce) },
    });
    expect(s.sent).toEqual([cbor]);

    // An unrelated landed tx leaves the target unreached: sent again.
    await s.driver.forward([s.spend([s.spare(2)])]);
    s.sent.length = 0;
    expect(await s.stage.run()).toEqual([]);
    expect(s.stage.lastReport()!.entry(txHash)!.action).toBe("resubmit");
    expect(s.sent).toEqual([cbor]);

    // Another tx reaches the target without spending the intent's inputs.
    await s.driver.forward([reach(s, nonce, s.spare(1))]);
    s.sent.length = 0;
    expect(await s.stage.run()).toEqual([]);
    expect(s.stage.lastReport()!.entry(txHash)).toMatchObject({
      action: "abandon",
    });
    expect(s.sent).toEqual([]);
    await s.driver.forward([]);
    await s.stage.run();
    expect(s.stage.lastReport()!.entry(txHash)).toMatchObject({
      status: { kind: "abandoned" },
    });
    expect(s.sent).toEqual([]);
    s.stage.close();
  });

  it("a refused record raises the family's named hold, which the family's next record clears", async () => {
    const s = await open();
    await withJournals(async ({ worker, main }) => {
      // An input the follower does not track: §8.2 refuses it.
      const untracked = s.driver.chain.outsideInput();
      const refused = await record(
        worker,
        key(untracked),
        s.spend([untracked]),
      );
      expect(Either.isLeft(refused) && refused.left).toMatchObject({
        reason: INTENT_INPUT_UNTRACKED,
      });
      await Effect.runPromise(main.refresh());
      expect(main.holds()).toMatchObject([{ reason: INTENT_INPUT_UNTRACKED }]);
      expect(main.holds()[0]!.detail).toContain(
        `list_insert ${key(untracked)}`,
      );

      const nonce = s.spare(0);
      const recorded = await record(worker, key(nonce), s.spend([nonce]));
      expect(Either.isRight(recorded)).toBe(true);
      expect(worker.holds()).toEqual([]);
      await Effect.runPromise(main.refresh());
      expect(main.holds()).toEqual([]);
    });
    s.stage.close();
  });
});
