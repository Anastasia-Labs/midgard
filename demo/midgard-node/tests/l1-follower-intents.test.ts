/**
 * The node follower's intent stage (plan §8.3 S6, §8.4, I1) over a
 * simulated chain on a SQLite and a Postgres follower store carrying the
 * landed state queue (P1) and the intent journal:
 *
 * - a live, wanted intent the mempool lacks gets its exact journaled bytes,
 *   at most once per tip; one in the mempool is left alone;
 * - a header family (attestation, correction, merge) whose header P1 no
 *   longer holds is abandoned; one whose header P1 holds is sent; every
 *   other family reads no projection;
 * - a landed intent, and one a foreign transaction beat to an input, are
 *   never sent;
 * - an unhealthy queue, a failed mempool read, a failed pass and an owed
 *   wallet seed are named holds, and nothing is abandoned for them.
 */
import {
  decodeTransaction,
  type FactStore,
  intentJournalProjection,
  type OutRef,
  readIntentEventsIn,
  recordIntentIn,
  WALLET_SEED_PENDING,
} from "@al-ft/midgard-l1-follower";
import { encodeSimTx, type SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  stateQueueProjection,
  stateQueueTrackedSet,
} from "../src/l1-state-queue/index.js";
import {
  createNodeIntentStage,
  INTENT_RECONCILE_FAILED,
  INTENT_RECONCILE_TRANSIENT,
  nodeFamilyPredicate,
  nodeIntentTrackedSet,
} from "../src/services/l1-follower.intents.js";
import {
  ChainDriver,
  storeOpener,
  testDatabases,
} from "./helpers/l1-events-store.js";
import {
  nodeDatum,
  QUEUE_ADDRESS,
  queueOutput,
  rootDatum,
  SIM_QUEUE_CONFIG,
  simHeader,
} from "./helpers/state-queue-sim.fixtures.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const K = 6;
const GENESIS = "00".repeat(28);
const ROOT = SDK.STATE_QUEUE_ROOT_ASSET_NAME;
const PREFIX = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX;
const SPARES = 6;

type Transport = {
  readonly sent: Buffer[];
  readonly mempool: Set<string>;
  failMempool: boolean;
  hasTx(txId: string): Promise<boolean>;
  submit(
    bytes: Uint8Array,
  ): Promise<{ accepted: true } | { accepted: false; rejection: Uint8Array }>;
  withLedgerState(): Promise<never>;
};

const fakeTransport = (): Transport => {
  const transport: Transport = {
    sent: [],
    mempool: new Set(),
    failMempool: false,
    hasTx: (txId) =>
      transport.failMempool
        ? Promise.reject(new Error("mempool read failed"))
        : Promise.resolve(transport.mempool.has(txId)),
    submit: (bytes) => {
      transport.sent.push(Buffer.from(bytes));
      return Promise.resolve({ accepted: true });
    },
    withLedgerState: () => Promise.reject(new Error("no ledger state")),
  };
  return transport;
};

/** A queue (root plus one node, header `first`) and `SPARES` plain outputs at the tracked address. */
const openScenario = async (dialect: "sqlite" | "postgres") => {
  const store = await storeOpener(dialect, databases)(
    [stateQueueProjection(SIM_QUEUE_CONFIG), intentJournalProjection],
    K,
  );
  opened.push(store);
  const chain = new ChainDriver(store, stateQueueTrackedSet(SIM_QUEUE_CONFIG));
  await chain.init();
  const nonce = () => chain.chain.nonce();
  const plain = { address: QUEUE_ADDRESS, lovelace: 3_000_000n };
  const [rootTx] = await chain.forward([
    {
      inputs: [chain.chain.outsideInput()],
      outputs: [
        queueOutput(ROOT, rootDatum(GENESIS, null)),
        ...Array.from({ length: SPARES }, () => plain),
      ],
      nonce: nonce(),
    },
  ]);
  const first = simHeader(1, GENESIS);
  const firstHash = SDK.stateQueueHeaderHash(first);
  await chain.forward([
    {
      inputs: [{ txHash: rootTx!, index: 0 }],
      outputs: [
        queueOutput(ROOT, rootDatum(GENESIS, firstHash)),
        queueOutput(PREFIX + firstHash, nodeDatum(first, "Unattested", null)),
      ],
      nonce: nonce(),
    },
  ]);
  const spare = (i: number): OutRef => ({ txHash: rootTx!, index: 1 + i });
  /** An own transaction spending `input` back to the tracked address. */
  const spend = (input: OutRef): SimTx => ({
    inputs: [input],
    outputs: [plain],
    nonce: nonce(),
  });
  const record = async (
    family: string,
    tx: SimTx,
    contentRef: string | null = null,
  ): Promise<Buffer> => {
    const txCbor = encodeSimTx(tx);
    const result = await store.transaction("write", (sqlTx) =>
      recordIntentIn(sqlTx, store.dialect, {
        family,
        workflowKey: `${family}:test`,
        txCbor,
        isOwnOutput: () => false,
        contentRef: contentRef === null ? null : Buffer.from(contentRef, "hex"),
      }),
    );
    expect(result.kind).toBe("recorded");
    return txCbor;
  };
  const transport = fakeTransport();
  const logs: string[] = [];
  const stage = (seededAddresses: readonly Buffer[] = []) =>
    createNodeIntentStage({
      store,
      transport,
      securityParameter: K,
      seededAddresses,
      wanted: nodeFamilyPredicate(store, SIM_QUEUE_CONFIG),
      log: (line) => logs.push(line),
    });
  const events = async (txCbor: Buffer) =>
    (
      await store.transaction("read", (sqlTx) =>
        readIntentEventsIn(sqlTx, decodeTransaction(txCbor).hash),
      )
    ).map((event) => event.kind);
  return {
    store,
    chain,
    nonce,
    first,
    firstHash,
    spare,
    spend,
    record,
    transport,
    logs,
    stage,
    events,
  };
};

const txId = (txCbor: Buffer): string =>
  decodeTransaction(txCbor).hash.toString("hex");

describe.each(["sqlite", "postgres"] as const)(
  "the node intent stage over a %s follower store",
  (dialect) => {
    it("resubmits the journaled bytes of live, wanted intents once per tip and abandons a header family P1 no longer holds", async () => {
      const s = await openScenario(dialect);
      const register = await s.record("register", s.spend(s.spare(0)));
      const attest = await s.record("attest", s.spend(s.spare(1)), s.firstHash);
      const merge = await s.record(
        "merge",
        s.spend(s.spare(2)),
        "ee".repeat(28),
      );
      const commit = await s.record("commit", s.spend(s.spare(3)));
      s.transport.mempool.add(txId(commit));
      const stage = s.stage();

      expect(await stage.run()).toEqual([]);
      const sent = s.transport.sent.map((bytes) => bytes.toString("hex"));
      expect(sent.sort()).toEqual(
        [register, attest].map((bytes) => bytes.toString("hex")).sort(),
      );
      const actions = new Map(
        stage
          .lastReport()!
          .intents.map((entry) => [entry.intent.family, entry.action]),
      );
      expect(Object.fromEntries(actions)).toEqual({
        register: "resubmit",
        attest: "resubmit",
        merge: "abandon",
        commit: "wait_in_mempool",
      });
      expect(await s.events(merge)).toEqual(["signed", "abandoned"]);
      expect(s.logs.some((line) => line.startsWith("abandon merge"))).toBe(
        true,
      );

      // Same tip: nothing is sent again; the abandoned merge is dead.
      expect(await stage.run()).toEqual([]);
      expect(s.transport.sent).toHaveLength(2);
      expect(
        stage.lastReport()!.intents.find((e) => e.intent.family === "merge")!
          .action,
      ).toBe("dead");

      // A new tip: the still-live intents get the same bytes once more.
      await s.chain.forward([]);
      await stage.run();
      expect(
        s.transport.sent
          .slice(2)
          .map((b) => b.toString("hex"))
          .sort(),
      ).toEqual(sent.sort());
      stage.close();
    });

    it("never sends a landed intent or one a foreign transaction beat to its input", async () => {
      const s = await openScenario(dialect);
      const landedTx = s.spend(s.spare(0));
      await s.record("reserve_payout", landedTx);
      await s.record("retire", s.spend(s.spare(1)));
      // A foreign transaction (never journaled) spends the retire's input.
      await s.chain.forward([landedTx, s.spend(s.spare(1))]);
      const stage = s.stage();

      expect(await stage.run()).toEqual([]);
      expect(s.transport.sent).toEqual([]);
      const byFamily = Object.fromEntries(
        stage
          .lastReport()!
          .intents.map((entry) => [
            entry.intent.family,
            [entry.status.kind, entry.action],
          ]),
      );
      expect(byFamily).toEqual({
        reserve_payout: ["landed", "follow"],
        retire: ["conflicted", "dead"],
      });
      stage.close();
    });

    it("holds instead of abandoning when the queue is unhealthy or the mempool read fails", async () => {
      const s = await openScenario(dialect);
      const attest = await s.record(
        "correction",
        s.spend(s.spare(0)),
        s.firstHash,
      );
      const orphan = simHeader(2, GENESIS);
      await s.chain.forward([
        {
          inputs: [s.chain.chain.outsideInput()],
          outputs: [
            queueOutput(
              PREFIX + SDK.stateQueueHeaderHash(orphan),
              nodeDatum(orphan, "Unattested", null),
            ),
          ],
          nonce: s.nonce(),
        },
      ]);
      const stage = s.stage();
      const holds = await stage.run();
      expect(holds).toHaveLength(1);
      expect(holds[0]!.reason).toBe(INTENT_RECONCILE_TRANSIENT);
      expect(holds[0]!.detail).toContain("unhealthy (orphan_node)");
      expect(s.transport.sent).toEqual([]);
      expect(await s.events(attest)).toEqual(["signed"]);

      const register = await s.record("register", s.spend(s.spare(1)));
      s.transport.failMempool = true;
      const failed = await stage.run();
      expect(failed.map((hold) => hold.reason)).toEqual([
        INTENT_RECONCILE_TRANSIENT,
        INTENT_RECONCILE_TRANSIENT,
      ]);
      expect(
        failed.some((hold) => hold.detail.includes("mempool read failed")),
      ).toBe(true);
      expect(await s.events(register)).toEqual(["signed"]);
      expect(stage.holds()).toEqual(failed);
      stage.close();
    });

    it("names an owed wallet seed and a failed pass as holds", async () => {
      const s = await openScenario(dialect);
      const stage = s.stage([QUEUE_ADDRESS]);
      const holds = await stage.run();
      expect(holds.map((hold) => hold.reason)).toEqual([WALLET_SEED_PENDING]);
      expect(holds[0]!.detail).toContain("ledger_unavailable");
      stage.close();
      opened.splice(opened.indexOf(s.store), 1);
      await s.store.close();
      const after = await s.stage().run();
      expect(after.map((hold) => hold.reason)).toEqual([
        INTENT_RECONCILE_FAILED,
      ]);
    });
  },
);

describe("the node intent tracked set", () => {
  it("is the protocol payment credentials and the hub-oracle policy, without the seeded wallets", () => {
    expect(
      nodeIntentTrackedSet({
        protocolPaymentCredentials: ["ab".repeat(28)],
        hubOraclePolicyId: "cd".repeat(28),
      }),
    ).toEqual({
      addresses: new Set(),
      paymentCredentials: new Set(["ab".repeat(28)]),
      policies: new Set(["cd".repeat(28)]),
    });
  });
});
