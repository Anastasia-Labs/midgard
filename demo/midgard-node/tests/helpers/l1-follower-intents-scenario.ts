/**
 * The node intent stage's test scenario (`l1-follower-intents*.test.ts`): a
 * SQLite or Postgres follower store carrying the landed state queue (P1)
 * and the intent journal, a simulated chain holding a queue (root plus one
 * node, header `first`) and `SPARES` plain outputs at the tracked address,
 * and a fake node transport whose mempool and ledger refusals a test sets.
 */
import {
  currentViewIn,
  decodeTransaction,
  type FactStore,
  intentJournalProjection,
  type OutRef,
  readIntentEventsIn,
  recordIntentIn,
} from "@al-ft/midgard-l1-follower";
import { encodeSimTx, type SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import {
  stateQueueProjection,
  stateQueueTrackedSet,
} from "../../src/l1-state-queue/index.js";
import { nodeFamilyPredicate } from "../../src/services/l1-follower.intent-predicates.js";
import { createNodeIntentStage } from "../../src/services/l1-follower.intents.js";
import {
  ChainDriver,
  storeOpener,
  type testDatabases,
} from "./l1-events-store.js";
import {
  nodeDatum,
  QUEUE_ADDRESS,
  queueOutput,
  rootDatum,
  SIM_QUEUE_CONFIG,
  simHeader,
} from "./state-queue-sim.fixtures.js";

export const K = 6;
export const GENESIS = "00".repeat(28);
const ROOT = SDK.STATE_QUEUE_ROOT_ASSET_NAME;
export const PREFIX = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX;
const SPARES = 6;

type Transport = {
  readonly sent: Buffer[];
  readonly mempool: Set<string>;
  /** Tx ids the ledger refuses. */
  readonly refuse: Set<string>;
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
    refuse: new Set(),
    failMempool: false,
    hasTx: (txId) =>
      transport.failMempool
        ? Promise.reject(new Error("mempool read failed"))
        : Promise.resolve(transport.mempool.has(txId)),
    submit: (bytes) => {
      transport.sent.push(Buffer.from(bytes));
      return Promise.resolve(
        transport.refuse.has(decodeTransaction(bytes).hash.toString("hex"))
          ? { accepted: false, rejection: Buffer.from("refused") }
          : { accepted: true },
      );
    },
    withLedgerState: () => Promise.reject(new Error("no ledger state")),
  };
  return transport;
};

/** A queue (root plus one node, header `first`) and `SPARES` plain outputs at the tracked address. */
export const intentStageScenarios =
  (databases: ReturnType<typeof testDatabases>, opened: FactStore[]) =>
  async (dialect: "sqlite" | "postgres") => {
    const store = await storeOpener(dialect, databases)(
      [stateQueueProjection(SIM_QUEUE_CONFIG), intentJournalProjection],
      K,
    );
    opened.push(store);
    const chain = new ChainDriver(
      store,
      stateQueueTrackedSet(SIM_QUEUE_CONFIG),
    );
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
      workflowKey = `${family}:test`,
    ): Promise<Buffer> => {
      const txCbor = encodeSimTx(tx);
      const result = await store.transaction("write", async (sqlTx) =>
        recordIntentIn(sqlTx, store.dialect, {
          family,
          workflowKey,
          txCbor,
          isOwnOutput: () => false,
          builtAt: (await currentViewIn(sqlTx, store.dialect))!,
          contentRef:
            contentRef === null ? null : Buffer.from(contentRef, "hex"),
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
        wanted: nodeFamilyPredicate({
          store,
          stateQueue: SIM_QUEUE_CONFIG,
          operatorSet: null,
          slotToPosixMs: (slot) => slot * 1000,
          horizonLagBlocks: 0,
        }),
        log: (line) => logs.push(line),
      });
    const eventLog = (txCbor: Buffer) =>
      store.transaction("read", (sqlTx) =>
        readIntentEventsIn(sqlTx, decodeTransaction(txCbor).hash),
      );
    const events = async (txCbor: Buffer) =>
      (await eventLog(txCbor)).map((event) => event.kind);
    return {
      store,
      chain,
      eventLog,
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

export const txId = (txCbor: Buffer): string =>
  decodeTransaction(txCbor).hash.toString("hex");
