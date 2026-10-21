/**
 * The node follower's tracked set gains the validation-trace dispute's
 * holders (case, game, timeout, boundary, award): a store that followed
 * the chain without them resets at its next start and replays from the
 * origin, unready until the replay reaches the tip, and then holds the
 * outputs at those holders that it never held before.
 */
import { createHash } from "node:crypto";

import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import {
  applyChainSyncEvent,
  type FactStore,
  liveUtxosIn,
  openPostgresFactStore,
  type OutRef,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import type * as SDK from "@al-ft/midgard-sdk";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { protocolPaymentCredentials } from "../src/services/intent-journal.tracked-set.js";
import { nodeFollowerStoreOptions } from "../src/services/l1-follower.store-options.js";
import { testDatabases } from "./helpers/l1-events-store.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const scriptHash = (name: string): string =>
  createHash("sha256").update(name).digest("hex").slice(0, 56);

const DISPUTE_HOLDERS = ["case", "game", "timeout", "boundary", "award"];

/** A deployment whose every validator has a script hash of its own name. */
const contracts = new Proxy(
  {},
  {
    get: (_, name) =>
      name === "fraudProofContracts"
        ? {
            validationTraceDispute: new Proxy(
              {},
              {
                get: (__, holder) => ({
                  spendingScriptHash: scriptHash(`dispute ${String(holder)}`),
                }),
              },
            ),
          }
        : { spendingScriptHash: scriptHash(String(name)) },
  },
) as SDK.MidgardValidators;

const disputeHolder = (holder: string) => scriptHash(`dispute ${holder}`);

/** The node's store over `connectionString`, tracking `credentials`. */
const openStore = (
  connectionString: string,
  credentials: readonly string[],
) => {
  const store = openPostgresFactStore({
    ...nodeFollowerStoreOptions({
      projections: [],
      securityParameter: 6,
      commitEventDepth: 0,
      protocolPaymentCredentials: credentials,
      hubOraclePolicyId: "cc".repeat(28),
    }),
    connection: { connectionString },
  });
  opened.push(store);
  return store;
};

const apply = async (store: FactStore, event: ChainSyncEvent) => {
  const { result } = await applyChainSyncEvent(store, event);
  expect(result.kind).toBe("applied");
};

const liveCount = (store: FactStore, outRefs: readonly OutRef[]) =>
  store.transaction("read", async (tx) => {
    const read = await liveUtxosIn(tx, store.dialect, {
      by: "outref",
      outRefs,
    });
    if (read.kind !== "ok") throw new Error(`live outputs: ${read.kind}`);
    return read.utxos.length;
  });

describe("the node follower's tracked set and the validation-trace dispute", () => {
  it("replays from the origin when it gains the dispute's holders, and then holds their outputs", async () => {
    const connectionString = await databases.create();
    const current = protocolPaymentCredentials(contracts);
    const disputeHashes = new Set(DISPUTE_HOLDERS.map(disputeHolder));
    const before = openStore(
      connectionString,
      current.filter((credential) => !disputeHashes.has(credential)),
    );
    expect((await before.start()).kind).toBe("ready");
    expect((await before.initialize(SIM_ORIGIN)).kind).toBe("initialized");

    // One block pays every holder of the dispute.
    const chain = new SimChain(simUniverse(), SIM_ORIGIN);
    const { event, encoded } = chain.forward([
      {
        inputs: [chain.outsideInput()],
        outputs: DISPUTE_HOLDERS.map((holder) => ({
          address: Buffer.from(`71${disputeHolder(holder)}`, "hex"),
          lovelace: 3_000_000n,
        })),
        nonce: chain.nonce(),
      },
    ]);
    const outRefs = DISPUTE_HOLDERS.map((_, index) => ({
      txHash: encoded.txHashes[0]!,
      index,
    }));
    await apply(before, event);
    expect(await liveCount(before, outRefs)).toBe(0);
    opened.splice(opened.indexOf(before), 1);
    await before.close();

    const store = openStore(connectionString, current);
    const start = await store.start();
    if (start.kind !== "ready") throw new Error(`start: ${start.kind}`);
    expect(start.trackedSet.kind).toBe("reset");
    expect(start.replaying).toBe(true);
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    await apply(store, event);
    expect(await liveCount(store, outRefs)).toBe(DISPUTE_HOLDERS.length);
    expect(await store.endTrackedSetReplay()).toBe("ended");
    expect((await store.trackedSetRecord())?.replaying).toBe(false);
  });
});
