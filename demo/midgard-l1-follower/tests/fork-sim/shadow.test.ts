import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { type FactStore, openSqliteFactStore } from "../../src/index.js";
import {
  compareAll,
  decodeUtxoAnswer,
  diffValues,
  firstDisagreement,
  Journal,
  JOURNAL_FILE,
  ledgerComparator,
  normalise,
  reading,
  readJournal,
  readSoakConfig,
  type ShadowComparator,
  type ShadowContext,
  SoakConfigError,
  summarise,
  unavailable,
} from "../../src/shadow/index.js";
import {
  encodeUtxoAnswer,
  SIM_ORIGIN,
  simStoreOptions,
  simUniverse,
} from "../../src/testing/index.js";
import { SIM_K } from "../support/fork-sim.js";
import { fakeLedger } from "../support/soak-fakes.js";

let dir: string;
const opened: FactStore[] = [];
beforeEach(async () => {
  dir = await mkdtemp(join(tmpdir(), "l1-shadow-"));
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
  await rm(dir, { recursive: true, force: true });
});

const universe = simUniverse();

describe("diffValues", () => {
  it("treats Maps and Sets by content, not insertion order", () => {
    const a = new Map<string, unknown>([
      ["b", 2n],
      ["a", Buffer.from([1])],
    ]);
    const b = new Map<string, unknown>([
      ["a", Buffer.from([1])],
      ["b", 2n],
    ]);
    expect(diffValues(a, b)).toEqual([]);
    expect(diffValues(new Set([3, 1]), new Set([1, 3]))).toEqual([]);
    expect(normalise({ z: undefined, y: 1n })).toEqual({ y: "1" });
  });

  it("names the path of each difference, including a missing key", () => {
    expect(
      diffValues(
        { rows: [{ id: 1, v: 2n }], only: true },
        { rows: [{ id: 1, v: 3n }] },
      ),
    ).toEqual([
      { path: "$.only", projected: true },
      { path: "$.rows[0].v", projected: "2", current: "3" },
    ]);
  });
});

describe("compareAll", () => {
  const context = {} as ShadowContext;
  const comparator = (
    name: string,
    projected: () => Promise<ReturnType<typeof reading>>,
    current: () => Promise<ReturnType<typeof reading>> = projected,
  ): ShadowComparator => ({ role: "watcher", name, projected, current });

  it("turns a throwing or unavailable side into a result and runs the rest", async () => {
    const results = await compareAll(
      [
        comparator("throws", () => Promise.reject(new Error("boom"))),
        comparator("late", () => Promise.resolve(unavailable("not yet"))),
        {
          ...comparator("observe", () => Promise.resolve(reading(1))),
          observe: () => Promise.reject(new Error("feed failed")),
        },
        comparator(
          "differs",
          () => Promise.resolve(reading(1)),
          () => Promise.resolve(reading(2)),
        ),
        comparator("equal", () => Promise.resolve(reading({ a: 1 }))),
      ],
      context,
    );
    expect(results.map((r) => r.outcome)).toEqual([
      "error",
      "skipped",
      "error",
      "differs",
      "equal",
    ]);
    expect(results[0]).toMatchObject({ error: "projected: boom" });
    expect(results[1]).toMatchObject({ reason: "projected: not yet" });
    expect(firstDisagreement(results)).toBe(
      "watcher/throws failed: projected: boom",
    );
    expect(firstDisagreement(results.slice(1, 2))).toBeNull();
  });
});

describe("journal", () => {
  it("survives a torn last line: counts it and appends on a fresh line", async () => {
    const journal = await Journal.open(dir);
    await journal.append({
      type: "stop",
      at: "t0",
      reason: "signal",
      detail: "x",
    });
    await journal.close();
    await writeFile(join(dir, JOURNAL_FILE), '{"type":"blo', { flag: "a" });
    const reopened = await Journal.open(dir);
    await reopened.append({
      type: "stop",
      at: "t1",
      reason: "limit",
      detail: "y",
    });
    await reopened.close();
    const { records, corrupt } = await readJournal(dir);
    expect(corrupt).toBe(1);
    expect(records.map((r) => r.type === "stop" && r.at)).toEqual(["t0", "t1"]);
    expect(summarise(records, corrupt)).toMatchObject({
      lastStop: { reason: "limit" },
      corruptLines: 1,
    });
  });
});

describe("readSoakConfig", () => {
  const base = {
    socketPath: "node.socket",
    networkMagic: 42,
    binaryPath: "/bin/sidecar",
    securityParameter: 2160,
    trackedSet: { addresses: ["70aa"], policies: ["bb"] },
    plugins: [{ module: "./committee.mjs" }],
  };
  const write = (value: unknown) =>
    writeFile(join(dir, "soak.json"), JSON.stringify(value));

  it("resolves relative paths against the soak directory and defaults the ledger addresses", async () => {
    await write(base);
    const config = await readSoakConfig(dir);
    expect(config.socketPath).toBe(join(dir, "node.socket"));
    expect(config.binaryPath).toBe("/bin/sidecar");
    expect(config.plugins).toEqual([
      { module: join(dir, "committee.mjs"), options: {} },
    ]);
    expect(config.ledgerAddresses.map((a) => a.toString("hex"))).toEqual([
      "70aa",
    ]);
    expect([...config.trackedSet.paymentCredentials]).toEqual([]);
  });

  it.each([
    ["networkMagic", { ...base, networkMagic: 0 }, /networkMagic/u],
    [
      "uppercase hex",
      { ...base, trackedSet: { addresses: ["70AA"] } },
      /trackedSet.addresses/u,
    ],
    ["plugins", { ...base, plugins: {} }, /plugins must be a list/u],
    ["socketPath", { ...base, socketPath: "" }, /socketPath/u],
  ])("refuses a bad %s", async (_, value, error) => {
    await write(value);
    await expect(readSoakConfig(dir)).rejects.toThrow(SoakConfigError);
    await expect(readSoakConfig(dir)).rejects.toThrow(error);
  });
});

describe("ledger stub comparator", () => {
  const preexisting = {
    outRef: { txHash: Buffer.alloc(32, 0x77), index: 3 },
    output: {
      address: universe.trackedAddress,
      lovelace: 9_000_000n,
      assets: new Map([[universe.trackedPolicy, new Map([["01", 5n]])]]),
      datum: Buffer.from("d87980", "hex"),
    },
  };

  it("decodes what the node's utxo_by_address answer encodes", () => {
    expect(decodeUtxoAnswer(encodeUtxoAnswer([preexisting]))).toEqual([
      {
        outRef: `${"77".repeat(32)}0003`,
        address: universe.trackedAddress.toString("hex"),
        lovelace: "9000000",
        assets: {
          [universe.trackedPolicy]: { "01": "5" },
        },
        datumHash: null,
        datum: "d87980",
        scriptRef: null,
      },
    ]);
  });

  const atOrigin = async (): Promise<ShadowContext> => {
    const store = openSqliteFactStore({
      ...simStoreOptions([], SIM_K, "sqlite"),
      path: ":memory:",
    });
    opened.push(store);
    await store.start();
    await store.initialize(SIM_ORIGIN);
    return {
      store,
      at: { point: SIM_ORIGIN.point, height: SIM_ORIGIN.height, generation: 0 },
    };
  };

  // Outputs that predate the store's origin are not its facts: without the
  // baseline every soak block would differ by them.
  it("subtracts the node's set at the store origin, and refuses a foreign baseline", async () => {
    const ledger = fakeLedger([], (_, utxos) => [preexisting, ...utxos]);
    const context = await atOrigin();
    const comparator = ledgerComparator({
      ledger,
      addresses: [universe.trackedAddress],
      dir,
    });
    expect(await compareAll([comparator], context)).toEqual([
      { role: "follower", name: "ledger-utxos", outcome: "equal" },
    ]);
    const saved = JSON.parse(
      await readFile(join(dir, "ledger-baseline.json"), "utf8"),
    ) as { outRefs: string[] };
    expect(saved.outRefs).toEqual([`${"77".repeat(32)}0003`]);
    await writeFile(
      join(dir, "ledger-baseline.json"),
      JSON.stringify({ origin: "1:00", outRefs: [] }),
    );
    const fresh = ledgerComparator({
      ledger,
      addresses: [universe.trackedAddress],
      dir,
    });
    expect(await compareAll([fresh], context)).toMatchObject([
      {
        outcome: "skipped",
        reason: expect.stringContaining("does not belong to origin") as unknown,
      },
    ]);
  });

  it("is skipped, not failed, when the node cannot acquire the point", async () => {
    const context = await atOrigin();
    const comparator = ledgerComparator({
      ledger: {
        withLedgerState: () => Promise.reject(new Error("acquire_failed")),
      },
      addresses: [universe.trackedAddress],
      dir,
    });
    expect(await compareAll([comparator], context)).toMatchObject([
      {
        outcome: "skipped",
        reason: expect.stringContaining("acquire_failed") as unknown,
      },
    ]);
  });
});
