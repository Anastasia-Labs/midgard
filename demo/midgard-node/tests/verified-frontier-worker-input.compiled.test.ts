/**
 * `WorkerInput.verifiedFrontier` across the built boundary: the main thread
 * records a verification of `confirmed_ledger` (`Globals`
 * `VERIFIED_CONFIRMED_LEDGER`), the built parent codec turns the record into
 * the commit worker's input, and a real worker thread running the built
 * worker modules reads it back and resolves a landed commit base from it.
 * The base is built only from the frontier a passed verification recorded,
 * at the frontier generation it read; before one, after a divergence clears
 * it, and once the ledger is at another generation, the commit waits for
 * the verification.
 */
import { Worker } from "node:worker_threads";

import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { recordConfirmedLedgerVerification } from "../src/mpf/confirmed-ledger-verification.js";
import { Globals } from "../src/services/globals.globals.js";

const codecUrl = new URL(
  "../dist/utils/commit-block-header.js",
  import.meta.url,
).href;
const baseUrl = new URL(
  "../dist/commit-block-header.landed-commit-base.js",
  import.meta.url,
).href;

const outRef = (label: number) => Buffer.alloc(36, label).toString("hex");
const entry = (label: number) => ({
  outref: outRef(label),
  output: Buffer.from(`output-${label.toString()}`).toString("hex"),
});

const F = { headerHash: "f0".repeat(28), utxosRoot: "a0".repeat(32) };
const H1 = "b1".repeat(28);
const H2 = "b2".repeat(28);

const row = (
  headerHash: string,
  parentHeaderHash: string,
  parentUtxosRoot: string,
  utxosRoot: string,
  spent: readonly string[],
  produced: readonly ReturnType<typeof entry>[],
) => ({
  headerHash,
  parentHeaderHash,
  parentUtxosRoot,
  utxosRoot,
  kind: "foreign",
  state: "processed",
  applied: true,
  depositIds: [],
  withdrawals: [],
  forcedIds: [],
  txIds: [],
  spent,
  produced,
});

/** `confirmed_ledger` holds 1 and 2 at F, read at frontier generation
 * `generation`; H1 spends 1 and produces 3, H2 spends `h2Spent` and
 * produces 4. */
const landed = (
  h2Spent: readonly string[] = [outRef(3)],
  generation = "5",
) => ({
  frontier: F,
  generation,
  confirmed: [entry(1), entry(2)],
  chain: [
    row(
      H1,
      F.headerHash,
      F.utxosRoot,
      "a1".repeat(32),
      [outRef(1)],
      [entry(3)],
    ),
    row(H2, H1, "a1".repeat(32), "a2".repeat(32), h2Spent, [entry(4)]),
  ],
});

/** What the built worker resolves at H2 from `input`. */
const consume = async (
  input: unknown,
  ledger: ReturnType<typeof landed> = landed(),
) => {
  const worker = new Worker(
    new URL(
      "./helpers/verified-frontier-compiled-consumer.mjs",
      import.meta.url,
    ),
    {
      // The built worker resolves with production import conditions, not
      // Vitest's inherited source-resolution condition.
      execArgv: [],
      workerData: { codecUrl, baseUrl, input, landed: ledger, headerHash: H2 },
    },
  );
  try {
    return await new Promise<unknown>((resolve, reject) => {
      worker.once("message", resolve);
      worker.once("error", reject);
      worker.once("exit", (code) => {
        if (code !== 0) reject(new Error(`consumer exited ${code.toString()}`));
      });
    });
  } finally {
    await worker.terminate();
  }
};

describe("the verified frontier across the built commit worker boundary", () => {
  it("builds a landed base only from the frontier a passed verification recorded", async () => {
    const codec = (await import(
      codecUrl
    )) as typeof import("../src/workers/utils/commit-block-header.js");
    const globals = await Effect.runPromise(
      Effect.provide(Globals, Globals.Default),
    );
    // What the commit action posts, from what the main thread recorded.
    const posted = () =>
      Effect.runPromise(Ref.get(globals.VERIFIED_CONFIRMED_LEDGER)).then(
        codec.verifiedFrontierWorkerInput,
      );

    // No verification yet: the commit waits for one.
    expect(await consume(await posted())).toEqual({
      refused: "CommitBaseUnverifiedError",
      headerHash: H2,
    });

    // A pass at F: the worker builds from it through H1 and H2.
    await Effect.runPromise(
      recordConfirmedLedgerVerification(globals, {
        kind: "verified",
        frontier: F,
        generation: "5",
        tail: { headerHash: H2, utxosRoot: "a2".repeat(32) },
        tailDigests: new Map(),
        entries: 2,
        durationMs: 1,
      }),
    );
    expect(await consume(await posted())).toEqual({
      root: "a2".repeat(32),
      keys: [outRef(2), outRef(4)].sort(),
    });
    // A delta that does not apply is refused by name across the boundary.
    expect(await consume(await posted(), landed([outRef(1)]))).toEqual({
      refused: "LocalLedgerDivergenceError",
      headerHash: H2,
    });
    // The same frontier and root at a later generation (`confirmed_ledger`
    // written since the pass): the commit waits.
    expect(await consume(await posted(), landed([outRef(3)], "6"))).toEqual({
      refused: "CommitBaseUnverifiedError",
      headerHash: H2,
    });

    // A divergence at F clears the record: the commit waits again.
    await Effect.runPromise(
      recordConfirmedLedgerVerification(globals, {
        kind: "diverged",
        frontier: F,
        generation: "5",
        tail: { headerHash: H2, utxosRoot: "a2".repeat(32) },
        entries: 2,
        durationMs: 1,
        divergences: [
          {
            check: "confirmed_ledger",
            headerHash: F.headerHash,
            root: F.utxosRoot,
            derivedRoot: "ee".repeat(32),
          },
        ],
      }),
    );
    expect(await consume(await posted())).toEqual({
      refused: "CommitBaseUnverifiedError",
      headerHash: H2,
    });
  }, 60_000);

  it("refuses a frontier payload the parent cannot produce", async () => {
    for (const verifiedFrontier of [
      { ...F, generation: "5", headerHash: F.headerHash.toUpperCase() },
      // A frontier with no generation.
      F,
    ])
      expect(await consume({ verifiedFrontier })).toEqual({
        refused: "MalformedVerifiedFrontierError",
      });
  }, 60_000);
});
