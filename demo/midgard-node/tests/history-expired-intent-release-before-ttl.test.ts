import { Effect } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  findBaseSpend,
  scanBaseSpend,
} from "../src/services/history-expired-intent-release.base-spend.js";
import {
  declineBeforeTtl,
  heldBaseOutput,
} from "../src/services/history-expired-intent-release.before-ttl.js";
import type { QueueView } from "../src/services/history-expired-intent-release.signed-commit-node.js";
import { makeSignedIntentDeferral } from "../src/services/history-expired-intent-release.table.js";
import {
  activeE,
  applyBlock,
  BASE_HEADER,
  BASE_OUT,
  BASE_TX,
  BINDING,
  binding,
  bytes,
  change,
  dispose,
  E,
  E_HEADER,
  FOREIGN,
  hex,
  insertJournal,
  receipt,
  ROOT_HEADER,
  run,
  seed,
  signedCommit,
  TTL,
} from "./helpers/history-expired-intent-release-before-ttl.js";

/**
 * The pre-TTL arm of signed-intent release, by the owner ruling of
 * 2026-09-26: an intent is replaced once it cannot land on the current chain,
 * meaning the observed head is past its TTL or its base output D is already
 * spent by something else. The evidence is the journaled canonical history
 * (block application receipts), read over SQL by output reference only.
 */

beforeEach(async () => {
  await run(seed);
});

describe("pre-TTL release on journaled base-spend evidence", () => {
  it("is pending before the TTL once a foreign transaction spent the base output, naming it", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* activeE();
        yield* applyBlock(1, [{ txHash: FOREIGN, inputs: [BASE_OUT] }]);
        return yield* dispose(makeSignedIntentDeferral(), change("forward", 1));
      }),
    );
    expect(result?.status).toBe("pending");
    expect(result?.reason).toContain(`before its TTL slot ${TTL}`);
    expect(result?.reason).toContain(
      `its base output ${BASE_OUT} was spent by transaction ${FOREIGN}`,
    );
  });

  it("is not pending before the TTL while the base output is unspent, whatever else names its transaction", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* activeE();
        // Another output of the base transaction spent, and a receipt that
        // names the base output only as an output.
        yield* applyBlock(1, [
          { txHash: FOREIGN, inputs: [`${BASE_TX}#1`] },
          { txHash: BASE_TX, inputs: [] },
        ]);
        return yield* dispose(makeSignedIntentDeferral(), change("forward", 1));
      }),
    );
    expect(result).toBeUndefined();
  });

  it("never treats the intent's own transaction spending the base as evidence: the landing is confirmed, never replaced", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* activeE();
        yield* applyBlock(1, [{ txHash: E.hash, inputs: [BASE_OUT] }]);
        return yield* dispose(makeSignedIntentDeferral(), change("forward", 1));
      }),
    );
    expect(result).toBeUndefined();
  });

  it("ignores a collateral-only spend, a non-canonical application and another binding's receipt", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* activeE();
        yield* applyBlock(1, [
          { txHash: FOREIGN, spends: "collaterals", inputs: [BASE_OUT] },
        ]);
        yield* applyBlock(2, [{ txHash: FOREIGN, inputs: [BASE_OUT] }], {
          canonical: false,
        });
        yield* applyBlock(3, [{ txHash: FOREIGN, inputs: [BASE_OUT] }], {
          digest: hex("other-binding"),
        });
        return yield* dispose(makeSignedIntentDeferral(), change("resume", 3));
      }),
    );
    expect(result).toBeUndefined();
  });

  it("keeps the TTL arm: pending at the TTL with no base-spend evidence", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* activeE();
        return yield* dispose(
          makeSignedIntentDeferral(),
          change("forward", 1, { slot: TTL }),
        );
      }),
    );
    expect(result?.status).toBe("pending");
    expect(result?.reason).toContain(
      `reached its validity upper bound (TTL slot ${TTL})`,
    );
  });

  it("is pending when a replaced sibling's commit on another incarnation of the same non-root base is included", async () => {
    const earlier = `${hex("earlier-incarnation")}#0`;
    const sibling = signedCommit(earlier, TTL);
    const result = await run(
      Effect.gen(function* () {
        yield* insertJournal({
          header: bytes("sibling-header", 28),
          status: Pending.Status.Abandoned,
          commit: sibling,
          baseOut: earlier,
          baseHeader: BASE_HEADER,
          createdAt: new Date(1_000_000),
          abandonment: "replacement",
        });
        yield* activeE();
        yield* applyBlock(1, [{ txHash: sibling.hash, inputs: [earlier] }]);
        return yield* dispose(makeSignedIntentDeferral(), change("forward", 1));
      }),
    );
    expect(result?.status).toBe("pending");
    expect(result?.reason).toContain(
      `the signed commit ${sibling.hash} of this node's replaced block on the same base`,
    );
  });

  it("is not pending when this node's block that a correction removed (or that was abandoned unattributed) landed on an earlier incarnation of the base", async () => {
    // X landed on D's earlier output and a correction then removed it,
    // leaving D at E's base output, unspent: E can still land.
    const earlier = `${hex("earlier-incarnation")}#0`;
    const results = await run(
      Effect.gen(function* () {
        const outcomes = [];
        for (const abandonment of ["correction", undefined] as const) {
          yield* seed;
          const removed = signedCommit(earlier, TTL);
          yield* insertJournal({
            header: bytes("removed-header", 28),
            status: Pending.Status.Abandoned,
            commit: removed,
            baseOut: earlier,
            baseHeader: BASE_HEADER,
            createdAt: new Date(1_000_000),
            ...(abandonment === undefined ? {} : { abandonment }),
          });
          yield* activeE();
          yield* applyBlock(1, [{ txHash: removed.hash, inputs: [earlier] }]);
          outcomes.push(
            yield* dispose(makeSignedIntentDeferral(), change("resume", 1)),
          );
        }
        return outcomes;
      }),
    );
    expect(results).toEqual([undefined, undefined]);
  });

  it("matches a root base by output only: a commit on another root output is not a sibling", async () => {
    const other = signedCommit(`${hex("other-root")}#0`, TTL);
    const result = await run(
      Effect.gen(function* () {
        yield* insertJournal({
          header: bytes("other-root-header", 28),
          status: Pending.Status.Abandoned,
          commit: other,
          baseOut: `${hex("other-root")}#0`,
          baseHeader: ROOT_HEADER,
          createdAt: new Date(1_000_000),
        });
        yield* activeE(ROOT_HEADER);
        yield* applyBlock(1, [{ txHash: other.hash, inputs: [] }]);
        return yield* dispose(makeSignedIntentDeferral(), change("forward", 1));
      }),
    );
    expect(result).toBeUndefined();
  });

  it("once preparation declined the evidence, does not re-fire it on later forward or resume changes, and re-fires after a rollback", async () => {
    const deferral = makeSignedIntentDeferral();
    const results = await run(
      Effect.gen(function* () {
        yield* activeE();
        yield* applyBlock(1, [{ txHash: FOREIGN, inputs: [BASE_OUT] }]);
        const first = yield* dispose(deferral, change("forward", 1));
        const spend = yield* scanBaseSpend({
          binding,
          headerHash: E_HEADER,
          intendedTxHash: Buffer.from(E.hash, "hex"),
          base: { outRef: BASE_OUT, headerHash: BASE_HEADER, utxosRoot: "" },
          fromHeight: -1,
          toHeight: 1,
        });
        const declined = yield* declineBeforeTtl({
          expired: false,
          baseSpend: spend,
          deferral,
          key: `${E_HEADER.toString("hex")}:${E.hash}`,
          reportKey: "before-ttl-test",
          context: "the test's signed commit",
          reason: "the exact-point queue decided nothing",
        });
        yield* applyBlock(2, []);
        const forward = yield* dispose(deferral, change("forward", 2));
        const resume = yield* dispose(deferral, change("resume", 2));
        const rollback = yield* dispose(deferral, change("rollback", 2));
        return { first, declined, forward, resume, rollback };
      }),
    );
    expect(results.first?.status).toBe("pending");
    expect(results.declined).toBe(true);
    expect(results.forward).toBeUndefined();
    expect(results.resume).toBeUndefined();
    expect(results.rollback?.status).toBe("pending");
  });

  it("lets evidence other than the declined one reopen it, and declines each only once", async () => {
    const deferral = makeSignedIntentDeferral();
    const key = `${E_HEADER.toString("hex")}:${E.hash}`;
    const OTHER = hex("other-spend");
    const decline = (height: number) =>
      Effect.gen(function* () {
        const spend = yield* scanBaseSpend({
          binding,
          headerHash: E_HEADER,
          intendedTxHash: Buffer.from(E.hash, "hex"),
          base: { outRef: BASE_OUT, headerHash: BASE_HEADER, utxosRoot: "" },
          fromHeight: -1,
          toHeight: height,
          declined: deferral.beforeTtl?.declined ?? new Set(),
        });
        return yield* declineBeforeTtl({
          expired: false,
          baseSpend: spend,
          deferral,
          key,
          reportKey: "before-ttl-mask-test",
          context: "the test's signed commit",
          reason: "the exact-point queue decided nothing",
        });
      });
    const results = await run(
      Effect.gen(function* () {
        yield* activeE();
        yield* applyBlock(1, [{ txHash: FOREIGN, inputs: [BASE_OUT] }]);
        yield* decline(1);
        // Evidence ranking after the declined one, seen on a resume (which
        // re-reads the whole history from the declined evidence onwards).
        yield* applyBlock(2, [{ txHash: OTHER, inputs: [BASE_OUT] }]);
        const other = yield* dispose(deferral, change("resume", 2));
        yield* decline(2);
        const both = yield* dispose(deferral, change("resume", 2));
        return { other, both };
      }),
    );
    expect(results.other?.status).toBe("pending");
    expect(results.other?.reason).toContain(`spent by transaction ${OTHER}`);
    expect(results.both).toBeUndefined();
    expect([...(deferral.beforeTtl?.declined ?? [])]).toEqual([FOREIGN, OTHER]);
  });

  it("scans only the blocks a forward change adds after a clean scan, and everything on a resume", async () => {
    const deferral = makeSignedIntentDeferral();
    const results = await run(
      Effect.gen(function* () {
        yield* activeE();
        yield* applyBlock(1, []);
        yield* applyBlock(2, []);
        const clean = yield* dispose(deferral, change("forward", 2));
        // Evidence appearing below the scanned height is not re-read by a
        // forward change from that height; a resume reads the whole history.
        yield* applyBlock(0, [{ txHash: FOREIGN, inputs: [BASE_OUT] }]);
        yield* applyBlock(3, []);
        const forward = yield* dispose(deferral, change("forward", 3));
        const resume = yield* dispose(deferral, change("resume", 3));
        return { clean, forward, resume, scanned: deferral.scanned };
      }),
    );
    expect(results.clean).toBeUndefined();
    expect(results.forward).toBeUndefined();
    expect(results.resume?.status).toBe("pending");
    expect(results.scanned).toBe(`${E_HEADER.toString("hex")}:${E.hash}@3`);
  });
});

describe("base-spend helpers", () => {
  it("skips an undecodable receipt and reports the first evidence by height", () => {
    const found = findBaseSpend(
      [
        { height: 1, receipt: "{not json" },
        {
          height: 2,
          receipt: receipt([{ txHash: FOREIGN, inputs: [BASE_OUT] }]),
        },
      ],
      BINDING,
      BASE_OUT,
      E.hash,
      new Set(),
    );
    expect(found).toEqual({ kind: "spent", txHash: FOREIGN, height: 2 });
  });

  it("declines before the TTL, on any evidence, while the exact-point queue still holds the base output", () => {
    const queue = {
      nodes: [{ node: { utxo: { txHash: BASE_TX, outputIndex: 0 } } }],
    } as unknown as QueueView;
    const live = { expired: false, retainedPlan: false };
    for (const spend of [
      { kind: "spent", txHash: FOREIGN, height: 1 },
      { kind: "sibling", txHash: hex("sibling-commit"), height: 1 },
    ] as const) {
      expect(
        heldBaseOutput({ ...live, baseSpend: spend }, queue, BASE_OUT),
      ).toContain(`the exact-point queue still holds its base output`);
      expect(
        heldBaseOutput(
          { ...live, expired: true, baseSpend: spend },
          queue,
          BASE_OUT,
        ),
      ).toBeUndefined();
      // Its own retained plan is resumed, never declined.
      expect(
        heldBaseOutput(
          { ...live, retainedPlan: true, baseSpend: spend },
          queue,
          BASE_OUT,
        ),
      ).toBeUndefined();
      expect(
        heldBaseOutput(
          { ...live, baseSpend: spend },
          { nodes: [] } as unknown as QueueView,
          BASE_OUT,
        ),
      ).toBeUndefined();
    }
  });

  it("never declines at the TTL or without evidence", async () => {
    const deferral = makeSignedIntentDeferral();
    const spend = { kind: "spent", txHash: FOREIGN, height: 1 } as const;
    const common = {
      deferral,
      key: "k",
      reportKey: "before-ttl-helper",
      context: "c",
      reason: "r",
    };
    expect(
      await Effect.runPromise(
        declineBeforeTtl({ ...common, expired: true, baseSpend: spend }),
      ),
    ).toBe(false);
    expect(
      await Effect.runPromise(
        declineBeforeTtl({ ...common, expired: false, baseSpend: undefined }),
      ),
    ).toBe(false);
    expect(deferral.beforeTtl).toBeUndefined();
  });
});
