import { CML } from "@lucid-evolution/lucid";
import { Effect, Logger, LogLevel, Ref, Schedule } from "effect";
import { describe, expect, it } from "vitest";

import {
  decodeUnacceptedSignedIntent,
  REBROADCAST_ACCEPTED_WAIT_MS,
  REBROADCAST_INITIAL_DELAY_MS,
  REBROADCAST_MAX_DELAY_MS,
  rebroadcastDelayMs,
  type RebroadcastDeps,
  rebroadcastOnce,
  type RebroadcastState,
  rebroadcastTick,
  runRebroadcastFiber,
  type UnacceptedSignedIntent,
} from "../src/fibers/signed-intent-rebroadcast.js";

const BASE = `${"aa".repeat(32)}#0`;

/** A signed commit whose validity interval is [invalidBefore, ttl). */
const signedIntent = (
  invalidBefore: number,
  ttl: number,
): UnacceptedSignedIntent => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("aa".repeat(32)), 0n),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    0n,
  );
  body.set_validity_interval_start(BigInt(invalidBefore));
  body.set_ttl(BigInt(ttl));
  const txHash = CML.hash_transaction(body).to_hex();
  const tx = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
  const cbor = tx.to_cbor_hex();
  tx.free();
  return decodeUnacceptedSignedIntent({
    header_hash: Buffer.from("bb".repeat(28), "hex"),
    intended_tx_hash: Buffer.from(txHash, "hex"),
    signed_tx_cbor: Buffer.from(cbor, "hex"),
    base_tail_out_ref: BASE,
  });
};

/** A chain the test moves: tip slot, clock, whether the base output is
 * unspent, and every submitted body. */
const harness = (intent: UnacceptedSignedIntent | undefined) => {
  const chain = {
    intent,
    tip: 0,
    now: 1_000_000,
    baseUnspent: true,
    failSubmit: false,
    submitted: [] as string[],
  };
  const deps: RebroadcastDeps<never> = {
    intent: Effect.sync(() => chain.intent),
    tipSlot: Effect.sync(() => chain.tip),
    baseUnspent: (outRef) =>
      Effect.sync(() => {
        expect(outRef).toBe(BASE);
        return chain.baseUnspent;
      }),
    submit: (cbor) =>
      Effect.suspend(() => {
        chain.submitted.push(cbor);
        return chain.failSubmit
          ? Effect.fail(new Error("provider refused"))
          : Effect.succeed(chain.intent!.txHash);
      }),
    nowMs: () => chain.now,
  };
  const state: RebroadcastState = new Map();
  const tick = () => Effect.runPromise(rebroadcastOnce(deps, state));
  return { chain, deps, state, tick };
};

/** Runs `effect` and returns its result with every WARN and DEBUG message it
 * logged. */
const withLogs = async <A, E>(effect: Effect.Effect<A, E>) => {
  const logs: { level: string; message: string }[] = [];
  const logger = Logger.make(({ logLevel, message }) => {
    logs.push({ level: logLevel.label, message: String(message) });
  });
  const result = await Effect.runPromise(
    effect.pipe(
      Effect.provide(Logger.replace(Logger.defaultLogger, logger)),
      Logger.withMinimumLogLevel(LogLevel.Debug),
    ),
  );
  return { result, logs };
};

describe("signed-intent rebroadcast", () => {
  it("decodes the journaled bytes and refuses bytes that are not the intended transaction", () => {
    const intent = signedIntent(100, 160);
    expect(intent.invalidBefore).toBe(100n);
    expect(intent.ttl).toBe(160n);
    expect(() =>
      decodeUnacceptedSignedIntent({
        header_hash: Buffer.from("bb".repeat(28), "hex"),
        intended_tx_hash: Buffer.from("cc".repeat(32), "hex"),
        signed_tx_cbor: Buffer.from(intent.signedTxCbor, "hex"),
        base_tail_out_ref: BASE,
      }),
    ).toThrow(/not its intended transaction/);
  });

  it("resubmits the identical journaled bytes once the tip reaches the lower bound, then stops once the provider accepted them", async () => {
    const intent = signedIntent(100, 160);
    const { chain, tick } = harness(intent);
    chain.tip = 90;
    // First sight leaves the commit worker its own submission.
    expect(await tick()).toBe("first_seen");
    expect(await tick()).toBe("backoff");
    chain.now += REBROADCAST_INITIAL_DELAY_MS;
    // Not yet valid: checked, never submitted, and not an attempt.
    expect(await tick()).toBe("not_yet_valid");
    expect(await tick()).toBe("not_yet_valid");
    expect(chain.submitted).toEqual([]);
    chain.tip = 100;
    expect(await tick()).toBe("submitted");
    expect(chain.submitted).toEqual([intent.signedTxCbor]);
    // Accepted: nothing is resubmitted for the whole bounded wait, however
    // long the chain takes to include it.
    const acceptedAt = chain.now;
    for (const wait of [
      rebroadcastDelayMs(1),
      REBROADCAST_MAX_DELAY_MS,
      REBROADCAST_ACCEPTED_WAIT_MS - 1,
    ]) {
      chain.now = acceptedAt + wait;
      expect(await tick()).toBe("accepted");
    }
    expect(chain.submitted).toEqual([intent.signedTxCbor]);
  });

  it("resubmits accepted bytes only when they left the mempool without landing: their base output still unspent past the bounded wait", async () => {
    const intent = signedIntent(100, 160);
    const { chain, deps, state, tick } = harness(intent);
    chain.tip = 120;
    expect(await tick()).toBe("first_seen");
    chain.now += REBROADCAST_INITIAL_DELAY_MS;
    expect(await tick()).toBe("submitted");
    chain.now += REBROADCAST_ACCEPTED_WAIT_MS;
    const resumed = await withLogs(rebroadcastOnce(deps, state));
    expect(resumed.result).toBe("submitted");
    expect(
      resumed.logs.filter(({ message }) => /left the mempool/.test(message)),
    ).toEqual([expect.objectContaining({ level: "WARN" })]);
    expect(chain.submitted).toEqual([intent.signedTxCbor, intent.signedTxCbor]);
    // Accepted again; this time it lands (its base output is spent).
    chain.now += REBROADCAST_ACCEPTED_WAIT_MS;
    chain.baseUnspent = false;
    expect(await tick()).toBe("base_spent");
    chain.now += REBROADCAST_ACCEPTED_WAIT_MS;
    expect(await tick()).toBe("base_spent");
    expect(chain.submitted).toHaveLength(2);
  });

  it("retries a refusal with backoff and warns about it once per intent", async () => {
    const intent = signedIntent(100, 160);
    const { chain, deps, state } = harness(intent);
    chain.tip = 120;
    chain.failSubmit = true;
    await Effect.runPromise(rebroadcastOnce(deps, state));
    const levels: string[] = [];
    for (let attempt = 1; attempt <= 3; attempt += 1) {
      chain.now += rebroadcastDelayMs(attempt);
      const refused = await withLogs(rebroadcastOnce(deps, state));
      expect(refused.result).toBe("submit_failed");
      levels.push(
        ...refused.logs.flatMap(({ level, message }) =>
          /Rebroadcast \d+ of signed commit/.test(message) ? [level] : [],
        ),
      );
    }
    expect(levels).toEqual(["WARN", "DEBUG", "DEBUG"]);
    chain.failSubmit = false;
    chain.now += REBROADCAST_MAX_DELAY_MS;
    expect(await Effect.runPromise(rebroadcastOnce(deps, state))).toBe(
      "submitted",
    );
    expect(chain.submitted).toHaveLength(4);
    // Another intent is warned about anew.
    const other = signedIntent(101, 160);
    chain.intent = other;
    chain.failSubmit = true;
    await Effect.runPromise(rebroadcastOnce(deps, state));
    chain.now += REBROADCAST_INITIAL_DELAY_MS;
    const first = await withLogs(rebroadcastOnce(deps, state));
    expect(first.logs.map(({ level }) => level)).toContain("WARN");
  });

  it("never resubmits once the tip reaches the TTL", async () => {
    const intent = signedIntent(100, 160);
    const { chain, tick } = harness(intent);
    chain.tip = 160;
    expect(await tick()).toBe("first_seen");
    chain.now += REBROADCAST_INITIAL_DELAY_MS;
    expect(await tick()).toBe("ttl_reached");
    chain.tip = 500;
    expect(await tick()).toBe("ttl_reached");
    expect(chain.submitted).toEqual([]);
  });

  it("never resubmits once the base output is spent", async () => {
    const intent = signedIntent(100, 160);
    const { chain, tick } = harness(intent);
    chain.tip = 120;
    expect(await tick()).toBe("first_seen");
    chain.now += REBROADCAST_INITIAL_DELAY_MS;
    expect(await tick()).toBe("submitted");
    chain.now += REBROADCAST_ACCEPTED_WAIT_MS;
    chain.baseUnspent = false;
    expect(await tick()).toBe("base_spent");
    chain.now += REBROADCAST_MAX_DELAY_MS;
    expect(await tick()).toBe("base_spent");
    expect(chain.submitted).toEqual([intent.signedTxCbor]);
  });

  it("forgets an intent that is no longer persisted and unaccepted", async () => {
    const intent = signedIntent(100, 160);
    const { chain, state, tick } = harness(intent);
    expect(await tick()).toBe("first_seen");
    chain.intent = undefined;
    expect(await tick()).toBe("none");
    expect(state.size).toBe(0);
  });

  it("backs off by doubling to a cap", () => {
    expect(
      [1, 2, 3, 4, 5, 6, 10].map((attempts) => rebroadcastDelayMs(attempts)),
    ).toEqual([2_000, 4_000, 8_000, 16_000, 30_000, 30_000, 30_000]);
  });
});

describe("signed-intent rebroadcast tick", () => {
  const globals = () =>
    Effect.runSync(
      Effect.gen(function* () {
        return {
          RESET_IN_PROGRESS: yield* Ref.make(false),
          COMMIT_WORKER_ACTIVE: yield* Ref.make(false),
          L1_CONTROL_PLANE: yield* Effect.makeSemaphore(1),
        };
      }),
    );

  it("never reads the journal while a reset or the commit worker runs, or while the L1 control plane is held", async () => {
    const intent = signedIntent(100, 160);
    const { chain, deps, state } = harness(intent);
    let reads = 0;
    const counted = {
      ...deps,
      intent: Effect.sync(() => {
        reads += 1;
        return chain.intent;
      }),
    };
    const g = globals();
    const tick = () =>
      Effect.runPromise(
        rebroadcastTick(g, counted, state, { last: undefined }),
      );
    Effect.runSync(Ref.set(g.RESET_IN_PROGRESS, true));
    expect(await tick()).toBe("reset_in_progress");
    Effect.runSync(Ref.set(g.RESET_IN_PROGRESS, false));
    Effect.runSync(Ref.set(g.COMMIT_WORKER_ACTIVE, true));
    expect(await tick()).toBe("commit_worker_active");
    Effect.runSync(Ref.set(g.COMMIT_WORKER_ACTIVE, false));
    const busy = await Effect.runPromise(
      g.L1_CONTROL_PLANE.withPermits(1)(
        rebroadcastTick(g, counted, state, { last: undefined }),
      ),
    );
    expect(busy).toBe("control_plane_busy");
    expect(reads).toBe(0);
    // Free: the pass runs.
    expect(await tick()).toBe("first_seen");
    expect(reads).toBe(1);
  });

  it.each(["RESET_IN_PROGRESS", "COMMIT_WORKER_ACTIVE"] as const)(
    "the scheduled fiber neither reads the journal nor submits while %s is set, and resubmits once it clears",
    async (flag) => {
      const intent = signedIntent(100, 160);
      const { chain, deps } = harness(intent);
      chain.tip = 120;
      let reads = 0;
      const clocked = {
        ...deps,
        intent: Effect.sync(() => {
          reads += 1;
          return chain.intent;
        }),
        nowMs: () => (chain.now += 10_000),
      };
      const g = globals();
      Effect.runSync(Ref.set(g[flag], true));
      // Four ticks; the flag clears after the second.
      const schedule = Schedule.recurs(3).pipe(
        Schedule.tapOutput((n) =>
          n === 1 ? Ref.set(g[flag], false) : Effect.void,
        ),
      );
      await Effect.runPromise(runRebroadcastFiber(g, clocked, schedule));
      // Gated ticks read nothing; the free ones see it, then resubmit it.
      expect(reads).toBe(2);
      expect(chain.submitted).toEqual([intent.signedTxCbor]);
      // Held set for the whole run: nothing at all.
      const held = harness(intent);
      held.chain.tip = 120;
      Effect.runSync(Ref.set(g[flag], true));
      await Effect.runPromise(
        runRebroadcastFiber(
          g,
          { ...held.deps, nowMs: () => (held.chain.now += 10_000) },
          Schedule.recurs(3),
        ),
      );
      expect(held.chain.submitted).toEqual([]);
    },
  );

  it("warns once about a persistent failure (bytes that are not the intended transaction), then logs it at debug", async () => {
    const intent = signedIntent(100, 160);
    const { deps, state } = harness(intent);
    const refused = {
      ...deps,
      intent: Effect.try(() =>
        decodeUnacceptedSignedIntent({
          header_hash: Buffer.from("bb".repeat(28), "hex"),
          intended_tx_hash: Buffer.from("cc".repeat(32), "hex"),
          signed_tx_cbor: Buffer.from(intent.signedTxCbor, "hex"),
          base_tail_out_ref: BASE,
        }),
      ),
    };
    const g = globals();
    const failures = { last: undefined };
    const levels: string[] = [];
    for (let tick = 0; tick < 3; tick += 1) {
      const { result, logs } = await withLogs(
        rebroadcastTick(g, refused, state, failures),
      );
      expect(result).toBe("failed");
      levels.push(
        ...logs.flatMap(({ level, message }) =>
          message.includes("not its intended transaction") ? [level] : [],
        ),
      );
      // A busy tick between two failing ones runs no pass, so the failure
      // is not warned again.
      if (tick === 0)
        expect(
          await Effect.runPromise(
            g.L1_CONTROL_PLANE.withPermits(1)(
              rebroadcastTick(g, refused, state, failures),
            ),
          ),
        ).toBe("control_plane_busy");
    }
    expect(levels).toEqual(["WARN", "DEBUG", "DEBUG"]);
    // A pass that runs again clears it: a later failure is warned anew.
    await Effect.runPromise(rebroadcastTick(g, deps, state, failures));
    expect(failures.last).toBeUndefined();
  });
});
