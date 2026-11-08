/**
 * The submit check pool with real worker threads: the production worker's
 * verdicts against the check run inline, the batching of one tick's bodies,
 * the bounds, the wait for a loading worker, and the replacement of a
 * worker that stops answering or never loads.
 */
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { pathToFileURL } from "node:url";

import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { encodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  buildCorpus,
  parseJunkMix,
  readCorpus,
} from "../scripts/submit-ingress-load.corpus.mjs";
import {
  checkSubmissionBody,
  type SubmissionCheck,
  type SubmissionCheckConfig,
} from "../src/commands/http/submission-check.js";
import {
  createSubmitCheckPool,
  type SubmitCheckOutcome,
  type SubmitCheckPool,
  type SubmitCheckPoolOptions,
} from "../src/services/submit-check-pool.js";

// The public testnet's rules, which the load corpus's valid txs pay.
const config: SubmissionCheckConfig = {
  maxSubmitTxCborBytes: MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
  rules: {
    expectedNetworkId: 0n,
    minFeeA: 1n,
    minFeeB: 2_000n,
    slotConfig: { ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod },
  },
};
const cannedWorker = pathToFileURL(
  path.resolve("tests/fixtures/submit-check-canned-worker.mjs"),
);

const pools: SubmitCheckPool[] = [];
afterEach(async () => {
  for (const pool of pools.splice(0)) await pool.close();
});

const startPool = (options: Partial<SubmitCheckPoolOptions> = {}) => {
  const pool = createSubmitCheckPool({
    size: 2,
    maxQueued: 1_024,
    maxQueuedBytes: MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
    config,
    ...options,
  });
  pools.push(pool);
  return pool;
};

/** Reserves a slot and posts `body`; resolves with its outcome. */
const check = (pool: SubmitCheckPool, body: Buffer) =>
  new Promise<SubmitCheckOutcome>((resolve, reject) => {
    const ticket = pool.reserve(Math.max(1, body.length));
    if (ticket === undefined) {
      reject(new Error("the pool refused a slot"));
      return;
    }
    // The pool takes the buffer; the caller's copy stays readable.
    ticket.check(Buffer.from(body), resolve);
  });

/** A verdict as plain data, buffers as hex, for deep comparison. */
const plain = (check: SubmissionCheck) =>
  JSON.parse(
    JSON.stringify(check, (_, value: unknown) =>
      typeof value === "bigint" ? `${value.toString()}n` : value,
    ),
    (_, value: unknown) =>
      value !== null &&
      typeof value === "object" &&
      (value as { type?: unknown }).type === "Buffer"
        ? Buffer.from((value as { data: number[] }).data).toString("hex")
        : value,
  ) as unknown;

const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "submit-check-pool-"));
afterAll(() => fs.rmSync(scratch, { recursive: true, force: true }));

describe("submit check pool", () => {
  it("gives the production worker's verdicts exactly as the check run inline", async () => {
    const corpus = path.join(scratch, "corpus");
    buildCorpus({
      directory: corpus,
      seed: "submit-check-pool",
      validCount: 24,
      junkMix: parseJunkMix(
        "bad-signature=0.1,low-fee=0.1,missing-input=0.1,bad-cbor=0.1,double-spend=0.1",
      ),
      keyCount: 8,
    });
    const bodies = [
      ...readCorpus(corpus).requests.map(({ body }) => Buffer.from(body)),
      Buffer.from("ff00ff", "hex"),
      encodeMidgardProofSubmission({
        transactionCbor: Buffer.from("8200", "hex"),
      }),
      encodeMidgardProofSubmission({
        transactionCbor: Buffer.alloc(
          MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes + 1,
        ),
      }),
    ];
    const pool = startPool();
    const outcomes = await Promise.all(bodies.map((body) => check(pool, body)));
    // The inline check below verifies signatures on this thread.
    await ledgerEd25519SignatureReady();
    const kinds = new Set<string>();
    outcomes.forEach((outcome, index) => {
      expect(outcome.kind).toBe("checked");
      if (outcome.kind !== "checked") return;
      const inline = checkSubmissionBody(bodies[index]!, config, Date.now());
      expect(plain(outcome.check)).toEqual(plain(inline));
      kinds.add(inline.ok ? "accepted" : `${inline.reason}:${inline.status}`);
    });
    // The set covers acceptance and every refusal kind the check gives.
    expect([...kinds].sort()).toEqual([
      "accepted",
      "invalid_transaction:400",
      "invalid_transaction:413",
      "malformed_envelope:400",
    ]);
    expect(pool.stats()).toMatchObject({ held: 0, heldBytes: 0, restarts: 0 });
  });

  it("sends the bodies of one tick to a worker as one message", async () => {
    const pool = startPool({ size: 1, entry: cannedWorker });
    const outcomes = await Promise.all(
      Array.from({ length: 10 }, (_, index) =>
        check(pool, Buffer.from([index])),
      ),
    );
    for (const [index, outcome] of outcomes.entries()) {
      expect(outcome.kind).toBe("checked");
      if (outcome.kind !== "checked" || !outcome.check.ok) continue;
      // The canned worker reports its batch size as the fee.
      expect(outcome.check.normalized.fee).toBe(10n);
      expect(outcome.check.normalized.txCanonicalCbor).toEqual(
        Buffer.from([index]),
      );
      expect(outcome.check.normalized.txFullHash).toEqual(
        Buffer.alloc(32, 0x22),
      );
    }
  });

  it("refuses a slot past the queue or byte bound, and frees it on release", () => {
    const pool = startPool({
      size: 1,
      entry: cannedWorker,
      maxQueued: 2,
      maxQueuedBytes: 100,
    });
    const first = pool.reserve(60)!;
    expect(first).toBeDefined();
    expect(pool.reserve(41)).toBeUndefined();
    const second = pool.reserve(40)!;
    expect(second).toBeDefined();
    expect(pool.reserve(1)).toBeUndefined();
    first.release();
    first.release();
    expect(pool.stats()).toMatchObject({ held: 1, heldBytes: 40 });
    second.release();
    expect(pool.reserve(100)).toBeDefined();
  });

  it("answers a batch its worker does not finish as unavailable and replaces the worker", async () => {
    const stopped: string[] = [];
    const pool = startPool({
      size: 1,
      entry: cannedWorker,
      batchTimeoutMs: 300,
      onWorkerStopped: (_, detail) => stopped.push(detail),
    });
    const hung = await check(pool, Buffer.from("hang"));
    expect(hung).toEqual({
      kind: "unavailable",
      detail: "submit check worker 0 did not answer a batch within 300 ms",
    });
    expect(stopped).toHaveLength(1);
    // The replacement answers once it is up.
    let next: SubmitCheckOutcome;
    const deadline = Date.now() + 10_000;
    do {
      await new Promise((resolve) => setTimeout(resolve, 50));
      next = await check(pool, Buffer.from([1]));
    } while (next.kind === "unavailable" && Date.now() < deadline);
    expect(next.kind).toBe("checked");
    expect(pool.stats()).toMatchObject({
      liveWorkers: 1,
      held: 0,
      restarts: 1,
    });
  });

  it("holds bodies posted while its worker loads, with no batch timer running", async () => {
    // The worker loads for longer than the batch timeout; a body posted at
    // once waits for it and is checked.
    const pool = startPool({
      size: 1,
      entry: cannedWorker,
      batchTimeoutMs: 300,
      config: { ...config, cannedLoadDelayMs: 900 } as SubmissionCheckConfig,
    });
    expect(pool.stats()).toMatchObject({ liveWorkers: 1, readyWorkers: 0 });
    const outcome = await check(pool, Buffer.from([7]));
    expect(outcome.kind).toBe("checked");
    expect(pool.stats()).toMatchObject({ readyWorkers: 1, restarts: 0 });
  });

  it("stops a worker that does not load in time and answers its waiting bodies", async () => {
    const stopped: string[] = [];
    const pool = startPool({
      size: 1,
      entry: cannedWorker,
      startTimeoutMs: 300,
      config: { ...config, cannedLoadDelayMs: -1 } as SubmissionCheckConfig,
      onWorkerStopped: (_, detail) => stopped.push(detail),
    });
    expect(await check(pool, Buffer.from([7]))).toEqual({
      kind: "unavailable",
      detail: "submit check worker 0 did not load within 300 ms",
    });
    expect(stopped).toEqual([
      "submit check worker 0 did not load within 300 ms",
    ]);
    expect(pool.stats()).toMatchObject({ held: 0, restarts: 1 });
  });

  it("tells a caller once a worker has loaded, or that none loaded in time", async () => {
    const loading = startPool({
      size: 1,
      entry: cannedWorker,
      config: { ...config, cannedLoadDelayMs: 300 } as SubmissionCheckConfig,
    });
    expect(loading.stats().readyWorkers).toBe(0);
    expect(await loading.whenReady(10_000)).toBe(true);
    expect(loading.stats().readyWorkers).toBe(1);
    expect(await loading.whenReady(0)).toBe(true);

    const stuck = startPool({
      size: 1,
      entry: cannedWorker,
      config: { ...config, cannedLoadDelayMs: -1 } as SubmissionCheckConfig,
    });
    expect(await stuck.whenReady(200)).toBe(false);
    const waiting = stuck.whenReady(10_000);
    await stuck.close();
    expect(await waiting).toBe(false);
  });

  it("answers every held body as unavailable when it closes", async () => {
    const pool = startPool({ size: 1, entry: cannedWorker });
    const pending = check(pool, Buffer.from("hang"));
    await new Promise((resolve) => setTimeout(resolve, 100));
    await pool.close();
    expect(await pending).toEqual({
      kind: "unavailable",
      detail: "submit check pool closed",
    });
    expect(pool.reserve(1)).toBeUndefined();
  });
});
