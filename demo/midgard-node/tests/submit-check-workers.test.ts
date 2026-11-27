/**
 * `POST /submit` on the submit check workers, through the raw handler on a
 * real HTTP server: a full check queue, a worker's verdict taken as it is,
 * and workers killed under load.
 */
import { request as httpRequest } from "node:http";

import { afterEach, describe, expect, it } from "vitest";

import {
  makeNativeSubmitTx,
  wrapNativeSubmitTx,
} from "./database.test/fixtures.submission.js";
import {
  admitted,
  cleanups,
  fixtureWorker,
  MEDIA,
  noLimits,
  runCleanups,
  serveSubmit,
  startPool,
} from "./helpers/submit-request-server.js";

afterEach(runCleanups);

/** Polls `condition` every 20 ms for up to 10 s. */
const until = async (condition: () => boolean) => {
  const deadline = Date.now() + 10_000;
  while (!condition()) {
    if (Date.now() > deadline) throw new Error("condition not met in 10 s");
    await new Promise((resolve) => setTimeout(resolve, 20));
  }
};

const CHECK_UNAVAILABLE = {
  status: 503,
  headers: expect.objectContaining({ "retry-after": "5" }),
  body: { error: "Submit check is unavailable; retry later" },
};

describe("POST /submit on the submit check workers", () => {
  it("answers 503 with keep-alive while the check queue is full, and frees a slot whose client goes away", async () => {
    const pool = startPool({ maxQueued: 1 });
    cleanups.push(() => pool.close());
    const node = await serveSubmit(noLimits, pool);
    const body = wrapNativeSubmitTx(makeNativeSubmitTx().txCanonicalCbor);
    // A request whose body is half sent holds the only slot.
    const slow = httpRequest({
      host: "127.0.0.1",
      port: node.port,
      path: "/submit",
      method: "POST",
      agent: false,
      headers: {
        "content-type": MEDIA,
        "content-length": body.length.toString(),
        "x-midgard-client-ip": "198.51.100.8",
      },
    });
    slow.on("error", () => {});
    slow.write(body.subarray(0, 10));
    await until(() => pool.stats().held === 1);

    const full = await node.call({ body });
    expect(full).toMatchObject({
      status: 503,
      headers: { "retry-after": "5" },
      body: { error: "Submit ingress capacity is full; retry later" },
    });
    expect(full.headers.connection).not.toBe("close");

    slow.destroy();
    await until(() => pool.stats().held === 0);
    expect((await node.call({ body })).status).toBe(202);
    // The refused request and the admitted one shared a connection.
    expect(node.connections()).toBe(2);
  });

  it("takes the worker's verdict as it is: the main thread neither decodes nor hashes the body", async () => {
    const pool = startPool({
      entry: fixtureWorker("submit-check-canned-worker.mjs"),
    });
    cleanups.push(() => pool.close());
    const node = await serveSubmit(noLimits, pool);
    // Not an envelope: a check on the main thread would refuse it.
    const reply = await node.call({ body: Buffer.from("not an envelope") });
    expect(reply).toMatchObject({
      status: 202,
      body: { txId: "11".repeat(32), duplicate: false },
    });
    expect(admitted.at(-1)).toEqual({
      txId: "11".repeat(32),
      txFullHash: "22".repeat(32),
    });
  });

  it(
    "answers 503 for the requests a killed worker held, replaces it, and keeps serving under load",
    { timeout: 120_000 },
    async () => {
      const stopped: string[] = [];
      const pool = startPool({
        size: 2,
        entry: fixtureWorker("submit-check-crashing-worker.mjs"),
        onWorkerStopped: (_, detail) => stopped.push(detail),
      });
      cleanups.push(() => pool.close());
      const node = await serveSubmit(noLimits, pool);
      const valid = () =>
        node.call({
          body: wrapNativeSubmitTx(makeNativeSubmitTx().txCanonicalCbor),
        });
      const admitWithin10s = async () => {
        const deadline = Date.now() + 10_000;
        for (;;) {
          const reply = await valid();
          if (reply.status === 202) return;
          expect(reply).toMatchObject(CHECK_UNAVAILABLE);
          if (Date.now() > deadline) throw new Error("no worker came back");
          await new Promise((resolve) => setTimeout(resolve, 50));
        }
      };
      const exit = Buffer.from("dead", "hex");
      const thrown = Buffer.from("bad0", "hex");

      await admitWithin10s();
      // A worker that exits, then one that throws, each alone.
      expect(await node.call({ body: exit })).toMatchObject(CHECK_UNAVAILABLE);
      await admitWithin10s();
      expect(await node.call({ body: thrown })).toMatchObject(
        CHECK_UNAVAILABLE,
      );
      await admitWithin10s();
      expect(stopped).toEqual([
        expect.stringMatching(/^submit check worker \d exited with code 3$/u),
        expect.stringMatching(
          /^submit check worker \d stopped on an error it did not handle: submit check worker test fault$/u,
        ),
      ]);

      // Under load: 200 valid submissions with a fault every 50.
      const replies = await Promise.all(
        Array.from({ length: 200 }, (_, index) =>
          index % 50 === 25
            ? node.call({ body: index % 100 === 25 ? exit : thrown })
            : valid(),
        ),
      );
      for (const [index, reply] of replies.entries()) {
        if (index % 50 === 25) expect(reply).toMatchObject(CHECK_UNAVAILABLE);
        else if (reply.status !== 202)
          expect(reply).toMatchObject(CHECK_UNAVAILABLE);
      }
      // Every request was answered; the ones no fault reached were admitted.
      // How many shared a batch with a fault, or waited on a replacement
      // still loading, depends on timing.
      expect(
        replies.filter((reply) => reply.status === 202).length,
      ).toBeGreaterThan(0);
      expect(stopped.length).toBeGreaterThan(2);

      // Nothing is held, and the replaced workers admit again.
      await admitWithin10s();
      await until(() => pool.stats().liveWorkers === 2);
      expect(pool.stats()).toMatchObject({ held: 0, heldBytes: 0 });
    },
  );
});
