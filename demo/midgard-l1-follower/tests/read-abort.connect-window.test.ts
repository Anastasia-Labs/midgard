import { describe, expect, it } from "vitest";

import { openPostgresBackend } from "../src/index.js";

/**
 * A read whose signal fires after its pooled connection arrived but before
 * the read began still ends at that abort and gives the connection up. The
 * pool is a stand-in for `pg`'s: it hands over a connection whose server
 * never answers, and fires the abort in the microtask after the hand-over,
 * which is the earliest the read can see it.
 */
describe("a read aborted as its connection arrives", () => {
  it("ends with the abort's reason and discards the connection", async () => {
    const controller = new AbortController();
    const released: unknown[] = [];
    let closed = false;
    const client = {
      on: () => undefined,
      off: () => undefined,
      // A server that never answers; a closed connection refuses at once,
      // as `pg`'s does once its pool ended it.
      query: () =>
        closed
          ? Promise.reject(new Error("the connection is closed"))
          : new Promise<never>(() => undefined),
      release: (reason?: unknown) => {
        released.push(reason);
        if (reason !== undefined) closed = true;
      },
    };
    const pool = {
      connect: () => {
        const connected = Promise.resolve(client);
        queueMicrotask(() => {
          void connected.then(() =>
            controller.abort(new Error("the read's deadline passed")),
          );
        });
        return connected;
      },
    };
    const backend = openPostgresBackend({ pool: pool as never });

    const read = backend.transaction("read", async () => "answered", {
      signal: controller.signal,
    });
    const outcome = await Promise.race([
      read.then(
        () => "answered",
        (error: unknown) => error,
      ),
      new Promise((resolve) => setTimeout(() => resolve("still open"), 1_000)),
    ]);

    expect(outcome).toEqual(new Error("the read's deadline passed"));
    expect(released).toHaveLength(1);
    expect(released[0]).toBeInstanceOf(Error);
  });
});
