import { Worker } from "node:worker_threads";

import { Effect, Ref } from "effect";
import { expect, it } from "vitest";

import { Globals } from "../src/services/globals.globals.js";
import type { CommitWorkerMessage } from "../src/workers/commit-worker-message.js";

const malformed = [
  {},
  { outRefs: "ab" },
  { outRefs: null },
  { outRefs: ["ab", 1] },
  { outRefs: ["AB"] },
  { outRefs: ["abc"] },
  { outRefs: [""] },
];

it("a revert notice crosses from the compiled worker codec to the compiled dispatcher with its outrefs, and a malformed one marks every key", async () => {
  const codecUrl = new URL(
    "../dist/utils/commit-block-header.js",
    import.meta.url,
  ).href;
  const dispatcher = (await import(
    new URL("../dist/commit-worker-message.js", import.meta.url).href
  )) as typeof import("../src/workers/commit-worker-message.js");
  const outRefs = ["82582000", "8258200a01"];
  const worker = new Worker(
    new URL("./helpers/reverted-notice-compiled-producer.mjs", import.meta.url),
    // The compiled codec uses production import conditions, not Vitest's
    // inherited source-resolution condition.
    { execArgv: [], workerData: { codecUrl, outRefs, malformed } },
  );
  let messages: unknown[];
  try {
    messages = await new Promise<unknown[]>((resolve, reject) => {
      const received: unknown[] = [];
      worker.on("message", (message) => {
        received.push(message);
        if (received.length === 1 + malformed.length) resolve(received);
      });
      worker.once("error", reject);
      worker.once("exit", (code) => {
        if (code !== 0) reject(new Error(`producer exited ${code}`));
      });
    });
  } finally {
    await worker.terminate();
  }

  await Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      const unmarked = { all: false, keys: new Set<string>() };
      const [notice, ...rest] = messages as CommitWorkerMessage[];
      expect(dispatcher.takeCommitWorkerOutput(globals, notice!)).toBe(
        undefined,
      );
      expect(yield* Ref.get(globals.MEMPOOL_LEDGER_STALE)).toStrictEqual({
        all: false,
        keys: new Set(outRefs),
      });
      for (const message of rest) {
        yield* Ref.set(globals.MEMPOOL_LEDGER_STALE, unmarked);
        expect(dispatcher.takeCommitWorkerOutput(globals, message)).toBe(
          undefined,
        );
        expect((yield* Ref.get(globals.MEMPOOL_LEDGER_STALE)).all).toBe(true);
      }
    }).pipe(Effect.provide(Globals.Default)),
  );
});
