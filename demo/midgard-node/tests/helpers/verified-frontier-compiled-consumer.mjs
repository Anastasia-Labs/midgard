// The compiled commit worker's side of `WorkerInput.verifiedFrontier`: it
// reads the frontier from the input its parent posted, with the built
// codec, and resolves a landed commit base from it with the built check.
// The landed ledger arrives as hex and is rebuilt into the Buffers the
// worker's database reads return.
import { parentPort, workerData } from "node:worker_threads";

import { Effect, Either } from "effect";

const { parseVerifiedFrontier } = await import(workerData.codecUrl);
const { verifiedLandedLedgerAt } = await import(workerData.baseUrl);

const bytes = (hex) => Buffer.from(hex, "hex");
const { landed } = workerData;
const ledger = {
  frontier: landed.frontier,
  generation: landed.generation,
  confirmed: landed.confirmed.map((entry) => ({
    tx_id: Buffer.alloc(32),
    address: "addr",
    outref: bytes(entry.outref),
    output: bytes(entry.output),
  })),
  chain: landed.chain.map((row) => ({
    ...row,
    spent: row.spent.map(bytes),
    produced: row.produced.map((entry) => ({
      outref: bytes(entry.outref),
      output: bytes(entry.output),
    })),
  })),
};

const outcome = await Effect.runPromise(
  Effect.either(
    Effect.flatMap(
      parseVerifiedFrontier(workerData.input.verifiedFrontier),
      (verified) =>
        verifiedLandedLedgerAt(ledger, workerData.headerHash, verified),
    ),
  ),
);
parentPort.postMessage(
  Either.isLeft(outcome)
    ? { refused: outcome.left._tag, headerHash: outcome.left.headerHash }
    : {
        root: outcome.right?.root,
        keys: [...(outcome.right?.ledger.keys() ?? [])].sort(),
      },
);
