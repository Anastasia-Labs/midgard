// A submit check worker for the pool's tests. It never decodes: every body
// is accepted with tx id 32 × 0x11, full hash 32 × 0x22 and the body itself
// as the canonical bytes, and `fee` is the number of bodies in the batch it
// came in. A body spelling "hang" in ASCII makes the worker spin forever.
// It loads, taking batches and saying so, after `config.cannedLoadDelayMs`
// (default 0), as a slow import would; never when that is negative.
import { parentPort, workerData } from "node:worker_threads";

const HANG = Buffer.from("hang");

const answer = ({ batch, bodies }) => {
  const transfer = [];
  const results = bodies.map((body) => {
    const view = new Uint8Array(body);
    if (Buffer.from(view).equals(HANG)) for (;;);
    const bytes = new ArrayBuffer(64 + view.length);
    const out = new Uint8Array(bytes);
    out.fill(0x11, 0, 32);
    out.fill(0x22, 32, 64);
    out.set(view, 64);
    transfer.push(bytes);
    return {
      ok: true,
      bytes,
      txIdHex: "11".repeat(32),
      spentOutRefs: [],
      producedOutRefs: [],
      fee: BigInt(bodies.length),
      byteLength: view.length,
    };
  });
  parentPort.postMessage({ batch, results }, transfer);
};

const loadDelayMs = workerData.config.cannedLoadDelayMs ?? 0;
if (loadDelayMs >= 0)
  setTimeout(() => {
    parentPort.on("message", answer);
    parentPort.postMessage({ ready: true });
  }, loadDelayMs);
// A worker that never loads stays up until it is stopped.
else setInterval(() => {}, 60_000);
