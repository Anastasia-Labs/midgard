import { writeFileSync } from "node:fs";
import { parentPort, threadId } from "node:worker_threads";

import sodium from "libsodium-wrappers-sumo";

// The validation worker as the node runs it (the built `dist/validation.js`),
// with its own libsodium initialisation set by the test: `fault` rejects,
// `never` never settles, `fault-once` rejects in the first worker only and
// `ready` is libsodium's own. Each worker records that it read it.
//
// Or the worker stops while it answers its readiness job: `crash` throws an
// error it does not handle (`crash-once`: in the first worker only),
// `out-of-memory` allocates until the heap limit the test gives it; or
// `load` imports a module that is not there in place of the validation
// worker.
const mode = process.env.MIDGARD_TEST_WORKER_READINESS_MODE;
const marker = process.env.MIDGARD_TEST_WORKER_READINESS_MARKER;
if (
  ![
    "fault",
    "never",
    "fault-once",
    "ready",
    "crash",
    "crash-once",
    "out-of-memory",
    "load",
  ].includes(mode) ||
  !marker
) {
  throw new Error("the worker readiness test sets its mode and marker");
}
if (mode === "load") {
  await import("./listen-startup.validation-worker-readiness.absent.mjs");
}
const firstWorker = () => {
  try {
    writeFileSync(`${marker}.first`, String(threadId), { flag: "wx" });
    return true;
  } catch {
    return false;
  }
};
if (mode === "crash" || (mode === "crash-once" && firstWorker())) {
  parentPort.once("message", () => {
    throw new Error("injected worker crash");
  });
}
if (mode === "out-of-memory") {
  parentPort.once("message", () => {
    const held = [];
    for (;;) held.push(new Array(1_000_000).fill(held.length));
  });
}
const faults = (() => {
  if (mode === "fault") return true;
  return mode === "fault-once" && firstWorker();
})();
const originalReady = sodium.ready;
const ready = faults
  ? Promise.reject(
      new Error("injected worker libsodium initialisation failure"),
    )
  : mode === "never"
    ? new Promise(() => undefined)
    : originalReady;
ready.catch(() => undefined);
Object.defineProperty(sodium, "ready", {
  configurable: true,
  get() {
    writeFileSync(`${marker}.${threadId}`, mode);
    return ready;
  },
});

await import("../dist/validation.js");
