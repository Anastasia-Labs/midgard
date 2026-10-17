import { writeFileSync } from "node:fs";
import { threadId } from "node:worker_threads";

import sodium from "libsodium-wrappers-sumo";

// The validation worker as the node runs it (the built `dist/validation.js`),
// with its own libsodium initialisation set by the test: `fault` rejects,
// `never` never settles, `fault-once` rejects in the first worker only and
// `ready` is libsodium's own. Each worker records that it read it.
const mode = process.env.MIDGARD_TEST_WORKER_READINESS_MODE;
const marker = process.env.MIDGARD_TEST_WORKER_READINESS_MARKER;
if (!["fault", "never", "fault-once", "ready"].includes(mode) || !marker) {
  throw new Error("the worker readiness test sets its mode and marker");
}
const faults = (() => {
  if (mode === "fault") return true;
  if (mode !== "fault-once") return false;
  try {
    writeFileSync(`${marker}.first`, String(threadId), { flag: "wx" });
    return true;
  } catch {
    return false;
  }
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
