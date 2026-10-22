/**
 * The defense material worker: one build per thread, its job in
 * `workerData`, its outcome posted once to the parent. A build that throws
 * is posted as `unavailable`, and the defender retries it at a later tick.
 */
import { parentPort, workerData } from "node:worker_threads";

import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";

import {
  buildPortableDefenseOutcome,
  type DefenseMaterialJob,
  type PortableDefenseOutcome,
} from "../validation-trace-defense/material-build.js";

const port = parentPort;
if (port === null)
  throw new Error("the defense material worker requires a parent port");

// Witness signatures are verified by libsodium, which initialises
// asynchronously: the build waits for it.
void ledgerEd25519SignatureReady()
  .then(() => buildPortableDefenseOutcome(workerData as DefenseMaterialJob))
  .then(
    (outcome) => port.postMessage(outcome),
    (cause: unknown) =>
      port.postMessage({
        kind: "unavailable",
        detail: formatUnknownError(cause),
      } satisfies PortableDefenseOutcome),
  );
