import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { it } from "vitest";

import { verifyJourneyConfiguration } from "./configuration.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(runDirectory === undefined)(
  "matches the live ledger to verified Preprod before publication",
  async () => {
    const env = Object.fromEntries(
      (await readFile(join(runDirectory!, "run.env"), "utf8"))
        .trim()
        .split("\n")
        .map((line) => {
          const separator = line.indexOf("=");
          return [line.slice(0, separator), line.slice(separator + 1)];
        }),
    );
    await verifyJourneyConfiguration({
      runDirectory: runDirectory!,
      ogmiosUrl: `http://127.0.0.1:${env.MIDGARD_PHASE4_OGMIOS_PORT}`,
    });
  },
);
