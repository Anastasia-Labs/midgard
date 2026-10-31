import { join } from "node:path";

import { afterEach, expect, it, vi } from "vitest";

import { runFinalAcceptance } from "../src/devnet-stack/acceptance.js";
import type { ChaosDeps } from "../src/devnet-stack/chaos.js";
import {
  fakeContext,
  removeFakeContexts,
} from "./devnet-stack-journey.fixtures.js";
const state = vi.hoisted(() => ({ deps: undefined as ChaosDeps | undefined }));
vi.mock("../src/devnet-stack/chaos.js", async (original) => ({
  ...(await original<typeof import("../src/devnet-stack/chaos.js")>()),
  productionChaosDeps: () => state.deps,
}));
vi.mock("../src/devnet-stack/services.js", () => ({
  serviceSpecs: () =>
    [
      "node",
      "da-committee-0",
      "da-committee-1",
      "public-retained-da",
      "watcher",
    ].map((name) => ({ name })),
  supervisorPaths: () => ({ pidDir: "/owned-unused-pid-dir" }),
}));
afterEach(removeFakeContexts);
it("does not inject the first fault when overall cancellation arrives during the final preflight read", async () => {
  const context = fakeContext();
  Object.assign(context.layout, {
    runDir: context.layout.nodeRoot,
    drillsLog: join(context.layout.nodeRoot, "drills.ndjson"),
  });
  Object.assign(context.run, { ogmiosPort: 2337, postgresPort: 5544 });
  const abort = new AbortController();
  let clock = 1_000_000;
  const composed: string[] = [];
  state.deps = {
    now: () => clock,
    sleep: async (ms) => {
      clock += ms;
    },
    readiness: async () => {
      // Matches production cancellation during an asynchronous final readiness fetch.
      if (clock >= 1_300_000) abort.abort();
      return [{ name: "node", alive: true, ready: true, pid: 44 }];
    },
    waitReady: async () => ({ graced: false }),
    supervisorEvents: () => [],
    compose: async (args) => {
      composed.push(args[0]!);
      return { code: 0, stderr: "" };
    },
    fetchNode: async () => ({}),
    environ: () => undefined,
    signalGroup: () => {
      throw new Error("unused process injection");
    },
  };
  await expect(
    runFinalAcceptance(context, {} as never, [], {
      deadlineMs: 10_000,
      signal: abort.signal,
    }),
  ).rejects.toThrow(/acceptance/);
  expect(composed).not.toContain("restart");
});
