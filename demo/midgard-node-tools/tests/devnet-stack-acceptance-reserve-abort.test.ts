import { join } from "node:path";

import { Command } from "commander";
import { afterEach, expect, it, vi } from "vitest";

import { registerFinalAcceptance } from "../src/devnet-stack/acceptance-cli.js";
import { Journal } from "../src/devnet-stack/journal.js";
import {
  fakeContext,
  removeFakeContexts,
} from "./devnet-stack-journey.fixtures.js";

const state = vi.hoisted(() => ({ invoked: false, builds: 0, submissions: 0 }));
vi.mock(
  "../src/devnet-stack/reserve-float-chain.js",
  async (importOriginal) => {
    const original =
      await importOriginal<
        typeof import("../src/devnet-stack/reserve-float-chain.js")
      >();
    return {
      ...original,
      provisionReserveFloat: (
        ...args: Parameters<typeof original.provisionReserveFloat>
      ) =>
        original.provisionReserveFloat(
          args[0],
          args[1],
          args[2],
          {
            reserveAddress: "owned-reserve-fixture",
            unspentAt: async () => {
              // The current production authority read is awaiting when SIGTERM arrives.
              process.emit("SIGTERM");
              await Promise.resolve();
              return [];
            },
            buildPayment: async () => {
              state.builds += 1;
              return {
                txId: "aa".repeat(32),
                signedTx: "owned-signed-bytes-fixture",
                input: "bb#0",
              };
            },
            submit: async () => {
              state.submissions += 1;
            },
            landed: async () => true,
            outputState: async () => "unspent",
            sleep: async () => {},
            now: Date.now,
            log: () => {},
          },
          args[4],
        ),
    };
  },
);
vi.mock("../src/devnet-stack/acceptance.js", () => ({
  runFinalAcceptance: async (
    _context: unknown,
    _pin: unknown,
    _assets: unknown,
    options: { signal: AbortSignal },
  ) => {
    state.invoked = true;
    if (options.signal.aborted) throw new Error("cancelled after provisioning");
    return {};
  },
}));
afterEach(removeFakeContexts);
it("does not start later provisioning work after SIGTERM requests cancellation", async () => {
  const context = fakeContext();
  Object.assign(context.layout, {
    journal: join(context.layout.nodeRoot, "controller-journal.json"),
    state: context.layout.nodeRoot,
  });
  new Journal(context.layout.journal).set("funding", { assets: [] });
  const command = new Command();
  registerFinalAcceptance(command, () => ({ context, oneShot: {} as never }));
  const task = command.parseAsync([
    "node",
    "cli",
    "acceptance",
    "--run-dir",
    context.layout.nodeRoot,
    "--deadline",
    "1",
  ]);
  await expect(
    task.catch((error) => {
      console.error(error.stack);
      throw error;
    }),
  ).rejects.toThrow(/cancelled/);
  expect(state.invoked).toBe(false);
  expect(state.builds).toBe(0);
  expect(state.submissions).toBe(0);
});
