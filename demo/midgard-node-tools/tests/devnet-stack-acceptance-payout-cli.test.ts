import { Command } from "commander";
import { beforeEach, expect, it, vi } from "vitest";

import { registerAcceptancePayouts } from "../src/devnet-stack/acceptance-payout-cli.js";

const controls = vi.hoisted(() => ({
  sample: vi.fn(),
  start: vi.fn(),
  layout: vi.fn(),
}));
vi.mock("../src/devnet-stack/acceptance-payout-sample.js", () => ({
  sampleAcceptancePayouts: controls.sample,
}));
vi.mock("../src/devnet-stack/fresh-controller.js", () => ({
  startRunUser: controls.start,
}));
vi.mock("../src/devnet-stack/layout.js", () => ({
  makeLayout: controls.layout,
}));
const args = [
  "node",
  "tools",
  "acceptance-payouts",
  "--run-dir",
  "/tmp/owned-fixture",
  "--timeout-ms",
  "10000",
  "--max-transaction-bytes",
  "16384",
  "--max-lineage-transactions",
  "8",
  "--max-settlement-rows",
  "16",
  "--max-kupo-response-bytes",
  "16384",
  "--max-utxo-response-bytes",
  "16384",
  "--max-reference-inputs",
  "4",
  "--block-scan-limit",
  "1",
  "--max-drill-evidence-bytes",
  "16384",
];
beforeEach(() => {
  vi.resetAllMocks();
  controls.layout.mockReturnValue({ runDir: "/tmp/owned-fixture" });
});
const program = () => {
  const command = new Command();
  command.exitOverride();
  registerAcceptancePayouts(command);
  return command;
};

it("uses both existing run guards and passes explicit bounds to the readonly sampler", async () => {
  const output = vi.spyOn(console, "log").mockImplementation(() => {});
  controls.sample.mockResolvedValue({ current: ["fixture"] });
  try {
    await program().parseAsync(args);
  } finally {
    output.mockRestore();
  }
  expect(controls.start.mock.calls.map((row) => row[1])).toEqual([
    "journey",
    "drill",
  ]);
  expect(controls.sample.mock.calls[0]![1]).toEqual({
    timeoutMs: 10000,
    maxTransactionBytes: 16384,
    maxLineageTransactions: 8,
    maxSettlementRows: 16,
    maxKupoResponseBytes: 16384,
    maxUtxoResponseBytes: 16384,
    maxReferenceInputs: 4,
    blockScanLimit: 1,
    maxDrillEvidenceBytes: 16384,
  });
  expect(controls.sample.mock.calls[0]![2]).toBeInstanceOf(AbortSignal);
});
it("refuses invalid bounds before run guards or sampler I/O", async () => {
  const bad = [...args];
  bad[bad.indexOf("10000")] = "0";
  await expect(program().parseAsync(bad)).rejects.toThrow(/positive whole/);
  expect(controls.start).not.toHaveBeenCalled();
  expect(controls.sample).not.toHaveBeenCalled();
});
it("propagates cancellation to its existing sampler signal and awaits physical cleanup", async () => {
  let active!: () => void;
  const entered = new Promise<void>((resolve) => {
    active = resolve;
  });
  let drained = false;
  controls.sample.mockImplementation(
    async (_layout, _limits, signal: AbortSignal) => {
      active();
      await new Promise<void>((resolve) =>
        signal.addEventListener("abort", () => resolve(), { once: true }),
      );
      await new Promise((resolve) => setTimeout(resolve, 20));
      drained = true;
      throw new Error("capture cancelled");
    },
  );
  const before = process.listenerCount("SIGTERM");
  const pending = program().parseAsync(args);
  const refusal = expect(pending).rejects.toThrow(/capture cancelled/);
  await entered;
  process.emit("SIGTERM");
  await refusal;
  expect(drained).toBe(true);
  expect(process.listenerCount("SIGTERM")).toBe(before);
});
