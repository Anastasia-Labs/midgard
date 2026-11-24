import "./canonical-evidence-source.q03-canonical-evidence-builders.js";

import { describe, expect, it, vi } from "vitest";

import { main as cliMain } from "../src/bin.js";

describe("labelled diagnostics", () => {
  it("rejects diagnostic grade on every prepare CLI verb before proof construction", async () => {
    const priorArgv = process.argv;
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    const cases = [
      ["prepare-double-spend", "--header-hash", "11".repeat(28)],
      [
        "prepare-invalid-range",
        "--header-hash",
        "11".repeat(28),
        "--block-slot",
        "10",
      ],
      ["prepare-non-existent-input", "--header-hash", "11".repeat(28)],
      [
        "prepare-zero-input",
        "--header-hash",
        "11".repeat(28),
        "--expected-transactions-root",
        "22".repeat(32),
      ],
    ];
    try {
      for (const commandArgs of cases) {
        process.argv = [
          "node",
          "midgard-fault-proofs",
          ...commandArgs,
          "--midgard-node-url",
          "http://operator.invalid",
        ];
        await expect(cliMain()).rejects.toThrow(/prohibited_trust_class/u);
      }
      expect(stderr).toHaveBeenCalledTimes(4);
      for (const call of stderr.mock.calls) {
        expect(call[0]).toMatch(/^DIAGNOSTIC EVIDENCE/u);
      }
    } finally {
      process.argv = priorArgv;
      stderr.mockRestore();
    }
  });
});
