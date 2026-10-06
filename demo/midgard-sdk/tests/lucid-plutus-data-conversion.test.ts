import { execFile } from "node:child_process";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { describe, expect, it } from "vitest";

describe.each(["esm", "cjs"])("Lucid Plutus Data conversion (%s)", (format) => {
  it("matches the unpatched conversion on random and boundary values", async () => {
    // Separate processes prove both installed module formats independently.
    const { stdout } = await promisify(execFile)(
      process.execPath,
      [
        fileURLToPath(
          new URL(
            "./support/lucid-plutus-data-conversion.mjs",
            import.meta.url,
          ),
        ),
        format,
        "400",
        format === "esm" ? "1" : "2",
      ],
      { maxBuffer: 16 * 1024 * 1024 },
    );
    const report = JSON.parse(stdout);
    expect(report.format).toBe(format);
    expect(report.mismatches).toEqual([]);
    expect(report.toCases).toBeGreaterThanOrEqual(800);
    expect(report.toRefusals).toBeGreaterThan(0);
    expect(report.fromCases).toBeGreaterThan(1_100);
    expect(report.fromRefusals).toBeGreaterThan(0);
    expect(report.largeInputs).toBeGreaterThan(0);
  }, 300_000);
});
