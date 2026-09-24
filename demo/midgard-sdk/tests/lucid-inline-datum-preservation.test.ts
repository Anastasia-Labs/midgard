import { execFile } from "node:child_process";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { describe, expect, it } from "vitest";

describe.each(["esm", "cjs"])("Lucid ordered Plutus data (%s)", (format) => {
  it.each(["static", "canonical", "delayed"])(
    "preserves inline, witness and redeemer maps through %s completion",
    async (mode) => {
      // Separate processes prove both installed module formats independently.
      const { stdout } = await promisify(execFile)(process.execPath, [
        fileURLToPath(
          new URL(
            "./support/lucid-inline-datum-preservation.mjs",
            import.meta.url,
          ),
        ),
        format,
        mode,
      ]);
      const report = JSON.parse(stdout);
      expect(report.format).toBe(format);
      expect(report.mode).toBe(mode);
      expect(report.submittedHash).toBe(report.completedHash);
      expect(report.finalEvaluationMatched).toBe(true);
      expect(report.evaluationCount).toBeGreaterThan(0);
      expect(report.providerDatumPreserved).toBe(true);
      expect(report.witnessDatumPreserved).toBe(true);
      expect(report.redeemerDataPreserved).toBe(true);
      expect(report.minimumAda).toBe(report.outputLovelace);
      expect(BigInt(report.executionSteps)).toBeGreaterThan(0n);
      expect(BigInt(report.executionMemory)).toBeGreaterThan(0n);
      expect(report.callbackCount > 0).toBe(mode === "delayed");
      expect(report.canonicalSerializersPreservedData).toBe(true);
      expect(report.canonicalSubmission).toBe(mode !== "static");
      expect(report.canonicalBodyHashPreserved).toBe(
        mode === "static" ? null : true,
      );
      expect(report.metadataPreserved).toBe(true);
      expect(report.hookRestored).toBe(true);
    },
  );
});
