import { execFile } from "node:child_process";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { describe, expect, it } from "vitest";

describe.each(["esm", "cjs"])(
  "static explicitly funded Lucid completion (%s)",
  (format) => {
    it("preserves final evaluation, collateral safety, and excluded paths", async () => {
      const { stdout } = await promisify(execFile)(process.execPath, [
        fileURLToPath(
          new URL("./support/lucid-static-completion.mjs", import.meta.url),
        ),
        format,
      ]);
      const report = JSON.parse(stdout);
      expect(report.format).toBe(format);
      expect(report.static.requestCount).toBe(2);
      expect(report.staticSubmitted).toBe(true);
      expect(report.insufficientFundsRefused).toBe(true);
      expect(report.finalFeeCollateralRefused).toBe(true);
      expect(report.feeSensitive.requestCount).toBeGreaterThanOrEqual(3);
      expect(report.feeSensitiveSubmitted).toBe(true);
      expect(report.feeSensitiveCollateralError).toContain(
        "Final transaction requires",
      );
      expect(report.feeSensitiveCollateralRetry.collateral).toBe(1_000_000);
      expect(report.customRequests).toBe(3);
      expect(report.configuredRequests).toBe(3);
      expect(report.coinSelection.requestCount).toBe(3);
    });
  },
);
