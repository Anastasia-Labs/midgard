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
      expect(BigInt(report.finalFeeCollateralTopUp.collateral)).toBe(
        (BigInt(report.finalFeeCollateralTopUp.fee) * 150n + 99n) / 100n,
      );
      expect(report.feeSensitive.requestCount).toBeGreaterThanOrEqual(3);
      expect(report.feeSensitiveSubmitted).toBe(true);
      expect(BigInt(report.feeSensitiveCollateralTopUp.collateral)).toBe(
        (BigInt(report.feeSensitiveCollateralTopUp.fee) * 150n + 99n) / 100n,
      );
      expect(report.feeSensitiveCollateralRetry.collateral).toBe(1_000_000);
      expect(report.customRequests).toBe(2);
      expect(report.configuredRequests).toBe(2);
      expect(report.coinSelection.requestCount).toBe(2);
    });
  },
);
