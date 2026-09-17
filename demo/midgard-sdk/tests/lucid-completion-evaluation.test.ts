import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { beforeAll, describe, expect, it } from "vitest";

type Completion = {
  requests: string[];
  callbacks: string[];
  cbor: string;
  hash: string;
  collateral: string;
};
type EvaluationReport = {
  first: Completion;
  second: Completion;
  changed: Completion;
  changedCollateral: Completion;
  customRequests: string[];
  configuredCustomRequests: string[];
  submittedHash: string;
  hookRestored: boolean;
};

describe.each(["esm", "cjs"])(
  "Lucid completion-scoped real UPLC evaluation (%s)",
  (format) => {
    let report: EvaluationReport;
    beforeAll(async () => {
      // Each format gets a fresh child so external module caches cannot mask a
      // missing patch or retain another test's evaluator hook.
      const { stdout } = await promisify(execFile)(
        process.execPath,
        [
          fileURLToPath(
            new URL(
              "./support/lucid-completion-evaluation.mjs",
              import.meta.url,
            ),
          ),
          format,
        ],
        { timeout: 30_000 },
      );
      report = JSON.parse(stdout) as EvaluationReport;
    }, 35_000);

    it("evaluates A, B and C once despite the delayed redeemer's repeated C pass", () => {
      // The fixed fixture resolves A/B/C before replaying C: keep convergence
      // behavior explicit while asserting that only its duplicate evaluation goes.
      expect(report.first.requests).toHaveLength(3);
      expect(new Set(report.first.requests).size).toBe(3);
      expect(report.first.callbacks).toEqual(["0", "0", "0"]);
      // Fixed private key and fixture: the pre-optimization signed body remains
      // byte-identical and is accepted by the actual emulator ledger.
      expect(report.first.hash).toBe(
        "7cfea67321d5be238cd4778b996ff8812c96b3343fff0f92b10bfcf81a745e90",
      );
      expect(
        createHash("sha256")
          .update(Buffer.from(report.first.cbor, "hex"))
          .digest("hex"),
      ).toBe(
        "ee9557b2ab70bc44bc83bbcffdbab284629859880353dcf89b8dbd4072d4952c",
      );
      expect(report.submittedHash).toBe(report.first.hash);
    });

    it("performs actual evaluation again for a separate identical completion", () => {
      expect(report.second.requests.length).toBeGreaterThan(0);
      expect(report.second.requests).toEqual(report.first.requests);
      expect(report.second.cbor).toBe(report.first.cbor);
    });

    it("reevaluates changed transaction and collateral context", () => {
      expect(new Set(report.changed.requests).size).toBe(3);
      expect(
        report.changed.requests.every(
          (hash) => !report.first.requests.includes(hash),
        ),
      ).toBe(true);
      expect(report.changed.hash).not.toBe(report.first.hash);
      expect(report.first.collateral).toBe("5000000");
      expect(report.changedCollateral.collateral).toBe("8000000");
      expect(report.changedCollateral.hash).not.toBe(report.first.hash);
      expect(new Set(report.changedCollateral.requests).size).toBe(3);
      // The first balancing pass precedes collateral selection and may match A;
      // the final request must reflect the independently changed collateral.
      expect(report.changedCollateral.requests.at(-1)).not.toBe(
        report.first.requests.at(-1),
      );
    });

    it("preserves repeated custom evaluator calls and their deliberate failure", () => {
      expect(report.customRequests).toHaveLength(4);
      expect(new Set(report.customRequests).size).toBe(3);
      expect(report.customRequests[3]).toBe(report.customRequests[2]);
    });

    it("preserves the Lucid-configured custom evaluator's repeated calls", () => {
      expect(report.configuredCustomRequests).toHaveLength(4);
      expect(new Set(report.configuredCustomRequests).size).toBe(3);
      expect(report.configuredCustomRequests[3]).toBe(
        report.configuredCustomRequests[2],
      );
    });

    it("restores the original UPLC module hook", () => {
      expect(report.hookRestored).toBe(true);
    });
  },
);
