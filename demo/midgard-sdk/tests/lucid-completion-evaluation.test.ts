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
  fee: string;
};
type EvaluationReport = {
  first: Completion;
  second: Completion;
  changed: Completion;
  changedCollateral: Completion;
  zeroCollateral: Completion;
  lowCollateral: Completion;
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
      // missing behavior or retain another test's evaluator hook.
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

    it("evaluates the draft and the final context once despite the delayed redeemer's repeated final pass", () => {
      // The fixed fixture evaluates the collateral-free draft once, then the
      // final context C, before replaying C: keep convergence behavior explicit
      // while asserting that neither a draft re-check nor the duplicate C runs.
      expect(report.first.requests).toHaveLength(2);
      expect(new Set(report.first.requests).size).toBe(2);
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
      expect(new Set(report.changed.requests).size).toBe(2);
      expect(
        report.changed.requests.every(
          (hash) => !report.first.requests.includes(hash),
        ),
      ).toBe(true);
      expect(report.changed.hash).not.toBe(report.first.hash);
      expect(report.first.collateral).toBe("5000000");
      expect(report.changedCollateral.collateral).toBe("8000000");
      expect(report.changedCollateral.hash).not.toBe(report.first.hash);
      expect(new Set(report.changedCollateral.requests).size).toBe(2);
      // The first balancing pass precedes collateral selection and may match A;
      // the final request must reflect the independently changed collateral.
      expect(report.changedCollateral.requests.at(-1)).not.toBe(
        report.first.requests.at(-1),
      );
    });

    it("tops collateral up to the final fee's percentage", () => {
      // 150% of the returned fee, rounded up, is the ledger's requirement.
      expect(BigInt(report.zeroCollateral.collateral)).toBe(
        (BigInt(report.zeroCollateral.fee) * 150n + 99n) / 100n,
      );
      expect(report.lowCollateral.collateral).toBe("1000000");
      expect(
        BigInt(report.lowCollateral.collateral) >=
          (BigInt(report.lowCollateral.fee) * 150n + 99n) / 100n,
      ).toBe(true);
    });

    it("preserves repeated custom evaluator calls and their deliberate failure", () => {
      // Draft once, then the final context and its repeat: a custom evaluator
      // sees the repeated request instead of a reused result.
      expect(report.customRequests).toHaveLength(3);
      expect(new Set(report.customRequests).size).toBe(2);
      expect(report.customRequests[2]).toBe(report.customRequests[1]);
    });

    it("preserves the Lucid-configured custom evaluator's repeated calls", () => {
      expect(report.configuredCustomRequests).toHaveLength(3);
      expect(new Set(report.configuredCustomRequests).size).toBe(2);
      expect(report.configuredCustomRequests[2]).toBe(
        report.configuredCustomRequests[1],
      );
    });

    it("restores the original UPLC module hook", () => {
      expect(report.hookRestored).toBe(true);
    });
  },
);
