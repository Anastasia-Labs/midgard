import { readFile } from "node:fs/promises";

import { verifyPhase2BenchmarkReports } from "./verify-phase2-benchmark-report.verify-phase2-benchmark-reports.mjs";
import { fail } from "./verify-phase2-benchmark-report.verify-stage-breport.mjs";

export const main = async () => {
  const [mode, ...paths] = process.argv.slice(2);
  if (mode === undefined) {
    fail(
      "usage: verify-phase2-benchmark-report.mjs <mode> <report.json> [...]",
    );
  }
  const reports = await Promise.all(
    paths.map(async (path) => JSON.parse(await readFile(path, "utf8"))),
  );
  const expectedFullCorpus =
    mode !== "full"
      ? undefined
      : {
          sha256: process.env.PHASE2_EXPECTED_FULL_CORPUS_SHA256 ?? "",
          rowCount: Number(
            process.env.PHASE2_EXPECTED_FULL_CORPUS_ROWS ?? Number.NaN,
          ),
        };
  const result = verifyPhase2BenchmarkReports(mode, reports, {
    expectedFullCorpus,
  });
  process.stdout.write(
    `${JSON.stringify({ mode, passed: true, result }, null, 2)}\n`,
  );
};
