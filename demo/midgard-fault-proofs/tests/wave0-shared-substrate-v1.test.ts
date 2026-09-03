import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  assertNoPositiveFaultProofLimitEscapes,
  scanFaultProofLimitEscapes,
} from "../src/proof-fit/limit-escape-scan-v1.js";
import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger-v1.js";
import {
  assertCompleteLifecycleCoverage,
  COMPLETE_LIFECYCLE_BASE_SCENARIOS,
  type CompleteLifecycleCoverage,
} from "../src/testing/complete-lifecycle-v1.js";

const completeCoverage = (): CompleteLifecycleCoverage => ({
  reasonArms: ["InputNotFound"],
  successfulDirectionByReason: {
    InputNotFound: ["accepted_invalid", "forced_rejection_wrong"],
  },
  scenarios: COMPLETE_LIFECYCLE_BASE_SCENARIOS,
  authenticatedSeamsMutated: ["forced_leaf", "descriptor"],
  cancelledPhysicalSteps: ["bind", "scan"],
  resumedAfterCheckpoint: true,
  adjacentOverBoundRefused: true,
});

const typescriptFilesBelow = async (directory: string): Promise<string[]> => {
  const entries = await readdir(directory, { withFileTypes: true });
  const files = await Promise.all(
    entries.map(async (entry): Promise<string[]> => {
      const path = join(directory, entry.name);
      return entry.isDirectory()
        ? await typescriptFilesBelow(path)
        : entry.isFile() && entry.name.endsWith(".ts")
          ? [path]
          : [];
    }),
  );
  return files.flat();
};

describe("Wave 0 shared off-chain substrate", () => {
  it("fails closed on positive limit escapes and permits only marked negative diagnostics", () => {
    const positive = scanFaultProofLimitEscapes({
      path: "positive.test.ts",
      source: `const parameters = { ${"maxTx" + "Size"}: 262_144 };\npublish({ ${"over" + "sized"}: true });`,
    });
    expect(() => assertNoPositiveFaultProofLimitEscapes(positive)).toThrow(
      /positive\.test\.ts:1 raised_tx_bytes.*positive\.test\.ts:2 oversized_publication/su,
    );
    const diagnostic = scanFaultProofLimitEscapes({
      path: "unpublishable.test.ts",
      source: [
        "// MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN",
        `publish({ ${"over" + "sized"}: true });`,
        "// MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_END",
      ].join("\n"),
    });
    expect(() =>
      assertNoPositiveFaultProofLimitEscapes(diagnostic),
    ).not.toThrow();
    expect(() =>
      assertNoPositiveFaultProofLimitEscapes(
        scanFaultProofLimitEscapes({
          path: "broken.test.ts",
          source: `// ${"MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_" + "END"}`,
        }),
      ),
    ).toThrow(/malformed_diagnostic_marker/);
  });

  it("finds no positive limit escape across the fault-proof TypeScript surface", async () => {
    const roots = [join(process.cwd(), "src"), join(process.cwd(), "tests")];
    const files = (await Promise.all(roots.map(typescriptFilesBelow))).flat();
    const findings = (
      await Promise.all(
        files.map(async (path) =>
          scanFaultProofLimitEscapes({
            path,
            source: await readFile(path, "utf8"),
          }),
        ),
      )
    ).flat();
    expect(() =>
      assertNoPositiveFaultProofLimitEscapes(findings),
    ).not.toThrow();
  });

  it("fails lifecycle coverage with one actionable list of every omission", () => {
    expect(() =>
      assertCompleteLifecycleCoverage({
        coverage: { ...completeCoverage(), scenarios: [] },
        expectedReasonArms: ["InputNotFound"],
        authenticationSeams: ["forced_leaf", "descriptor", "item"],
        cancellablePhysicalSteps: ["bind", "scan", "finalize"],
        resumable: true,
        hasAdjacentConsensusBound: true,
      }),
    ).toThrow(
      /scenarios:.*authentication seams: item.*cancel steps: finalize/u,
    );
    expect(() =>
      assertCompleteLifecycleCoverage({
        coverage: completeCoverage(),
        expectedReasonArms: ["InputNotFound"],
        authenticationSeams: ["forced_leaf", "descriptor"],
        cancellablePhysicalSteps: ["bind", "scan"],
        resumable: true,
        hasAdjacentConsensusBound: true,
      }),
    ).not.toThrow();
  });

  it("builds deterministic positive-margin ledgers and writes their digest", async () => {
    const input = {
      category: "exampleFamily",
      blueprintSha256: "ab".repeat(32),
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: [
        {
          name: "step-01",
          kind: "lifecycle" as const,
          maximumShape: "10,000 committed items",
          signedBytes: 16_000,
          memoryUnits: 16_000_000n,
          cpuUnits: 9_000_000_000n,
        },
        {
          name: "publish-bind",
          kind: "publication" as const,
          maximumShape: "applied testnet script",
          signedBytes: 15_800,
          memoryUnits: 0n,
          cpuUnits: 0n,
        },
      ],
    };
    const ledger = buildVanRossemFitLedger(input);
    expect(buildVanRossemFitLedger(input)).toStrictEqual(ledger);
    expect(ledger.entries.map((entry) => entry.name)).toStrictEqual([
      "step-01",
      "publish-bind",
    ]);
    const path = join(
      process.env.TMPDIR ?? "/tmp",
      `midgard-fit-${ledger.ledgerSha256}.json`,
    );
    await writeVanRossemFitLedger(path, ledger);
    expect(JSON.parse(await readFile(path, "utf8"))).toStrictEqual(ledger);
  });

  it("refuses hard-boundary and publication-reserve failures", () => {
    const base = {
      category: "exampleFamily",
      blueprintSha256: "ab".repeat(32),
      compilerVersion: "aiken v1.1.23+5adf783",
    };
    expect(() =>
      buildVanRossemFitLedger({
        ...base,
        measurements: [
          {
            name: "at-hard-boundary",
            kind: "lifecycle",
            maximumShape: "maximum",
            signedBytes: 16_384,
            memoryUnits: 1n,
            cpuUnits: 1n,
          },
        ],
      }),
    ).toThrow(/no positive Van Rossem margin/);
    expect(() =>
      buildVanRossemFitLedger({
        ...base,
        measurements: [
          {
            name: "publication-without-reserve",
            kind: "publication",
            maximumShape: "applied script",
            signedBytes: 15_873,
            memoryUnits: 1n,
            cpuUnits: 1n,
          },
        ],
      }),
    ).toThrow(/reliable target/);
  });
});
