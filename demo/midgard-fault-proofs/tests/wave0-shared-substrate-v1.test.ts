import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  assertNoPositiveFaultProofLimitEscapesV1,
  scanFaultProofLimitEscapesV1,
} from "../src/proof-fit/limit-escape-scan-v1.js";
import {
  buildVanRossemFitLedgerV1,
  writeVanRossemFitLedgerV1,
} from "../src/proof-fit/van-rossem-fit-ledger-v1.js";
import {
  assertCompleteLifecycleCoverageV1,
  COMPLETE_LIFECYCLE_BASE_SCENARIOS_V1,
  type CompleteLifecycleCoverageV1,
} from "../src/testing/complete-lifecycle-v1.js";

const completeCoverage = (): CompleteLifecycleCoverageV1 => ({
  reasonArms: ["InputNotFound"],
  successfulDirectionByReason: {
    InputNotFound: ["accepted_invalid", "forced_rejection_wrong"],
  },
  scenarios: COMPLETE_LIFECYCLE_BASE_SCENARIOS_V1,
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
    const positive = scanFaultProofLimitEscapesV1({
      path: "positive.test.ts",
      source: `const parameters = { ${"maxTx" + "Size"}: 262_144 };\npublish({ ${"over" + "sized"}: true });`,
    });
    expect(() => assertNoPositiveFaultProofLimitEscapesV1(positive)).toThrow(
      /positive\.test\.ts:1 raised_tx_bytes.*positive\.test\.ts:2 oversized_publication/su,
    );
    const diagnostic = scanFaultProofLimitEscapesV1({
      path: "unpublishable.test.ts",
      source: [
        "// MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN",
        `publish({ ${"over" + "sized"}: true });`,
        "// MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_END",
      ].join("\n"),
    });
    expect(() =>
      assertNoPositiveFaultProofLimitEscapesV1(diagnostic),
    ).not.toThrow();
    expect(() =>
      assertNoPositiveFaultProofLimitEscapesV1(
        scanFaultProofLimitEscapesV1({
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
          scanFaultProofLimitEscapesV1({
            path,
            source: await readFile(path, "utf8"),
          }),
        ),
      )
    ).flat();
    expect(() =>
      assertNoPositiveFaultProofLimitEscapesV1(findings),
    ).not.toThrow();
  });

  it("fails lifecycle coverage with one actionable list of every omission", () => {
    expect(() =>
      assertCompleteLifecycleCoverageV1({
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
      assertCompleteLifecycleCoverageV1({
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
    const ledger = buildVanRossemFitLedgerV1(input);
    expect(buildVanRossemFitLedgerV1(input)).toStrictEqual(ledger);
    expect(ledger.entries.map((entry) => entry.name)).toStrictEqual([
      "step-01",
      "publish-bind",
    ]);
    const path = join(
      process.env.TMPDIR ?? "/tmp",
      `midgard-fit-${ledger.ledgerSha256}.json`,
    );
    await writeVanRossemFitLedgerV1(path, ledger);
    expect(JSON.parse(await readFile(path, "utf8"))).toStrictEqual(ledger);
  });

  it("refuses hard-boundary and publication-reserve failures", () => {
    const base = {
      category: "exampleFamily",
      blueprintSha256: "ab".repeat(32),
      compilerVersion: "aiken v1.1.23+5adf783",
    };
    expect(() =>
      buildVanRossemFitLedgerV1({
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
      buildVanRossemFitLedgerV1({
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
