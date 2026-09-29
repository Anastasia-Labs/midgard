import { mkdtemp, readdir, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it, vi } from "vitest";

import {
  assertNoPositiveFaultProofLimitEscapes,
  scanFaultProofLimitEscapes,
} from "../src/proof-fit/limit-escape-scan.js";
import {
  buildVanRossemFitLedger,
  FitLedgerWriteRefusedError,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  assertCompleteLifecycleCoverage,
  COMPLETE_LIFECYCLE_BASE_SCENARIOS,
  type CompleteLifecycleCoverage,
} from "../src/testing/complete-lifecycle.js";

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

  it("detects computed publication switches and multiline limits", () => {
    const cases = [
      `publish({ ${"over" + "sized"}: script.length > limit });`,
      `publish({ ${"over" + "sized"} });`,
      `const { ${"over" + "sized"} = false } = options;`,
      `const p = { ${"maxTx" + "Size"}:\n262_144 };`,
      `const p = { ${"maxTx" + "Size"}: 16384, ${"maxTx" + "Size"}: 262144 };`,
      `parameters.${"maxTx" + "ExMem"} = 100_000_000n;`,
    ];
    for (const source of cases) {
      expect(
        () =>
          assertNoPositiveFaultProofLimitEscapes(
            scanFaultProofLimitEscapes({ path: "regression.ts", source }),
          ),
        source,
      ).toThrow();
    }
  });

  it("ignores safe literals, types and quoted examples, and requires actual diagnostic comments", () => {
    const examples = [
      "// publish({ oversized: true });",
      'const example = "publish({ oversized: true });";',
      "type Options = { oversized: boolean; maxTxSize: number };",
      "complete({ oversized: (false as const), maxTxSize: 16_384 });",
    ];
    for (const source of examples) {
      expect(scanFaultProofLimitEscapes({ path: "safe.ts", source })).toEqual(
        [],
      );
    }
    const source = [
      'const marker = "MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN";',
      "parameters['maxTxSize'] = 2.62144e5;",
      "parameters.maxTxExSteps = 10_000_000_001n;",
    ].join("\n");
    expect(scanFaultProofLimitEscapes({ path: "limits.ts", source })).toEqual([
      {
        path: "limits.ts",
        line: 2,
        kind: "raised_tx_bytes",
        diagnosticOnly: false,
      },
      {
        path: "limits.ts",
        line: 3,
        kind: "raised_tx_cpu",
        diagnosticOnly: false,
      },
    ]);
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
    // Parsing the entire source and test tree is a repository check; the measured
    // concurrent run exceeded the unit-test default of five seconds.
  }, 30_000);

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
    const directory = await mkdtemp(join(tmpdir(), "midgard-fit-"));
    const path = join(directory, "example-fit-ledger.json");
    try {
      vi.stubEnv("MIDGARD_WRITE_FIT_LEDGER", "1");
      await writeVanRossemFitLedger(path, ledger);
      expect(JSON.parse(await readFile(path, "utf8"))).toStrictEqual(ledger);
    } finally {
      vi.unstubAllEnvs();
      await rm(directory, { recursive: true, force: true });
    }
  });

  it.each([undefined, "", "0", "true"])(
    "refuses to write a ledger when MIDGARD_WRITE_FIT_LEDGER is %j",
    async (flag) => {
      const ledger = buildVanRossemFitLedger({
        category: "exampleFamily",
        blueprintSha256: "ab".repeat(32),
        compilerVersion: "aiken v1.1.23+5adf783",
        measurements: [
          {
            name: "step-01",
            kind: "lifecycle",
            maximumShape: "one item",
            signedBytes: 1_000,
            memoryUnits: 1_000n,
            cpuUnits: 1_000n,
          },
        ],
      });
      const directory = await mkdtemp(join(tmpdir(), "midgard-fit-"));
      const path = join(directory, "nested", "example-fit-ledger.json");
      try {
        vi.stubEnv("MIDGARD_WRITE_FIT_LEDGER", flag);
        const refusal = writeVanRossemFitLedger(path, ledger);
        await expect(refusal).rejects.toBeInstanceOf(
          FitLedgerWriteRefusedError,
        );
        await expect(refusal).rejects.toThrow(
          `refused to write fit ledger ${path}: fit ledgers are written only under MIDGARD_WRITE_FIT_LEDGER=1`,
        );
        // Nothing reached the disk: no ledger, no temporary file, no directory.
        expect(await readdir(directory)).toStrictEqual([]);
      } finally {
        vi.unstubAllEnvs();
        await rm(directory, { recursive: true, force: true });
      }
    },
  );

  it("writes a caller-named output path without the global flag", async () => {
    const ledger = buildVanRossemFitLedger({
      category: "exampleFamily",
      blueprintSha256: "ab".repeat(32),
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: [
        {
          name: "step-01",
          kind: "lifecycle",
          maximumShape: "one item",
          signedBytes: 1_000,
          memoryUnits: 1_000n,
          cpuUnits: 1_000n,
        },
      ],
    });
    const directory = await mkdtemp(join(tmpdir(), "midgard-fit-"));
    const path = join(directory, "requested-fit-ledger.json");
    try {
      vi.stubEnv("MIDGARD_WRITE_FIT_LEDGER", undefined);
      await writeVanRossemFitLedger(path, ledger, { namedByCaller: true });
      expect(JSON.parse(await readFile(path, "utf8"))).toStrictEqual(ledger);
    } finally {
      vi.unstubAllEnvs();
      await rm(directory, { recursive: true, force: true });
    }
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
