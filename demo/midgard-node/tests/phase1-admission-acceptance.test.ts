import { describe, expect, it } from "vitest";

import {
  evaluateStageAAdmissionGate,
  parseLinuxCpuTopology,
  requirePhase1AdmissionIsolation,
  type StageAAdmissionReport,
} from "@/e2e/phase1-admission-acceptance.js";

const safeEnvironment = (): NodeJS.ProcessEnv => ({
  PHASE1_ADMISSION_OPERATOR: "1",
  PHASE1_ADMISSION_RUN_TOKEN: "short_20260710",
  PHASE1_ADMISSION_HTTP_PORT: "19081",
  PHASE1_ADMISSION_CORPUS_PATH: "logs/corpus.ndjson",
  POSTGRES_DB: "midgard_phase1_acceptance_short_20260710",
  POSTGRES_HOST: "127.0.0.1",
  POSTGRES_PORT: "55431",
});

const passingReport = (): StageAAdmissionReport => ({
  measuredDurationMs: 300_000,
  offered: 1_515_000,
  accepted202: 1_515_000,
  duplicate200: 0,
  rejectedOrFailed: 0,
  latencyMs: { p99: 250 },
  corpus: { sha256: "abc", expectedSha256: "abc" },
  topology: {
    logicalCpuIds: [0, 2, 4, 6, 8, 10, 12, 14],
    physicalCoreIds: ["0:0", "0:1", "0:2", "0:3", "0:4", "0:5", "0:6", "0:7"],
  },
});

describe("Phase 1 admission acceptance harness", () => {
  it("requires explicit isolated database and socket boundaries", () => {
    expect(requirePhase1AdmissionIsolation(safeEnvironment())).toMatchObject({
      database: "midgard_phase1_acceptance_short_20260710",
      postgresPort: 55431,
      httpPort: 19081,
    });
    expect(() =>
      requirePhase1AdmissionIsolation({
        ...safeEnvironment(),
        POSTGRES_DB: "midgard",
      }),
    ).toThrow(/POSTGRES_DB must match/u);
    expect(() =>
      requirePhase1AdmissionIsolation({
        ...safeEnvironment(),
        POSTGRES_PORT: "5433",
      }),
    ).toThrow(/reserved for the live demo/u);
    expect(() =>
      requirePhase1AdmissionIsolation({
        ...safeEnvironment(),
        PHASE1_ADMISSION_HTTP_PORT: "3000",
      }),
    ).toThrow(/must differ from the live HTTP/u);
    expect(() =>
      requirePhase1AdmissionIsolation({
        ...safeEnvironment(),
        POSTGRES_HOST: "postgres.internal",
      }),
    ).toThrow(/must be loopback/u);
  });

  it("proves eight affinity CPUs map to eight physical cores", () => {
    const topology = parseLinuxCpuTopology(
      [
        "# CPU,CORE,SOCKET",
        "0,0,0",
        "1,0,0",
        "2,1,0",
        "4,2,0",
        "6,3,0",
        "8,4,0",
        "10,5,0",
        "12,6,0",
        "14,7,0",
      ].join("\n"),
      [0, 2, 4, 6, 8, 10, 12, 14],
    );
    expect(topology.physicalCoreIds).toHaveLength(8);
    expect(() => parseLinuxCpuTopology("0,0,0\n1,0,0\n", [0, 1])).toThrow(
      /SMT siblings/u,
    );
  });

  it("fails closed on every missing numeric gate", () => {
    expect(evaluateStageAAdmissionGate(passingReport())).toEqual({
      passed: true,
      acceptedTps: 5_050,
      reasons: [],
    });
    const failed = evaluateStageAAdmissionGate({
      ...passingReport(),
      measuredDurationMs: 299_999,
      offered: 1_500_000,
      accepted202: 1_499_999,
      rejectedOrFailed: 1,
      latencyMs: { p99: null },
      corpus: { sha256: "wrong", expectedSha256: "pinned" },
      topology: {
        logicalCpuIds: [0, 2, 4, 6, 8, 10, 12],
        physicalCoreIds: ["0:0", "0:1", "0:2", "0:3", "0:4", "0:5", "0:6"],
      },
    });
    expect(failed.passed).toBe(false);
    expect(failed.reasons).toEqual(
      expect.arrayContaining([
        expect.stringContaining("below five minutes"),
        expect.stringContaining("p99 is missing"),
        expect.stringContaining("SHA-256"),
        expect.stringContaining("eight distinct physical cores"),
      ]),
    );
  });
});
