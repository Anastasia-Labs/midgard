import { describe, expect, it } from "vitest";

import {
  decodeArchitectureGCommitCandidateSeedInputV1,
  decodeArchitectureGCorpusFundingV1,
  validateArchitectureGCommitCandidateSeedResultV1,
} from "@/workers/utils/mpf-commit-candidate-seed-artifacts.js";

const hash = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);

const phase1FormalBindingIdentity = {
  schemaVersion:
    "midgard-architecture-g-phase1-formal-binding-identity-v1" as const,
  path: "/evidence/phase1-formal-binding.json",
  sha256: hash(1),
  deploymentManifestId: "deployment-manifest-id",
  nodeImageId: "sha256:node-image",
  nodeContainerId: "node-container-id",
  walletSetSha256: hash(2),
  fundingSetSha256: hash(3),
  corpus: {
    path: "/evidence/corpus.ndjson",
    indexPath: "/evidence/corpus.ndjson.index.ndjson",
    manifestPath: "/evidence/corpus.ndjson.manifest.json",
    sliceId: "phase1-live",
    corpusSha256: hash(4),
    indexSha256: hash(5),
    manifestSha256: hash(6),
  },
  generationResult: {
    path: "/evidence/generation-result.json",
    sha256: hash(7),
    schemaVersion: "midgard-stress-corpus-generation-v1" as const,
  },
  harness: { scenarioId: hash(8), engineId: hash(9) },
};

const runtimeIdentity = {
  schemaVersion: "midgard-architecture-g-runtime-identity-v1" as const,
  version: "v22.22.2",
  execPath: "/opt/node-v22.22.2/bin/node",
  executableSha256: hash(10),
};

const seedInput = () => ({
  schemaVersion: "midgard-architecture-g-commit-candidate-seed-v1",
  phase1FormalBinding: structuredClone(phase1FormalBindingIdentity),
  runtimeIdentity: structuredClone(runtimeIdentity),
  corpusSlicePath: "/evidence/canonical-corpus-slice.ndjson",
  corpusSliceSha256: hash(11),
  fundingMapPath: "/evidence/canonical-corpus-funding.json",
  fundingMapSha256: hash(12),
  expectedTransactionCount: 2,
  firstTimestampIso: "2026-07-27T00:00:00.000Z",
});

const funding = () => ({
  schemaVersion: "midgard-architecture-g-corpus-funding-v1",
  corpusSha256: phase1FormalBindingIdentity.corpus.corpusSha256,
  sliceSha256: hash(11),
  entries: [
    {
      walletId: "wallet-0",
      outref: `${hash(20)}#0`,
      outputCbor: "00",
    },
    {
      walletId: "wallet-1",
      outref: `${hash(21)}#1`,
      outputCbor: "01",
    },
  ],
});

describe("Architecture G commit-candidate seed V1 artifacts", () => {
  it("accepts the complete canonical seed input", () => {
    const value = seedInput();
    expect(decodeArchitectureGCommitCandidateSeedInputV1(value)).toBe(value);
  });

  it.each([
    (value: ReturnType<typeof seedInput>) =>
      Object.assign(value, { unknown: true }),
    (value: ReturnType<typeof seedInput>) =>
      Object.assign(value.phase1FormalBinding, { unknown: true }),
    (value: ReturnType<typeof seedInput>) =>
      Object.assign(value.runtimeIdentity, { unknown: true }),
    (value: ReturnType<typeof seedInput>) =>
      void (value.corpusSlicePath = "relative/slice.ndjson"),
    (value: ReturnType<typeof seedInput>) =>
      void (value.corpusSliceSha256 = "bad"),
    (value: ReturnType<typeof seedInput>) =>
      void (value.expectedTransactionCount = 0),
    (value: ReturnType<typeof seedInput>) =>
      void (value.firstTimestampIso = "2026-07-27T00:00:00Z"),
    (value: ReturnType<typeof seedInput>) =>
      void (value.firstTimestampIso = "2026-99-99T00:00:00.000Z"),
  ])("rejects an incomplete or noncanonical seed input %#", (mutate) => {
    const value = seedInput();
    mutate(value);
    expect(() =>
      decodeArchitectureGCommitCandidateSeedInputV1(value),
    ).toThrow();
  });

  it("accepts funding only when top-level and entry identities are exact", () => {
    const value = funding();
    expect(
      decodeArchitectureGCorpusFundingV1({
        value,
        expectedCorpusSha256: phase1FormalBindingIdentity.corpus.corpusSha256,
        expectedSliceSha256: hash(11),
      }),
    ).toBe(value);
  });

  it.each([
    (value: ReturnType<typeof funding>) =>
      Object.assign(value, { unknown: true }),
    (value: ReturnType<typeof funding>) =>
      Object.assign(value.entries[0]!, { unknown: true }),
    (value: ReturnType<typeof funding>) => void (value.corpusSha256 = hash(30)),
    (value: ReturnType<typeof funding>) => void (value.sliceSha256 = hash(31)),
    (value: ReturnType<typeof funding>) =>
      void (value.entries[1]!.walletId = value.entries[0]!.walletId),
    (value: ReturnType<typeof funding>) =>
      void (value.entries[1]!.outref = value.entries[0]!.outref),
    (value: ReturnType<typeof funding>) =>
      void (value.entries[0]!.outref = `${"AB".repeat(32)}#0`),
    (value: ReturnType<typeof funding>) =>
      void (value.entries[0]!.outputCbor = "0"),
    (value: ReturnType<typeof funding>) =>
      void (value.entries[0]!.outputCbor = "AA"),
    (value: ReturnType<typeof funding>) => void value.entries.splice(0),
  ])("rejects extended, mismatched, or malformed funding %#", (mutate) => {
    const value = funding();
    mutate(value);
    expect(() =>
      decodeArchitectureGCorpusFundingV1({
        value,
        expectedCorpusSha256: phase1FormalBindingIdentity.corpus.corpusSha256,
        expectedSliceSha256: hash(11),
      }),
    ).toThrow();
  });

  it("validates the complete seed result before emission", () => {
    const value = {
      schemaVersion: "midgard-architecture-g-commit-candidate-seed-result-v1",
      databaseName: "midgard_phase3_arch_g_50k_20260727",
      corpusSliceSha256: hash(11),
      mempoolTxCount: 2,
      fundingCount: 2,
      terminalLedgerCount: 2,
      deltaCount: 2,
    };
    const validate = (candidate: unknown) =>
      validateArchitectureGCommitCandidateSeedResultV1({
        value: candidate,
        expectedDatabaseName: value.databaseName,
        expectedCorpusSliceSha256: hash(11),
        expectedTransactionCount: 2,
      });
    expect(validate(value)).toBe(value);
    expect(() => validate({ ...value, unknown: true })).toThrow();
    expect(() => validate({ ...value, mempoolTxCount: 1 })).toThrow();
    expect(() => validate({ ...value, fundingCount: 0 })).toThrow();
    expect(() => validate({ ...value, terminalLedgerCount: 0 })).toThrow();
    expect(() => validate({ ...value, deltaCount: 1 })).toThrow();
    expect(() => validate({ ...value, databaseName: "midgard" })).toThrow();
    expect(() => validate({ ...value, corpusSliceSha256: hash(40) })).toThrow();
  });
});
