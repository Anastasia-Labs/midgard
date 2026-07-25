import { describe, expect, it } from "vitest";

import {
  assertMidgardCapabilityParityReportV1Complete,
  buildMidgardCapabilityParityReportV1,
  type CardanoCapabilitySnapshotV1,
  MIDGARD_CAPABILITY_DIMENSIONS_V1,
  type MidgardCapabilityBoundaryEvidenceSetV1,
} from "../src/index.js";

const digest = (fill: string): string => fill.repeat(64);

const completeBoundaryEvidence = (): MidgardCapabilityBoundaryEvidenceSetV1 =>
  Object.fromEntries(
    MIDGARD_CAPABILITY_DIMENSIONS_V1.map((dimension) => [
      dimension,
      {
        cardanoBoundaryFixtureDigest: digest("1"),
        midgardAdjacentBoundaryDigest: digest("2"),
        normalProofPathDigest: digest("3"),
        forcedProofPathDigest: digest("4"),
        appliedValidatorsDigest: digest("5"),
        concreteMeasurementsDigest: digest("6"),
      },
    ]),
  );

const cardanoSnapshot = (
  overrides: Partial<CardanoCapabilitySnapshotV1["parameters"]> = {},
): CardanoCapabilitySnapshotV1 => ({
  version: 1,
  network: "cardano-mainnet",
  effectiveEpoch: 640,
  observedAt: "2026-07-25T00:00:00.000Z",
  source: {
    kind: "trusted_cardano_node",
    identity: "cardano-node-mainnet-a",
    tip: "12345678.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
  },
  pendingProtocolParameterChanges: {
    queried: true,
    morePermissiveChangePending: false,
  },
  parameters: {
    protocolMajorVersion: 10,
    protocolMinorVersion: 0,
    maxTxSize: 16_384,
    maxValueSize: 5_000,
    maxTxExecutionMemoryUnits: "16500000",
    maxTxExecutionCpuUnits: "10000000000",
    maxReferenceScriptBytesPerTransaction: 204_800,
    ...overrides,
  },
});

describe("V1 Cardano capability parity evidence", () => {
  it("sets a digest only for a complete trusted passing report", () => {
    const report = buildMidgardCapabilityParityReportV1(
      cardanoSnapshot(),
      completeBoundaryEvidence(),
    );

    expect(report.blockers).toEqual([]);
    expect(report.rows).toHaveLength(MIDGARD_CAPABILITY_DIMENSIONS_V1.length);
    expect(report.rows.every((row) => row.status === "pass")).toBe(true);
    expect(report.reportDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(() =>
      assertMidgardCapabilityParityReportV1Complete(report),
    ).not.toThrow();
  });

  it("keeps the digest unset when any boundary path is missing", () => {
    const evidence = completeBoundaryEvidence();
    const report = buildMidgardCapabilityParityReportV1(cardanoSnapshot(), {
      ...evidence,
      total_transaction_bytes: undefined,
    });

    expect(report.reportDigest).toBeNull();
    expect(report.blockers).toContain(
      "total_transaction_bytes:boundary_evidence_incomplete",
    );
    expect(() => assertMidgardCapabilityParityReportV1Complete(report)).toThrow(
      /incomplete/u,
    );
  });

  it("fails closed when the reference-script network limit is absent", () => {
    const report = buildMidgardCapabilityParityReportV1(
      cardanoSnapshot({
        maxReferenceScriptBytesPerTransaction: null,
      }),
      completeBoundaryEvidence(),
    );

    expect(report.reportDigest).toBeNull();
    expect(report.blockers).toContain(
      "reference_script_material_bytes:unknown",
    );
  });

  it("reports every Midgard limit below a raised Cardano boundary", () => {
    const report = buildMidgardCapabilityParityReportV1(
      cardanoSnapshot({ maxTxSize: 32_768 }),
      completeBoundaryEvidence(),
    );

    expect(report.reportDigest).toBeNull();
    expect(
      report.rows.find((row) => row.dimension === "spend_inputs_field_bytes"),
    ).toMatchObject({
      cardanoRequired: "65536",
      midgardSupported: "32768",
      status: "fail",
    });
    expect(
      report.rows.find((row) => row.dimension === "spend_input_count"),
    ).toMatchObject({
      cardanoRequired: "32768",
      midgardSupported: "16384",
      status: "fail",
    });
  });

  it("rejects diagnostics and unknown or more-permissive pending changes", () => {
    const base = cardanoSnapshot();
    for (const snapshot of [
      {
        ...base,
        source: { ...base.source, kind: "diagnostic_fixture" as const },
      },
      {
        ...base,
        pendingProtocolParameterChanges: {
          queried: false,
          morePermissiveChangePending: null,
        },
      },
      {
        ...base,
        pendingProtocolParameterChanges: {
          queried: true,
          morePermissiveChangePending: true,
        },
      },
    ]) {
      expect(
        buildMidgardCapabilityParityReportV1(
          snapshot,
          completeBoundaryEvidence(),
        ).reportDigest,
      ).toBeNull();
    }
  });

  it("treats malformed execution-unit values as unknown", () => {
    const report = buildMidgardCapabilityParityReportV1(
      cardanoSnapshot({ maxTxExecutionCpuUnits: "10e9" }),
      completeBoundaryEvidence(),
    );

    expect(report.reportDigest).toBeNull();
    expect(report.blockers).toContain(
      "transaction_execution_cpu_units:unknown",
    );
  });
});
