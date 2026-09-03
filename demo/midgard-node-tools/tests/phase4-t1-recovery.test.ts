import { describe, expect, it } from "vitest";

import {
  assertPhase4T1Gate,
  assertPhase4T1NoopAdvance,
  assertPhase4T1ReplacementAttemptOrdering,
  decodePhase4T1AdvanceEvidenceV1,
  decodePhase4T1ProbeEvidenceV1,
  decodePhase4T1RecoveryAttestationV1,
  parseAndValidatePhase4T1RecoveryAttestation,
  PHASE4_T1_ACCEPTANCE_TOKEN,
  PHASE4_T1_PROBE_SCHEMA,
  PHASE4_T1_RECOVERY_SCHEMA,
  type Phase4T1CanonicalTip,
  type Phase4T1ProbeEvidence,
  requireCardanoHash,
  requireL2HeaderHash,
  requireL2TransactionId,
} from "../src/commands/phase4-t1-recovery.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const emptyRoot =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";

const baseTip = (): Phase4T1CanonicalTip => ({
  headerHash: h28("11"),
  outRef: `${h32("a1")}#0`,
  datumKind: "confirmed",
  prevHeaderHash: h28("00"),
  prevUtxosRoot: null,
  utxosRoot: h32("31"),
  transactionsRoot: null,
  depositsRoot: null,
  withdrawalsRoot: null,
  forcedTransactionsRoot: null,
  transitionTraceRoot: null,
  eventToStepRoot: null,
  withdrawalCount: null,
  forcedTransactionCount: null,
  l2TransactionCount: null,
  depositCount: null,
  totalEventCount: null,
  transitionStepCount: null,
  startTimeMs: 1_000,
  endTimeMs: 2_000,
});

const recoveredTip = (): Phase4T1CanonicalTip => ({
  headerHash: h28("22"),
  outRef: `${h32("b2")}#1`,
  datumKind: "header",
  prevHeaderHash: h28("11"),
  prevUtxosRoot: h32("31"),
  utxosRoot: h32("31"),
  transactionsRoot: emptyRoot,
  depositsRoot: emptyRoot,
  withdrawalsRoot: emptyRoot,
  forcedTransactionsRoot: emptyRoot,
  transitionTraceRoot: emptyRoot,
  eventToStepRoot: emptyRoot,
  withdrawalCount: "0",
  forcedTransactionCount: "0",
  l2TransactionCount: "0",
  depositCount: "0",
  totalEventCount: "0",
  transitionStepCount: "0",
  startTimeMs: 2_000,
  endTimeMs: 12_000,
});

const probe = (
  canonicalTip: Phase4T1CanonicalTip,
  canonicalHeaderHashes: readonly string[],
): Phase4T1ProbeEvidence => ({
  schemaVersion: PHASE4_T1_PROBE_SCHEMA,
  snapshotIdentitySha256: h32("9a"),
  attemptId: "t1-attempt-1",
  canonicalHeaderHashes,
  canonicalTip,
});

describe("Phase 4 T1 hash domains and gate", () => {
  it("keeps 28-byte L2 headers separate from 32-byte Cardano hashes", () => {
    expect(requireL2HeaderHash(h28("ab"), "L2")).toBe(h28("ab"));
    expect(requireL2TransactionId(h32("ef"), "L2 tx")).toBe(h32("ef"));
    expect(requireCardanoHash(h32("cd"), "Cardano")).toBe(h32("cd"));
    expect(() => requireL2HeaderHash(h32("ab"), "L2")).toThrow(/56/u);
    expect(() => requireCardanoHash(h28("cd"), "Cardano")).toThrow(/64/u);
    expect(() => requireL2TransactionId(h28("ef"), "L2 tx")).toThrow(
      /L2 transaction id/u,
    );
  });

  it("requires both local-devnet authorization tokens and exact snapshot binding", () => {
    const expected = {
      snapshotIdentitySha256: h32("9a"),
      attemptId: "t1-attempt-1",
    };
    const env = {
      MIDGARD_PHASE4_PROCESS_ACCEPTANCE: "pipelined-commit-live-v1",
      MIDGARD_PHASE4_PROCESS_TARGET: "local-devnet",
      MIDGARD_PHASE4_T1_ACCEPTANCE_TOKEN: PHASE4_T1_ACCEPTANCE_TOKEN,
      MIDGARD_PHASE4_T1_SNAPSHOT_IDENTITY_SHA256: h32("9a"),
      MIDGARD_PHASE4_T1_ATTEMPT_ID: "t1-attempt-1",
    };
    expect(assertPhase4T1Gate({ env, ...expected })).toEqual(expected);
    expect(() =>
      assertPhase4T1Gate({
        env: { ...env, MIDGARD_PHASE4_PROCESS_TARGET: "Preprod" },
        ...expected,
      }),
    ).toThrow(/local-devnet/u);
    expect(() =>
      assertPhase4T1Gate({
        env: {
          ...env,
          MIDGARD_PHASE4_T1_SNAPSHOT_IDENTITY_SHA256: h32("8b"),
        },
        ...expected,
      }),
    ).toThrow(/snapshot identity/u);
  });
});

describe("Phase 4 T1 canonical no-op advance", () => {
  it("proves F links to B, advances beyond N, preserves the UTxO root, and has an empty transition", () => {
    const abandoned = h28("44");
    expect(
      assertPhase4T1NoopAdvance({
        before: probe(baseTip(), [h28("11")]),
        after: probe(recoveredTip(), [h28("11"), h28("22")]),
        expectedBaseHeaderHash: h28("11"),
        abandonedHeaderHash: abandoned,
        minimumEndTimeMs: 10_000,
      }),
    ).toMatchObject({
      baseHeaderHash: h28("11"),
      recoveredTipHeaderHash: h28("22"),
      abandonedHeaderHash: abandoned,
      rootsPreserved: true,
      transitionIsEmpty: true,
    });
  });

  it("fails closed on a reintroduced N, changed root, wrong predecessor, or insufficient end time", () => {
    const validBefore = probe(baseTip(), [h28("11")]);
    const validAfter = probe(recoveredTip(), [h28("11"), h28("22")]);
    const input = {
      before: validBefore,
      after: validAfter,
      expectedBaseHeaderHash: h28("11"),
      abandonedHeaderHash: h28("44"),
      minimumEndTimeMs: 10_000,
    };
    expect(() =>
      assertPhase4T1NoopAdvance({
        ...input,
        after: probe(recoveredTip(), [h28("11"), h28("22"), h28("44")]),
      }),
    ).toThrow(/reappeared/u);
    expect(() =>
      assertPhase4T1NoopAdvance({
        ...input,
        after: probe({ ...recoveredTip(), utxosRoot: h32("99") }, [
          h28("11"),
          h28("22"),
        ]),
      }),
    ).toThrow(/UTxO root/u);
    expect(() =>
      assertPhase4T1NoopAdvance({
        ...input,
        after: probe({ ...recoveredTip(), prevHeaderHash: h28("33") }, [
          h28("11"),
          h28("22"),
        ]),
      }),
    ).toThrow(/link/u);
    expect(() =>
      assertPhase4T1NoopAdvance({ ...input, minimumEndTimeMs: 20_000 }),
    ).toThrow(/end-time/u);
  });

  it("decodes exact probe and canonical-advance V1 artifacts only", () => {
    const before = probe(baseTip(), [h28("11")]);
    const after = probe(recoveredTip(), [h28("11"), h28("22")]);
    const invariants = assertPhase4T1NoopAdvance({
      before,
      after,
      expectedBaseHeaderHash: h28("11"),
      abandonedHeaderHash: h28("44"),
      minimumEndTimeMs: 10_000,
    });
    const advance = {
      schemaVersion: "midgard-phase4-t1-canonical-advance-v1",
      snapshotIdentitySha256: before.snapshotIdentitySha256,
      attemptId: before.attemptId,
      abandonedHeaderHash: h28("44"),
      before,
      submittedTxHash: h32("66"),
      recoveredTipHeaderHash: h28("22"),
      blockOutRef: `${h32("77")}#0`,
      txSize: 123,
      blockEndTimeMs: 12_000,
      after,
      invariants,
    } as const;
    expect(decodePhase4T1ProbeEvidenceV1(before)).toEqual(before);
    expect(decodePhase4T1AdvanceEvidenceV1(advance)).toEqual(advance);

    for (const mutation of [
      { ...before, schemaVersion: "midgard-phase4-t1-probe-v2" },
      { ...before, unknown: true },
      {
        ...before,
        canonicalTip: { ...before.canonicalTip, unknown: true },
      },
      {
        ...before,
        canonicalTip: {
          ...before.canonicalTip,
          outRef: `${h32("a1")}#00`,
        },
      },
      {
        ...before,
        canonicalHeaderHashes: [h28("11"), h28("11")],
      },
      {
        ...before,
        schemaVersion: "midgard-phase4-t1-canonical-advance-v1",
      },
    ]) {
      expect(() => decodePhase4T1ProbeEvidenceV1(mutation)).toThrow();
    }
    const { attemptId: _attemptId, ...missingProbeKey } = before;
    expect(() => decodePhase4T1ProbeEvidenceV1(missingProbeKey)).toThrow(
      "fields",
    );
    expect(() =>
      decodePhase4T1AdvanceEvidenceV1({
        ...advance,
        after: { ...advance.after, unknown: true },
      }),
    ).toThrow();
    expect(() =>
      decodePhase4T1AdvanceEvidenceV1({
        ...advance,
        recoveredTipHeaderHash: h28("33"),
      }),
    ).toThrow("not bound");
  });
});

describe("Phase 4 T1 per-attempt process evidence", () => {
  it("does not accept a matching candidate from historical append-log bytes", () => {
    const recovered = h28("22");
    const historical = `pipeline_trace phase=candidate_ready base_header_hash=${recovered}\npipeline_trace phase=candidate_submitted\n`;
    const currentAttempt =
      "recovered canonical chain tip\n" +
      `pipeline_trace phase=candidate_ready base_header_hash=${h28("33")}\n` +
      "pipeline_trace phase=candidate_submitted\n";
    expect(() =>
      assertPhase4T1ReplacementAttemptOrdering({
        attemptLog: historical + currentAttempt,
        recoveredTipHeaderHash: recovered,
      }),
    ).toThrow(/no replacement candidate/u);
  });

  it("requires stale recovery, then an F-based candidate, then submission in the same attempt", () => {
    const recovered = h28("22");
    const replacementLine = `pipeline_trace phase=candidate_ready candidate_id=c1 base_header_hash=${recovered}`;
    expect(
      assertPhase4T1ReplacementAttemptOrdering({
        attemptLog: [
          "recovered canonical chain tip",
          replacementLine,
          "pipeline_trace phase=candidate_submitted candidate_id=c1",
        ].join("\n"),
        recoveredTipHeaderHash: recovered,
      }),
    ).toEqual({ replacementCandidateLine: replacementLine });
    expect(() =>
      assertPhase4T1ReplacementAttemptOrdering({
        attemptLog: `${replacementLine}\npipeline_trace phase=candidate_submitted`,
        recoveredTipHeaderHash: recovered,
      }),
    ).toThrow(/stale-pending/u);
  });
});

describe("Phase 4 T1 recovery attestation", () => {
  const expected = {
    scenarioLabel: "t1-recovered-tip",
    attemptId: "t1-attempt-1",
    composeProject: "midgard_phase4_process_gate",
    networkMagic: 42,
    snapshotIdentitySha256: h32("9a"),
    abandonedHeaderHash: h28("44"),
    abandonedSubmittedTxHash: h32("55"),
    baseHeaderHash: h28("11"),
  } as const;
  const valid = {
    schemaVersion: PHASE4_T1_RECOVERY_SCHEMA,
    ...expected,
    snapshotSetSha256: h32("8b"),
    recoveredTipHeaderHash: h28("22"),
    canonicalAdvanceTxHash: h32("66"),
    journalSha256Before: h32("77"),
    journalSha256After: h32("77"),
    cardanoTip: { slot: 120, hash: h32("88") },
    kupoCheckpoint: 120,
  };

  it("accepts exact snapshot-bound, journal-identical, provider-synchronized evidence", () => {
    expect(
      parseAndValidatePhase4T1RecoveryAttestation({
        output: JSON.stringify(valid),
        expected,
      }),
    ).toEqual(valid);
  });

  it("rejects stale identity, mismatched journal bytes, provider lag, and a 64-hex recovered L2 tip", () => {
    for (const changed of [
      { ...valid, snapshotIdentitySha256: h32("aa") },
      { ...valid, journalSha256After: h32("aa") },
      { ...valid, kupoCheckpoint: 119 },
      { ...valid, recoveredTipHeaderHash: h32("22") },
      { ...valid, fabricatedEvidence: true },
    ]) {
      expect(() =>
        parseAndValidatePhase4T1RecoveryAttestation({
          output: JSON.stringify(changed),
          expected,
        }),
      ).toThrow();
    }
  });

  it("rejects wrong-family, missing, and unknown nested recovery shapes", () => {
    expect(decodePhase4T1RecoveryAttestationV1(valid)).toEqual(valid);
    const { snapshotSetSha256: _snapshotSetSha256, ...missing } = valid;
    for (const changed of [
      missing,
      { ...valid, schemaVersion: PHASE4_T1_PROBE_SCHEMA },
      { ...valid, unexpected: true },
      { ...valid, cardanoTip: { ...valid.cardanoTip, unexpected: true } },
      { ...valid, canonicalAdvanceTxHash: h32("AA") },
    ]) {
      expect(() => decodePhase4T1RecoveryAttestationV1(changed)).toThrow();
    }
  });
});
