import "./utils.js";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  DepositsDB,
  ForeignTipReconciliationsDB,
} from "../src/database/index.js";
import { sha256 } from "../src/sha256.js";
import {
  pruneSettledForeignTipReconciliations,
  reconcileOverdueAwaitingEventsAgainstRetainedForeignTips,
} from "../src/workers/t2-foreign-event-reconciliation.js";
import {
  headerFor,
  IN_WINDOW,
  indexDeposit,
  INGESTED_PAST_WINDOW,
  nonEmptyWindowHeader,
  onNode,
  recordForeignTip,
  WINDOW_END_MS,
} from "./foreign-tip-gate.fixtures.js";

const EMPTY_ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;
const NONEMPTY_ROOT = "11".repeat(32);
const MARKER = makeDeploymentMarker("ab".repeat(32));
const OTHER_MARKER = makeDeploymentMarker("cd".repeat(32));
const FOREIGN_HEADER_HASH = Buffer.alloc(28, 0x31);
const REPLACED_HEADER_HASH = Buffer.alloc(28, 0x32);
const PAYLOAD_A = Buffer.from("d8799f4101ff", "hex");
const PAYLOAD_B = Buffer.from("d8799f4102ff", "hex");
const pending = (): ForeignTipReconciliationsDB.ForeignTipReconciliation => ({
  version: ForeignTipReconciliationsDB.FOREIGN_TIP_RECONCILIATION_VERSION,
  deploymentMarker: MARKER,
  consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
  foreignHeaderHash: FOREIGN_HEADER_HASH,
  replacedBaseHeaderHash: REPLACED_HEADER_HASH,
  foreignHeaderCbor: Buffer.from("d87980", "hex"),
  blockStartTime: new Date("2026-06-21T00:00:00.000Z"),
  blockEndTime: new Date("2026-06-21T00:00:10.000Z"),
  commitments: {
    depositsRoot: NONEMPTY_ROOT,
    forcedTransactionsRoot: EMPTY_ROOT,
    withdrawalsRoot: EMPTY_ROOT,
    depositCount: 1n,
    forcedTransactionCount: 0n,
    withdrawalCount: 0n,
  },
  evidence: { kind: ForeignTipReconciliationsDB.EvidenceKind.Pending },
  resolution: {
    kind: ForeignTipReconciliationsDB.Status.Awaiting,
    reason: "pending_evidence",
  },
});

const verifiedEmpty =
  (): ForeignTipReconciliationsDB.ForeignTipReconciliation => ({
    ...pending(),
    commitments: {
      depositsRoot: EMPTY_ROOT,
      forcedTransactionsRoot: EMPTY_ROOT,
      withdrawalsRoot: EMPTY_ROOT,
      depositCount: 0n,
      forcedTransactionCount: 0n,
      withdrawalCount: 0n,
    },
    evidence: {
      kind: ForeignTipReconciliationsDB.EvidenceKind.VerifiedEmpty,
    },
    resolution: { kind: ForeignTipReconciliationsDB.Status.Resolved },
  });

const verifiedDa =
  (): ForeignTipReconciliationsDB.ForeignTipReconciliation => ({
    ...pending(),
    evidence: {
      kind: ForeignTipReconciliationsDB.EvidenceKind.VerifiedDa,
      schemaVersion: 1,
      payloadCbor: PAYLOAD_A,
      payloadSha256: sha256(PAYLOAD_A),
    },
    resolution: { kind: ForeignTipReconciliationsDB.Status.Resolved },
  });

const daIdentity = (payload = PAYLOAD_A) => ({
  headerHash: FOREIGN_HEADER_HASH,
  schemaVersion: 1,
  consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
  payloadCbor: payload,
  payloadSha256: sha256(payload),
});

describe("ForeignTipReconciliationV1 exact evidence", () => {
  it("accepts the sole pending, verified-empty, and verified-DA V1 shapes", () => {
    const pendingResult =
      ForeignTipReconciliationsDB.parseForeignTipReconciliation(pending());
    const emptyResult =
      ForeignTipReconciliationsDB.parseForeignTipReconciliation(
        verifiedEmpty(),
      );
    const daResult =
      ForeignTipReconciliationsDB.parseForeignTipReconciliation(verifiedDa());

    expect(pendingResult.evidence.kind).toBe("pending_v1");
    expect(emptyResult.evidence.kind).toBe("verified_empty_v1");
    expect(daResult.evidence.kind).toBe("verified_da_v1");
    expect(daResult.deploymentMarker).toEqual(MARKER);
  });

  it("authenticates new DA against deployment, profile, header, and digest", () => {
    const authenticated =
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidence({
        reconciliation: pending(),
        deploymentMarker: MARKER,
        evidence: daIdentity(),
      });

    expect(authenticated.headerHash).toEqual(FOREIGN_HEADER_HASH);
    expect(authenticated.payloadSha256).toEqual(sha256(PAYLOAD_A));
  });

  it("accepts only an exact replay of retained verified DA evidence", () => {
    const authenticated =
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidence({
        reconciliation: verifiedDa(),
        deploymentMarker: MARKER,
        evidence: daIdentity(),
      });

    expect(authenticated.payloadCbor).toEqual(PAYLOAD_A);
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidence({
        reconciliation: verifiedDa(),
        deploymentMarker: MARKER,
        evidence: daIdentity(PAYLOAD_B),
      }),
    ).toThrow(/substitution/u);
  });

  it.each([
    ["unknown version", () => ({ ...pending(), version: 2 })],
    [
      "missing version",
      () => {
        const { version: _version, ...withoutVersion } = pending();
        return withoutVersion;
      },
    ],
    ["top-level extension", () => ({ ...pending(), extension: true })],
    [
      "deployment marker alias",
      () => ({
        ...pending(),
        deploymentMarker: {
          schema: MARKER.schemaVersion,
          manifestId: MARKER.manifestId,
        },
      }),
    ],
    [
      "implicit consensus profile default",
      () => {
        const { consensusProfileId: _profile, ...withoutProfile } = pending();
        return withoutProfile;
      },
    ],
    [
      "profile substitution",
      () => ({
        ...pending(),
        consensusProfileId: "midgard-consensus-v0",
      }),
    ],
    [
      "commitment extension",
      () => ({
        ...pending(),
        commitments: { ...pending().commitments, transactionsRoot: EMPTY_ROOT },
      }),
    ],
    [
      "legacy evidence discriminator",
      () => ({
        ...pending(),
        evidence: { kind: "pending" },
      }),
    ],
    [
      "unknown evidence discriminator",
      () => ({
        ...pending(),
        evidence: { kind: "verified_da_v2" },
      }),
    ],
    [
      "verified DA digest mismatch",
      () => ({
        ...verifiedDa(),
        evidence: {
          kind: ForeignTipReconciliationsDB.EvidenceKind.VerifiedDa,
          schemaVersion: 1,
          payloadCbor: PAYLOAD_A,
          payloadSha256: Buffer.alloc(32, 0xff),
        },
      }),
    ],
    [
      "pending evidence marked resolved",
      () => ({
        ...pending(),
        resolution: { kind: ForeignTipReconciliationsDB.Status.Resolved },
      }),
    ],
    [
      "verified-empty evidence for non-empty commitments",
      () => ({
        ...pending(),
        evidence: {
          kind: ForeignTipReconciliationsDB.EvidenceKind.VerifiedEmpty,
        },
        resolution: { kind: ForeignTipReconciliationsDB.Status.Resolved },
      }),
    ],
    [
      "verified DA evidence for empty commitments",
      () => ({
        ...verifiedEmpty(),
        evidence: verifiedDa().evidence,
      }),
    ],
    [
      "resolution alias",
      () => ({
        ...pending(),
        resolution: { status: "awaiting", reason: "pending_evidence" },
      }),
    ],
    [
      "empty awaiting reason",
      () => ({
        ...pending(),
        resolution: {
          kind: ForeignTipReconciliationsDB.Status.Awaiting,
          reason: "",
        },
      }),
    ],
  ])("rejects %s", (_label, candidate) => {
    expect(() =>
      ForeignTipReconciliationsDB.parseForeignTipReconciliation(candidate()),
    ).toThrow();
  });

  it("rejects deployment, header, profile, digest, and empty-evidence replay substitutions", () => {
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidence({
        reconciliation: pending(),
        deploymentMarker: OTHER_MARKER,
        evidence: daIdentity(),
      }),
    ).toThrow();
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidence({
        reconciliation: pending(),
        deploymentMarker: MARKER,
        evidence: {
          ...daIdentity(),
          headerHash: Buffer.alloc(28, 0xff),
        },
      }),
    ).toThrow(/header\/profile/u);
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidence({
        reconciliation: pending(),
        deploymentMarker: MARKER,
        evidence: {
          ...daIdentity(),
          consensusProfileId: "midgard-consensus-v0",
        },
      }),
    ).toThrow();
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidence({
        reconciliation: pending(),
        deploymentMarker: MARKER,
        evidence: {
          ...daIdentity(),
          payloadSha256: Buffer.alloc(32, 0xff),
        },
      }),
    ).toThrow(/digest/u);
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidence({
        reconciliation: verifiedEmpty(),
        deploymentMarker: MARKER,
        evidence: daIdentity(),
      }),
    ).toThrow(/cannot be replaced/u);
  });
});

const gate = () =>
  reconcileOverdueAwaitingEventsAgainstRetainedForeignTips({
    eventsIngestedThrough: INGESTED_PAST_WINDOW,
  });

const retainedHashes = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ readonly hash: string }>`
    SELECT encode(foreign_header_hash, 'hex') AS hash
    FROM foreign_tip_reconciliations ORDER BY block_end_time
  `;
  return rows.map((row) => row.hash);
});

/** A stale non-empty foreign block over (startMs, endMs]. */
const staleHeader = (startMs: number, endMs: number) =>
  nonEmptyWindowHeader({
    startTime: BigInt(startMs),
    endTime: BigInt(endMs),
  });

describe("retained foreign-tip evidence scoping and isolation", () => {
  it("ignores another deployment's evidence instead of failing the commit", async () => {
    const result = await onNode(
      Effect.gen(function* () {
        yield* recordForeignTip(nonEmptyWindowHeader(), OTHER_MARKER);
        yield* indexDeposit({ [DepositsDB.Columns.INCLUSION_TIME]: IN_WINDOW });
        const otherOnly = yield* gate();
        const active = yield* recordForeignTip(
          nonEmptyWindowHeader({ prevHeaderHash: "44".repeat(28) }),
        );
        return { otherOnly, active, both: yield* gate() };
      }),
    );
    expect(result.otherOnly.type).toBe("Ready");
    expect(result.both).toMatchObject({
      type: "AwaitingForeignDa",
      foreignHeaderHash: result.active,
      reason: "missing",
    });
  });

  it("isolates a row that cannot be replayed, keeps replaying the others, and still gates on its window", async () => {
    const result = await onNode(
      Effect.gen(function* () {
        const broken = yield* recordForeignTip(nonEmptyWindowHeader());
        const sql = yield* SqlClient.SqlClient;
        yield* sql`
          UPDATE foreign_tip_reconciliations
          SET foreign_header_cbor = ${Buffer.from("d87980", "hex")}
          WHERE foreign_header_hash = ${Buffer.from(broken, "hex")}
        `;
        // A healthy empty-root foreign block right after the broken window.
        yield* recordForeignTip(
          headerFor({
            startTime: BigInt(WINDOW_END_MS),
            endTime: BigInt(WINDOW_END_MS + 10_000),
          }),
        );
        const released = yield* indexDeposit({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(WINDOW_END_MS + 5_000),
        });
        const emptyBrokenWindow = yield* gate();
        yield* indexDeposit({ [DepositsDB.Columns.INCLUSION_TIME]: IN_WINDOW });
        return {
          broken,
          released,
          emptyBrokenWindow,
          occupiedBrokenWindow: yield* gate(),
        };
      }),
    );
    expect(result.emptyBrokenWindow).toEqual({
      type: "Ready",
      absent: {
        deposits: [result.released],
        forcedTransactions: [],
        withdrawals: [],
      },
    });
    expect(result.occupiedBrokenWindow).toMatchObject({
      type: "AwaitingForeignDa",
      foreignHeaderHash: result.broken,
      reason: "replay_failed",
    });
  });
});

describe("settled foreign-tip evidence pruning", () => {
  it("prunes only rows past the horizon, from both now and the ingestion barrier, that no event can still reach", async () => {
    const horizonMs = MIDGARD_RETENTION_WINDOW.requiredRetentionMs;
    const base = 10_000_000;
    const lateEndMs = base + 50_000 + horizonMs;
    const result = await onNode(
      Effect.gen(function* () {
        const settled = yield* recordForeignTip(
          staleHeader(base, base + 10_000),
        );
        const occupied = yield* recordForeignTip(
          staleHeader(base + 20_000, base + 30_000),
        );
        yield* indexDeposit({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(base + 25_000),
        });
        const otherDeployment = yield* recordForeignTip(
          staleHeader(base + 40_000, base + 50_000),
          OTHER_MARKER,
        );
        yield* indexDeposit({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(base + 45_000),
        });
        const lateIngested = yield* recordForeignTip(
          staleHeader(lateEndMs - 10_000, lateEndMs),
        );
        const recent = yield* recordForeignTip(nonEmptyWindowHeader());
        const behindBarrier = yield* pruneSettledForeignTipReconciliations({
          now: new Date(),
          eventsIngestedThrough: new Date(lateEndMs + horizonMs / 2),
        });
        const afterBehindBarrier = yield* retainedHashes;
        const caughtUp = yield* pruneSettledForeignTipReconciliations({
          now: new Date(),
          eventsIngestedThrough: INGESTED_PAST_WINDOW,
        });
        return {
          ids: { settled, occupied, otherDeployment, lateIngested, recent },
          behindBarrier,
          afterBehindBarrier,
          caughtUp,
          afterCaughtUp: yield* retainedHashes,
          awaiting: yield* ForeignTipReconciliationsDB.countAwaiting,
        };
      }),
    );
    expect(result.behindBarrier).toBe(2);
    expect(result.afterBehindBarrier).toEqual([
      result.ids.occupied,
      result.ids.lateIngested,
      result.ids.recent,
    ]);
    expect(result.caughtUp).toBe(1);
    expect(result.afterCaughtUp).toEqual([
      result.ids.occupied,
      result.ids.recent,
    ]);
    expect(result.awaiting).toBe(2);
  });
});
