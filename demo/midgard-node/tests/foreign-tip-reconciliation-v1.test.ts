import { MIDGARD_CONSENSUS_PROFILE_V1_ID } from "@al-ft/midgard-core/consensus-profile-v1";
import { makeDeploymentMarkerV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { ForeignTipReconciliationsDB } from "@/database/index.js";
import { sha256 } from "@/sha256.js";

const EMPTY_ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;
const NONEMPTY_ROOT = "11".repeat(32);
const MARKER = makeDeploymentMarkerV1("ab".repeat(32));
const OTHER_MARKER = makeDeploymentMarkerV1("cd".repeat(32));
const FOREIGN_HEADER_HASH = Buffer.alloc(28, 0x31);
const REPLACED_HEADER_HASH = Buffer.alloc(28, 0x32);
const PAYLOAD_A = Buffer.from("d8799f4101ff", "hex");
const PAYLOAD_B = Buffer.from("d8799f4102ff", "hex");
const pending = (): ForeignTipReconciliationsDB.ForeignTipReconciliationV1 => ({
  version: ForeignTipReconciliationsDB.FOREIGN_TIP_RECONCILIATION_V1_VERSION,
  deploymentMarker: MARKER,
  consensusProfileId: MIDGARD_CONSENSUS_PROFILE_V1_ID,
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
  (): ForeignTipReconciliationsDB.ForeignTipReconciliationV1 => ({
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
  (): ForeignTipReconciliationsDB.ForeignTipReconciliationV1 => ({
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
  consensusProfileId: MIDGARD_CONSENSUS_PROFILE_V1_ID,
  payloadCbor: payload,
  payloadSha256: sha256(payload),
});

describe("ForeignTipReconciliationV1 exact evidence", () => {
  it("accepts the sole pending, verified-empty, and verified-DA V1 shapes", () => {
    const pendingResult =
      ForeignTipReconciliationsDB.parseForeignTipReconciliationV1(pending());
    const emptyResult =
      ForeignTipReconciliationsDB.parseForeignTipReconciliationV1(
        verifiedEmpty(),
      );
    const daResult =
      ForeignTipReconciliationsDB.parseForeignTipReconciliationV1(verifiedDa());

    expect(pendingResult.evidence.kind).toBe("pending_v1");
    expect(emptyResult.evidence.kind).toBe("verified_empty_v1");
    expect(daResult.evidence.kind).toBe("verified_da_v1");
    expect(daResult.deploymentMarker).toEqual(MARKER);
  });

  it("authenticates new DA against deployment, profile, header, and digest", () => {
    const authenticated =
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidenceV1({
        reconciliation: pending(),
        deploymentMarker: MARKER,
        evidence: daIdentity(),
      });

    expect(authenticated.headerHash).toEqual(FOREIGN_HEADER_HASH);
    expect(authenticated.payloadSha256).toEqual(sha256(PAYLOAD_A));
  });

  it("accepts only an exact replay of retained verified DA evidence", () => {
    const authenticated =
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidenceV1({
        reconciliation: verifiedDa(),
        deploymentMarker: MARKER,
        evidence: daIdentity(),
      });

    expect(authenticated.payloadCbor).toEqual(PAYLOAD_A);
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidenceV1({
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
      ForeignTipReconciliationsDB.parseForeignTipReconciliationV1(candidate()),
    ).toThrow();
  });

  it("rejects deployment, header, profile, digest, and empty-evidence replay substitutions", () => {
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidenceV1({
        reconciliation: pending(),
        deploymentMarker: OTHER_MARKER,
        evidence: daIdentity(),
      }),
    ).toThrow();
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidenceV1({
        reconciliation: pending(),
        deploymentMarker: MARKER,
        evidence: {
          ...daIdentity(),
          headerHash: Buffer.alloc(28, 0xff),
        },
      }),
    ).toThrow(/header\/profile/u);
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidenceV1({
        reconciliation: pending(),
        deploymentMarker: MARKER,
        evidence: {
          ...daIdentity(),
          consensusProfileId: "midgard-consensus-v0",
        },
      }),
    ).toThrow();
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidenceV1({
        reconciliation: pending(),
        deploymentMarker: MARKER,
        evidence: {
          ...daIdentity(),
          payloadSha256: Buffer.alloc(32, 0xff),
        },
      }),
    ).toThrow(/digest/u);
    expect(() =>
      ForeignTipReconciliationsDB.authenticateForeignTipDaEvidenceV1({
        reconciliation: verifiedEmpty(),
        deploymentMarker: MARKER,
        evidence: daIdentity(),
      }),
    ).toThrow(/cannot be replaced/u);
  });
});
