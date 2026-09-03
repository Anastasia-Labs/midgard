import { MIDGARD_CONSENSUS_PROFILE_V1_ID } from "@al-ft/midgard-core/consensus-profile-v1";
import { makeDeploymentMarkerV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";

const ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;
const CANDIDATE_ROOT = "11".repeat(32);
const MARKER = makeDeploymentMarkerV1("ab".repeat(32));

const metadata =
  (): PendingBlockFinalizationsDB.PendingBlockFinalizationMetadata => ({
    deploymentMarker: MARKER,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_V1_ID,
    stateQueueLeaseToken: "lease-v1",
    baseSnapshotId: "snapshot-v1",
    baseTailOutRef: `${"12".repeat(32)}#0`,
    baseTailHeaderHash: Buffer.alloc(28, 0x12),
    baseTailDatumCbor: "d87980",
    baseRoots: {
      utxosRoot: ROOT,
      forcedTransactionsRoot: ROOT,
      transactionsRoot: ROOT,
      depositsRoot: ROOT,
      withdrawalsRoot: ROOT,
    },
    blockStartTime: new Date("2026-06-20T00:00:00.000Z"),
    expectedRoots: {
      utxosRoot: CANDIDATE_ROOT,
      forcedTransactionsRoot: ROOT,
      transactionsRoot: ROOT,
      depositsRoot: ROOT,
      withdrawalsRoot: ROOT,
      transitionTraceRoot: ROOT,
      eventToStepRoot: ROOT,
      validationTracesRoot: ROOT,
    },
    expectedCounts: {
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
      validationTraceCount: 0n,
    },
  });

const ledgerDelta = (): PendingBlockFinalizationsDB.LedgerDeltaInput => ({
  spent: [Buffer.from("0102", "hex")],
  produced: [
    {
      [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]: Buffer.from(
        "0304",
        "hex",
      ),
      [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]: Buffer.from(
        "0506",
        "hex",
      ),
    },
  ],
});

const deltaEnvelope =
  (): PendingBlockFinalizationsDB.PendingBlockFinalizationV1 => ({
    version: PendingBlockFinalizationsDB.PENDING_BLOCK_FINALIZATION_V1_VERSION,
    metadata: metadata(),
    replay: {
      kind: PendingBlockFinalizationsDB.PendingBlockFinalizationReplayKindV1
        .LedgerDelta,
      ledgerDelta: ledgerDelta(),
    },
  });

const nativeEnvelope =
  (): PendingBlockFinalizationsDB.PendingBlockFinalizationV1 => ({
    version: PendingBlockFinalizationsDB.PENDING_BLOCK_FINALIZATION_V1_VERSION,
    metadata: metadata(),
    replay: {
      kind: PendingBlockFinalizationsDB.PendingBlockFinalizationReplayKindV1
        .LedgerDeltaWithNativeMpf,
      ledgerDelta: ledgerDelta(),
      nativeMpfReplay: {
        schema: 1,
        ownerBinarySha256: Buffer.alloc(32, 0x21),
        baseRoot: Buffer.from(ROOT, "hex"),
        candidateRoot: Buffer.from(CANDIDATE_ROOT, "hex"),
        eventLog: Buffer.alloc(92, 0x22),
        eventLogDigest: Buffer.alloc(32, 0x23),
        eventRoots: Buffer.alloc(0),
        eventCount: 0,
      },
    },
  });

describe("PendingBlockFinalizationV1 exact envelope", () => {
  it("accepts the sole delta-only V1 replay shape", () => {
    const parsed =
      PendingBlockFinalizationsDB.parsePendingBlockFinalizationV1(
        deltaEnvelope(),
      );

    expect(parsed.version).toBe(1);
    expect(parsed.replay.kind).toBe("ledger_delta_v1");
    expect(parsed.metadata.deploymentMarker).toEqual(MARKER);
    expect(parsed.replay.ledgerDelta.spent[0]?.toString("hex")).toBe("0102");
  });

  it("accepts the sole native-MPF V1 replay shape bound to metadata roots", () => {
    const parsed =
      PendingBlockFinalizationsDB.parsePendingBlockFinalizationV1(
        nativeEnvelope(),
      );

    expect(parsed.replay.kind).toBe("ledger_delta_native_mpf_v1");
    if (parsed.replay.kind !== "ledger_delta_native_mpf_v1") {
      throw new Error("expected native replay");
    }
    expect(parsed.replay.nativeMpfReplay.baseRoot.toString("hex")).toBe(ROOT);
    expect(parsed.replay.nativeMpfReplay.candidateRoot.toString("hex")).toBe(
      CANDIDATE_ROOT,
    );
  });

  it.each([
    ["unknown version", () => ({ ...deltaEnvelope(), version: 2 })],
    [
      "missing version",
      () => {
        const { version: _version, ...withoutVersion } = deltaEnvelope();
        return withoutVersion;
      },
    ],
    [
      "legacy formatVersion alias",
      () => {
        const { version: _version, ...rest } = deltaEnvelope();
        return { ...rest, formatVersion: 1 };
      },
    ],
    ["top-level extension", () => ({ ...deltaEnvelope(), extension: true })],
    [
      "metadata extension",
      () => {
        const envelope = deltaEnvelope();
        return {
          ...envelope,
          metadata: { ...envelope.metadata, legacySnapshot: [] },
        };
      },
    ],
    [
      "nested root alias",
      () => {
        const envelope = deltaEnvelope();
        return {
          ...envelope,
          metadata: {
            ...envelope.metadata,
            baseRoots: {
              ...envelope.metadata.baseRoots,
              utxoRoot: envelope.metadata.baseRoots.utxosRoot,
            },
          },
        };
      },
    ],
    [
      "implicit profile default",
      () => {
        const envelope = deltaEnvelope();
        const { consensusProfileId: _profile, ...rest } = envelope.metadata;
        return { ...envelope, metadata: rest };
      },
    ],
    [
      "profile substitution",
      () => {
        const envelope = deltaEnvelope();
        return {
          ...envelope,
          metadata: {
            ...envelope.metadata,
            consensusProfileId: "midgard-consensus-v0",
          },
        };
      },
    ],
    [
      "deployment-marker alias",
      () => {
        const envelope = deltaEnvelope();
        return {
          ...envelope,
          metadata: {
            ...envelope.metadata,
            deploymentMarker: {
              schema: MARKER.schemaVersion,
              manifestId: MARKER.manifestId,
            },
          },
        };
      },
    ],
    [
      "unknown replay discriminator",
      () => {
        const envelope = deltaEnvelope();
        return {
          ...envelope,
          replay: { ...envelope.replay, kind: "ledger_delta" },
        };
      },
    ],
    [
      "native replay hidden under delta discriminator",
      () => {
        const native = nativeEnvelope();
        if (native.replay.kind !== "ledger_delta_native_mpf_v1") {
          throw new Error("expected native replay");
        }
        return {
          ...native,
          replay: {
            ...native.replay,
            kind: "ledger_delta_v1",
          },
        };
      },
    ],
    [
      "native replay omitted under native discriminator",
      () => ({
        ...deltaEnvelope(),
        replay: {
          ...deltaEnvelope().replay,
          kind: "ledger_delta_native_mpf_v1",
        },
      }),
    ],
    [
      "native candidate-root mismatch",
      () => {
        const envelope = nativeEnvelope();
        if (envelope.replay.kind !== "ledger_delta_native_mpf_v1") {
          throw new Error("expected native replay");
        }
        return {
          ...envelope,
          replay: {
            ...envelope.replay,
            nativeMpfReplay: {
              ...envelope.replay.nativeMpfReplay,
              candidateRoot: Buffer.alloc(32, 0xff),
            },
          },
        };
      },
    ],
    [
      "count mismatch",
      () => {
        const envelope = deltaEnvelope();
        return {
          ...envelope,
          metadata: {
            ...envelope.metadata,
            expectedCounts: {
              ...envelope.metadata.expectedCounts,
              depositCount: 1n,
            },
          },
        };
      },
    ],
    [
      "duplicate spent outref",
      () => {
        const envelope = deltaEnvelope();
        return {
          ...envelope,
          replay: {
            ...envelope.replay,
            ledgerDelta: {
              ...envelope.replay.ledgerDelta,
              spent: [Buffer.from("01", "hex"), Buffer.from("01", "hex")],
            },
          },
        };
      },
    ],
  ])("rejects %s", (_label, candidate) => {
    expect(() =>
      PendingBlockFinalizationsDB.parsePendingBlockFinalizationV1(candidate()),
    ).toThrow();
  });
});
