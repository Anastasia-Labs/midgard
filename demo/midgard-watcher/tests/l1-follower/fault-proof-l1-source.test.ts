import {
  admitFraudProofRawL1Snapshot,
  computeFraudProofRawL1RollbackCursor,
  FraudProofL1CheckpointChangedError,
  type FraudProofL1ObservationDepth,
  FraudProofL1UnavailableError,
  type FraudProofRawL1Point,
  type FraudProofRawL1Snapshot,
} from "@al-ft/midgard-fault-proofs";
import { openSqliteFactStore } from "@al-ft/midgard-l1-follower";
import { simStoreOptions } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  createWatcherFaultProofL1Source,
  WatcherFaultProofL1RefusedError,
} from "../../src/l1-follower/fault-proof-l1-source.js";
import {
  type FollowerRawReads,
  type RawReadRefusal,
  refused,
} from "../../src/l1-follower/raw-reads.types.js";
import {
  bech32,
  D,
  type Fixture,
  fixture,
  harness,
  pruneFixture,
  X,
} from "../support/l1-follower-raw-reads-fixture.js";
import { describeSignedRecovery } from "../support/l1-follower-raw-signed-recovery-cases.js";
import {
  nodeDouble,
  nodeUnit,
  RECOVERY_DEPTH,
  RELEASE,
  requestFor,
  SOURCE_ID,
  sourceOver,
  STATE_QUEUE_ADDRESS,
} from "../support/l1-follower-raw-source-fixture.js";

/**
 * The watcher's fault-proof L1 source over the follower (ticket W1), on the
 * raw-read simulator chain (k = 3). The oracle is the simulator: boundary
 * points are the points the chain served at each height, never the store's
 * answer.
 */

type Captured = FraudProofRawL1Snapshot;

const capture = async (
  fx: Pick<Fixture, "store">,
  reads: FollowerRawReads,
  observationDepth: FraudProofL1ObservationDepth,
  request = requestFor("ab".repeat(28)),
): Promise<Captured> =>
  (await sourceOver(fx, reads)
    .snapshotAuthority({ releaseFinality: RELEASE, observationDepth })
    .capture(request)) as Captured;

/** The fixture with `extra` empty blocks above b5, and the point served at each height. */
const extended = async (extra: number) => {
  const fx = await fixture();
  const pointAt = new Map([
    [Number(fx.points.p4.blockNo), fx.points.p4],
    [Number(fx.points.p5.blockNo), fx.points.p5],
  ]);
  const forward = async () => {
    const { point } = await fx.forward([]);
    pointAt.set(Number(point.blockNo), point);
  };
  for (let i = 0; i < extra; i += 1) await forward();
  return { fx, pointAt, forward, request: requestFor(fx.header) };
};

describe("snapshot authority: boundary depth", () => {
  it("pins the boundary exactly minimum-deep per observation depth", async () => {
    const { fx, pointAt, request } = await extended(4);
    const tipHeight = fx.chain.tip.height;
    for (const [depth, minimum] of [
      ["inclusion", 1],
      ["release_finality", 2],
      ["recovery_finality", RECOVERY_DEPTH],
    ] as const) {
      const snapshot = await capture(fx, fx.reads(), depth, request);
      expect(snapshot.cursor.point).toEqual(
        pointAt.get(tipHeight - minimum + 1),
      );
      expect(snapshot.cursor.tip).toEqual(fx.tipPoint());
      expect(snapshot.cursor.confirmationDepth).toBe(minimum);
    }
    // b5 is exactly recovery-deep: its commit is in the history.
    const deep = await capture(fx, fx.reads(), "recovery_finality", request);
    expect(deep.cursor.point).toEqual(fx.points.p5);
    expect(deep.history[0]!.transactionHashes).toEqual([fx.hashes.commit]);
  });

  it("one block short, the recovery boundary is b4: the commit is not yet visible", async () => {
    const { fx, request } = await extended(3);
    const snapshot = await capture(
      fx,
      fx.reads(),
      "recovery_finality",
      request,
    );
    expect(snapshot.cursor.point).toEqual(fx.points.p4);
    expect(snapshot.history[0]!.transactionHashes).toEqual([]);
    expect(snapshot.transactions).toEqual([]);
  });

  it("is unavailable until the chain above the origin is minimum-deep", async () => {
    const h = await harness();
    for (let i = 0; i < RECOVERY_DEPTH - 2; i += 1) await h.forward([]);
    const recovery = capture(h, h.reads(), "recovery_finality");
    await expect(recovery).rejects.toBeInstanceOf(FraudProofL1UnavailableError);
    await expect(recovery).rejects.toThrow(/not 5 blocks deep/u);
    expect(
      (await capture(h, h.reads(), "release_finality")).cursor.tip,
    ).toEqual(h.tipPoint());
    await h.forward([]);
    const deep = await capture(h, h.reads(), "recovery_finality");
    expect(deep.cursor.confirmationDepth).toBe(RECOVERY_DEPTH);
  });

  it("a boundary the pruning removed is refused point_beyond_retention", async () => {
    const fx = await fixture();
    await pruneFixture(fx);
    const pruned = capture(fx, fx.reads(), "recovery_finality");
    await expect(pruned).rejects.toBeInstanceOf(
      WatcherFaultProofL1RefusedError,
    );
    await expect(pruned).rejects.toMatchObject({
      reason: "point_beyond_retention",
    });
    // Within the window the same store still captures.
    const shallow = await capture(
      fx,
      fx.reads(),
      "inclusion",
      requestFor(fx.header),
    );
    expect(shallow.history[0]!.transactionHashes).toEqual([fx.hashes.commit]);
  });
});

describe("snapshot authority: rollback during a capture", () => {
  /** Reads that roll the chain back (and grow a new branch) before the first `times` scope reads. */
  const rollingReads = (
    fx: Fixture,
    times: number,
    depth: number,
    pointAt?: Map<number, FraudProofRawL1Point>,
  ) => {
    const reads = fx.reads();
    const calls = { scope: 0 };
    const rolling: FollowerRawReads = {
      ...reads,
      addressUtxosAtPoint: async (address, point) => {
        calls.scope += 1;
        if (calls.scope <= times) {
          await fx.backward(depth);
          for (let i = 0; i <= depth; i += 1) {
            const { point: served } = await fx.forward([]);
            pointAt?.set(Number(served.blockNo), served);
          }
        }
        return await reads.addressUtxosAtPoint(address, point);
      },
    };
    return { rolling, calls };
  };

  it("a rollback across the boundary fails the attempt; the retry captures cleanly", async () => {
    const { fx, pointAt, request } = await extended(3);
    const { rolling, calls } = rollingReads(fx, 1, 2, pointAt);
    const snapshot = await capture(fx, rolling, "release_finality", request);
    expect(calls.scope).toBe(2);
    expect(snapshot.cursor.tip).toEqual(fx.tipPoint());
    expect(snapshot.cursor.point).toEqual(pointAt.get(fx.chain.tip.height - 1));
    expect(snapshot.history[0]!.transactionHashes).toEqual([fx.hashes.commit]);
  });

  it("a rollback of the tip alone (boundary kept) still restarts the capture", async () => {
    const { fx, request } = await extended(3);
    const { rolling, calls } = rollingReads(fx, 1, 1);
    const snapshot = await capture(fx, rolling, "release_finality", request);
    expect(calls.scope).toBe(2);
    expect(snapshot.cursor.tip).toEqual(fx.tipPoint());
  });

  it("without a rollback the capture runs once", async () => {
    const { fx, request } = await extended(3);
    const { rolling, calls } = rollingReads(fx, 0, 2);
    await capture(fx, rolling, "release_finality", request);
    expect(calls.scope).toBe(1);
  });

  it("gives up after two retries with checkpoint-changed", async () => {
    const { fx, request } = await extended(3);
    const { rolling, calls } = rollingReads(fx, 5, 2);
    await expect(
      capture(fx, rolling, "release_finality", request),
    ).rejects.toBeInstanceOf(FraudProofL1CheckpointChangedError);
    expect(calls.scope).toBe(3);
  });
});

const REFUSALS: readonly RawReadRefusal[] = [
  "untracked_address",
  "unit_not_projected",
  "point_not_canonical",
  "point_beyond_retention",
  "not_initialized",
  "beyond_retention",
  "l1_input_before_origin",
  "l1_input_unresolved",
  "not_stored",
  "not_at_point",
  "phase2_invalid",
];

describe("snapshot authority: refusals become named errors", () => {
  it("maps every refusal kind of every read to its named error", async () => {
    const { fx, request } = await extended(1);
    const reads = fx.reads();
    const refusing = (
      read: "addressUtxosAtPoint" | "unitHistoryAtPoint" | "rawTransaction",
      reason: RawReadRefusal,
      calls: { n: number },
    ): FollowerRawReads => ({
      ...reads,
      [read]: async () => {
        calls.n += 1;
        return refused(reason, `stub ${reason}`);
      },
    });
    for (const read of [
      "addressUtxosAtPoint",
      "unitHistoryAtPoint",
      "rawTransaction",
    ] as const)
      for (const reason of REFUSALS) {
        const calls = { n: 0 };
        const attempt = capture(
          fx,
          refusing(read, reason, calls),
          "inclusion",
          request,
        );
        if (reason === "point_not_canonical" || reason === "not_at_point") {
          await expect(attempt).rejects.toBeInstanceOf(
            FraudProofL1CheckpointChangedError,
          );
          expect(calls.n).toBe(3);
        } else if (reason === "not_initialized") {
          await expect(attempt).rejects.toBeInstanceOf(
            FraudProofL1UnavailableError,
          );
          expect(calls.n).toBe(1);
        } else {
          await expect(attempt).rejects.toBeInstanceOf(
            WatcherFaultProofL1RefusedError,
          );
          await expect(attempt).rejects.toMatchObject({
            reason,
            detail: `stub ${reason}`,
          });
          expect(calls.n).toBe(1);
        }
      }
    // The unstubbed reads answer the same request.
    expect(
      (await capture(fx, reads, "inclusion", request)).transactions,
    ).toHaveLength(1);
  });

  it("the follower's own refusals surface by reason", async () => {
    const { fx, request } = await extended(1);
    const untracked = capture(fx, fx.reads(), "inclusion", {
      ...request,
      scopes: [{ role: "hub_oracle", address: bech32(X) }],
    });
    await expect(untracked).rejects.toMatchObject({
      name: "WatcherFaultProofL1RefusedError",
      reason: "untracked_address",
    });
    const root = `${D.stateQueueMint}${SDK.STATE_QUEUE_ROOT_ASSET_NAME}`;
    await expect(
      capture(fx, fx.reads(), "inclusion", {
        ...request,
        historyUnits: [root],
      }),
    ).rejects.toMatchObject({ reason: "unit_not_projected" });
    await pruneFixture(fx);
    await expect(
      capture(fx, fx.reads(), "inclusion", requestFor("cd".repeat(28))),
    ).rejects.toMatchObject({ reason: "beyond_retention" });
    // A store without a cursor is unavailable, not refused.
    const empty = openSqliteFactStore({
      ...simStoreOptions([], 3, "sqlite"),
      path: ":memory:",
    });
    await empty.start();
    await expect(
      capture({ store: empty }, fx.reads(), "inclusion", request),
    ).rejects.toBeInstanceOf(FraudProofL1UnavailableError);
  });
});

describe("snapshot authority: admitted, provenance exact", () => {
  it("returns the admitted snapshot with the persisted provenance strings", async () => {
    const { fx, request } = await extended(1);
    const snapshot = await capture(fx, fx.reads(), "release_finality", request);
    expect(
      admitFraudProofRawL1Snapshot({
        value: snapshot,
        request,
        releaseFinality: RELEASE,
        observationDepth: "release_finality",
      }),
    ).toEqual(snapshot);
    expect(snapshot.provenance).toEqual({
      trustClass: "authenticated_cardano_l1",
      sourceId: "midgard-local-kupo-http-ogmios-ws-source-v1:watcher-test",
      grade: "security",
      sourceMode: "local_kupo_ogmios",
      kupoCheckpoint: fx.points.p5,
      ogmiosTip: fx.tipPoint(),
    });
    expect(snapshot.cursor.rollbackCursor).toBe(
      computeFraudProofRawL1RollbackCursor({
        deploymentIdentityDigest: RELEASE.deploymentIdentityDigest,
        blueprintHash: RELEASE.blueprintHash,
        finalityPolicyDigest: RELEASE.policyDigest,
        sourceId: SOURCE_ID,
        pointId: fx.points.p5.pointId,
      }),
    );
    expect(snapshot.scopes).toEqual([
      expect.objectContaining({
        role: "state_queue",
        address: STATE_QUEUE_ADDRESS,
      }),
    ]);
    expect(snapshot.scopes[0]!.utxos.map(({ outRef }) => outRef)).toContain(
      `${fx.hashes.commit}#1`,
    );
    expect(snapshot.historyUnits).toEqual([nodeUnit(fx.header)]);
    const [commit] = snapshot.transactions;
    expect(commit!.txHash).toBe(fx.hashes.commit);
    expect(commit!.confirmationDepth).toBe(2);
  });

  it("admission runs: a request off the release identity is refused", async () => {
    const { fx, request } = await extended(1);
    await expect(
      capture(fx, fx.reads(), "release_finality", {
        ...request,
        blueprintHash: "ee".repeat(32),
      }),
    ).rejects.toThrow(/not bound to the verified release identity/u);
  });

  it("takes only a complete sourceId under the persisted prefix", async () => {
    const fx = await fixture();
    for (const sourceId of [
      "",
      "midgard-local-kupo-http-ogmios-ws-source-v1:",
      "midgard-local-kupo-http-ogmios-ws-source-v1: padded",
      "other-source-v1:watcher",
    ])
      expect(() =>
        createWatcherFaultProofL1Source({
          store: fx.store,
          rawReads: fx.reads(),
          node: nodeDouble().node,
          sourceId,
        }),
      ).toThrow(/sourceId/u);
    expect(() => sourceOver(fx, fx.reads())).not.toThrow();
  });
});

describeSignedRecovery();
