import {
  admitFraudProofRawL1Snapshot,
  type FraudProofRawL1Snapshot,
  fraudProofSignedTransactionRecovery,
} from "@al-ft/midgard-fault-proofs";
import { type SimTx, simTxHash } from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import { WatcherFaultProofL1RefusedError } from "../../src/l1-follower/fault-proof-l1-source.js";
import type { FollowerRawReads } from "../../src/l1-follower/raw-reads.types.js";
import {
  bech32,
  C,
  type Fixture,
  fixture,
  hex,
  K,
  pruneFixture,
  T,
  X,
} from "../support/l1-follower-raw-reads-fixture.js";
import {
  RELEASE,
  requestFor,
  signedOf,
  sourceOver,
} from "../support/l1-follower-raw-source-fixture.js";

/**
 * The watcher's fault-proof L1 source answering a scope that names its
 * out-refs: it reads those outputs alone, by out-ref, on the raw-read
 * simulator chain, and refuses what it cannot answer exactly.
 */

const capture = async (
  fx: Pick<Fixture, "store">,
  reads: FollowerRawReads,
  request: ReturnType<typeof requestFor>,
): Promise<FraudProofRawL1Snapshot> =>
  (await sourceOver(fx, reads)
    .snapshotAuthority({
      releaseFinality: RELEASE,
      observationDepth: "inclusion",
    })
    .capture(request)) as FraudProofRawL1Snapshot;

const OUR_DATUM = Buffer.from("d87980", "hex");

/** The fixture with one empty block above b5. */
const extended = async () => {
  const fx = await fixture();
  await fx.forward([]);
  return { fx };
};

describe("snapshot authority: out-ref scopes", () => {
  /** A scope of tracked address T that names `outRefs`, and no history. */
  const scopedRequest = (
    header: string,
    outRefs: readonly string[],
    address = bech32(T),
  ) =>
    requestFor(header, {
      scopes: [{ role: "proof_chunk", address, outRefs }],
      historyUnits: [],
    });

  it("holds only the named outputs still unspent at the address, never the rest of it", async () => {
    const { fx } = await extended();
    const { a, b, v } = fx.hashes;
    const whole = await capture(
      fx,
      fx.reads(),
      requestFor(fx.header, {
        scopes: [{ role: "proof_chunk", address: bech32(T) }],
        historyUnits: [],
      }),
    );
    expect(whole.scopes[0]!.utxos.map(({ outRef }) => outRef)).toEqual(
      expect.arrayContaining([`${b}#0`, `${v}#0`]),
    );

    // V#0 is live at T; A#0 was spent; A#2 sits at C; the last never existed.
    const outRefs = [`${v}#0`, `${a}#0`, `${a}#2`, `${"ee".repeat(32)}#0`];
    const request = scopedRequest(fx.header, outRefs);
    const snapshot = await capture(fx, fx.reads(), request);
    expect(snapshot.scopes).toEqual([
      {
        role: "proof_chunk",
        address: bech32(T),
        outRefs,
        utxos: whole.scopes[0]!.utxos.filter(
          ({ outRef }) => outRef === `${v}#0`,
        ),
      },
    ]);
    expect(
      admitFraudProofRawL1Snapshot({
        value: snapshot,
        request,
        releaseFinality: RELEASE,
        observationDepth: "inclusion",
      }),
    ).toEqual(snapshot);
  });

  it("leaves out a live out-ref that sits at another tracked address", async () => {
    const { fx } = await extended();
    const g = `${fx.hashes.g}#0`;
    // G#0 is live at C, which the follower tracks by credential.
    const atC = await capture(
      fx,
      fx.reads(),
      scopedRequest(fx.header, [g], bech32(C)),
    );
    expect(atC.scopes[0]!.utxos.map(({ outRef }) => outRef)).toEqual([g]);
    const atT = await capture(fx, fx.reads(), scopedRequest(fx.header, [g]));
    expect(atT.scopes[0]!.utxos).toEqual([]);
  });

  it("refuses an out-ref scope of an address the follower does not track", async () => {
    const { fx } = await extended();
    const untracked = capture(
      fx,
      fx.reads(),
      scopedRequest(fx.header, [`${fx.hashes.u}#0`], bech32(X)),
    );
    await expect(untracked).rejects.toBeInstanceOf(
      WatcherFaultProofL1RefusedError,
    );
    await expect(untracked).rejects.toMatchObject({
      reason: "untracked_address",
    });
  });

  /** The scope's utxos at `observationDepth`, by out-ref. */
  const heldAt = async (
    fx: Fixture,
    outRefs: readonly string[],
    observationDepth: "inclusion" | "recovery_finality" = "inclusion",
  ) =>
    (
      (await sourceOver(fx, fx.reads())
        .snapshotAuthority({ releaseFinality: RELEASE, observationDepth })
        .capture(scopedRequest(fx.header, outRefs))) as FraudProofRawL1Snapshot
    ).scopes[0]!.utxos.map(({ outRef }) => outRef);

  /** One of our transactions: spends V#0 (live at T) and pays T a datum. */
  const ours = (fx: Fixture, invalidAfter?: number): SimTx => ({
    inputs: [{ txHash: Buffer.from(fx.hashes.v, "hex"), index: 0 }],
    outputs: [{ address: T, lovelace: 2_500_000n, datum: OUR_DATUM }],
    ...(invalidAfter === undefined ? {} : { invalidAfter }),
    nonce: fx.chain.nonce(),
  });

  // Each case: how our transaction stands once the follower has pruned.
  it.each([
    ["not yet landed", "rebroadcast"],
    ["dropped", "rebroadcast"],
    ["expired", "expired"],
    ["rolled back for good", "rebroadcast"],
  ] as const)(
    "after pruning, reads the output of our transaction %s as absent, and signed recovery goes ahead",
    async (state, recovered) => {
      const fx = await fixture();
      const tx = ours(
        fx,
        state === "expired" ? Number(fx.points.p5.slot) : undefined,
      );
      const txHash = hex(simTxHash(tx));
      if (state === "rolled back for good") {
        await fx.forward([tx]);
        expect(await heldAt(fx, [`${txHash}#0`])).toEqual([`${txHash}#0`]);
        await fx.backward(1);
      }
      await pruneFixture(fx);
      expect(await heldAt(fx, [`${txHash}#0`])).toEqual([]);
      const recovery = fraudProofSignedTransactionRecovery(
        sourceOver(fx, fx.reads()),
        RELEASE,
      );
      expect(
        (await recovery.observeSignedTransaction(signedOf(tx))).status,
      ).toBe(recovered);
      if (state === "not yet landed") {
        await fx.forward([tx]);
        expect(await heldAt(fx, [`${txHash}#0`])).toEqual([`${txHash}#0`]);
      }
    },
  );

  it("after pruning, reads a spent output the pruning removed as absent", async () => {
    const fx = await fixture();
    await pruneFixture(fx);
    // A#0 was spent in b2 and pruned; B#0 is still live at T.
    expect(await heldAt(fx, [`${fx.hashes.a}#0`, `${fx.hashes.b}#0`])).toEqual([
      `${fx.hashes.b}#0`,
    ]);
  });

  it("refuses an out-ref pruning may have removed at a capture point below the prune boundary", async () => {
    const fx = await fixture();
    // Q#0 is unspent in its block (kept: R#0 is live there), spent in the
    // next block, which the pruning then reaches.
    const q: SimTx = {
      inputs: [fx.chain.outsideInput()],
      outputs: [{ address: T, lovelace: 2_000_000n }],
      nonce: fx.chain.nonce(),
    };
    const r: SimTx = {
      inputs: [fx.chain.outsideInput()],
      outputs: [{ address: T, lovelace: 3_000_000n }],
      nonce: fx.chain.nonce(),
    };
    const created = await fx.forward([q, r]);
    const qHash = created.hashes[0]!;
    expect(await heldAt(fx, [`${qHash}#0`])).toEqual([`${qHash}#0`]);
    const spent = await fx.forward([
      {
        inputs: [{ txHash: Buffer.from(qHash, "hex"), index: 0 }],
        outputs: [{ address: X, lovelace: 1_500_000n }],
        nonce: fx.chain.nonce(),
      },
    ]);
    for (let i = 0; i < K; i += 1) await fx.forward([]);
    const prunedThrough = await fx.pruneAll();
    expect(prunedThrough).toBeGreaterThanOrEqual(Number(spent.point.slot));
    expect(Number(created.point.slot)).toBeLessThan(prunedThrough);
    // The recovery-final point (5 deep) is Q's block, below the boundary.
    const refused = heldAt(fx, [`${qHash}#0`], "recovery_finality");
    await expect(refused).rejects.toBeInstanceOf(
      WatcherFaultProofL1RefusedError,
    );
    await expect(refused).rejects.toMatchObject({
      reason: "beyond_retention",
    });
  });
});
