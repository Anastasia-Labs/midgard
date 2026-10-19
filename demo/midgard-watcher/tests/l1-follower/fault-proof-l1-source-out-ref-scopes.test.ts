import {
  admitFraudProofRawL1Snapshot,
  type FraudProofRawL1Snapshot,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import { WatcherFaultProofL1RefusedError } from "../../src/l1-follower/fault-proof-l1-source.js";
import type { FollowerRawReads } from "../../src/l1-follower/raw-reads.types.js";
import {
  bech32,
  type Fixture,
  fixture,
  T,
  X,
} from "../support/l1-follower-raw-reads-fixture.js";
import {
  RELEASE,
  requestFor,
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

  it("refuses an out-ref whose row pruning may have removed, never reading it as absent", async () => {
    const { fx } = await extended();
    const pruned: FollowerRawReads = {
      ...fx.reads(),
      utxosByOutRefAtPoint: async (outRefs) => ({
        kind: "ok",
        value: {
          outputs: [],
          spends: [],
          unknown: [],
          beyondRetention: [...outRefs],
        },
      }),
    };
    await expect(
      capture(fx, pruned, scopedRequest(fx.header, [`${fx.hashes.v}#0`])),
    ).rejects.toMatchObject({ reason: "beyond_retention" });
  });
});
