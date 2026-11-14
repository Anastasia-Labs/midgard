import {
  type FactStore,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  encodeTxBody,
  encodeWitnessSet,
  SIM_ORIGIN,
  simStoreOptions,
  type SimTx,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { watcherProjection } from "../../src/l1-follower/projection.js";
import { createFollowerRawReads } from "../../src/l1-follower/raw-reads.js";
import {
  rawPointOf,
  requireResolvedInputs,
} from "../../src/l1-follower/raw-reads.types.js";
import {
  bech32,
  byOutRef,
  C,
  D,
  expected,
  fixture,
  harness,
  hex,
  K,
  okValue,
  random,
  reasonOf,
  SEED,
  SEED_LABEL,
  seedExpected,
  T,
  X,
} from "../support/l1-follower-raw-reads-fixture.js";
import { outRef, twinScenario } from "../support/l1-follower-raw-twin.js";

/**
 * Behaviour of the follower-backed raw reads (ticket W1) on a hand-built
 * simulator chain (`l1-follower-raw-reads-fixture.ts`). The oracle is the
 * simulator's own transaction specs: the exact bytes an output must read
 * back as come from the spec's encoding, never from the store.
 */

describe("follower raw reads: refusals before initialization", () => {
  it("every read refuses not_initialized (or answers nothing) without a cursor", async () => {
    const store: FactStore = openSqliteFactStore({
      ...simStoreOptions([watcherProjection(D)], K, "sqlite"),
      path: ":memory:",
    });
    expect((await store.start()).kind).toBe("ready");
    const reads = createFollowerRawReads(store, {
      stateQueuePolicyId: D.stateQueueMint,
    });
    const point = rawPointOf({
      ...SIM_ORIGIN.point,
      height: SIM_ORIGIN.height,
    });
    expect(reasonOf(await reads.rawTransaction(random(1), point))).toBe(
      "not_initialized",
    );
    expect(reasonOf(await reads.addressUtxosAtPoint(bech32(T), point))).toBe(
      "not_initialized",
    );
    expect(reasonOf(await reads.utxosByOutRefAtPoint([], point))).toBe(
      "not_initialized",
    );
    expect(reasonOf(await reads.predecessorPoint(point))).toBe(
      "not_initialized",
    );
  });
});

describe("follower raw reads on the simulator chain", () => {
  it("addressUtxosAtPoint: exact live outputs, the seed refused, untracked refused", async () => {
    const fx = await fixture();
    const reads = fx.reads();
    const { A, B, F, V, G } = fx.specs;
    const { a, b, f, v, g } = fx.hashes;
    expect(
      reasonOf(await reads.addressUtxosAtPoint(bech32(X), fx.points.p1)),
    ).toBe("untracked_address");
    // The seed is live at b1 and its bytes were never in a stored body.
    expect(
      reasonOf(await reads.addressUtxosAtPoint(bech32(T), fx.points.p1)),
    ).toBe("l1_input_before_origin");
    expect(
      okValue(await reads.addressUtxosAtPoint(bech32(T), fx.points.p2)),
    ).toEqual([expected(B, b, 0)]);
    expect(
      byOutRef(
        okValue(await reads.addressUtxosAtPoint(bech32(T), fx.points.p3)),
      ),
    ).toEqual(byOutRef([expected(B, b, 0), expected(V, v, 0)]));
    // The credential address: A#2 at b1, the collateral return at b2, G's at b3.
    expect(
      okValue(await reads.addressUtxosAtPoint(bech32(C), fx.points.p1)),
    ).toEqual([expected(A, a, 2)]);
    expect(
      okValue(await reads.addressUtxosAtPoint(bech32(C), fx.points.p2)),
    ).toEqual([expected(F, f, 1)]);
    expect(
      okValue(await reads.addressUtxosAtPoint(bech32(C), fx.points.p3)),
    ).toEqual([expected(G, g, 0)]);
  });

  it("utxosByOutRefAtPoint: outputs, spends, provable unknowns, the collateral-return index", async () => {
    const fx = await fixture();
    const reads = fx.reads();
    const { A, F } = fx.specs;
    const { a, b, f } = fx.hashes;
    const atB1 = okValue(
      await reads.utxosByOutRefAtPoint(
        [`${a}#0`, `${a}#1`, `${a}#9`, `${b}#0`, `${a}#2`],
        fx.points.p1,
      ),
    );
    expect(byOutRef(atB1.outputs)).toEqual(
      byOutRef([expected(A, a, 0), expected(A, a, 2)]),
    );
    // Untracked, nonexistent, not yet created: provably unknown before any pruning.
    expect([...atB1.unknown].sort()).toEqual(
      [`${a}#1`, `${a}#9`, `${b}#0`].sort(),
    );
    expect(atB1.spends).toEqual([]);
    expect(atB1.beyondRetention).toEqual([]);
    const atB2 = okValue(
      await reads.utxosByOutRefAtPoint(
        [`${a}#0`, `${f}#1`, `${f}#0`, SEED_LABEL],
        fx.points.p2,
      ),
    );
    expect(atB2.outputs).toEqual([expected(F, f, 1)]);
    // A failed tx creates only its collateral return, at index outputs.len().
    expect(atB2.unknown).toEqual([`${f}#0`]);
    expect(byOutRef(atB2.spends)).toEqual(
      byOutRef([
        { outRef: `${a}#0`, spendingTxHash: b, spendPoint: fx.points.p2 },
        { outRef: SEED_LABEL, spendingTxHash: b, spendPoint: fx.points.p2 },
      ]),
    );
    // A live seed needs its exact bytes: refused, never a stand-in.
    expect(
      reasonOf(await reads.utxosByOutRefAtPoint([SEED_LABEL], fx.points.p1)),
    ).toBe("l1_input_before_origin");
  });

  it("unitHistoryAtPoint: a queued header's history, unprojected units refused", async () => {
    const fx = await fixture();
    const reads = fx.reads();
    const unit = `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${fx.header}`;
    expect(okValue(await reads.unitHistoryAtPoint(unit, fx.points.p5))).toEqual(
      {
        checkpoint: fx.points.p5,
        transactions: [
          { txHash: fx.hashes.commit, inclusionPoint: fx.points.p5 },
        ],
      },
    );
    expect(
      okValue(await reads.unitHistoryAtPoint(unit, fx.points.p4)).transactions,
    ).toEqual([]);
    const unknown = `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${"ab".repeat(28)}`;
    expect(
      okValue(await reads.unitHistoryAtPoint(unknown, fx.points.p5))
        .transactions,
    ).toEqual([]);
    for (const other of [
      `${D.stateQueueMint}${SDK.STATE_QUEUE_ROOT_ASSET_NAME}`,
      // A policy no projection follows (the hub-oracle policy now is one).
      `${"7f".repeat(28)}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${fx.header}`,
    ])
      expect(
        reasonOf(await reads.unitHistoryAtPoint(other, fx.points.p5)),
      ).toBe("unit_not_projected");
  });

  it("transactionInclusion: stored txs at their points; an absent tx is null before pruning", async () => {
    const fx = await fixture();
    const reads = fx.reads();
    expect(okValue(await reads.transactionInclusion(fx.hashes.b))).toEqual(
      fx.points.p2,
    );
    expect(okValue(await reads.transactionInclusion(fx.hashes.f))).toEqual(
      fx.points.p2,
    );
    expect(okValue(await reads.transactionInclusion(fx.hashes.g))).toEqual(
      fx.points.p3,
    );
    // U touched nothing tracked: never stored, and nothing was pruned.
    expect(okValue(await reads.transactionInclusion(fx.hashes.u))).toBeNull();
    expect(okValue(await reads.transactionInclusion(random(7)))).toBeNull();
  });

  it("rawTransaction: the stored body, inputs from stored bodies, named refusals", async () => {
    const fx = await fixture();
    const reads = fx.reads();
    const { A, B, F } = fx.specs;
    const { a, b, f, g, v } = fx.hashes;
    const tipHeight = fx.chain.tip.height;
    const raw = okValue(await reads.rawTransaction(b, fx.points.p2));
    expect(raw.transaction).toEqual({
      txHash: b,
      bodyCbor: hex(encodeTxBody(B)),
      witnessSetCbor: hex(encodeWitnessSet(B)),
      redeemersCbor: null,
      isValid: true,
      inclusionPoint: fx.points.p2,
      confirmationDepth: tipHeight - Number(fx.points.p2.blockNo) + 1,
      resolvedInputs: raw.transaction.resolvedInputs,
      resolvedReferenceInputs: [expected(A, a, 2)],
    });
    // A#1 is untracked but its creating body is stored: it resolves.
    expect(byOutRef(raw.transaction.resolvedInputs)).toEqual(
      byOutRef([expected(A, a, 0), expected(A, a, 1)]),
    );
    expect(raw.unresolvedInputs).toEqual([
      { outRef: SEED_LABEL, reason: "l1_input_before_origin" },
    ]);
    expect(raw.unresolvedReferenceInputs).toEqual([]);
    expect(
      reasonOf(
        requireResolvedInputs(await reads.rawTransaction(b, fx.points.p2)),
      ),
    ).toBe("l1_input_before_origin");
    // The collateral return resolves from the failed tx's body at outputs.len().
    expect(
      okValue(await reads.rawTransaction(g, fx.points.p3)).transaction
        .resolvedInputs,
    ).toEqual([expected(F, f, 1)]);
    // U#0's creating tx is not stored and no ledger state was given.
    expect(
      okValue(await reads.rawTransaction(v, fx.points.p3)).unresolvedInputs,
    ).toEqual([{ outRef: `${fx.hashes.u}#0`, reason: "l1_input_unresolved" }]);
    expect(reasonOf(await reads.rawTransaction(f, fx.points.p2))).toBe(
      "phase2_invalid",
    );
    expect(reasonOf(await reads.rawTransaction(b, fx.points.p1))).toBe(
      "not_at_point",
    );
    expect(reasonOf(await reads.rawTransaction(random(9), fx.tipPoint()))).toBe(
      "not_stored",
    );
  });

  it("rawTransaction: inputs without a stored body resolve from the ledger at the predecessor", async () => {
    const fx = await fixture();
    const reads = fx.reads(true);
    const { U: Utx } = fx.specs;
    const { b, u, v } = fx.hashes;
    const viaLedger = okValue(await reads.rawTransaction(v, fx.points.p3));
    expect(viaLedger.transaction.resolvedInputs).toEqual([expected(Utx, u, 0)]);
    expect(viaLedger.unresolvedInputs).toEqual([]);
    // b1 is more than k below the tip: the seed B spends is not acquirable.
    expect(
      okValue(await reads.rawTransaction(b, fx.points.p2)).unresolvedInputs,
    ).toEqual([{ outRef: SEED_LABEL, reason: "l1_input_before_origin" }]);
    // Once b2 is more than k below the tip the node cannot acquire it.
    for (let i = 0; i < K + 1; i += 1) await fx.forward([]);
    expect(
      okValue(await reads.rawTransaction(v, fx.points.p3)).unresolvedInputs,
    ).toEqual([{ outRef: `${u}#0`, reason: "l1_input_unresolved" }]);
  });

  it("rawTransaction: a seed's exact bytes come from the ledger while its spend is within k", async () => {
    const h = await harness();
    const spend: SimTx = {
      inputs: [SEED.outRef],
      outputs: [{ address: T, lovelace: 4_000_000n }],
      nonce: h.chain.nonce(),
    };
    const landed = await h.forward([spend]);
    const txHash = landed.hashes[0]!;
    const withLedger = okValue(
      await h.reads(true).rawTransaction(txHash, landed.point),
    );
    expect(withLedger.transaction.resolvedInputs).toEqual([seedExpected()]);
    expect(withLedger.unresolvedInputs).toEqual([]);
    expect(
      reasonOf(
        requireResolvedInputs(
          await h.reads().rawTransaction(txHash, landed.point),
        ),
      ),
    ).toBe("l1_input_before_origin");
  });

  it("predecessorPoint: the parent block, down to the origin", async () => {
    const fx = await fixture();
    const reads = fx.reads();
    expect(okValue(await reads.predecessorPoint(fx.points.p2))).toEqual(
      fx.points.p1,
    );
    expect(okValue(await reads.predecessorPoint(fx.points.p1))).toEqual(
      rawPointOf({ ...SIM_ORIGIN.point, height: SIM_ORIGIN.height }),
    );
    expect(
      reasonOf(
        await reads.predecessorPoint(
          rawPointOf({ ...SIM_ORIGIN.point, height: SIM_ORIGIN.height }),
        ),
      ),
    ).toBe("beyond_retention");
  });

  it("points off the stored chain are refused point_not_canonical", async () => {
    const fx = await fixture();
    const reads = fx.reads();
    const orphan = (await fx.forward([])).point;
    await fx.backward(1);
    await fx.forward([]);
    const wrongHeight = {
      ...fx.points.p2,
      blockNo: (Number(fx.points.p2.blockNo) + 1).toString(),
    };
    for (const point of [orphan, wrongHeight]) {
      expect(reasonOf(await reads.addressUtxosAtPoint(bech32(T), point))).toBe(
        "point_not_canonical",
      );
      expect(reasonOf(await reads.utxosByOutRefAtPoint([], point))).toBe(
        "point_not_canonical",
      );
      expect(
        reasonOf(
          await reads.unitHistoryAtPoint(
            `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${fx.header}`,
            point,
          ),
        ),
      ).toBe("point_not_canonical");
      expect(reasonOf(await reads.predecessorPoint(point))).toBe(
        "point_not_canonical",
      );
    }
  });
});

describe("follower raw reads: collateral returns and pruning across rollbacks", () => {
  it("a failed transaction's collateral return reads at index outputs.len() (ruling 6)", async () => {
    const h = await harness();
    const A: SimTx = {
      inputs: [h.chain.outsideInput()],
      outputs: [{ address: T, lovelace: 5_000_000n }],
      nonce: h.chain.nonce(),
    };
    const a = (await h.forward([A])).hashes[0]!;
    const F: SimTx = {
      inputs: [outRef(a, 0)],
      collaterals: [outRef(a, 0)],
      outputs: [
        { address: T, lovelace: 1_000_000n },
        { address: C, lovelace: 2_000_000n },
        { address: T, lovelace: 1_500_000n },
      ],
      collateralReturn: { address: C, lovelace: 4_500_000n },
      isValid: false,
      nonce: h.chain.nonce(),
    };
    const landedF = await h.forward([F]);
    const f = landedF.hashes[0]!;
    const G: SimTx = {
      inputs: [outRef(f, 3)],
      outputs: [{ address: T, lovelace: 4_000_000n }],
      nonce: h.chain.nonce(),
    };
    const landedG = await h.forward([G]);
    const reads = h.reads();
    const labels = [0, 1, 2, 3].map((index) => `${f}#${index.toString()}`);
    const atF = okValue(
      await reads.utxosByOutRefAtPoint([...labels, `${a}#0`], landedF.point),
    );
    expect(atF.outputs).toEqual([expected(F, f, 3)]);
    expect([...atF.unknown].sort()).toEqual(labels.slice(0, 3).sort());
    expect(atF.spends).toEqual([
      { outRef: `${a}#0`, spendingTxHash: f, spendPoint: landedF.point },
    ]);
    expect(
      okValue(await reads.addressUtxosAtPoint(bech32(C), landedF.point)),
    ).toEqual([expected(F, f, 3)]);
    expect(
      okValue(await reads.rawTransaction(landedG.hashes[0]!, landedG.point))
        .transaction.resolvedInputs,
    ).toEqual([expected(F, f, 3)]);
    expect(reasonOf(await reads.rawTransaction(f, landedF.point))).toBe(
      "phase2_invalid",
    );
  });

  it("prunes before and after deep rollbacks: retention exact, reads match the oracle (ruling 9)", async () => {
    const { w, A, a, B, b, unit, landedCommit, r, landedR } =
      await twinScenario();
    // Prune, then roll back the k blocks the pruning left.
    const first = await w.pruneAndDiff();
    expect(first.diff).toBeNull();
    let reads = w.pruned.reads();
    const before = okValue(
      await reads.utxosByOutRefAtPoint(
        [`${a}#0`, `${a}#1`, `${a}#2`],
        w.pruned.tipPoint(),
      ),
    );
    expect(before.outputs).toEqual([expected(A, a, 2)]);
    expect(before.spends).toEqual([
      { outRef: `${a}#1`, spendingTxHash: r, spendPoint: landedR.point },
    ]);
    expect(before.beyondRetention).toEqual([`${a}#0`]);
    await w.backward(K);
    reads = w.pruned.reads();
    // R is undone: A#1 is live again, and R is provably not included.
    expect(
      okValue(await reads.utxosByOutRefAtPoint([`${a}#1`], w.pruned.tipPoint()))
        .outputs,
    ).toEqual([expected(A, a, 1)]);
    expect(
      okValue(await reads.transactionInclusion(r, Number(landedR.point.slot))),
    ).toBeNull();
    // A new branch spends A#1 elsewhere; pruning after the rollback.
    const R2: SimTx = {
      inputs: [outRef(a, 1)],
      outputs: [{ address: C, lovelace: 3_000_000n }],
      nonce: w.chain.nonce(),
    };
    const landedR2 = await w.forward([R2]);
    const r2 = landedR2.hashes[0]!;
    await w.empty(K);
    const second = await w.pruneAndDiff();
    expect(second.diff).toBeNull();
    expect(second.prunedThrough).toBeGreaterThanOrEqual(
      Number(landedR2.point.slot),
    );
    reads = w.pruned.reads();
    const tip = w.pruned.tipPoint();
    // R's old slot is still above the window: provably absent; unhinted, the pruning may have held it.
    expect(
      okValue(await reads.transactionInclusion(r, Number(landedR.point.slot))),
    ).toBeNull();
    expect(reasonOf(await reads.transactionInclusion(r))).toBe(
      "beyond_retention",
    );
    expect(
      okValue(await reads.utxosByOutRefAtPoint([`${a}#1`], tip))
        .beyondRetention,
    ).toEqual([`${a}#1`]);
    const atC = byOutRef([
      expected(A, a, 2),
      expected(B, b, 0),
      expected(R2, r2, 0),
    ]);
    expect(
      byOutRef(okValue(await reads.addressUtxosAtPoint(bech32(C), tip))),
    ).toEqual(atC);
    expect(
      okValue(await reads.rawTransaction(r2, landedR2.point)).transaction
        .resolvedInputs,
    ).toEqual([expected(A, a, 1)]);
    expect(
      okValue(await reads.unitHistoryAtPoint(unit, tip)).transactions,
    ).toEqual([
      { txHash: landedCommit.hashes[0]!, inclusionPoint: landedCommit.point },
    ]);
    // A second deep rollback after the pruning, then pruning again.
    await w.backward(K);
    await w.empty(K + 1);
    const third = await w.pruneAndDiff();
    expect(third.diff).toBeNull();
    const reference = w.reference.reads();
    reads = w.pruned.reads();
    for (const address of [bech32(C), bech32(T)])
      expect(
        await reads.addressUtxosAtPoint(address, w.pruned.tipPoint()),
      ).toEqual(
        await reference.addressUtxosAtPoint(address, w.reference.tipPoint()),
      );
  });
});
