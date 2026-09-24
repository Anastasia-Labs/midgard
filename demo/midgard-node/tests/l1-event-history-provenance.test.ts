import { describe, expect, it } from "vitest";

import {
  type HistoryIncarnation,
  historyIncarnationDigest,
  reverseHistoryProvenance,
  stageHistoryProvenance,
} from "../src/l1-event-history-provenance.js";
import type {
  HistoryTransition,
  HistoryTransitionEvent,
} from "../src/l1-event-history-transition.js";

// Pure provenance/undo fixtures, not ledger validity or source-admission proofs.
const bindingDigest = "aa".repeat(32);
const tx = (n: number) => n.toString(16).padStart(64, "0");
const out = (n: number, outputIndex = 0) => ({ txHash: tx(n), outputIndex });
const event = (n = 1): HistoryTransitionEvent => ({
  key: "bb".repeat(32),
  idCbor: "01",
  inclusionTime: 9_007_199_254_740_993n,
  factsCbor: "02",
  payloadCbor: "03",
  originalAssetsCbor: "04",
  outRef: out(n),
});
const admission = (
  n = 1,
  kind: HistoryTransition["kind"] = "deposit",
): HistoryTransition => ({
  kind,
  operation: "InsertOrder",
  transactionHash: tx(n),
  consumed: [],
  produced: [],
  admission: event(n),
  continuations: [],
});
const continuation = (before = 1, after = 2): HistoryTransition => ({
  kind: "deposit",
  operation: "InsertFiller",
  transactionHash: tx(after),
  consumed: [out(before)],
  produced: [],
  continuations: [{ key: event().key, before: out(before), after: out(after) }],
});
const retirement = (
  before = 2,
  n = 3,
  kind: HistoryTransition["kind"] = "deposit",
): HistoryTransition => ({
  kind,
  operation: "RetireOrder",
  transactionHash: tx(n),
  consumed: [out(before)],
  produced: [],
  continuations: [],
  retirement: {
    event: { ...event(), outRef: out(before) },
    reason: kind === "deposit" ? "absorbed" : "payout_initialized",
    observerRedeemerIndex: 4,
    witnessCbor: "05",
  },
});
const stage = (
  incarnations: readonly HistoryIncarnation[],
  transitions: readonly HistoryTransition[],
  block = 100,
) =>
  stageHistoryProvenance({
    bindingDigest,
    block: { point: { id: tx(block), slot: block * 10, height: block } },
    incarnations,
    transitions: transitions.map((transition, transactionIndex) => ({
      transactionIndex,
      transition,
    })),
  });

describe("history incarnation provenance and rollback images", () => {
  it("keeps admission identity and original Value through continuation and retirement", () => {
    const admitted = stage([], [admission()])[0]!.after;
    const moved = stage([admitted], [continuation()], 101)[0]!.after;
    const retired = stage([moved], [retirement()], 102)[0]!.after;
    expect(moved.id).toBe(admitted.id);
    expect(retired.id).toBe(admitted.id);
    expect(retired.event).toEqual(admitted.event);
    expect(moved.placement!.admission).toEqual(admitted.placement!.admission);
    expect(moved.placement!.current!.outRef).toEqual(out(2));
    expect(retired.placement!.current).toBeNull();
    expect(retired.placement!.retirement).toMatchObject({
      reason: "absorbed",
      outRef: out(2),
      witnessCbor: "05",
    });
    expect(retired.placement!.admission.blockHash).toBe(tx(100));
    expect(retired.event.inclusionTime).toBe(9_007_199_254_740_993n);
    expect(() => stage([retired], [admission(4)], 103)).toThrow(
      /including a retired origin/,
    );
  });

  it("reverses retirement and pointer movement, retaining reverted admission as an orphan", () => {
    const admitted = stage([], [admission()]);
    const moved = stage([admitted[0]!.after], [continuation()], 101);
    const retired = stage([moved[0]!.after], [retirement()], 102);
    expect(reverseHistoryProvenance(retired, [retired[0]!.after])).toEqual([
      moved[0]!.after,
    ]);
    expect(reverseHistoryProvenance(moved, [moved[0]!.after])).toEqual([
      admitted[0]!.after,
    ]);
    const orphan = reverseHistoryProvenance(admitted, [admitted[0]!.after])[0]!;
    expect(orphan.id).toBe(admitted[0]!.after.id);
    expect(orphan.event).toEqual(admitted[0]!.after.event);
    expect(orphan.placement).toBeNull();
  });

  it("uses one pre-block image for same-block admission, continuation and retirement", () => {
    const changes = stage([], [admission(), continuation(), retirement()]);
    expect(changes).toHaveLength(1);
    expect(changes[0]!.before).toBeNull();
    expect(changes[0]!.after.placement!.retirement!.at.transactionIndex).toBe(
      2,
    );
    expect(
      reverseHistoryProvenance(
        changes,
        changes.map((c) => c.after),
      )[0]!.placement,
    ).toBeNull();
  });

  it("preserves different admissions of the same ID on replacement branches", () => {
    const first = stage([], [admission()]);
    const orphan = reverseHistoryProvenance(first, [first[0]!.after])[0]!;
    const replacement = stage([orphan], [admission(4)], 200)[0]!;
    expect(replacement.after.id).not.toBe(orphan.id);
    expect(replacement.after.event.idCbor).toBe(orphan.event.idCbor);
    expect(orphan.placement).toBeNull();
    expect(replacement.before).toBeNull();
  });

  it("reuses immutable identity when the identical transaction is re-included", () => {
    const first = stage([], [admission()]);
    const orphan = reverseHistoryProvenance(first, [first[0]!.after])[0]!;
    const replacement = stage([orphan], [admission()], 200)[0]!;
    expect(replacement.after.id).toBe(orphan.id);
    expect(replacement.before).toEqual(orphan);
    expect(replacement.after.placement!.admission.blockHash).toBe(tx(200));
    expect(
      reverseHistoryProvenance([replacement], [replacement.after]),
    ).toEqual([orphan]);
    expect(() =>
      stage(
        [orphan],
        [
          {
            ...admission(),
            admission: { ...event(), originalAssetsCbor: "06" },
          },
        ],
        201,
      ),
    ).toThrow(/immutable admission facts/);
  });

  it("stages both kinds independently in one transaction", () => {
    const transitions = [admission(), admission(1, "withdrawal")].map(
      (transition) => ({ transactionIndex: 0, transition }),
    );
    const changes = stageHistoryProvenance({
      bindingDigest,
      block: { point: { id: tx(100), slot: 1000, height: 100 } },
      incarnations: [],
      transitions,
    });
    expect(changes).toHaveLength(2);
    expect(changes[0]!.after.id).not.toBe(changes[1]!.after.id);
    const withdrawal = changes[1]!.after;
    const retired = stage([withdrawal], [retirement(1, 2, "withdrawal")])[0]!
      .after;
    expect(retired.placement!.retirement!.reason).toBe("payout_initialized");
  });

  it("refuses unknown origins and stale pointer locations", () => {
    expect(() => stage([], [continuation()])).toThrow(
      /missing canonical origin/,
    );
    expect(() => stage([], [retirement()])).toThrow(/missing canonical origin/);
    const admitted = stage([], [admission()])[0]!.after;
    expect(() => stage([admitted], [continuation(2, 3)])).toThrow(
      /stale Order location/,
    );
    expect(() =>
      stage(
        [admitted],
        [
          {
            ...retirement(1),
            retirement: {
              ...retirement(1).retirement!,
              event: { ...event(), originalAssetsCbor: "06" },
            },
          },
        ],
      ),
    ).toThrow(/immutable admission facts/);
  });

  it("rejects duplicate canonical origins and foreign source bindings", () => {
    const a = stage([], [admission()])[0]!.after;
    const b = stage([], [admission(4)], 200)[0]!.after;
    expect(() => stage([a, b], [])).toThrow(/multiple canonical origins/);
    expect(() => stage([{ ...a, bindingDigest: "cc".repeat(32) }], [])).toThrow(
      /binding disagrees/,
    );
    expect(() => stage([a, a], [])).toThrow(/identity or binding/);
  });

  it("rejects stale rollback poststate and changed immutable before-images atomically", () => {
    const a = stage([], [admission()])[0]!.after;
    const moved = stage([a], [continuation()]);
    expect(() => reverseHistoryProvenance(moved, [a])).toThrow(/poststate/);
    expect(() =>
      reverseHistoryProvenance(
        [
          {
            ...moved[0]!,
            before: { ...a, event: { ...a.event, payloadCbor: "06" } },
          },
        ],
        [moved[0]!.after],
      ),
    ).toThrow(/immutable incarnation/);
    expect(() =>
      reverseHistoryProvenance([...moved, ...moved], [moved[0]!.after]),
    ).toThrow(/poststate/);
    expect(a.placement!.current!.outRef).toEqual(out(1));
  });

  it("copies and freezes caller-owned nested records before publishing a digest", () => {
    const mutableRef = out(1);
    const original = { ...event(), outRef: mutableRef };
    const result = stage([], [{ ...admission(), admission: original }])[0]!
      .after;
    const digest = historyIncarnationDigest(result);
    mutableRef.outputIndex = 5;
    expect(result.event.outRef.outputIndex).toBe(0);
    expect(historyIncarnationDigest(result)).toBe(digest);
    expect(Object.isFrozen(result)).toBe(true);
    expect(Object.isFrozen(result.event.outRef)).toBe(true);
    expect(Object.isFrozen(result.placement!.current!.at)).toBe(true);
  });

  it("rejects duplicate, reversed and inconsistent transaction ordering", () => {
    const base = {
      bindingDigest,
      block: { point: { id: tx(100), slot: 1000, height: 100 } },
      incarnations: [],
    };
    expect(() =>
      stageHistoryProvenance({
        ...base,
        transitions: [0, 0].map((transactionIndex) => ({
          transactionIndex,
          transition: admission(),
        })),
      }),
    ).toThrow(/duplicate kind/);
    expect(() =>
      stageHistoryProvenance({
        ...base,
        transitions: [
          { transactionIndex: 1, transition: admission() },
          { transactionIndex: 0, transition: continuation() },
        ],
      }),
    ).toThrow(/ledger transaction order/);
    expect(() =>
      stageHistoryProvenance({
        ...base,
        transitions: [
          { transactionIndex: 0, transition: admission() },
          { transactionIndex: 0, transition: admission(2, "withdrawal") },
        ],
      }),
    ).toThrow(/different transactions/);
  });
});
