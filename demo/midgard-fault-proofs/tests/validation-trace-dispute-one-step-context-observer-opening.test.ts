/** The observer opening our runner plans for a CEK context step of the
 * observer stage, from the step's context control and its evidence: the
 * absence of observers before any is folded, their finish once every one
 * the transaction requires is, and no opening for an observer item's
 * evidence, which the runner does not drive. */
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { cekContextObserverOpening } from "../src/validation-trace-dispute/one-step-context.js";

/** A context control at the observer stage with `count` observers required
 * and `folded` of them folded. */
const context = (count: bigint, folded: bigint) =>
  new Constr<Data>(0, [
    5n,
    ...Array<Data>(14).fill(0n),
    count,
    new Constr(0, ["", folded, 0n, 0n]),
    ...Array<Data>(7).fill(0n),
  ]);

const NO_EVIDENCE = Data.to(new Constr(0, []));
const EMPTY = Data.to(new Constr(0, []));
const FINISHED = Data.to(new Constr(1, []));

describe("validation-trace one-step CEK context observer opening", () => {
  it("opens no observers before any is folded", () => {
    expect(cekContextObserverOpening(context(0n, 0n), NO_EVIDENCE)).toBe(EMPTY);
  });

  it("finishes the observers once every one required is folded, and not before", () => {
    expect(cekContextObserverOpening(context(2n, 2n), NO_EVIDENCE)).toBe(
      FINISHED,
    );
    expect(cekContextObserverOpening(context(2n, 1n), NO_EVIDENCE)).toBe(EMPTY);
  });

  it("plans no opening for an observer item's evidence, or evidence other than an empty first constructor", () => {
    for (const evidence of [
      new Constr(1, [3n, 0n, new Constr(0, [])]),
      new Constr(1, []),
      new Constr(0, [0n]),
    ])
      expect(
        cekContextObserverOpening(context(1n, 0n), Data.to(evidence)),
      ).toBeUndefined();
  });
});
