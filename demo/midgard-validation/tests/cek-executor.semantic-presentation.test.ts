import { DataConstr, DataI, DataList } from "@harmoniclabs/plutus-data";
import { Builtin, constT, UPLCConst, type UPLCTerm } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  apply,
  data,
  forced,
  ints,
  listData,
  pairData,
  semanticStep,
  SHAPES,
  unit,
} from "./cek-executor.semantic-presentation.shapes.js";
import VECTORS from "./fixtures/cek-builtin-step-evidence-vectors.json" with { type: "json" };

// A semantic builtin step (tags 29 to 50 except mapData and unMapData)
// presents each Data argument, each list or pair argument whose memory the
// verifier derives from its payload nodes, and each Data, list or pair result
// by its semantic summary. The summary has the value's root and memory, so
// the machine states are those of the revealed step; only the witness
// shrinks, and a near-maximum source no longer has to be revealed.

/** The source-list length at the direct payload bound (9,215 bytes). */
const MAX_ITEMS = 9_200;

/** Near-maximum sources; the revealed witness of each is over 9 KB. */
const MAX_SHAPES: readonly {
  readonly name: string;
  readonly pinned?: string;
  readonly term: () => UPLCTerm;
}[] = [
  {
    name: "sndPair",
    pinned: "max_snd_pair_9200",
    term: () =>
      apply(
        forced(Builtin.sndPair, 2),
        pairData(new DataI(0n), new DataList(ints(MAX_ITEMS))),
      ),
  },
  {
    name: "headList",
    term: () => apply(forced(Builtin.headList, 1), listData(ints(MAX_ITEMS))),
  },
  {
    name: "tailList",
    term: () => apply(forced(Builtin.tailList, 1), listData(ints(MAX_ITEMS))),
  },
  {
    name: "tailList over integers",
    term: () =>
      apply(
        forced(Builtin.tailList, 1),
        UPLCConst.listOf(constT.int)(
          ints(MAX_ITEMS).map((item) => item.int) as never,
        ),
      ),
  },
  {
    name: "mkCons",
    pinned: "max_mk_cons_data_9200",
    term: () =>
      apply(
        forced(Builtin.mkCons, 1),
        data(new DataI(0n)),
        listData(ints(MAX_ITEMS)),
      ),
  },
  {
    name: "chooseData",
    term: () =>
      apply(
        forced(Builtin.chooseData, 1),
        data(new DataList(ints(MAX_ITEMS))),
        unit,
        unit,
        unit,
        unit,
        unit,
      ),
  },
  {
    name: "unConstrData",
    pinned: "max_un_constr_data_9200",
    term: () =>
      apply(Builtin.unConstrData, data(new DataConstr(0n, ints(MAX_ITEMS)))),
  },
  {
    name: "equalsData",
    term: () =>
      apply(
        Builtin.equalsData,
        data(new DataList(ints(MAX_ITEMS))),
        data(new DataI(0n)),
      ),
  },
  {
    name: "mkPairData",
    term: () =>
      apply(
        Builtin.mkPairData,
        data(new DataList(ints(MAX_ITEMS))),
        data(new DataI(0n)),
      ),
  },
  {
    name: "listData",
    term: () => apply(Builtin.listData, listData(ints(MAX_ITEMS))),
  },
];

/**
 * The pinned evidence, by label. The vector file is the one source: the Aiken
 * presentation module imports the same bytes from the module generated from
 * it, proves each step and refuses each presented value moved by one word.
 */
const pinnedEvidence = (): ReadonlyMap<string, string> =>
  new Map(
    VECTORS.semanticPresentation.map(({ name, evidence }) => [name, evidence]),
  );

const PINNED = [...SHAPES, ...MAX_SHAPES].flatMap(({ pinned, term }) =>
  pinned === undefined ? [] : [{ pinned, term }],
);

describe("semantic builtin steps present Data, list and pair values by summary", () => {
  it.each(SHAPES)(
    "$name presents its arguments and result by kind and proves",
    (shape) => {
      const step = semanticStep(shape.term());
      expect(step.allVerify).toBe(true);
      expect(step.witness.arguments.map((argument) => argument.kind)).toEqual(
        shape.arguments,
      );
      expect(
        step.witness.kind === "executeBuiltinSemantic"
          ? step.witness.result.kind
          : null,
      ).toBe(shape.result);
    },
  );

  it("pins the steps the executor emits in the vector file", () => {
    const pinned = pinnedEvidence();
    expect([...pinned.keys()].sort()).toEqual(
      PINNED.map((row) => row.pinned).sort(),
    );
    for (const { pinned: label, term } of PINNED) {
      expect(
        Buffer.from(semanticStep(term()).evidence).toString("hex"),
        label,
      ).toBe(pinned.get(label));
    }
  }, 60_000);

  it.each(MAX_SHAPES)(
    "$name over a near-maximum source proves with a witness under 1 KB",
    ({ term }) => {
      const step = semanticStep(term());
      expect(step.allVerify).toBe(true);
      expect(step.evidence.length).toBeLessThan(1_024);
    },
  );
});
