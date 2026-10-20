import {
  type Data,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { Builtin, constT, UPLCConst, type UPLCTerm } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { commitMidgardCekDataTree } from "../src/cek-data-tree.js";
import {
  MIDGARD_CEK_MAX_REVEALED_PAYLOAD_MEMORY,
  MIDGARD_CEK_REVEALED_BYTE_MEMORY,
  MIDGARD_CEK_REVEALED_ITEM_MEMORY,
  midgardCekRevealIsCostly,
} from "../src/cek-executor.reveal-cost.js";
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

  // Each row builds and proves a program over a 9,200-item source: under a
  // second on a developer machine and over 5 s on a loaded CI runner.
  it.each(MAX_SHAPES)(
    "$name over a near-maximum source proves with a witness under 1 KB",
    { timeout: 60_000 },
    ({ term }) => {
      const step = semanticStep(term());
      expect(step.allVerify).toBe(true);
      expect(step.evidence.length).toBeLessThan(1_024);
    },
  );
});

/** Every Data item in `value`, itself included. */
const dataItems = (value: Data): number => {
  const children: Data[] =
    value instanceof DataList
      ? value.list
      : value instanceof DataConstr
        ? value.fields
        : value instanceof DataMap
          ? (value.map as DataPair<Data, Data>[]).flatMap((entry) => [
              entry.fst,
              entry.snd,
            ])
          : [];
  return children.reduce((sum, child) => sum + dataItems(child), 1);
};

/** Whether revealing `value` in full costs more than one payload may. */
const revealIsCostly = (value: Data) => {
  const tree = commitMidgardCekDataTree(value);
  const material = <T>(
    nodes: ReadonlyMap<string, { readonly node: T }>,
    root: Uint8Array,
  ): T => {
    const found = nodes.get(Buffer.from(root).toString("hex"));
    if (found === undefined) throw new Error("missing committed node");
    return found.node;
  };
  const itemLimit =
    (MIDGARD_CEK_MAX_REVEALED_PAYLOAD_MEMORY -
      tree.cborLength * MIDGARD_CEK_REVEALED_BYTE_MEMORY) /
    MIDGARD_CEK_REVEALED_ITEM_MEMORY;
  return {
    itemLimit: Number(itemLimit),
    costly: midgardCekRevealIsCostly(
      {
        node: (root) => material(tree.dataNodes, root),
        link: (root) => material(tree.listNodes, root),
        pairLink: (root) => material(tree.pairNodes, root),
      },
      { kind: "data" },
      tree.root,
    ),
  };
};

const pair = (key: Data, value: Data) => new DataPair(key, value);

/** Short payloads with exactly the item limit, and one item more. */
const ITEM_LIMIT_ROWS: readonly {
  readonly name: string;
  readonly overLimit: boolean;
  readonly value: Data;
}[] = [
  { name: "list at the limit", overLimit: false, value: new DataList(ints(7)) },
  {
    name: "list over the limit",
    overLimit: true,
    value: new DataList(ints(8)),
  },
  {
    name: "map at the limit",
    overLimit: false,
    value: new DataMap([
      pair(new DataI(1n), new DataI(2n)),
      pair(new DataI(3n), new DataI(4n)),
      pair(new DataI(5n), new DataList([new DataI(6n)])),
    ]),
  },
  {
    name: "map over the limit",
    overLimit: true,
    value: new DataMap([
      pair(new DataI(1n), new DataI(2n)),
      pair(new DataI(3n), new DataI(4n)),
      pair(new DataI(5n), new DataI(6n)),
      pair(new DataI(7n), new DataI(8n)),
    ]),
  },
  {
    name: "constr at the limit",
    overLimit: false,
    value: new DataConstr(0n, ints(7)),
  },
  {
    name: "constr over the limit",
    overLimit: true,
    value: new DataConstr(0n, ints(8)),
  },
  {
    name: "list whose last item holds the item at the limit",
    overLimit: false,
    value: new DataList([...ints(5), new DataList([new DataI(1n)])]),
  },
  {
    name: "list whose last item holds the item over the limit",
    overLimit: true,
    value: new DataList([...ints(6), new DataList([new DataI(1n)])]),
  },
];

describe("revealing a short Data payload in full", () => {
  it.each(ITEM_LIMIT_ROWS)(
    "$name is costly exactly when its items exceed the limit",
    ({ overLimit, value }) => {
      const { itemLimit, costly } = revealIsCostly(value);
      expect(dataItems(value)).toBe(overLimit ? itemLimit + 1 : itemLimit);
      expect(costly).toBe(overLimit);
    },
  );
});
