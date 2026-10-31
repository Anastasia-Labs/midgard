import { UPLCConst } from "../../src/uplc-constant.js";
import { DataB } from "../../src/uplc-data.js";
import { type UPLCTerm } from "../../src/uplc-term.js";
import {
  app,
  countdown,
  dataInt,
  discard,
  emptyDataList,
  emptyPairList,
  fix,
  headList,
  ifZero,
  int,
  lam,
  type MidgardCekPreparedProgram,
  mkCons,
  prepareMidgardCekProgram,
  seq,
  sndPair,
  v,
  walkList,
} from "./cek-uplc-terms.js";
import { builtin } from "./uplc-builtins.js";

/**
 * Adversarial CEK program shapes, built at runtime from a size parameter.
 *
 * Every shape is a script `\ctx. [(\_. ()) body]` whose work grows linearly
 * with `n` under the Plutus cost model; the script returns Unit once `body`
 * has run. A structural executor whose per-step work is
 * proportional only to the nodes each step creates runs every shape in time
 * linear in `n`; work proportional to an unchanged subtree shows up as
 * super-linear growth.
 */

const script = (body: UPLCTerm): UPLCTerm => lam(discard(body));

/** Grows a `[Data]` list by one small element per iteration. */
const consGrowth = (n: number): UPLCTerm =>
  countdown(n, emptyDataList(), mkCons(dataInt(7n), v(0)));

/** Grows a pair list by one `(I 7, I 7)` entry per iteration. */
const pairGrowth = (n: number): UPLCTerm =>
  countdown(
    n,
    emptyPairList(),
    mkCons(app(builtin("mkPairData"), dataInt(7n), dataInt(7n)), v(0)),
  );

/** `appendByteString` doubling from one byte to 4,096 bytes. */
const doublingChain = (): UPLCTerm => {
  let term: UPLCTerm = UPLCConst.byteString(
    new DataB(Buffer.from([0x5a])).bytes,
  );
  for (let doubling = 0; doubling < 12; doubling += 1) {
    term = app(lam(app(builtin("appendByteString"), v(0), v(0))), term);
  }
  return term;
};

/**
 * `let x_1 = 1 in ... let x_n = n in body`, where `body` reads the outermost
 * binding three times: each read walks the whole environment.
 */
const deepEnvironment = (n: number): UPLCTerm => {
  const outermost = v(n - 1);
  let term: UPLCTerm = app(
    builtin("addInteger"),
    app(builtin("addInteger"), outermost, outermost),
    outermost,
  );
  for (let index = n; index >= 1; index -= 1) {
    term = app(lam(term), int(index));
  }
  return term;
};

/** Non-tail recursion `f n = n == 0 ? 0 : 1 + f (n - 1)`. */
const deepContinuation = (n: number): UPLCTerm =>
  app(
    fix(
      lam(
        lam(
          ifZero(
            v(0),
            int(0),
            app(
              builtin("addInteger"),
              int(1),
              app(v(1), app(builtin("subtractInteger"), v(0), int(1))),
            ),
          ),
        ),
      ),
    ),
    int(n),
  );

export type MidgardCekAdversarialShape = {
  readonly label: string;
  /** The script body for size `n`; `n` is the shape's growth parameter. */
  readonly term: (n: number) => UPLCTerm;
};

export const MIDGARD_CEK_ADVERSARIAL_SHAPES: readonly MidgardCekAdversarialShape[] =
  [
    {
      label: "mkCons list growth",
      term: (n) => script(consGrowth(n)),
    },
    {
      label: "headList/tailList walk of a grown list",
      term: (n) => script(walkList(consGrowth(n))),
    },
    {
      label: "constrData nesting",
      term: (n) =>
        script(
          countdown(
            n,
            dataInt(0n),
            app(builtin("constrData"), int(0), mkCons(v(0), emptyDataList())),
          ),
        ),
    },
    {
      label: "listData nesting",
      term: (n) =>
        script(
          countdown(
            n,
            dataInt(0n),
            app(builtin("listData"), mkCons(v(0), emptyDataList())),
          ),
        ),
    },
    {
      label: "mapData nesting",
      term: (n) =>
        script(
          countdown(
            n,
            dataInt(0n),
            app(
              builtin("mapData"),
              mkCons(
                app(builtin("mkPairData"), dataInt(0n), v(0)),
                emptyPairList(),
              ),
            ),
          ),
        ),
    },
    {
      label: "constrData nesting unwrapped by unConstrData",
      term: (n) =>
        script(
          countdown(
            n,
            countdown(
              n,
              dataInt(0n),
              app(builtin("constrData"), int(0), mkCons(v(0), emptyDataList())),
            ),
            headList(sndPair(app(builtin("unConstrData"), v(0)))),
          ),
        ),
    },
    {
      label: "listData nesting unwrapped by unListData",
      term: (n) =>
        script(
          countdown(
            n,
            countdown(
              n,
              dataInt(0n),
              app(builtin("listData"), mkCons(v(0), emptyDataList())),
            ),
            headList(app(builtin("unListData"), v(0))),
          ),
        ),
    },
    {
      label: "constrData widening",
      term: (n) =>
        script(
          countdown(
            n,
            emptyDataList(),
            app(
              lam(seq(app(builtin("constrData"), int(0), v(0)), v(0))),
              mkCons(dataInt(7n), v(0)),
            ),
          ),
        ),
    },
    {
      label: "listData widening",
      term: (n) =>
        script(
          countdown(
            n,
            emptyDataList(),
            app(
              lam(seq(app(builtin("listData"), v(0)), v(0))),
              mkCons(dataInt(7n), v(0)),
            ),
          ),
        ),
    },
    {
      label: "mapData widening",
      term: (n) =>
        script(
          app(builtin("unMapData"), app(builtin("mapData"), pairGrowth(n))),
        ),
    },
    {
      label: "appendByteString doubling chains",
      term: (n) => script(countdown(n, int(0), seq(doublingChain(), v(0)))),
    },
    {
      label: "serialiseData of a grown structure",
      term: (n) =>
        script(
          app(
            lam(
              seq(
                app(builtin("serialiseData"), v(0)),
                seq(
                  app(builtin("serialiseData"), v(0)),
                  app(builtin("serialiseData"), v(0)),
                ),
              ),
            ),
            app(builtin("listData"), consGrowth(n)),
          ),
        ),
    },
    {
      label: "deep environment",
      term: (n) => script(deepEnvironment(n)),
    },
    {
      label: "deep continuation stack",
      term: (n) => script(deepContinuation(n)),
    },
  ];

/**
 * `n` per gated shape: the shape at `2n` spends at least 90% of the
 * per-transaction execution budget in memory or CPU. The deep environment is
 * not gated: the test Flat encoder cannot author it near the cap.
 */
export const MIDGARD_CEK_ADVERSARIAL_HALF_CAP_SIZES: Readonly<
  Record<string, number>
> = {
  "mkCons list growth": 2_162,
  "headList/tailList walk of a grown list": 1_086,
  "constrData nesting": 1_938,
  "listData nesting": 2_036,
  "mapData nesting": 1_836,
  "constrData nesting unwrapped by unConstrData": 965,
  "listData nesting unwrapped by unListData": 1_044,
  "constrData widening": 1_594,
  "listData widening": 1_660,
  "mapData widening": 1_938,
  "appendByteString doubling chains": 562,
  "serialiseData of a grown structure": 1_196,
  "deep continuation stack": 2_493,
};

/**
 * The script context every shape receives. No shape contains this constant,
 * so the context's semantic witness never shares a value root with a source
 * constant.
 */
const SHAPE_CONTEXT = new DataB(
  Buffer.from("midgard-cek-adversarial-shape-context"),
);

/** Compiles a shape at size `n` and applies it to a fixed script context. */
export const prepareMidgardCekShape = (
  shape: MidgardCekAdversarialShape,
  n: number,
): MidgardCekPreparedProgram =>
  prepareMidgardCekProgram(shape.term(n), SHAPE_CONTEXT);
