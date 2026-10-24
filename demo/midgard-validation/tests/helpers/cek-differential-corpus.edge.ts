import { constT, UPLCConst } from "../../src/uplc-constant.js";
import { DataB } from "../../src/uplc-data.js";
import { type UPLCTerm } from "../../src/uplc-term.js";
import {
  B,
  BASE_ITEMS,
  bData,
  bigInteger,
  bytes,
  bytesConst,
  C,
  chooseList,
  constrData,
  dataList,
  equalsData,
  I,
  iData,
  L,
  LARGE_LIST,
  letIn,
  listData,
  M,
  mapData,
  type MidgardCekDifferentialCase,
  mkPairData,
  program,
  serialiseData,
  unBData,
  unConstrData,
  unIData,
  unListData,
  unMapData,
} from "./cek-differential-builders.js";
import {
  app,
  data,
  dataInt,
  emptyDataList,
  fstPair,
  headList,
  int,
  mkCons,
  nullList,
  seqAll,
  sndPair,
  tailList,
  v,
  walkList,
} from "./cek-uplc-terms.js";
import { builtin } from "./uplc-builtins.js";

export const edgeDataCases = (): MidgardCekDifferentialCase[] => [
  program(
    "byte strings at the chunking edge",
    seqAll(
      ...[0, 23, 24, 63, 64, 65, 100, 255, 256].map((length) =>
        letIn(
          data(B(length)),
          seqAll(
            unBData(v(0)),
            bData(unBData(v(0))),
            serialiseData(v(0)),
            equalsData(v(0), data(B(length))),
            listData(mkCons(v(0), emptyDataList())),
          ),
        ),
      ),
    ),
  ),
  program(
    "integers at the bignum edge",
    seqAll(
      ...[
        0n,
        23n,
        -24n,
        2n ** 63n,
        2n ** 64n,
        -(2n ** 64n) - 1n,
        bigInteger,
        -bigInteger,
      ].map((value) =>
        letIn(
          iData(int(value)),
          seqAll(
            unIData(v(0)),
            serialiseData(v(0)),
            equalsData(v(0), iData(int(value))),
            listData(mkCons(v(0), emptyDataList())),
            constrData(int(1), mkCons(v(0), emptyDataList())),
          ),
        ),
      ),
    ),
  ),
  ...[0n, 6n, 7n, 127n, 128n, 200n, 2n ** 64n - 1n, 2n ** 64n, bigInteger].map(
    (index) =>
      program(
        `constructor index ${index}`,
        letIn(
          constrData(int(index), mkCons(dataInt(index), emptyDataList())),
          seqAll(
            unConstrData(v(0)),
            fstPair(unConstrData(v(0))),
            serialiseData(v(0)),
            equalsData(v(0), v(0)),
            mkPairData(v(0), v(0)),
          ),
        ),
      ),
  ),
  program(
    "constructor source constants",
    seqAll(
      ...[
        C(0, []),
        C(7, [I(1n)]),
        C(200, [B(3), L([])]),
        C(2n ** 64n - 1n, [I(2n)]),
      ].map((value) =>
        letIn(
          data(value),
          seqAll(
            unConstrData(v(0)),
            sndPair(unConstrData(v(0))),
            serialiseData(v(0)),
          ),
        ),
      ),
    ),
  ),
  program(
    "map with duplicate keys",
    letIn(
      data(
        M([
          [I(1n), B(2)],
          [I(1n), B(3)],
          [B(70), L([I(1n)])],
          [I(1n), M([])],
        ]),
      ),
      seqAll(
        unMapData(v(0)),
        mapData(unMapData(v(0))),
        equalsData(v(0), mapData(unMapData(v(0)))),
        serialiseData(v(0)),
        walkList(unMapData(v(0))),
      ),
    ),
  ),
  program(
    "nested lists",
    letIn(
      data(L([L([L([I(1n)]), L([])]), L([]), M([[L([]), L([B(66)])]])])),
      seqAll(
        unListData(v(0)),
        unListData(headList(unListData(v(0)))),
        unListData(headList(unListData(headList(unListData(v(0)))))),
        listData(tailList(unListData(v(0)))),
        serialiseData(v(0)),
        walkList(unListData(v(0))),
      ),
    ),
  ),
  program(
    "empty lists and nil builtins",
    seqAll(
      app(builtin("mkNilData"), UPLCConst.unit),
      app(builtin("mkNilPairData"), UPLCConst.unit),
      mapData(app(builtin("mkNilPairData"), UPLCConst.unit)),
      listData(app(builtin("mkNilData"), UPLCConst.unit)),
      constrData(int(3), app(builtin("mkNilData"), UPLCConst.unit)),
      nullList(emptyDataList()),
      chooseList(emptyDataList(), int(1), int(2)),
      unListData(data(L([]))),
      unMapData(data(M([]))),
      sndPair(unConstrData(data(C(0, [])))),
    ),
  ),
];

export const typedListCases = (): MidgardCekDifferentialCase[] => {
  const workout = (list: UPLCTerm, item: UPLCTerm): UPLCTerm =>
    letIn(
      mkCons(item, list),
      seqAll(
        headList(v(0)),
        tailList(v(0)),
        nullList(v(0)),
        chooseList(v(0), int(1), int(2)),
        mkCons(headList(v(0)), tailList(v(0))),
        walkList(v(0)),
      ),
    );
  return [
    program(
      "integer list",
      workout(
        UPLCConst.listOf(constT.int)([1n, bigInteger, -bigInteger]),
        int(-5),
      ),
    ),
    program(
      "byte string list",
      workout(
        UPLCConst.listOf(constT.byteStr)([
          new DataB(bytes(70, 2)).bytes,
          new DataB(bytes(0, 0)).bytes,
        ]),
        bytesConst(65, 3),
      ),
    ),
    program(
      "string list",
      workout(
        UPLCConst.listOf(constT.str)(["a", "été"]),
        UPLCConst.str("long ".repeat(20)),
      ),
    ),
    program(
      "boolean list",
      workout(
        mkCons(
          UPLCConst.bool(true),
          mkCons(UPLCConst.bool(false), UPLCConst.listOf(constT.bool)([])),
        ),
        UPLCConst.bool(true),
      ),
    ),
    program(
      "unit list",
      workout(
        mkCons(UPLCConst.unit, UPLCConst.listOf(constT.unit)([])),
        UPLCConst.unit,
      ),
    ),
    program(
      "pair list",
      letIn(
        mkCons(
          UPLCConst.pairOf(constT.int, constT.bool)(7n, true),
          UPLCConst.listOf(constT.pairOf(constT.int, constT.bool))([]),
        ),
        seqAll(
          fstPair(headList(v(0))),
          sndPair(headList(v(0))),
          tailList(v(0)),
          walkList(v(0)),
        ),
      ),
    ),
  ];
};

export const failureCases = (): MidgardCekDifferentialCase[] => [
  program("headList of an empty list", headList(emptyDataList())),
  program("tailList of an empty list", tailList(emptyDataList())),
  program("headList of an integer", headList(int(1))),
  program("unConstrData of an integer", unConstrData(dataInt(1n))),
  program("unConstrData of a large list", unConstrData(v(0)), LARGE_LIST),
  program("unMapData of an integer", unMapData(dataInt(1n))),
  program("unMapData of a large list", unMapData(v(0)), LARGE_LIST),
  program("unListData of a map", unListData(data(M([[I(1n), I(2n)]])))),
  program("unIData of a byte string", unIData(data(B(3)))),
  program("unBData of an integer", unBData(dataInt(1n))),
  program(
    "mkCons of an integer onto a Data list",
    mkCons(int(1), emptyDataList()),
  ),
  program(
    "mkCons of Data onto an integer list",
    mkCons(dataInt(1n), UPLCConst.listOf(constT.int)([1n])),
  ),
  program(
    "mkCons onto a large Data list with a wrong item",
    mkCons(int(1), unListData(v(0))),
    LARGE_LIST,
  ),
  program(
    "mkCons of a byte string onto a near-bound Data list",
    mkCons(bytesConst(200, 9), dataList(BASE_ITEMS)),
  ),
  program(
    "constrData with a negative index",
    constrData(int(-1), emptyDataList()),
  ),
  program(
    "constrData of an integer list",
    constrData(int(0), UPLCConst.listOf(constT.int)([1n])),
  ),
  program("fstPair of an integer", fstPair(int(1))),
  program("chooseList of an integer", chooseList(int(1), int(1), int(2))),
  program("mapData of a Data list", mapData(dataList([I(1n)]))),
  program("listData of an integer", listData(int(1))),
];
