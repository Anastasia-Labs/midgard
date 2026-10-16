/**
 * The scripts behind the script-decode step golden vectors, and the walk each
 * one is driven through.
 *
 * Driven from two sides. The generator
 * (`scripts/generate-script-decode-step-v1-goldens.mjs`) walks every row
 * through the built `dist/` port of the step machine and emits the JSON
 * fixture and the Aiken module
 * `onchain/aiken/lib/midgard/script-decode-v1-golden.test.ak`, which replays
 * each walk with `step_with_frames_v1` and checks the hash of every run datum
 * and the record. The vitest suite
 * (`tests/script-decode-step-v1.golden.test.ts`) walks the same rows through
 * `src/` and compares them with the checked-in fixture.
 *
 * The synthetic rows mirror the walks in
 * `onchain/aiken/lib/midgard/script-decode-v1.test.ak`, written with the same
 * Flat writer. The blueprint rows are compiled validators frozen as hex, with
 * the script hash the blueprint gave each.
 *
 * This module imports nothing from the package, so either side can load it.
 */

import {
  app,
  bData,
  builtin,
  caseOf,
  cborData,
  con,
  constr,
  constrData,
  delay,
  err,
  force,
  iData,
  lam,
  listData,
  mapData,
  pow2,
  program,
  repeat,
  varT,
  vBool,
  vBytes,
  vInt,
  vList,
  vPair,
  vUnit,
  vValue,
} from "./script-decode-step-v1.writers.mjs";

// ## The rows

/** The CBOR byte-string wrapper a compiled script carries its Flat in. */
const wrap = (flat) => {
  const n = flat.length;
  const head =
    n < 24
      ? Buffer.from([0x40 + n])
      : n < 256
        ? Buffer.from([0x58, n])
        : Buffer.from([0x59, n >> 8, n & 255]);
  return Buffer.concat([head, flat]);
};

/** `mixed_program` in script-decode-v1.test.ak. */
const mixedProgram = () =>
  program(
    1,
    app(
      lam(
        constr(3, [
          varT(1),
          con([0], vInt(-pow2(900))),
          con(
            [8],
            vBytes(
              cborData(
                listData([
                  constrData(200, [bData(Buffer.alloc(150, 3))]),
                  mapData([[iData(pow2(70)), constrData(0, [])]]),
                ]),
              ),
            ),
          ),
          con(
            [13],
            vValue([
              [
                "01",
                [
                  ["aa", 3n],
                  ["bb", -pow2(100)],
                ],
              ],
            ]),
          ),
          con(
            [7, 7, 6, 2, 7, 5, 1],
            vPair(
              vBytes(Buffer.from("e282ac", "hex")),
              vList([vBytes([0]), vBytes([])]),
            ),
          ),
          caseOf(err(), [delay(builtin(7)), force(err())]),
        ]),
      ),
      con([4], vBool(true)),
    ),
  );

/** A Data constant holding a long, nested list. */
const dataListProgram = () => {
  let nested = listData([iData(1)]);
  for (let depth = 0; depth < 12; depth += 1) {
    nested = listData([nested, bData([depth])]);
  }
  const items = Array.from({ length: 70 }, (_, index) =>
    index % 7 === 0
      ? constrData(index % 3, [iData(index), bData(Buffer.alloc(index, 1))])
      : index % 5 === 0
        ? mapData([[bData([index]), iData(-pow2(index))]])
        : iData(index * 1000),
  );
  return program(0, con([8], vBytes(cborData(listData([...items, nested])))));
};

const deepProgram = () => {
  let body = app(err(), err());
  for (let index = 0; index < 60; index += 1) body = lam(delay(body));
  return program(0, body);
};

/**
 * Compiled validators from the blueprint, frozen as hex: the title, the script
 * hash the blueprint gave it, and its compiled code.
 */
export const SCRIPT_DECODE_BLUEPRINT_SCRIPTS = [
  [
    "operator_directory/registered_operators.spend.spend",
    "72f05b6a9f1d7ba25460687a553a0afc150f34df970cb884290bfd1c",
    "58c3010100229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264646644b30013370e900118039baa00189919914c005660026464660020026eacc03cc040c040c040c040c034dd51807804912cc004006007132325980099b9100d0018acc004cdc7806800c4dd59808001401500e44cc010010c04c00d00e1bae300e0013010001403c297adef6c608a518a5040314a14a28048c02c004c02cc030004c020dd5000c59006180400098041804800980400098021baa0088a4d13656400801",
  ],
  [
    "hub_oracle.mint.mint",
    "f223fe1533983f82d51020e8f0c17f1635696b9207b290abbcacfae2",
    "5901da0101002229800aba2aba1aba0aab9faab9eaab9dab9a9bae0024888888896600264653001300900198049805000cc0240092225980099b8748000c020dd500144ca60024446466446600400400244b3001001801c4c8c96600266e440180062b30013371e00c0031375a60240050054041133004004301600340406eb8c040004c04c005011198028020018a40012300e300f300f300f300f0019bae300d300a3754006911192cc004c8c8cdd79ba633007002004374c6602466ec130118574d4944474152445f434f5252454354494f4e5f4c4f434b0037500026602466ec0dd48069ba80014bd6f7b6304c00400600700c40146eacc00cc038dd5003456600266e1d20020018acc004cdc3a4000601a6ea8c8cc004004dd6180918079baa0072259800800c530103d87a80008992cc004cdd7980a18089baa001018899ba548000cc04c0052f5c11330030033015002403c6026002808a29462c80622b30013370e9000800c528c5900c20188b201898009bab3002300d375400b001805200611191919800800802112cc004006007132325980099b910060018acc004cdc7803000c4dd59808001401500e44cc010010c05000d00e1bae300e0013011001403c297adef6c608b200e180480098021baa0098a4d1365640081",
  ],
  [
    "user_events/witness.main.publish",
    "102ec1432932e121cdd24976443b9c7f067130abf99e8cbce871b280",
    "5902bc010100229800aba2aba1aab9faab9eaab9dab9a9bae002488888896600264653001300800198041804800cc0200092225980099b8748018c020dd500146600260126ea800a6e1d20029b874800260106ea800d222232332259800980280244c8c966002602a0050048b2026375c602600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4c04c004c040dd5005456600266e1d2004004899192cc004c05400a00916404c6eb4c04c004c040dd500545900e201c40382653001300100198071baa0099180918099809980998099809800a444b3001300700289919912cc004c028006260160051598009805800c4cdc3a400200514a0809901319199119801001000912cc004006007132325980099b910150018acc004cdc780a800c4dd6980c001401501644cc010010c06c00d0161bae30160013018001405c6464660020026eacc060c064c064c064c064c058dd5007112cc004006007132325980099b910070018acc004cdc7803800c4dd5980c801401501744cc010010c07000d0171bae301700130190014060297adef6c60148000c048dd50031bae301430123754019159800980400144c8c966002003168992cc004cdd78008044566002601460286ea8006264b3001300c3015375400315980099baf3018301637540046030602c6ea800629462c80a22c80a0c05c00a2c809a2c8098c058009015180b000998019bac3001301237540146eb4c050c048dd50064566002601060226ea8016264b30010018b44c966002601260266ea80062b30013375e602c60286ea8004c058c050dd5003c528c59012459012180a800a028330033758600260246ea8028dd6980a18091baa00c8b202040408080444b30013371200290004400a2b30010028a5eb8233001003980a0014cdc0240020028019012201e375a602000a601e60200088b200e180400098021baa0088a4d13656400801",
  ],
  [
    "fraud_proofs/validation_trace/award_v1.main.spend",
    "6a28df0d39ffebcd9d80640e81c0e3839d84d33bfb6af7f3a0657422",
    "59064f01010022229800aba2aba1aba0aab9faab9eaab9dab9a9bae0049bae00348888888896600264653001300a00198051805800cdc3a4005300a0024888966002600460146ea800e2653001300f00198079808000cdc3a400091119912cc004c00c006264646644b300130190038034590161bad3016001375a602c004602c00260226ea802a2b300130080018992cc004c058006264b3001300530123754003132323298009bad30190019bad30190039bad3019002488966002603a00900a8b2034180c800980c00098099baa0018b202230150018b20263011375401516403c8078660026e9520009b914881009b8f4881004888cc88cc8a600244464b3001300c301937540031325980099baf301a0010048980d800c590191919bb0301e001301e301f0013758603a60346ea80062c80c16600266e2000520008a60103d87a8000899802801800a02e912cc004cdc4000a400114c103d87a8000899801801000a02c9192cc004c07000626464b3001301e0018991980e980d0019980e980d0009980e980d800a5eb80c0740062c80d8dd5980c800980d800c590191919800800801112cc004006297adef6c6089919912cc004c03000a2b3001300b0028800c401101a44cc07800ccc01401400501a1bae3019001301e002301c00140689112cc004c02cc060dd500944c8cc88c96600266e1d2004301c3754003132598009811000c4c9660026022603c6ea8006264b30013375e604600202915980099baf374e60106eacc08cc090c080dd5181198120009ba733022375203a660446046006660449810101004bd7045660026024603e6ea804e2b30013232330010013758604a01044b30010018a508acc004cdc79bae30260010038a518998010011813800a04240906eb8c08cc080dd5181198101baa0138a518b203c8b203c8b203c8b203c301f37546044603e6ea80062c80e8cc01cdd618108029bad30210038b203e301d375400316406d30013756603e6040005300d3301e375203297ae09bad301f30200014018603c603c603c603c603c603c603c603c00260346ea804cc064dd5007c4cc88cc8966002601e60386ea8042264b30014a3132598009808980f1baa001899912cc004c04cc080dd5000c56600266ebc054c090c084dd500144c8cc8966002603660466ea800a2646464b30013371e6eb8c0a80080922b3001301e375a6054605600315980099b89300a003300a375660546056604e6ea801233001302737546054604e6ea806a6eb8c0a8c09cdd5002cdd71815000cc09cdd500224444646464b30013370e900218169baa0018acc004c094c0b4dd518189819001456600266ebcc0c40100d22b30013375e6e9cc058dd598188019ba7330303752054660606ea4014cc0c130010101004bd70456600266ebcc0c4c0b8dd5000980f998181818803a5eb82264b30013021302e37540031323259800981a800c4c96600266e3cdd7181a801804c4c966002604a60646ea8006260546eb4c0d8c0ccdd5181b18199baa0018b20623035303600b8b2060375a60680031640c86068002605e6ea80062c816a60026eacc0c4c0c8c0c8c0c8c0c8c0c8c0c8c0c804a603e660606ea40a92f5c1375a6062606402680c22c81622c81622c81622c81622c8160c0c0004c0c0004c0bc0051640951640951640946054002601a0026eacc09cc0a000e2c8110c094c088dd51812800981298111baa002302137546048604a60426ea800a2c80fa2c80f8c088c07cdd5000998041bac3022003375a60440091640746600e6eb0c084010dd69810802c5901c119199119801001000912cc00400600713232598009809800c56600260240031375a60460050054085133004004302700340846eb8c084004c090005022191919800800802112cc0040060071323259800980a000c56600260260031375660480050054089133004004302800340886eb8c088004c0940050230a5eb7bdb180520008b2036301f002301f301f00130193754603860326ea8048c064dd5007a02e180100118008009112cc00400a298103d87a80008992cc004c02000a2600e660306e9ccc060c054004cc060c0580052f5c097ae08cc004012603400733700900080120084050603000480b08896600200514c103d87a80008acc004c01c0062600c6602e603000497ae08cc00400e6032005337009000800a006404c80b0601c6ea802060166ea800e2c80486014002600a6ea802a29344d95900301",
  ],
];

/**
 * Constant lists of each element type, long enough that their elements are
 * decoded many to a tick, with elements the bulk path leaves to one-node
 * ticks mixed in.
 */
const bulkPrograms = () => {
  const range = (n) => Array.from({ length: n }, (_, index) => index);
  const utf8 = (text) => Buffer.from(text, "utf8");
  return [
    [
      "bulk list bool",
      program(
        0,
        con([7, 5, 4], vList(range(300).map((i) => vBool(i % 3 === 0)))),
      ),
    ],
    [
      "bulk list unit",
      program(0, con([7, 5, 3], vList(range(400).map(vUnit)))),
    ],
    [
      "bulk list integer",
      program(
        0,
        con(
          [7, 5, 0],
          vList(
            range(160).map((i) =>
              vInt(i % 40 === 17 ? -pow2(150) : i % 2 === 0 ? i * 977 : -i),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk list bytestring",
      program(
        0,
        con(
          [7, 5, 1],
          vList(
            range(90).map((i) =>
              vBytes(Buffer.alloc(i === 45 ? 300 : i % 9, i)),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk list string",
      program(
        0,
        con(
          [7, 5, 2],
          vList(
            range(70).map((i) =>
              vBytes(utf8(i % 5 === 0 ? "" : `e${i}\u20ac`)),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk list of a unit pair chain",
      program(
        0,
        con(
          [7, 5, 7, 7, 6, 3, 7, 7, 6, 3, 7, 7, 6, 4, 0],
          vList(
            range(60).map((i) =>
              vPair(
                vUnit(),
                vPair(vUnit(), vPair(vBool(i % 2 === 1), vInt(i))),
              ),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk list of pairs with a unit second",
      program(
        0,
        con(
          [7, 5, 7, 7, 6, 0, 3],
          vList(range(150).map((i) => vPair(vInt(i * 31 - 900), vUnit()))),
        ),
      ),
    ],
    [
      "bulk nested lists",
      program(
        0,
        con(
          [7, 5, 7, 5, 4],
          vList(
            range(50).map((i) =>
              vList(range(i % 7).map((j) => vBool(j % 2 === 0))),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk list Data",
      program(
        0,
        con([7, 5, 8], vList(range(12).map((i) => vBytes(cborData(iData(i)))))),
      ),
    ],
    [
      "bulk list string refused mid-list",
      program(
        0,
        con(
          [7, 5, 2],
          vList(
            range(80).map((i) =>
              vBytes(i === 61 ? Buffer.from([0xc3, 0x28]) : utf8(`s${i}`)),
            ),
          ),
        ),
      ),
    ],
    [
      "bulk list unit truncated",
      program(0, con([7, 5, 3], vList(range(500).map(vUnit)))).subarray(0, 20),
    ],
  ];
};

/**
 * One script per refusal class 1-17 (classes 5 and 11 are the phase-2 classes
 * of a `Decoded` record), plus scripts whose record turns on a single rule:
 * the scope depth inside a constr, a padding or filler offset, a builtin
 * offset, a version. Each is `[label, script bytes, fuel]`.
 */
const refusalScripts = () => {
  const flat = (raw) => Buffer.from(raw, "hex");
  return [
    ["class 1, a CBOR integer", Buffer.from("01", "hex"), 1],
    [
      "class 2, a byte after the CBOR byte string",
      Buffer.concat([wrap(program(1, con([3], vUnit()))), Buffer.from([0])]),
      1,
    ],
    ["class 3, no final pad", wrap(flat("0101003766980103c2410100")), 1],
    ["class 4, a final pad of 03", wrap(flat("0101003766980103c241010003")), 1],
    [
      "class 4, a byte-string filler ending in an early 1 bit",
      wrap(flat("010100488401aa0001")),
      1,
    ],
    ["class 4, a final pad ending inside a byte", wrap(flat("0101007430")), 1],
    ["phase-2 class 5, version 1.2.0", wrap(flat("01020061")), 1],
    ["class 6, term tag 10", wrap(flat("010100a1")), 1],
    ["class 7, constr in 1.0.0", wrap(flat("0100008001")), 1],
    ["class 8, builtin tag 101", wrap(flat("0101007ca1")), 1],
    ["class 8, builtin tag 106", wrap(flat("0101007d41")), 1],
    [
      "class 9, a constr of 1,025 fields",
      wrap(program(1, constr(0, repeat(1025, builtin(1))))),
      64,
    ],
    [
      "class 10, a constr tag over 64 bits",
      wrap(flat("0101008ffffffffffffffffff021")),
      1,
    ],
    ["phase-2 class 11, variable index 0", wrap(flat("010100200001")), 1],
    [
      "a variable bound by a lambda inside a constr",
      wrap(flat("01010080080001")),
      1,
    ],
    ["class 12, type tag 14", wrap(flat("0101004f01")), 1],
    [
      "class 13, a constant type of 33 tags",
      wrap(flat("0101004bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5801")),
      1,
    ],
    [
      "class 14, a BLS12-381 G1 constant",
      wrap(flat(`0101004c8130${"00".repeat(48)}0001`)),
      1,
    ],
    ["class 15, a string of ff", wrap(flat("010100490101ff0001")), 1],
    ["class 16, a Data constant of ff", wrap(flat("0101004c0101ff0001")), 1],
    [
      "class 17, a Value with a zero quantity",
      wrap(flat("0101004ea10081000001")),
      1,
    ],
  ];
};

const PLUTUS_V3 = 3;
const MIDGARD_V1 = 128;

/**
 * Every row: a label, the script language, the script bytes, and the fuel of
 * each Walk.
 */
export const scriptDecodeStepScripts = () => [
  ["mixed program", PLUTUS_V3, wrap(mixedProgram()), 3],
  [
    "dense constr",
    MIDGARD_V1,
    wrap(program(1, constr(0, repeat(100, builtin(1))))),
    7,
  ],
  ["deep binders", PLUTUS_V3, wrap(deepProgram()), 5],
  ["Data list", PLUTUS_V3, wrap(dataListProgram()), 4],
  [
    "refused value",
    PLUTUS_V3,
    wrap(program(0, con([13], vValue([["01", [["", 0n]]]])))),
    1,
  ],
  [
    "Data with a trailing byte string",
    PLUTUS_V3,
    wrap(Buffer.from("0101004c01020000000001", "hex")),
    1,
  ],
  [
    "Data with a trailing break",
    PLUTUS_V3,
    wrap(Buffer.from("0101004c010200ff0001", "hex")),
    1,
  ],
  ...bulkPrograms().flatMap(([label, flat]) => [
    [label, PLUTUS_V3, wrap(flat), 1],
    [`${label}, 3 ticks a Walk`, PLUTUS_V3, wrap(flat), 3],
  ]),
  ...refusalScripts().map(([label, script, fuel]) => [
    label,
    PLUTUS_V3,
    script,
    fuel,
  ]),
  ...SCRIPT_DECODE_BLUEPRINT_SCRIPTS.map(([title, , code]) => [
    title,
    PLUTUS_V3,
    Buffer.from(code, "hex"),
    64,
  ]),
];

const hex = (bytes) => Buffer.from(bytes).toString("hex");

const recordView = (record) =>
  record.k === "decoded"
    ? {
        k: record.k,
        programVersion: record.programVersion,
        phase2Class: record.phase2Class,
        termRoot: hex(record.termRoot),
      }
    : { k: record.k, class: record.class, bitOffset: record.bitOffset };

/**
 * Walks every row with the given port of the step machine. Each Walk reveals
 * exactly the frames it pops; a row records how many, and the hash of the run
 * datum the Walk writes.
 */
export const buildScriptDecodeStepRows = ({
  planWalk,
  runData,
  recordData,
  serialise,
  blake2b256,
}) =>
  scriptDecodeStepScripts().map(([label, language, script, fuel]) => {
    const walk = planWalk(language, script, fuel);
    const runHash = (run) => hex(blake2b256(serialise(runData(run))));
    return {
      label,
      language,
      fuel,
      script: hex(script),
      scriptHash: hex(walk.run.scriptHash),
      initialRunHash: runHash(walk.run),
      steps: walk.steps.map((step) => ({
        visible: step.reveals.length,
        runHash: runHash(step.after),
      })),
      record: recordView(walk.record),
      recordHash: hex(blake2b256(serialise(recordData(walk.record)))),
    };
  });
