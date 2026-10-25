import {
  decodeMidgardTxOutput,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { Builtin, ErrorUPLC, UPLCConst } from "@harmoniclabs/uplc";

import { MidgardRedeemerTag } from "../src/index.js";
import { app, lam, v } from "./helpers/cek-uplc-terms.js";
import { MIDGARD_TX_EX_UNITS_CAP } from "./helpers/plutus-spend-input.js";
import {
  plutusProgram,
  type ProgramSpend,
} from "./phase-b.indeterminate.fixture.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

/**
 * Plutus spends whose redeemer, or whose spent output's datum, is Data nested
 * thousands of levels deep, in the canonical encoding a Midgard transaction
 * carries. Cardano evaluates every such script; so must Phase B, whichever
 * thread and stack it runs on.
 */

export type DeepShape = "list" | "constr" | "map";

/** Canonical Plutus Data `depth` levels deep, around an empty innermost value. */
export const deepCanonicalData = (shape: DeepShape, depth: number): Buffer => {
  switch (shape) {
    case "list":
      return Buffer.concat([
        Buffer.alloc(depth, 0x9f),
        Buffer.from([0x80]),
        Buffer.alloc(depth, 0xff),
      ]);
    case "constr":
      return Buffer.concat([
        Buffer.from("d8799f".repeat(depth), "hex"),
        Buffer.from("d87980", "hex"),
        Buffer.alloc(depth, 0xff),
      ]);
    case "map":
      return Buffer.concat([
        Buffer.from("a100".repeat(depth), "hex"),
        Buffer.from([0x00]),
      ]);
  }
};

/**
 * The same nesting with definite arrays, which Cardano accepts as Data but a
 * Midgard transaction never carries: its Data must be canonical.
 */
export const deepDefiniteArrayData = (depth: number): Buffer =>
  Buffer.concat([Buffer.alloc(depth, 0x81), Buffer.from([0x80])]);

/** Compares the whole script context with itself, then returns unit. */
export const walkingProgram = () =>
  plutusProgram(app(lam(UPLCConst.unit), app(Builtin.equalsData, v(0), v(0))));

/** Compares the whole script context with itself, then fails. */
export const walkingFailingProgram = () =>
  plutusProgram(app(lam(new ErrorUPLC()), app(Builtin.equalsData, v(0), v(0))));

type Program = ReturnType<typeof walkingProgram>;

const spendRedeemers = (data: Buffer) =>
  makeRedeemersCbor([
    {
      tag: MidgardRedeemerTag.Spend,
      index: 0n,
      data,
      exUnits: [MIDGARD_TX_EX_UNITS_CAP.memory, MIDGARD_TX_EX_UNITS_CAP.steps],
    },
  ]);

/** The deepest `shape` whose single Spend redeemer fits the redeemers bound. */
export const deepestRedeemerDepth = (shape: DeepShape): number => {
  const fits = (depth: number) =>
    spendRedeemers(deepCanonicalData(shape, depth)).length <=
    MIDGARD_CONSENSUS_LIMITS.maxRedeemersPreimageBytes;
  const perLevel =
    deepCanonicalData(shape, 1).length - deepCanonicalData(shape, 0).length;
  let depth = Math.floor(
    (MIDGARD_CONSENSUS_LIMITS.maxRedeemersPreimageBytes -
      spendRedeemers(deepCanonicalData(shape, 0)).length) /
      perLevel,
  );
  while (!fits(depth)) depth -= 1;
  if (fits(depth + 1))
    throw new Error(`${shape} redeemer depth ${depth} is not the deepest`);
  return depth;
};

const emptyConstr = Buffer.from("d87980", "hex");

/** The spent output, locked by `scriptHash`, carrying `datum` when given. */
const lockedOutput = (scriptHash: string, datum?: Buffer): Buffer => {
  const output = makeProtectedScriptOutput(scriptHash, FUNDED_OUTPUT_LOVELACE);
  return datum === undefined
    ? output
    : encodeMidgardTxOutput({
        ...decodeMidgardTxOutput(output),
        datum: { kind: "inline", cbor: datum },
      });
};

/** The hash of the script `program` deploys as. */
export const programScriptHash = (program: Program): string =>
  hashScriptWitness(plutusV3ScriptWitness(program.scriptBytes));

/**
 * A spend of one output locked by `program`, with `redeemer` as its Spend
 * redeemer and `datum` as the spent output's inline datum; or a spend of the
 * ledger entry `spent`, already locked by `program`.
 */
export const deepSpend = (
  seed: number,
  program: Program,
  {
    redeemer = emptyConstr,
    datum,
    spent,
  }: {
    redeemer?: Buffer;
    datum?: Buffer;
    spent?: Readonly<{ outRef: Buffer; output: Buffer }>;
  },
): ProgramSpend => {
  const script = plutusV3ScriptWitness(program.scriptBytes);
  const locked = spent?.outRef ?? outRefFromByte(seed);
  return {
    candidate: makePhaseBCandidate({
      spent: [locked],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: spendRedeemers(redeemer),
      scriptLanguages: ["PlutusV3"],
    }),
    preState: new Map([
      [
        locked.toString("hex"),
        spent?.output ?? lockedOutput(hashScriptWitness(script), datum),
      ],
    ]),
  };
};

/** The deepest list datum a spent ledger output can carry. */
export const deepestSpentDatumDepth = (): number => {
  const scriptHash = programScriptHash(walkingProgram());
  const fits = (depth: number) =>
    lockedOutput(scriptHash, deepCanonicalData("list", depth)).length <=
    MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes;
  let depth = Math.floor(
    MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes / 2,
  );
  while (!fits(depth)) depth -= 1;
  return depth;
};

export type DeepRow = Readonly<{
  label: string;
  /** Accepted by its script once the whole context is walked. */
  accepting: (seed: number) => ProgramSpend;
  /** Fails on its own once the whole context is walked. */
  failing: (seed: number) => ProgramSpend;
}>;

const redeemerRow = (shape: DeepShape, depth: number): DeepRow => {
  const redeemer = deepCanonicalData(shape, depth);
  return {
    label: `a ${shape} redeemer ${depth.toString()} deep`,
    accepting: (seed) => deepSpend(seed, walkingProgram(), { redeemer }),
    failing: (seed) => deepSpend(seed, walkingFailingProgram(), { redeemer }),
  };
};

/**
 * Each shape past the depth that exhausted the evaluator's stack, and the
 * deepest redeemer and spent datum a transaction can carry.
 */
export const deepRows = (): readonly DeepRow[] => {
  const datumDepth = deepestSpentDatumDepth();
  const datum = deepCanonicalData("list", datumDepth);
  return [
    redeemerRow("list", 4_000),
    redeemerRow("constr", 3_000),
    redeemerRow("map", 4_000),
    redeemerRow("list", deepestRedeemerDepth("list")),
    redeemerRow("constr", deepestRedeemerDepth("constr")),
    redeemerRow("map", deepestRedeemerDepth("map")),
    {
      label: `a list datum ${datumDepth.toString()} deep on the spent output`,
      accepting: (seed) => deepSpend(seed, walkingProgram(), { datum }),
      failing: (seed) => deepSpend(seed, walkingFailingProgram(), { datum }),
    },
  ];
};

/** A shallow spend through the same walking script. */
export const shallowSpend = (seed: number): ProgramSpend =>
  deepSpend(seed, walkingProgram(), {});
