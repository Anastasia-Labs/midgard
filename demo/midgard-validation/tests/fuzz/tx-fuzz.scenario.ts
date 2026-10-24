import { encodeCbor, MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";

import { encodeMidgardCekPlutusData } from "../../src/cek-constant.js";
import { encodeMidgardCekCardanoFlatProgram } from "../../src/cek-program.cardano-flat.js";
import {
  MidgardRedeemerTag,
  validationMachineLedgerRoot,
} from "../../src/index.js";
import { UPLCConst } from "../../src/uplc-constant.js";
import { type Data } from "../../src/uplc-data.js";
import { UPLCProgram, type UPLCTerm } from "../../src/uplc-term.js";
import {
  makeRng,
  randomData,
  type Rng,
} from "../fixtures/cek-builtin-cardano-v1.arguments.mjs";
import { app, lam, seq, v } from "../helpers/cek-uplc-terms.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
  TEST_SIGNER_HASH,
} from "../validation-fixtures.js";
import type { ValidationMachineEventReplayInput } from "../validation-machine-event-replay.trace.js";
import {
  cekStepFuzzProgram,
  FUZZ_PRIMARY_TAGS,
  toData,
} from "./cek-step-fuzz.programs.js";

/**
 * Seeded whole transactions for the transaction differential: one PlutusV3
 * spend whose script is a step-differential program, over generated ledger
 * fields (extra pub-key inputs, a reference input, outputs, the validity
 * interval, required signers, the redeemer and its declared ex-units). The
 * script either returns unit after running the program, or returns the
 * program's own value.
 */

export type TxFuzzScenario = {
  readonly label: string;
  /** The script as `(program 1.1.0 term)` Cardano Flat. */
  readonly scriptFlat: Buffer;
  /** The declared ex-units of the spend redeemer. */
  readonly exUnits: { readonly memory: bigint; readonly steps: bigint };
  /** Builtin tags the script names, for the reference leg's known classes. */
  readonly primaryTag: number;
  readonly returnsUnit: boolean;
  readonly redeemer: Data;
  readonly event: ValidationMachineEventReplayInput;
};

/** The block slot every scenario is validated at. */
const BLOCK_SLOT = 100n;
const NO_BOUND = undefined;

/**
 * The declared budgets a scenario picks from: the reference evaluator's own
 * default budget (the Cardano per-transaction maximum), a mid-size budget and
 * a tight one that ordinary programs exceed.
 */
const EX_UNIT_CHOICES = [
  { memory: 14_000_000n, steps: 10_000_000_000n },
  { memory: 4_000_000n, steps: 1_000_000_000n },
  { memory: 20_000n, steps: 8_000_000n },
] as const;

const VALIDITY_STARTS = [NO_BOUND, 50n, BLOCK_SLOT, BLOCK_SLOT + 1n] as const;
const VALIDITY_ENDS = [NO_BOUND, BLOCK_SLOT, BLOCK_SLOT + 1n, 200n] as const;

const pickExUnits = (rng: Rng) =>
  rng.chance(0.7) ? EX_UNIT_CHOICES[0] : rng.pick([...EX_UNIT_CHOICES]);

/** The script: the program applied to the context, then unit, or bare. */
const scriptTerm = (program: UPLCTerm, returnsUnit: boolean): UPLCTerm =>
  returnsUnit ? lam(seq(app(program, v(0)), UPLCConst.unit)) : program;

const requiredSigners = (rng: Rng): readonly Buffer[] => {
  switch (rng.int(4)) {
    case 0:
      return [Buffer.from(TEST_SIGNER_HASH, "hex")];
    case 1:
      // A key hash nobody signs for.
      return [Buffer.alloc(28, 0x5a)];
    default:
      return [];
  }
};

/** The scenario of `seed`. */
export const txFuzzScenario = async (seed: number): Promise<TxFuzzScenario> => {
  const rng = makeRng(seed);
  const primaryTag = rng.pick(FUZZ_PRIMARY_TAGS);
  const program = cekStepFuzzProgram(seed, primaryTag);
  const returnsUnit = rng.chance(0.85);
  const scriptFlat = encodeMidgardCekCardanoFlatProgram(
    new UPLCProgram([1, 1, 0], scriptTerm(program.term, returnsUnit)),
  );
  const script = plutusV3ScriptWitness(encodeCbor(scriptFlat));
  const spent = outRefFromByte(0x1d);
  const spentOutput = makeProtectedScriptOutput(
    hashScriptWitness(script),
    FUNDED_OUTPUT_LOVELACE,
  );
  const extraInputs = Array.from({ length: rng.int(3) }, (_, index) => ({
    outRef: outRefFromByte(0x30 + index),
    output: makeOutput(FUNDED_OUTPUT_LOVELACE),
  }));
  const reference = rng.chance(0.3)
    ? [{ outRef: outRefFromByte(0x50), output: makeOutput(3_000_000n) }]
    : [];
  const total = FUNDED_OUTPUT_LOVELACE * BigInt(1 + extraInputs.length);
  const outputCount = 1 + rng.int(3);
  const share = total / BigInt(outputCount);
  const outputs = Array.from({ length: outputCount }, (_, index) =>
    makeOutput(index === 0 ? total - share * BigInt(outputCount - 1) : share),
  );
  const exUnits = pickExUnits(rng);
  const redeemer: Data = toData(randomData(rng, 2));
  const validityIntervalStart = rng.pick([...VALIDITY_STARTS]);
  const validityIntervalEnd = rng.pick([...VALIDITY_ENDS]);
  const transaction = makeNativeTx({
    spendInputs: [spent, ...extraInputs.map(({ outRef }) => outRef)],
    referenceInputs: reference.map(({ outRef }) => outRef),
    outputs,
    scriptWitnesses: [script],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      {
        tag: MidgardRedeemerTag.Spend,
        index: 0n,
        data: encodeMidgardCekPlutusData(redeemer),
        exUnits: [exUnits.memory, exUnits.steps],
      },
    ]),
    scriptLanguages: ["PlutusV3"],
    requiredSignerItems: requiredSigners(rng),
    ...(validityIntervalStart === NO_BOUND ? {} : { validityIntervalStart }),
    ...(validityIntervalEnd === NO_BOUND ? {} : { validityIntervalEnd }),
  });
  const ledgerWitnessEntries = [
    { outRef: spent, output: spentOutput },
    ...extraInputs,
    ...reference,
  ];
  const priorRoot = await validationMachineLedgerRoot(
    [...ledgerWitnessEntries].sort((left, right) =>
      Buffer.compare(left.outRef, right.outRef),
    ),
  );
  return {
    label: `tx seed ${seed.toString()} (${program.label})`,
    scriptFlat,
    exUnits,
    primaryTag,
    returnsUnit,
    redeemer,
    event: {
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
      canonicalTransactionCbor: transaction.txCbor,
      ledgerWitnessEntries,
      priorUtxosRoot: priorRoot.toString("hex"),
      blockTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: BLOCK_SLOT,
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      sourceKind: "normal",
    },
  };
};
