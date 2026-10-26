import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";

import { RejectCodes } from "../src/index.js";
import {
  KEY_ADDRESS,
  native,
  NATIVE_POLICY,
  type Row,
} from "./emission-order.support.js";
import {
  makeMintPreimageCbor,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";

/**
 * The ValueAndMint asset bound counts entries: every asset of every spend
 * input's value, every asset of every output and every mint entry is one
 * entry, whether or not its unit was folded before. Every row here repeats the
 * same 1,024 units across its inputs and outputs, so the transaction has far
 * fewer distinct units than the bound; the rows sit at the bound and one entry
 * past it, at each fold.
 *
 * Each spend input holds the same 1,024 units (two-byte names under the
 * native policy, quantity 1). Fifteen spends fold 15,360 entries and one
 * output paying the units back folds 1,024 more: exactly the bound. The
 * crossing rows add one entry in the input, output or mint fold.
 */

const LIMIT = MIDGARD_CONSENSUS_LIMITS.maxDistinctAssetCount;
const UNITS = 1_024;
const SPENDS = (LIMIT - UNITS) / UNITS;
const INPUT_LOVELACE = 10_000_000n;

const nameHex = (name: number): string =>
  Buffer.from([name >> 8, name & 0xff]).toString("hex");

/** The 1,024 units, unit `n` at `quantityOf(n)`. */
const units = (
  quantityOf: (unit: number) => bigint,
): Map<string, Map<string, bigint>> =>
  new Map([
    [
      NATIVE_POLICY,
      new Map(
        Array.from(
          { length: UNITS },
          (_, unit) => [nameHex(unit), quantityOf(unit)] as const,
        ),
      ),
    ],
  ]);

const holdingAll = (lovelace: bigint) =>
  makeOutput(
    lovelace,
    KEY_ADDRESS,
    units(() => 1n),
  );

/** `count` key-address spends, each holding the 1,024 units. */
const spendsOf = (count: number) =>
  Array.from({ length: count }, (_, index) => outRefFromByte(0x20 + index));
const stateOf = (spends: readonly Buffer[]) =>
  spends.map((outRef) => [outRef, holdingAll(INPUT_LOVELACE)] as const);

const spends = spendsOf(SPENDS);
const paidIn = INPUT_LOVELACE * BigInt(SPENDS);

/** One output paying every unit and all the lovelace back: 1,024 entries. */
const payBack = makeOutput(
  paidIn,
  KEY_ADDRESS,
  units(() => BigInt(SPENDS)),
);

export const ASSET_ENTRY_ROWS: readonly Row[] = [
  {
    // 15,360 input entries and 1,024 output entries: exactly the bound.
    name: "f_asset_entries_at_bound",
    kind: "accept",
    guard: true,
    state: stateOf(spends),
    tx: { spendInputs: spends, outputs: [payBack] },
    expected: null,
  },
  {
    // A sixteenth spend fills the bound in the input fold; a seventeenth
    // repeats unit 0, the entry past the bound. The machine writes its unset
    // sentinel for the input index.
    name: "f_asset_entries_input_crossing",
    kind: "refusal",
    guard: false,
    state: stateOf(spendsOf(SPENDS + 2)),
    tx: {
      spendInputs: spendsOf(SPENDS + 2),
      outputs: [
        makeOutput(
          INPUT_LOVELACE * BigInt(SPENDS + 2),
          KEY_ADDRESS,
          units(() => BigInt(SPENDS + 2)),
        ),
      ],
    },
    expected: {
      code: RejectCodes.AssetCount,
      reason: {
        InputAssetAccumulationLimit: { input_index: -1n, asset_index: 0n },
      },
    },
  },
  {
    // Output 0 fills the bound; output 1 pays back one unit 0 the inputs
    // already carry, the entry past the bound.
    name: "f_asset_entries_output_crossing",
    kind: "refusal",
    guard: false,
    state: stateOf(spends),
    tx: {
      spendInputs: spends,
      outputs: [
        makeOutput(
          paidIn - INPUT_LOVELACE,
          KEY_ADDRESS,
          units((unit) => BigInt(unit === 0 ? SPENDS - 1 : SPENDS)),
        ),
        makeOutput(
          INPUT_LOVELACE,
          KEY_ADDRESS,
          new Map([[NATIVE_POLICY, new Map([[nameHex(0), 1n]])]]),
        ),
      ],
    },
    expected: {
      code: RejectCodes.AssetCount,
      reason: {
        OutputAssetAccumulationLimit: { output_index: 1n, asset_index: 0n },
      },
    },
  },
  {
    // The inputs and the output fill the bound; burning one unit 0 is the
    // entry past it.
    name: "f_asset_entries_mint_crossing",
    kind: "refusal",
    guard: false,
    state: stateOf(spends),
    tx: {
      spendInputs: spends,
      outputs: [
        makeOutput(
          paidIn,
          KEY_ADDRESS,
          units((unit) => BigInt(unit === 0 ? SPENDS - 1 : SPENDS)),
        ),
      ],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([
          [
            Buffer.from(NATIVE_POLICY, "hex"),
            new Map([[Buffer.from(nameHex(0), "hex"), -1n]]),
          ],
        ]),
      ),
      scriptWitnesses: [native],
    },
    expected: {
      code: RejectCodes.AssetCount,
      reason: { MintAssetAccumulationLimit: { mint_index: 0n } },
    },
  },
];
