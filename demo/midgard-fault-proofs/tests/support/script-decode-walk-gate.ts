/**
 * The walk-cost gate of script decoding: the record of any script up to the
 * 16,384-byte bound is walked within 1,500 Walk transactions, and each row
 * reports its count against the target, D_case / 2 at the public profile's
 * transaction rate: 877.
 *
 * Each row records the largest script of one shape through the real Walk
 * builder, which gives every Walk the most ticks that build within the
 * ledger's limits, and counts the Walks. The rows hold the costliest shape
 * per bit of each decoding construct and the costliest shapes a search over
 * pair chains, list nesting (lists up to 15 deep, chains up to 6 deep,
 * innermost lists just over a tick's units), Data and element patterns found,
 * and application chains over small constants, whose decoding changes
 * control class at every constant.
 *
 * The rows are spread over several test files, one group each, because one
 * process building every row's Walks runs the transaction builder's
 * WebAssembly heap out. Every shape belongs to exactly one group (checked by
 * the type below).
 */
import { scriptDecodeRecordOfScript } from "@al-ft/midgard-validation";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  recordScriptDecode,
  type ScriptDecodeDeployment,
} from "../../src/script-decode/index.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./emulator/protocol-parameters.js";
import { injectScriptDecodeDeployment } from "./script-decode-emulator.js";
import {
  largestScriptOf,
  type WalkShape,
} from "./script-decode-walk-shapes.js";

/** The most Walks a record may take. */
export const WALK_CEILING = 1_500;
/** D_case / 2 over the public profile's transaction interval. */
const PUBLIC_WALK_TARGET = 877;
const PLUTUS_V3 = 3;
const MAX_TX_BYTES = 16_384;

/** The gate's rows by test file. */
const WALK_GATE_GROUPS = {
  "terms and Data": [
    "lambda-delay",
    "case of error branches",
    "Data nested arrays",
    "Data list of I 0",
  ],
  "flat lists": [
    "list bool",
    "list (list unit)",
    "list unit",
    "list (pair unit unit)",
    "list of an 8-leaf unit pair tree",
    "list integer",
    "list bytestring",
  ],
  "unit pair chains": [
    "list of a unit pair chain over bool",
    "list of a unit pair chain over a pair of bools",
    "list of a unit pair chain over value",
    "list of a unit pair chain over Data",
    "list of a unit pair chain over list unit",
  ],
  "unit pair chains over list unit": [
    "list of a unit pair chain over list unit, every element [()]",
    "list of a unit pair chain over a pair of list unit and unit, every element ([()], ())",
  ],
  // One row each: the two rows in one process walk past a CI job's limit.
  "unit pair chains over list Data, alternating": [
    "list of a unit pair chain over list Data, [] and [d] alternating",
  ],
  "unit pair chains over list Data, every element": [
    "list of a unit pair chain over list Data, every element [d]",
  ],
  "unit pair chains over pairs of lists": [
    "list of a unit pair chain over a pair of list unit and list Data",
    "list of a unit pair chain over a pair of list unit, every element ([()], [()])",
  ],
  "nested lists": [
    "lists 14 deep over 103 [()]",
    "lists 2 deep over 23 unit pair chains of depth 6 over [()]",
    "lists 6 deep, [] ahead of each deeper list, over 31 unit pair chains of depth 4 over [()]",
  ],
  "nested lists 4 deep": [
    "lists 4 deep, [] ahead of each deeper list, over 26 unit pair chains of depth 5 over [()]",
    "lists 4 deep, a one-path list ahead of each deeper list, over 26 unit pair chains of depth 5 over [()]",
  ],
  // The application chains walk longest; they are four groups so that no
  // one test file holds a CI shard past its time limit.
  "application chains of unit": ["apply chain of unit"],
  "application chains of integers": [
    "apply chain of integer 0",
    "apply chain of 200-bit integer",
  ],
  "application chains of empty value and list unit": [
    "apply chain of empty value",
    "apply chain of list unit [()]",
  ],
  "alternating application chains": [
    "apply chain of Data I 0 and list unit [()], alternating",
    "apply chain of integer 0 and empty value, alternating",
  ],
} as const satisfies Record<string, readonly WalkShape[]>;

type WalkGateGroup = keyof typeof WALK_GATE_GROUPS;
type GroupedShape = (typeof WALK_GATE_GROUPS)[WalkGateGroup][number];
// Fails to typecheck when a shape is in no group.
const everyShapeIsGrouped: [Exclude<WalkShape, GroupedShape>] extends [never]
  ? true
  : never = true;
void everyShapeIsGrouped;

/** The gate's rows of one group. */
export const describeWalkGate = (group: WalkGateGroup): void => {
  describe(`script-decode walk-cost gate: ${group}`, () => {
    let lucid: LucidEvolution;
    let deployment: ScriptDecodeDeployment;

    beforeAll(async () => {
      const account = generateEmulatorAccount({ lovelace: 900_000_000_000n });
      const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
      deployment = injectScriptDecodeDeployment(emulator);
      lucid = await Lucid(emulator, "Custom");
      lucid.selectWallet.fromSeed(account.seedPhrase);
    });

    it.each(WALK_GATE_GROUPS[group])(
      "walks the 16,384-byte %s script within the gate",
      async (shape) => {
        const { count, script } = largestScriptOf(shape);
        expect(script.length).toBeLessThanOrEqual(MAX_TX_BYTES);
        const recorded = await recordScriptDecode({
          lucid,
          deployment,
          language: PLUTUS_V3,
          scriptBytes: script,
          maxWalks: WALK_CEILING,
        });
        expect(recorded.record).toEqual(
          scriptDecodeRecordOfScript(PLUTUS_V3, script),
        );
        const { maxTxExMem, maxTxExSteps } = lucid.config().protocolParameters!;
        for (const walk of recorded.walks) {
          expect(walk.exUnits.mem).toBeLessThanOrEqual(maxTxExMem);
          expect(walk.exUnits.steps).toBeLessThanOrEqual(maxTxExSteps);
          expect(walk.txBytes).toBeLessThanOrEqual(MAX_TX_BYTES);
        }
        console.info(
          `[script-decode-walk-gate] ${JSON.stringify({
            shape,
            scriptBytes: script.length,
            repetitions: count,
            record: recorded.record?.k,
            walks: recorded.walks.length,
            withinTarget: recorded.walks.length <= PUBLIC_WALK_TARGET,
            ticks: recorded.walks.reduce((sum, walk) => sum + walk.fuel, 0),
            maxMem: Math.max(
              ...recorded.walks.map((walk) => Number(walk.exUnits.mem)),
            ),
            maxSteps: Math.max(
              ...recorded.walks.map((walk) => Number(walk.exUnits.steps)),
            ),
            maxTxBytes: Math.max(...recorded.walks.map((walk) => walk.txBytes)),
          })}`,
        );
        expect(recorded.walks.length).toBeLessThanOrEqual(WALK_CEILING);
      },
      1_800_000,
    );
  });
};
