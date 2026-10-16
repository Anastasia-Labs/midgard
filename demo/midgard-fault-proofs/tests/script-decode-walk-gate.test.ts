/**
 * The walk-cost gate of script decoding: the record of any script up to the
 * 16,384-byte bound is walked within D_case / 2 at the public profile's
 * transaction rate, 877 Walk transactions.
 *
 * Each row records the largest script of one shape through the real Walk
 * builder, which sizes every Walk from the budget the previous one measured,
 * and counts the Walks.
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
} from "../src/script-decode/index.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { injectScriptDecodeDeployment } from "./support/script-decode-emulator.js";
import {
  largestScriptOf,
  WALK_SHAPES,
  type WalkShape,
} from "./support/script-decode-walk-shapes.js";

/** D_case / 2 over the public profile's transaction interval. */
const PUBLIC_WALK_GATE = 877;
const PLUTUS_V3 = 3;
const MAX_TX_BYTES = 16_384;

const SHAPES = Object.keys(WALK_SHAPES) as WalkShape[];

describe("script-decode walk-cost gate", () => {
  let lucid: LucidEvolution;
  let deployment: ScriptDecodeDeployment;

  beforeAll(async () => {
    const account = generateEmulatorAccount({ lovelace: 900_000_000_000n });
    const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
    deployment = injectScriptDecodeDeployment(emulator);
    lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
  });

  it.each(SHAPES)(
    "walks the 16,384-byte %s script within the gate",
    async (shape) => {
      const { count, script } = largestScriptOf(shape);
      expect(script.length).toBeLessThanOrEqual(MAX_TX_BYTES);
      const recorded = await recordScriptDecode({
        lucid,
        deployment,
        language: PLUTUS_V3,
        scriptBytes: script,
        maxWalks: PUBLIC_WALK_GATE,
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
          ticks: recorded.walks.reduce((sum, walk) => sum + walk.fuel, 0),
          maxMem: Math.max(
            ...recorded.walks.map((walk) => Number(walk.exUnits.mem)),
          ),
        })}`,
      );
      expect(recorded.walks.length).toBeLessThanOrEqual(PUBLIC_WALK_GATE);
    },
    1_800_000,
  );
});
