/**
 * Each script-decode class staker in both polarities, on an emulator
 * deployment: it steps an honest segment of its class, and it refuses a Walk
 * whose one segment of its class claims a machine the step does not reach.
 * The forged Walk chains its links and its successor exactly, so the run's
 * spend accepts it and only the staker can refuse.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  nextScriptDecodeChainedWalk,
  type ScriptDecodeChainedWalk,
  scriptDecodeScriptHash,
  scriptDecodeWalkRecord,
  startScriptDecodeWalk,
} from "@al-ft/midgard-validation";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  bindScriptDecodeRun,
  buildScriptDecodeWalk,
  buildScriptDecodeWalkTx,
  type ScriptDecodeDeployment,
  scriptDecodeRunDatum,
  scriptDecodeWalkStakes,
  stageScriptDecodeBytes,
} from "../src/script-decode/index.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { injectScriptDecodeDeployment } from "./support/script-decode-emulator.js";
import { WALK_SHAPES } from "./support/script-decode-walk-shapes.js";

const PLUTUS_V3 = 3;
// A constant list of unit pair chains over Data: its one-tick Walks step every
// class, each alone in some Walk. Each class gets its own repetition count,
// so its own script and run.
const scriptFor = (name: SDK.ScriptDecodeStakerClassName) =>
  WALK_SHAPES["list of a unit pair chain over Data"](
    2 + SDK.SCRIPT_DECODE_STAKER_CLASS_NAMES.indexOf(name),
  );

/** The refusal names a withdrawal and not the run's spend. */
const expectWithdrawRefusal = async (
  build: () => Promise<unknown>,
): Promise<void> => {
  let failure: unknown;
  try {
    await build();
  } catch (error) {
    failure = error;
  }
  expect(failure, "the Walk should be refused").toBeDefined();
  const text = failure instanceof Error ? failure.message : String(failure);
  expect(text).toMatch(/Withdraw\[\d+\]/u);
  expect(text).not.toMatch(/Spend\[\d+\]/u);
};

describe("script-decode class stakers", () => {
  let lucid: LucidEvolution;
  let deployment: ScriptDecodeDeployment;

  beforeAll(async () => {
    const account = generateEmulatorAccount({ lovelace: 5_000_000_000n });
    const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
    deployment = injectScriptDecodeDeployment(emulator);
    lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
  });

  it.each(SDK.SCRIPT_DECODE_STAKER_CLASS_NAMES)(
    "the %s staker steps its class honestly and refuses a segment claiming a machine it does not reach",
    async (name) => {
      const script = scriptFor(name);
      const scriptHashHex = scriptDecodeScriptHash(PLUTUS_V3, script).toString(
        "hex",
      );
      const staging = await stageScriptDecodeBytes({
        lucid,
        scriptBytes: script,
      });
      let cursor = startScriptDecodeWalk(PLUTUS_V3, script);
      let runUtxo = await bindScriptDecodeRun({
        lucid,
        deployment,
        language: PLUTUS_V3,
        scriptHashHex,
        initialRun: cursor.run,
        staging,
      });
      const step = async (walk: ScriptDecodeChainedWalk) =>
        (
          await buildScriptDecodeWalk({
            lucid,
            deployment,
            scriptHashHex,
            runUtxo,
            staging,
            walk,
          })
        ).submit();

      for (;;) {
        expect(scriptDecodeWalkRecord(cursor)).toBeUndefined();
        const honest = nextScriptDecodeChainedWalk(cursor, 1);
        const [segment, ...others] = honest.walk.segments;
        if (segment?.within !== name || others.length > 0) {
          runUtxo = await step(honest.walk);
          cursor = honest.next;
          continue;
        }
        const stakes = scriptDecodeWalkStakes(deployment, runUtxo, honest.walk);
        expect(stakes.map(({ staker }) => staker)).toEqual([
          deployment.validator.stakers[name],
        ]);

        // The machine two ticks on, claimed as this one-tick segment's end.
        const further = nextScriptDecodeChainedWalk(cursor, 2).walk.after;
        expect(scriptDecodeRunDatum(further)).not.toBe(
          scriptDecodeRunDatum(honest.walk.after),
        );
        await expectWithdrawRefusal(() =>
          buildScriptDecodeWalkTx({
            lucid,
            deployment,
            scriptHashHex,
            runUtxo,
            staging,
            after: further,
            stakes: scriptDecodeWalkStakes(deployment, runUtxo, {
              ...honest.walk,
              segments: [{ ...segment, reached: further.machine }],
              after: further,
            }),
          }),
        );

        const before = runUtxo;
        runUtxo = await step(honest.walk);
        expect(runUtxo).not.toEqual(before);
        expect(runUtxo.datum).toBe(scriptDecodeRunDatum(honest.walk.after));
        return;
      }
    },
    300_000,
  );
});
