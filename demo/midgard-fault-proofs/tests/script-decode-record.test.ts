import {
  nextScriptDecodeChainedWalk,
  type ScriptDecodeRecord,
  scriptDecodeRecordOfScript,
  scriptDecodeScriptHash,
  scriptDecodeWalkRecord,
  startScriptDecodeWalk,
} from "@al-ft/midgard-validation";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  bindScriptDecodeRun,
  buildScriptDecodeWalk,
  buildScriptDecodeWalkTx,
  findScriptDecodeRecord,
  finishScriptDecodeRun,
  recordScriptDecode,
  SCRIPT_DECODE_CLOSE_REDEEMER,
  type ScriptDecodeDeployment,
  scriptDecodeRecordDatum,
  scriptDecodeRunDatum,
  scriptDecodeWalkStakes,
  stageScriptDecodeBytes,
} from "../src/script-decode/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { injectScriptDecodeDeployment } from "./support/script-decode-emulator.js";

// PlutusV3 script bytes: the flat program 1.1.0 `(con unit ())`, which
// decodes, and four bytes that are not a program (refused with class 3).
const decodable = Buffer.from("450101004981", "hex");
const undecodable = Buffer.from("4401020304", "hex");
const PLUTUS_V3 = 3;

const hashHex = (bytes: Uint8Array) =>
  scriptDecodeScriptHash(PLUTUS_V3, bytes).toString("hex");

describe("script-decode record builders", () => {
  let lucid: LucidEvolution;
  let deployment: ScriptDecodeDeployment;

  beforeAll(async () => {
    const account = generateEmulatorAccount({ lovelace: 5_000_000_000n });
    const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
    deployment = injectScriptDecodeDeployment(emulator);
    lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
  });

  it("records a decodable script once and finds it afterwards", async () => {
    const first = await recordScriptDecode({
      lucid,
      deployment,
      language: PLUTUS_V3,
      scriptBytes: decodable,
    });
    expect(first.record).toMatchObject({ k: "decoded", phase2Class: 0 });
    expect(first.walks.length).toBeGreaterThan(0);
    expect(first.recordUtxo.datum).toBe(
      scriptDecodeRecordDatum(first.record as ScriptDecodeRecord),
    );
    expect(first.recordUtxo.address).toBe(
      deployment.validator.spendingScriptAddress,
    );
    expect(
      await findScriptDecodeRecord({
        lucid,
        deployment,
        scriptHashHex: hashHex(decodable),
      }),
    ).toEqual(first.recordUtxo);
    const again = await recordScriptDecode({
      lucid,
      deployment,
      language: PLUTUS_V3,
      scriptBytes: decodable,
    });
    expect(again.walks).toEqual([]);
    expect(again.recordUtxo).toEqual(first.recordUtxo);
    await expectOnchainRefusal(() =>
      lucid
        .newTx()
        .collectFrom([first.recordUtxo], SCRIPT_DECODE_CLOSE_REDEEMER)
        .readFrom([deployment.reference])
        .complete({ localUPLCEval: true }),
    );
  }, 300_000);

  it("refuses a forged run, Walk, or record and accepts the honest one", async () => {
    const scriptHashHex = hashHex(undecodable);
    const staging = await stageScriptDecodeBytes({
      lucid,
      scriptBytes: undecodable,
    });
    const start = startScriptDecodeWalk(PLUTUS_V3, undecodable);
    const bind = (initialRun: typeof start.run) =>
      bindScriptDecodeRun({
        lucid,
        deployment,
        language: PLUTUS_V3,
        scriptHashHex,
        initialRun,
        staging,
      });
    await expectOnchainRefusal(() =>
      bind(startScriptDecodeWalk(PLUTUS_V3, decodable).run),
    );
    let runUtxo: UTxO = await bind(start.run);

    let cursor = start;
    let forgedOnce = false;
    while (scriptDecodeWalkRecord(cursor) === undefined) {
      const honest = nextScriptDecodeChainedWalk(cursor, 1);
      if (!forgedOnce) {
        // A successor two ticks on, claimed by a one-tick Walk.
        const further = nextScriptDecodeChainedWalk(cursor, 2).walk.after;
        expect(scriptDecodeRunDatum(further)).not.toBe(
          scriptDecodeRunDatum(honest.walk.after),
        );
        await expectOnchainRefusal(() =>
          buildScriptDecodeWalkTx({
            lucid,
            deployment,
            scriptHashHex,
            runUtxo,
            staging,
            after: further,
            stakes: scriptDecodeWalkStakes(deployment, runUtxo, honest.walk),
          }),
        );
        forgedOnce = true;
      }
      runUtxo = await (
        await buildScriptDecodeWalk({
          lucid,
          deployment,
          scriptHashHex,
          runUtxo,
          staging,
          walk: honest.walk,
        })
      ).submit();
      cursor = honest.next;
    }

    const record = scriptDecodeWalkRecord(cursor);
    expect(record).toEqual(scriptDecodeRecordOfScript(PLUTUS_V3, undecodable));
    if (record?.k !== "refused") throw new Error("expected a refused record");
    expect(record.class).toBe(3);
    const finish = (claimed: ScriptDecodeRecord) =>
      finishScriptDecodeRun({
        lucid,
        deployment,
        scriptHashHex,
        runUtxo,
        record: claimed,
      });
    await expectOnchainRefusal(() => finish({ ...record, class: 2 }));
    const recordUtxo = await finish(record);
    expect(recordUtxo.datum).toBe(scriptDecodeRecordDatum(record));
  }, 300_000);
});
