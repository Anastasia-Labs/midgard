/** A record that stops part-way resumes its open run on the rerun. */
import { scriptDecodeRecordOfScript } from "@al-ft/midgard-validation";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  recordScriptDecode,
  type ScriptDecodeDeployment,
  scriptDecodeRunTokenName,
  scriptDecodeStagingDatums,
} from "../src/script-decode/index.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { injectScriptDecodeDeployment } from "./support/script-decode-emulator.js";
import { delayChainScript } from "./support/script-decode-plutus-max-shape.js";

const PLUTUS_V3 = 3;

/** A delay chain of a few full Walks, refused at its last bit. */
const script = delayChainScript(1_000);

const setup = async () => {
  const account = generateEmulatorAccount({ lovelace: 5_000_000_000n });
  const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
  const deployment = injectScriptDecodeDeployment(emulator);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  return { emulator, deployment, lucid };
};

const openRuns = (
  lucid: LucidEvolution,
  deployment: ScriptDecodeDeployment,
): Promise<UTxO[]> => {
  const { policyId, spendingScriptAddress } = deployment.validator;
  const scriptHashHex = scriptDecodeRecordOfScript(
    PLUTUS_V3,
    script,
  ).scriptHash.toString("hex");
  return lucid.utxosAtWithUnit(
    spendingScriptAddress,
    `${policyId}${scriptDecodeRunTokenName(scriptHashHex)}`,
  );
};

describe("script-decode record resume", () => {
  it("resumes the open run after a stop part-way, without a second Bind", async () => {
    const { deployment, lucid } = await setup();
    await expect(
      recordScriptDecode({
        lucid,
        deployment,
        language: PLUTUS_V3,
        scriptBytes: script,
        maxWalks: 2,
      }),
    ).rejects.toThrow(/needs more than 2 Walks/u);
    const [open, ...others] = await openRuns(lucid, deployment);
    expect(others).toEqual([]);
    expect(open).toBeDefined();

    const rerun = await recordScriptDecode({
      lucid,
      deployment,
      language: PLUTUS_V3,
      scriptBytes: script,
    });
    expect(rerun.record).toEqual(scriptDecodeRecordOfScript(PLUTUS_V3, script));
    expect(rerun.walks.length).toBeGreaterThan(0);
    // The run the first attempt left open is the one finished: it is spent
    // and no run of the script is left open beside the record.
    expect(await lucid.utxosByOutRef([open!])).toEqual([]);
    expect(await openRuns(lucid, deployment)).toEqual([]);
    // The staged bytes of the first attempt served the rerun.
    const staged = (await lucid.utxosAt(await lucid.wallet().address())).filter(
      ({ datum }) => scriptDecodeStagingDatums(script).includes(datum ?? ""),
    );
    expect(staged).toHaveLength(scriptDecodeStagingDatums(script).length);
  }, 600_000);
});
