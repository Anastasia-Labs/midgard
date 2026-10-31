/**
 * A record that stops part-way resumes its open run on the rerun, and a Walk
 * the ledger rejects is refitted under refreshed parameters, a bounded number
 * of times.
 */
import { scriptDecodeRecordOfScript } from "@al-ft/midgard-validation";
import {
  CML,
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
  ScriptDecodeWalkRejectedError,
  transactionExUnits,
} from "../src/script-decode/index.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { injectScriptDecodeDeployment } from "./support/script-decode-emulator.js";
import { delayChainScript } from "./support/script-decode-plutus-max-shape.js";

const PLUTUS_V3 = 3;

/** A delay chain of a few full Walks, refused at its last bit. */
const script = delayChainScript(1_000);

const hasWithdrawals = (txCbor: string): boolean =>
  CML.Transaction.from_cbor_hex(txCbor).body().withdrawals() !== undefined;

/**
 * An emulator that applies the ledger's per-transaction execution limit at
 * submission, from its own current parameters, and can be told to reject
 * every Walk (the only transactions that withdraw).
 */
class LedgerLimitEmulator extends Emulator {
  rejected = 0;
  rejectWalks = false;

  override submitTx(tx: string): Promise<string> {
    const used = transactionExUnits(tx);
    const { maxTxExMem, maxTxExSteps } = this.protocolParameters;
    if (
      used.mem > maxTxExMem ||
      used.steps > maxTxExSteps ||
      (this.rejectWalks && hasWithdrawals(tx))
    ) {
      this.rejected += 1;
      return Promise.reject(new Error("ExUnitsTooBigUTxO"));
    }
    return super.submitTx(tx);
  }
}

const setup = async () => {
  const account = generateEmulatorAccount({ lovelace: 5_000_000_000n });
  const emulator = new LedgerLimitEmulator(
    [account],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
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

describe("script-decode record resume and refit", () => {
  it("resumes the open run after a stop part-way, without a second Bind", async () => {
    const { deployment, lucid } = await setup();
    // The chunks a run stages, kept by the out-refs it published.
    const ownOutputs = createOwnOutputJournal();
    await expect(
      recordScriptDecode({
        lucid,
        deployment,
        language: PLUTUS_V3,
        scriptBytes: script,
        maxWalks: 2,
        ownOutputs,
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
      ownOutputs,
    });
    expect(rerun.record).toEqual(scriptDecodeRecordOfScript(PLUTUS_V3, script));
    expect(rerun.walks.length).toBeGreaterThan(0);
    // The run the first attempt left open is the one finished: it is spent
    // and no run of the script is left open beside the record.
    expect(await lucid.utxosByOutRef([open!])).toEqual([]);
    expect(await openRuns(lucid, deployment)).toEqual([]);
    // The staged bytes the first attempt published served the rerun.
    const staged = (await lucid.utxosAt(await lucid.wallet().address())).filter(
      ({ datum }) => scriptDecodeStagingDatums(script).includes(datum ?? ""),
    );
    expect(staged).toHaveLength(scriptDecodeStagingDatums(script).length);
  }, 600_000);

  it("refits a Walk the ledger rejects under refreshed parameters", async () => {
    const { emulator, deployment, lucid } = await setup();
    // The ledger lowers its execution limit after Lucid read its parameters.
    const lowered = (EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem * 6n) / 10n;
    emulator.protocolParameters = {
      ...EMULATOR_PROTOCOL_PARAMETERS,
      maxTxExMem: lowered,
    };
    let refreshes = 0;
    const result = await recordScriptDecode({
      lucid,
      deployment,
      language: PLUTUS_V3,
      scriptBytes: script,
      refreshLedgerParameters: async () => {
        refreshes += 1;
        await lucid.switchProvider(emulator);
      },
    });
    expect(result.record).toEqual(
      scriptDecodeRecordOfScript(PLUTUS_V3, script),
    );
    expect(emulator.rejected).toBe(1);
    expect(refreshes).toBe(1);
    expect(result.walks.length).toBeGreaterThan(1);
    for (const walk of result.walks) {
      expect(walk.exUnits.mem).toBeLessThanOrEqual(lowered);
    }
    expect(await openRuns(lucid, deployment)).toEqual([]);
  }, 600_000);

  it("ends a rejection that persists in a typed error after the bounded attempts", async () => {
    const { emulator, deployment, lucid } = await setup();
    emulator.rejectWalks = true;
    let refreshes = 0;
    const attempt = recordScriptDecode({
      lucid,
      deployment,
      language: PLUTUS_V3,
      scriptBytes: script,
      maxSubmitAttempts: 3,
      refreshLedgerParameters: async () => {
        refreshes += 1;
        if (refreshes > 5) throw new Error("refit is not bounded");
        await lucid.switchProvider(emulator);
      },
    });
    const error = await attempt.catch((caught: unknown) => caught);
    expect(error).toBeInstanceOf(ScriptDecodeWalkRejectedError);
    expect((error as ScriptDecodeWalkRejectedError).attempts).toBe(3);
    expect(emulator.rejected).toBe(3);
    expect(refreshes).toBe(2);
    // The run stays open for a later rerun to resume.
    expect(await openRuns(lucid, deployment)).toHaveLength(1);
  }, 600_000);
});
