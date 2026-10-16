/**
 * Records how Cardano decodes one script: finds the record for its hash, or
 * stages the bytes, binds a run, walks it to a verdict, and mints the record.
 */
import {
  nextScriptDecodeWalkStep,
  type ScriptDecodeRecord,
  scriptDecodeScriptHash,
  scriptDecodeWalkRecord,
  startScriptDecodeWalk,
} from "@al-ft/midgard-validation";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  bindScriptDecodeRun,
  buildScriptDecodeWalk,
  finishScriptDecodeRun,
  type ScriptDecodeDeployment,
  type ScriptDecodeExUnits,
  stageScriptDecodeBytes,
} from "./submit.js";

/** Ticks the first Walk takes before the budget it uses is measured. */
export const SCRIPT_DECODE_INITIAL_FUEL = 32;

/** The share of each transaction budget a Walk aims to use. */
const BUDGET_TARGET = 0.85;

/** One submitted Walk. */
export type ScriptDecodeWalkReport = Readonly<{
  fuel: number;
  exUnits: ScriptDecodeExUnits;
  txBytes: number;
}>;

/** The record UTxO for `scriptHashHex`, if one has been minted. */
export const findScriptDecodeRecord = async ({
  lucid,
  deployment,
  scriptHashHex,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly scriptHashHex: string;
}): Promise<UTxO | undefined> => {
  const { policyId, spendingScriptAddress } = deployment.validator;
  const records = await lucid.utxosAtWithUnit(
    spendingScriptAddress,
    `${policyId}${scriptHashHex}`,
  );
  return records[0];
};

const budgetLimits = (lucid: LucidEvolution) => {
  const protocol = lucid.config().protocolParameters;
  if (protocol === undefined) {
    throw new Error("script decode: the walk needs ledger parameters");
  }
  return { mem: protocol.maxTxExMem, steps: protocol.maxTxExSteps };
};

/** The fuel the next Walk takes, scaled from what the last one used. */
const nextFuel = (
  fuel: number,
  used: ScriptDecodeExUnits,
  limits: { readonly mem: bigint; readonly steps: bigint },
): number => {
  const share = Math.max(
    Number(used.mem) / Number(limits.mem),
    Number(used.steps) / Number(limits.steps),
  );
  const scaled = Math.floor((fuel * BUDGET_TARGET) / Math.max(share, 1e-6));
  return Math.max(1, Math.min(scaled, fuel * 2));
};

/**
 * The record UTxO of `scriptBytes` in `language`, minting it if there is none
 * yet. Each Walk's fuel adapts to the budget the previous one used, and halves
 * when a Walk cannot be built. With `maxWalks`, a walk that needs more Walks
 * than that throws instead of submitting the next one.
 */
export const recordScriptDecode = async ({
  lucid,
  deployment,
  language,
  scriptBytes,
  initialFuel = SCRIPT_DECODE_INITIAL_FUEL,
  maxWalks = Number.POSITIVE_INFINITY,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly language: number;
  readonly scriptBytes: Uint8Array;
  readonly initialFuel?: number;
  readonly maxWalks?: number;
}): Promise<{
  readonly recordUtxo: UTxO;
  readonly record: ScriptDecodeRecord | undefined;
  readonly walks: readonly ScriptDecodeWalkReport[];
}> => {
  const scriptHashHex = scriptDecodeScriptHash(language, scriptBytes).toString(
    "hex",
  );
  const existing = await findScriptDecodeRecord({
    lucid,
    deployment,
    scriptHashHex,
  });
  if (existing !== undefined) {
    return { recordUtxo: existing, record: undefined, walks: [] };
  }
  const limits = budgetLimits(lucid);
  const staging = await stageScriptDecodeBytes({ lucid, scriptBytes });
  let cursor = startScriptDecodeWalk(language, scriptBytes);
  let runUtxo = await bindScriptDecodeRun({
    lucid,
    deployment,
    language,
    scriptHashHex,
    initialRun: cursor.run,
    staging,
  });
  const walks: ScriptDecodeWalkReport[] = [];
  let fuel = initialFuel;
  let record = scriptDecodeWalkRecord(cursor);
  while (record === undefined) {
    if (walks.length >= maxWalks) {
      throw new Error(
        `script decode: the walk needs more than ${maxWalks.toString()} Walks`,
      );
    }
    const { step, next } = nextScriptDecodeWalkStep(cursor, fuel);
    let built: Awaited<ReturnType<typeof buildScriptDecodeWalk>>;
    try {
      built = await buildScriptDecodeWalk({
        lucid,
        deployment,
        scriptHashHex,
        runUtxo,
        staging,
        step,
      });
    } catch (error) {
      if (fuel === 1) throw error;
      fuel = Math.floor(fuel / 2);
      continue;
    }
    runUtxo = await built.submit();
    walks.push({ fuel, exUnits: built.exUnits, txBytes: built.txBytes });
    cursor = next;
    record = scriptDecodeWalkRecord(cursor);
    fuel = nextFuel(fuel, built.exUnits, limits);
  }
  const recordUtxo = await finishScriptDecodeRun({
    lucid,
    deployment,
    scriptHashHex,
    runUtxo,
    record,
  });
  return { recordUtxo, record, walks };
};
