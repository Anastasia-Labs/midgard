/**
 * Records how Cardano decodes one script: finds the record for its hash, or
 * stages the bytes, binds a run (or resumes an open one), walks it to a
 * verdict, and mints the record.
 */
import {
  nextScriptDecodeChainedWalk,
  type ScriptDecodeChainedWalk,
  type ScriptDecodeRecord,
  scriptDecodeScriptHash,
  type ScriptDecodeStakerClass,
  type ScriptDecodeWalkCursor,
  scriptDecodeWalkRecord,
  startScriptDecodeWalk,
} from "@al-ft/midgard-validation";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { OwnOutputJournal } from "../workflow/own-outputs.js";
import { plutusItemScript } from "./item-record.js";
import {
  resumeScriptDecodeRun,
  type ScriptDecodeRunPosition,
} from "./resume.js";
import { stageScriptDecodeBytes } from "./stage.js";
import {
  bindScriptDecodeRun,
  buildScriptDecodeWalk,
  finishScriptDecodeRun,
  type ScriptDecodeDeployment,
  type ScriptDecodeExUnits,
} from "./submit.js";

/** Ticks the first Walk tries before the budget it uses is measured. */
export const SCRIPT_DECODE_INITIAL_FUEL = 32;

/** Submissions of one Walk tried before its rejection is final. */
export const SCRIPT_DECODE_WALK_SUBMIT_ATTEMPTS = 3;

/** The most builds tried for one Walk while searching its fuel. */
const MAX_FUEL_TRIES = 16;

/**
 * The ledger rejected one Walk on every attempt, each refitted from the run's
 * cursor after the ledger parameters were refreshed.
 */
export class ScriptDecodeWalkRejectedError extends Error {
  readonly attempts: number;
  readonly rejections: readonly unknown[];

  constructor(rejections: readonly unknown[]) {
    const last = rejections.at(-1);
    super(
      `script decode: the ledger rejected a Walk on all ${rejections.length.toString()} attempts; the last: ${
        last instanceof Error ? last.message : String(last)
      }`,
    );
    this.name = "ScriptDecodeWalkRejectedError";
    this.attempts = rejections.length;
    this.rejections = rejections;
  }
}

/** Re-reads the ledger parameters and cost models from the Lucid provider. */
export const refreshScriptDecodeLedgerParameters =
  (lucid: LucidEvolution) => async () => {
    const { provider } = lucid.config();
    if (provider === undefined) {
      throw new Error("script decode: no provider to refresh the parameters");
    }
    await lucid.switchProvider(provider);
  };

/** One submitted Walk. */
export type ScriptDecodeWalkReport = Readonly<{
  fuel: number;
  /** The class of each of its segments, in chain order. */
  segments: readonly ScriptDecodeStakerClass[];
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

/**
 * The record UTxO of the script in a script-decoding `item`, which the proof
 * step that reads it must reference: undefined for a native or malformed
 * item, which takes none. A Plutus item whose script is not recorded yet (or
 * with no `deployment` to look in) throws, so the step is never submitted
 * without its record.
 */
export const requireScriptDecodeRecordForItem = async ({
  lucid,
  deployment,
  item,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment | undefined;
  readonly item: Uint8Array;
  readonly label: string;
}): Promise<UTxO | undefined> => {
  const script = plutusItemScript(item);
  if (script === undefined) return undefined;
  const record =
    deployment === undefined
      ? undefined
      : await findScriptDecodeRecord({
          lucid,
          deployment,
          scriptHashHex: script.scriptHashHex,
        });
  if (record === undefined) {
    throw new Error(
      `${label}: the item's Plutus script has no completed decode record`,
    );
  }
  return record;
};

/** The ledger's per-transaction execution and size limits. */
export const scriptDecodeLedgerLimits = (lucid: LucidEvolution) => {
  const protocol = lucid.config().protocolParameters;
  if (protocol === undefined) {
    throw new Error("script decode: the walk needs ledger parameters");
  }
  return {
    mem: protocol.maxTxExMem,
    steps: protocol.maxTxExSteps,
    bytes: protocol.maxTxSize,
  };
};

type Limits = ReturnType<typeof scriptDecodeLedgerLimits>;

/** The larger share of a transaction's budget a Walk used. */
const budgetShare = (used: ScriptDecodeExUnits, limits: Limits): number =>
  Math.max(
    Number(used.mem) / Number(limits.mem),
    Number(used.steps) / Number(limits.steps),
  );

type Built = Awaited<ReturnType<typeof buildScriptDecodeWalk>>;

/** The Walk `fitScriptDecodeWalk` settled on, built and signed. */
export type ScriptDecodeFittedWalk = Readonly<{
  fuel: number;
  walk: ScriptDecodeChainedWalk;
  next: ScriptDecodeWalkCursor;
  built: Built;
}>;

/**
 * The Walk from `cursor` with the most ticks that still builds: within the
 * ledger's execution and size limits, and accepted by the local evaluation
 * of the validator. The search starts at `guess`, scales up from what a
 * built Walk used, and halves the gap after a Walk that does not build.
 * Throws the last build error when not even one tick builds.
 */
export const fitScriptDecodeWalk = async (
  cursor: ScriptDecodeWalkCursor,
  guess: number,
  limits: Limits,
  build: (walk: ScriptDecodeChainedWalk) => Promise<Built>,
): Promise<ScriptDecodeFittedWalk> => {
  let best: ScriptDecodeFittedWalk | undefined;
  let failed = Number.POSITIVE_INFINITY;
  let lastError: unknown;
  let fuel = Math.max(1, guess);
  for (let tries = 0; tries < MAX_FUEL_TRIES; tries += 1) {
    const { walk, next } = nextScriptDecodeChainedWalk(cursor, fuel);
    let built: Built | undefined;
    try {
      built = await build(walk);
      if (BigInt(built.txBytes) > BigInt(limits.bytes)) {
        throw new Error("script decode: the Walk exceeds the size limit");
      }
    } catch (error) {
      built = undefined;
      lastError = error;
    }
    if (built === undefined) {
      failed = fuel;
    } else {
      best = { fuel: walk.fuel, walk, next, built };
      // A finished walk takes no more ticks, whatever the fuel.
      if (scriptDecodeWalkRecord(next) !== undefined) break;
    }
    const known = best?.fuel ?? 0;
    if (failed <= known + 1) break;
    if (best === undefined) {
      fuel = Math.max(1, Math.floor(fuel / 2));
      continue;
    }
    const share = budgetShare(best.built.exUnits, limits);
    const scaled = Math.floor(best.fuel / Math.max(share, 1e-6));
    const ceiling = Number.isFinite(failed) ? failed - 1 : 2 * known;
    fuel =
      built === undefined
        ? Math.floor((known + failed) / 2)
        : Math.min(Math.max(scaled, known + 1), ceiling);
  }
  if (best === undefined) throw lastError;
  return best;
};

/**
 * The record UTxO of `scriptBytes` in `language`, minting it if there is none
 * yet. An open run of the script (an interrupted earlier record) is resumed
 * from its on-chain cursor. The bytes are staged afresh unless `ownOutputs`
 * holds chunks an earlier call staged and journaled there, still live; they
 * are found by their out-refs, never by a search of the wallet's address.
 * Each Walk takes the most ticks that build within the ledger's limits
 * (`fitScriptDecodeWalk`). A Walk the ledger rejects is refitted after
 * `refreshLedgerParameters` (by default, re-reading them from the provider),
 * from the run then on chain, at most `maxSubmitAttempts` times in all before
 * `ScriptDecodeWalkRejectedError`. With `maxWalks`, a walk that needs more
 * Walks than that throws instead of submitting the next one.
 */
export const recordScriptDecode = async ({
  lucid,
  deployment,
  language,
  scriptBytes,
  initialFuel = SCRIPT_DECODE_INITIAL_FUEL,
  maxWalks = Number.POSITIVE_INFINITY,
  maxSubmitAttempts = SCRIPT_DECODE_WALK_SUBMIT_ATTEMPTS,
  refreshLedgerParameters = refreshScriptDecodeLedgerParameters(lucid),
  ownOutputs,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly language: number;
  readonly scriptBytes: Uint8Array;
  readonly initialFuel?: number;
  readonly maxWalks?: number;
  readonly maxSubmitAttempts?: number;
  readonly refreshLedgerParameters?: () => Promise<void>;
  /** Where the staged chunks are journaled, for a later call to reuse. */
  readonly ownOutputs?: OwnOutputJournal;
}): Promise<{
  readonly recordUtxo: UTxO;
  readonly record: ScriptDecodeRecord | undefined;
  readonly walks: readonly ScriptDecodeWalkReport[];
}> => {
  const scriptHashHex = scriptDecodeScriptHash(language, scriptBytes).toString(
    "hex",
  );
  const findRecord = () =>
    findScriptDecodeRecord({ lucid, deployment, scriptHashHex });
  const existing = await findRecord();
  if (existing !== undefined) {
    return { recordUtxo: existing, record: undefined, walks: [] };
  }
  const resume = () =>
    resumeScriptDecodeRun({
      lucid,
      deployment,
      language,
      scriptBytes,
      scriptHashHex,
    });
  let limits = scriptDecodeLedgerLimits(lucid);
  const staging = await stageScriptDecodeBytes({
    lucid,
    scriptBytes,
    ...(ownOutputs === undefined ? {} : { ownOutputs }),
  });
  const bind = async (): Promise<ScriptDecodeRunPosition> => {
    const cursor = startScriptDecodeWalk(language, scriptBytes);
    const runUtxo = await bindScriptDecodeRun({
      lucid,
      deployment,
      language,
      scriptHashHex,
      initialRun: cursor.run,
      staging,
    });
    return { runUtxo, cursor };
  };
  let { runUtxo, cursor } = (await resume()) ?? (await bind());
  const walks: ScriptDecodeWalkReport[] = [];
  let fuel = initialFuel;
  let rejections: unknown[] = [];
  let record = scriptDecodeWalkRecord(cursor);
  while (record === undefined) {
    if (walks.length >= maxWalks) {
      throw new Error(
        `script decode: the walk needs more than ${maxWalks.toString()} Walks`,
      );
    }
    const spent = runUtxo;
    const fitted = await fitScriptDecodeWalk(cursor, fuel, limits, (walk) =>
      buildScriptDecodeWalk({
        lucid,
        deployment,
        scriptHashHex,
        runUtxo: spent,
        staging,
        walk,
      }),
    );
    try {
      runUtxo = await fitted.built.submit();
    } catch (rejection) {
      rejections.push(rejection);
      if (rejections.length >= maxSubmitAttempts) {
        throw new ScriptDecodeWalkRejectedError(rejections);
      }
      await refreshLedgerParameters();
      limits = scriptDecodeLedgerLimits(lucid);
      // The rejected Walk may yet have landed, or another prover may have
      // walked the run: refit from whatever run is on chain now.
      const recorded = await findRecord();
      if (recorded !== undefined) {
        return { recordUtxo: recorded, record: undefined, walks };
      }
      const position = await resume();
      if (position === undefined) {
        throw new Error("script decode: no open run after a rejected Walk", {
          cause: rejection,
        });
      }
      ({ runUtxo, cursor } = position);
      record = scriptDecodeWalkRecord(cursor);
      continue;
    }
    rejections = [];
    walks.push({
      fuel: fitted.fuel,
      segments: fitted.walk.segments.map(({ within }) => within),
      exUnits: fitted.built.exUnits,
      txBytes: fitted.built.txBytes,
    });
    cursor = fitted.next;
    record = scriptDecodeWalkRecord(cursor);
    fuel = fitted.fuel;
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
