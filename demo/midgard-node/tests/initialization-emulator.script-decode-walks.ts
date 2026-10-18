/**
 * Script-decode Walks over the deployment a genuine Init leaves behind: the
 * validator and its class stakers as reference scripts at the
 * reference-script wallet, and the stakers' reward accounts registered.
 * Also the tampered Walks the lifecycle test expects the validator to refuse.
 */
import {
  type ScriptDecodeDeployment,
  type ScriptDecodePositionedSegment,
  type ScriptDecodeStake,
  scriptDecodeWalkStakes,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import type { ScriptDecodeChainedWalk } from "@al-ft/midgard-validation";
import {
  type LucidEvolution,
  type OutRef,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

/**
 * A withdraw-zero script outside the family that accepts anything: the
 * PlutusV3 flat program 1.1.0 `(lam x (con unit ()))`.
 */
export const OUTSIDE_STAKER: SDK.WithdrawalValidator = (() => {
  const withdrawalScript: Script = {
    type: "PlutusV3",
    script: "450101002499",
  };
  return {
    withdrawalScriptCBOR: withdrawalScript.script,
    withdrawalScript,
    withdrawalScriptHash: validatorToScriptHash(withdrawalScript),
  };
})();

const referenceOf = (utxos: readonly UTxO[], scriptHash: string): UTxO => {
  const found = utxos.find(
    ({ scriptRef }) =>
      scriptRef != null && validatorToScriptHash(scriptRef) === scriptHash,
  );
  if (found === undefined) {
    throw new Error(`no reference script UTxO for ${scriptHash}`);
  }
  return found;
};

/** The script-decode deployment the Init published at `address`. */
export const scriptDecodeDeploymentAt = async (
  lucid: LucidEvolution,
  address: string,
  validator: SDK.ScriptDecodeValidator,
): Promise<ScriptDecodeDeployment> => {
  const utxos = await lucid.utxosAt(address);
  return {
    validator,
    reference: referenceOf(utxos, validator.spendingScriptHash),
    stakerReferences: Object.fromEntries(
      SDK.SCRIPT_DECODE_STAKER_CLASS_NAMES.map((name) => [
        name,
        referenceOf(utxos, validator.stakers[name].withdrawalScriptHash),
      ]),
    ) as Record<SDK.ScriptDecodeStakerClassName, UTxO>,
  };
};

/** Pays the wallet one output with `datum` inline or `scriptRef`. */
const payWallet = async (
  lucid: LucidEvolution,
  datum: string | undefined,
  scriptRef: Script | undefined,
): Promise<UTxO> => {
  lucid.clearUTxOOverride();
  const address = await lucid.wallet().address();
  const signed = await (
    await lucid
      .newTx()
      .pay.ToAddressWithData(
        address,
        datum === undefined ? undefined : { kind: "inline", value: datum },
        {},
        scriptRef,
      )
      .complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const [paid] = (await lucid.utxosAt(address)).filter(
    (utxo) =>
      utxo.txHash === txHash &&
      (datum === undefined || utxo.datum === datum) &&
      (scriptRef === undefined || utxo.scriptRef != null),
  );
  if (paid === undefined) throw new Error("wallet output not found");
  return paid;
};

/**
 * A wallet output carrying `datum` inline, the same datum as a run: an input
 * a tampered Walk can name in place of the spent run.
 */
export const payDecoy = (lucid: LucidEvolution, datum: string) =>
  payWallet(lucid, datum, undefined);

/** A wallet output carrying the outside staker as a reference script. */
export const publishOutsideStaker = (lucid: LucidEvolution) =>
  payWallet(lucid, undefined, OUTSIDE_STAKER.withdrawalScript);

const positioned = (
  walk: ScriptDecodeChainedWalk,
): ScriptDecodePositionedSegment[] =>
  walk.segments.map((segment, position) => ({ position, segment }));

/**
 * Mutant 1: every segment passed by a staker outside the family, read from
 * `reference`.
 */
export const outsideStakes = (
  run: OutRef,
  walk: ScriptDecodeChainedWalk,
  reference: UTxO,
): ScriptDecodeStake[] => [
  { staker: OUTSIDE_STAKER, reference, run, segments: positioned(walk) },
];

/** Mutant 3: the honest stakes, naming `other` as the run they step. */
export const anotherRunStakes = (
  deployment: ScriptDecodeDeployment,
  run: OutRef,
  walk: ScriptDecodeChainedWalk,
  other: OutRef,
): ScriptDecodeStake[] =>
  scriptDecodeWalkStakes(deployment, run, walk).map((stake) => ({
    ...stake,
    run: other,
  }));

/** Mutant 11: the honest stakes without the segment at position 1. */
export const gapStakes = (
  deployment: ScriptDecodeDeployment,
  run: OutRef,
  walk: ScriptDecodeChainedWalk,
): ScriptDecodeStake[] => {
  expect(walk.segments.length).toBeGreaterThanOrEqual(3);
  return scriptDecodeWalkStakes(deployment, run, walk)
    .map((stake) => ({
      ...stake,
      segments: stake.segments.filter(({ position }) => position !== 1),
    }))
    .filter(({ segments }) => segments.length > 0);
};

/**
 * Mutant 13: the honest stakes, with the latest segment of a class that
 * steps more than one segment replaced by that class's first segment. Each
 * staker still steps every segment it holds honestly, but the replaced
 * segment does not start where the one before it ends.
 */
export const brokenLinkStakes = (
  deployment: ScriptDecodeDeployment,
  run: OutRef,
  walk: ScriptDecodeChainedWalk,
): ScriptDecodeStake[] => {
  const stakes = scriptDecodeWalkStakes(deployment, run, walk);
  const repeated = stakes.find(({ segments }) => segments.length > 1);
  if (repeated === undefined) {
    throw new Error("the Walk steps no class in more than one segment");
  }
  const first = repeated.segments[0]!.segment;
  const last = repeated.segments.at(-1)!;
  return stakes.map((stake) =>
    stake === repeated
      ? {
          ...stake,
          segments: stake.segments.map((entry) =>
            entry === last ? { ...entry, segment: first } : entry,
          ),
        }
      : stake,
  );
};

/** The refusal names the run's spend and no staker's withdrawal. */
export const expectSpendRefusal = async (
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
  expect(text).toMatch(
    /failed script execution\s+Spend\[\d+\]|Spend\[\d+\] failed/u,
  );
  expect(text).not.toMatch(/Withdraw\[\d+\]/u);
};
