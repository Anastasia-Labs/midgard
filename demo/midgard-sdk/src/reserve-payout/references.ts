import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  type LucidEvolution,
  type Script,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  fetchReferenceScriptUtxosProgram,
  type ReferenceScriptAuthPolicyRef,
  type ReferenceScriptResolved,
} from "../reference-scripts.js";
import * as SDK from "./primitives.js";

export type ReservePayoutReferenceScripts = {
  readonly depositMinting?: UTxO;
  readonly depositSpending?: UTxO;
  readonly historyList?: UTxO;
  readonly historyRetirement?: UTxO;
  readonly withdrawalMinting?: UTxO;
  readonly withdrawalSpending?: UTxO;
  readonly reserveSpending?: UTxO;
  readonly payoutSpending?: UTxO;
  readonly payoutMinting?: UTxO;
};

type MutableReservePayoutReferenceScripts = {
  -readonly [Key in keyof ReservePayoutReferenceScripts]?: UTxO;
};

const referenceScriptFields = [
  ["deposit minting", "depositMinting"],
  ["deposit spending", "depositSpending"],
  ["deposit history retirement", "historyRetirement"],
  ["withdrawal history retirement", "historyRetirement"],
  ["withdrawal minting", "withdrawalMinting"],
  ["withdrawal spending", "withdrawalSpending"],
  ["reserve spending", "reserveSpending"],
  ["payout spending", "payoutSpending"],
  ["payout minting", "payoutMinting"],
] as const satisfies readonly (readonly [
  string,
  keyof ReservePayoutReferenceScripts,
])[];

const referenceScriptFieldByName = new Map<
  string,
  keyof ReservePayoutReferenceScripts
>(referenceScriptFields);

/** Resolves the targets no explicit reference covers, each through its role
 * token under `authPolicy` (the node's live resolution; a payout build runs
 * in the settlement worker's bounded heap, so it never lists the wallet). */
export const resolveReferenceScriptsProgram = (
  lucid: LucidEvolution,
  address: string | undefined,
  targets: readonly {
    readonly name: string;
    readonly script: Script;
  }[],
  authPolicy: ReferenceScriptAuthPolicyRef,
  explicit?: ReservePayoutReferenceScripts,
): Effect.Effect<readonly ReferenceScriptResolved[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (address === undefined) {
      return [];
    }
    const unresolvedTargets = targets.filter(
      (target) => !hasExplicitReferenceScript(explicit, target.name),
    );
    if (unresolvedTargets.length <= 0) {
      return [];
    }
    return yield* fetchReferenceScriptUtxosProgram(
      lucid,
      address,
      unresolvedTargets,
      authPolicy,
    );
  });

const hasExplicitReferenceScript = (
  explicit: ReservePayoutReferenceScripts | undefined,
  name: string,
): boolean =>
  explicit?.[referenceScriptFieldByName.get(name)!] !== undefined ||
  ((name === "deposit spending" || name === "withdrawal spending") &&
    explicit?.historyList !== undefined);

const resolvedReferenceScript = (
  resolved: readonly ReferenceScriptResolved[],
  name: string,
): UTxO | undefined => resolved.find((entry) => entry.name === name)?.utxo;

export const mergeReferenceScripts = (
  explicit: ReservePayoutReferenceScripts | undefined,
  resolved: readonly ReferenceScriptResolved[],
): ReservePayoutReferenceScripts => {
  const merged: MutableReservePayoutReferenceScripts = { ...explicit };
  for (const [name, field] of referenceScriptFields) {
    merged[field] ??= resolvedReferenceScript(resolved, name);
  }
  return merged;
};

export const referenceInputs = (
  hubOracleRefInput: UTxO,
  additional: readonly (UTxO | undefined)[],
): readonly UTxO[] => {
  const byOutRef = new Map<string, UTxO>();
  for (const utxo of [
    hubOracleRefInput,
    ...additional.filter(
      (candidate): candidate is UTxO => candidate !== undefined,
    ),
  ]) {
    byOutRef.set(outRefLabel(utxo), utxo);
  }
  return [...byOutRef.values()];
};

export const attachIfMissing = (
  tx: TxBuilder,
  script: Script,
  referenceScript: UTxO | undefined,
): TxBuilder => (referenceScript === undefined ? tx.attach.Script(script) : tx);
