import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  type LucidEvolution,
  type Script,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { isSameScriptRef } from "@/reference-scripts.js";
import * as SDK from "@/reserve-payout/primitives.js";

type ReferenceScriptTarget = {
  readonly name: string;
  readonly script: Script;
};

export type ReferenceScriptResolved = {
  readonly name: string;
  readonly utxo: UTxO;
};

const fetchReferenceScriptUtxosProgram = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  targets: readonly ReferenceScriptTarget[],
): Effect.Effect<readonly ReferenceScriptResolved[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const referenceScriptUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(referenceScriptsAddress),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to fetch reference-script UTxOs at ${referenceScriptsAddress}`,
          cause,
        }),
    });
    return yield* Effect.forEach(targets, (target) =>
      Effect.gen(function* () {
        const resolved = [...referenceScriptUtxos]
          .filter((utxo) => isSameScriptRef(utxo.scriptRef, target.script))
          .sort(compareOutRefs)[0];
        if (resolved === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message: "Missing reference script",
              cause: `${target.name} at ${referenceScriptsAddress}`,
            }),
          );
        }
        return {
          name: target.name,
          utxo: resolved,
        };
      }),
    );
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof SDK.StateQueueError
        ? cause
        : new SDK.StateQueueError({
            message: "Failed to resolve required reference scripts",
            cause,
          }),
    ),
  );

export type ReservePayoutReferenceScripts = {
  readonly depositMinting?: UTxO;
  readonly depositSpending?: UTxO;
  readonly depositWitnessCertificate?: UTxO;
  readonly withdrawalMinting?: UTxO;
  readonly withdrawalSpending?: UTxO;
  readonly withdrawalWitnessCertificate?: UTxO;
  readonly membershipProofWithdrawal?: UTxO;
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
  ["deposit witness certificate", "depositWitnessCertificate"],
  ["withdrawal minting", "withdrawalMinting"],
  ["withdrawal spending", "withdrawalSpending"],
  ["withdrawal witness certificate", "withdrawalWitnessCertificate"],
  ["membership proof withdrawal", "membershipProofWithdrawal"],
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

export const resolveReferenceScriptsProgram = (
  lucid: LucidEvolution,
  address: string | undefined,
  targets: readonly {
    readonly name: string;
    readonly script: Script;
  }[],
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
    );
  });

const hasExplicitReferenceScript = (
  explicit: ReservePayoutReferenceScripts | undefined,
  name: string,
): boolean => explicit?.[referenceScriptFieldByName.get(name)!] !== undefined;

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
    merged[field] =
      explicit?.[field] ?? resolvedReferenceScript(resolved, name);
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
