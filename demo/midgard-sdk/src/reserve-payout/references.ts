import * as SDK from "@/reserve-payout/primitives.js";
import {
  type LucidEvolution,
  type Script,
  type TxBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";

type ReferenceScriptTarget = {
  readonly name: string;
  readonly script: Script;
};

export type ReferenceScriptResolved = {
  readonly name: string;
  readonly utxo: UTxO;
};

const isSameScriptRef = (
  left: Script | null | undefined,
  right: Script,
): boolean => {
  if (left === undefined || left === null || left.type !== right.type) {
    return false;
  }
  try {
    return validatorToScriptHash(left) === validatorToScriptHash(right);
  } catch {
    return false;
  }
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

const referenceScriptFieldByName = {
  "deposit minting": "depositMinting",
  "deposit spending": "depositSpending",
  "deposit witness certificate": "depositWitnessCertificate",
  "withdrawal minting": "withdrawalMinting",
  "withdrawal spending": "withdrawalSpending",
  "withdrawal witness certificate": "withdrawalWitnessCertificate",
  "membership proof withdrawal": "membershipProofWithdrawal",
  "reserve spending": "reserveSpending",
  "payout spending": "payoutSpending",
  "payout minting": "payoutMinting",
} as const satisfies Record<string, keyof ReservePayoutReferenceScripts>;

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
): boolean =>
  explicit?.[
    referenceScriptFieldByName[name as keyof typeof referenceScriptFieldByName]
  ] !== undefined;

const resolvedReferenceScript = (
  resolved: readonly ReferenceScriptResolved[],
  name: string,
): UTxO | undefined => resolved.find((entry) => entry.name === name)?.utxo;

export const mergeReferenceScripts = (
  explicit: ReservePayoutReferenceScripts | undefined,
  resolved: readonly ReferenceScriptResolved[],
): ReservePayoutReferenceScripts => ({
  ...explicit,
  depositMinting:
    explicit?.depositMinting ??
    resolvedReferenceScript(resolved, "deposit minting"),
  depositSpending:
    explicit?.depositSpending ??
    resolvedReferenceScript(resolved, "deposit spending"),
  depositWitnessCertificate:
    explicit?.depositWitnessCertificate ??
    resolvedReferenceScript(resolved, "deposit witness certificate"),
  withdrawalMinting:
    explicit?.withdrawalMinting ??
    resolvedReferenceScript(resolved, "withdrawal minting"),
  withdrawalSpending:
    explicit?.withdrawalSpending ??
    resolvedReferenceScript(resolved, "withdrawal spending"),
  withdrawalWitnessCertificate:
    explicit?.withdrawalWitnessCertificate ??
    resolvedReferenceScript(resolved, "withdrawal witness certificate"),
  membershipProofWithdrawal:
    explicit?.membershipProofWithdrawal ??
    resolvedReferenceScript(resolved, "membership proof withdrawal"),
  reserveSpending:
    explicit?.reserveSpending ??
    resolvedReferenceScript(resolved, "reserve spending"),
  payoutSpending:
    explicit?.payoutSpending ??
    resolvedReferenceScript(resolved, "payout spending"),
  payoutMinting:
    explicit?.payoutMinting ??
    resolvedReferenceScript(resolved, "payout minting"),
});

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
