import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { compareOutRefs, outRefLabel } from "@/tx-context.js";

export type ReferenceScriptTarget = {
  readonly name: string;
  readonly script: Script;
};

export type ReferenceScriptResolved = {
  readonly name: string;
  readonly utxo: UTxO;
};

export const isSameScriptRef = (
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

export const fetchReferenceScriptUtxosProgram = (
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
    return targets.map((target) => {
      const resolved = [...referenceScriptUtxos]
        .filter((utxo) => isSameScriptRef(utxo.scriptRef, target.script))
        .sort(compareOutRefs)[0];
      if (resolved === undefined) {
        throw new Error(
          `Missing reference script ${target.name} at ${referenceScriptsAddress}`,
        );
      }
      return {
        name: target.name,
        utxo: resolved,
      };
    });
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

export const referenceScriptByName = (
  resolved: readonly ReferenceScriptResolved[],
  name: string,
): UTxO => {
  const found = resolved.find((candidate) => candidate.name === name);
  if (found === undefined) {
    throw new Error(`Missing resolved reference script: ${name}`);
  }
  return found.utxo;
};

export const describeReferenceScriptInputs = (
  utxos: readonly UTxO[],
): string => utxos.map(outRefLabel).join(",");
