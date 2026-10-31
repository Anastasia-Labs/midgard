import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type ReferenceScriptAuthPolicyRef,
  referenceScriptAuthTokenNameText,
  referenceScriptAuthUnit,
  type ReferenceScriptResolved,
  type ReferenceScriptTarget,
} from "./reference-scripts.create-reference-script-auth-policy.js";
import {
  hasReferenceScriptAuthRole,
  isSameScriptRef,
} from "./reference-scripts.resolve-reference-script-publication-layout.js";
import { StateQueueError } from "./state-queue.js";

/** Role-token reads in flight at once while resolving several targets. */
const REFERENCE_SCRIPT_FETCH_CONCURRENCY = 4;

/** Target sets up to this size are read through their role tokens; larger
 * ones (bootstrap, deploy and ensure pass every published script, ~940 on the
 * lc1 devnet) read the wallet once rather than make a request per target. On
 * lc1's Kupo a filtered read took ~26 ms against ~186 ms for the wallet. */
export const REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT = 16;

/** Runs one provider read of the resolution; a caller wraps it in its own
 * retry policy. */
export type ReferenceScriptProviderRead = <A>(
  label: string,
  read: Effect.Effect<A, StateQueueError>,
) => Effect.Effect<A, StateQueueError>;

/**
 * Whether the live resolution accepts `utxo` for `target`: it sits at the
 * reference-script address, holds the target's role token under the auth
 * policy, and carries the target's script.
 */
export const acceptsReferenceScriptUtxo = (
  utxo: UTxO,
  referenceScriptsAddress: string,
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): boolean =>
  utxo.address === referenceScriptsAddress &&
  hasReferenceScriptAuthRole(utxo, target, authPolicy) &&
  isSameScriptRef(utxo.scriptRef, target.script);

/** The UTxO the live resolution picks for `target`, if any: the lowest
 * outRef among the accepted ones, whatever order the provider listed them. */
export const resolveReferenceScriptUtxo = (
  utxos: readonly UTxO[],
  referenceScriptsAddress: string,
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): UTxO | undefined =>
  utxos
    .filter((utxo) =>
      acceptsReferenceScriptUtxo(
        utxo,
        referenceScriptsAddress,
        target,
        authPolicy,
      ),
    )
    .sort(compareOutRefs)[0];

/** Why a resolution refused a target. `reference-script-not-indexed`: the
 * read succeeded but no accepted holder is in it, which a provider still
 * re-applying blocks after a rollback also reports, so a later read can
 * resolve it. `reference-script-unknown-target`: the target has no role
 * token, so no read ever resolves it. */
export type ReferenceScriptResolutionFailureReason =
  | "reference-script-not-indexed"
  | "reference-script-unknown-target";

/** A resolution refusal. It stays a {@link StateQueueError}, so every caller
 * that handles one is unchanged; `retryable` tells a provider retry policy
 * whether another read can clear it. */
export class ReferenceScriptResolutionError extends StateQueueError {
  readonly reason: ReferenceScriptResolutionFailureReason;
  readonly retryable: boolean;

  constructor(args: {
    readonly message: string;
    readonly cause: unknown;
    readonly reason: ReferenceScriptResolutionFailureReason;
  }) {
    super({ message: args.message, cause: args.cause });
    this.reason = args.reason;
    this.retryable = args.reason === "reference-script-not-indexed";
  }
}

const resolveReferenceScriptTarget = (
  candidates: readonly UTxO[],
  referenceScriptsAddress: string,
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): Effect.Effect<ReferenceScriptResolved, StateQueueError> => {
  const resolved = resolveReferenceScriptUtxo(
    candidates,
    referenceScriptsAddress,
    target,
    authPolicy,
  );
  return resolved === undefined
    ? Effect.fail(
        new ReferenceScriptResolutionError({
          message: "Missing reference script",
          cause: `${target.name} at ${referenceScriptsAddress} with role token ${referenceScriptAuthTokenNameText(
            target.name,
          )}`,
          reason: "reference-script-not-indexed",
        }),
      )
    : Effect.succeed({ name: target.name, utxo: resolved });
};

const fetchFailure = (referenceScriptsAddress: string) => (cause: unknown) =>
  new StateQueueError({
    message: `Failed to fetch reference-script UTxOs at ${referenceScriptsAddress}`,
    cause,
  });

const roleTokenUnit = (
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): Effect.Effect<string, StateQueueError> =>
  Effect.try({
    try: () => referenceScriptAuthUnit(authPolicy.policyId, target.name),
    catch: (cause) =>
      new ReferenceScriptResolutionError({
        message: "Unknown reference-script target",
        cause,
        reason: "reference-script-unknown-target",
      }),
  });

/**
 * Resolves each target from the UTxOs holding its own auth role token only.
 * A deployment's reference-script wallet holds hundreds of scripts; reading
 * all of them to pick a few decoded every script on each call (about 400 MB
 * of JS heap for a 945-script devnet wallet), which exhausted the settlement
 * worker's heap. The live resolution requires that role token anyway, so the
 * narrower read resolves exactly the same UTxO. Target sets above
 * {@link REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT} still read the wallet once.
 */
export const fetchReferenceScriptUtxosProgram = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  targets: readonly ReferenceScriptTarget[],
  authPolicy: ReferenceScriptAuthPolicyRef,
  providerRead: ReferenceScriptProviderRead = (_label, read) => read,
): Effect.Effect<readonly ReferenceScriptResolved[], StateQueueError> =>
  // Each read and the resolution of its result run as one provider step: a
  // read that succeeds without the holder is retried like a failed read.
  Effect.forEach(targets, (target) => roleTokenUnit(target, authPolicy)).pipe(
    Effect.flatMap((units) =>
      targets.length > REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT
        ? providerRead(
            `reference-script UTxO fetch at ${referenceScriptsAddress}`,
            Effect.tryPromise({
              try: () => lucid.utxosAt(referenceScriptsAddress),
              catch: fetchFailure(referenceScriptsAddress),
            }).pipe(
              Effect.flatMap((referenceScriptUtxos) =>
                Effect.forEach(targets, (target) =>
                  resolveReferenceScriptTarget(
                    referenceScriptUtxos,
                    referenceScriptsAddress,
                    target,
                    authPolicy,
                  ),
                ),
              ),
            ),
          )
        : Effect.forEach(
            targets,
            (target, index) =>
              providerRead(
                `reference-script UTxO fetch for ${target.name} at ${referenceScriptsAddress}`,
                Effect.tryPromise({
                  try: () =>
                    lucid.utxosAtWithUnit(
                      referenceScriptsAddress,
                      units[index]!,
                    ),
                  catch: fetchFailure(referenceScriptsAddress),
                }).pipe(
                  Effect.flatMap((candidates) =>
                    resolveReferenceScriptTarget(
                      candidates,
                      referenceScriptsAddress,
                      target,
                      authPolicy,
                    ),
                  ),
                ),
              ),
            { concurrency: REFERENCE_SCRIPT_FETCH_CONCURRENCY },
          ),
    ),
    Effect.mapError((cause) =>
      cause instanceof StateQueueError
        ? cause
        : new StateQueueError({
            message: "Failed to resolve required reference scripts",
            cause,
          }),
    ),
  );
