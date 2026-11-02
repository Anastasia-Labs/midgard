import * as SDK from "@al-ft/midgard-sdk";
import {
  createReferenceScriptAuthPolicy,
  type ReferenceScriptAuthPolicy,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { ReferenceScriptCommandName } from "../deployable-scripts.js";
import { RunStateError } from "../e2e/run-state.js";
import type { IntentJournal } from "../services/intent-journal.js";
import { formatAda } from "../transactions/operators/funding-preflight.js";
import {
  referencePublicationDurationEstimateMs,
  type ReferencePublicationOptions,
  referencePublicationOptions,
  referenceScriptDepositLovelace,
} from "../transactions/reference-publication.js";
import {
  deploymentInitializationNonceOutRefKeys,
  fetchReferenceScriptUtxosAt,
  nodeRuntimeReferenceScriptTargets,
  referencePublicationPlainBalanceRequired,
  referenceScriptTargetsByCommand,
  referenceScriptWalletWorkingCapital,
  utxoOutRefKey,
} from "../transactions/reference-scripts.js";
import { awaitWalletViewUtxos } from "../transactions/reference-scripts.wallet-view-utxos.js";
import { isPlainAdaOnlyUtxo } from "../transactions/wallet-hygiene.js";
import {
  recordedReferenceScriptAuthPolicyProgram,
  type ReferenceScriptAuthPolicyDeployment,
  resolveReferenceScriptAuthPolicyProgram,
} from "./deployment-run-state.resolve-reference-script-auth-policy-program.js";

const minutes = (ms: number): string => `${Math.floor(ms / 60_000)} min`;

const chainNowMs = (lucid: LucidEvolution): number =>
  lucid.slotToUnixTime(lucid.currentSlot());

/**
 * How many reference-script auth tokens of `policy` sit at the publication
 * address once nothing can still mint under it. Publication never sends
 * under a policy with `minRemainingMs` or less left, so every transaction
 * it sent is past its validity once that point plus one validity window
 * has passed; the canonical view is read after it.
 */
const publishedUnderPolicy = (
  lucid: LucidEvolution,
  address: string,
  policy: ReferenceScriptAuthPolicy,
  minRemainingMs: number,
  publication: ReferencePublicationOptions,
): Effect.Effect<number, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const settledAtMs =
      policy.expiresAtUnixTime -
      minRemainingMs +
      SDK.REFERENCE_SCRIPT_PUBLICATION_VALIDITY_MS;
    yield* Effect.tryPromise({
      try: async () => {
        while (
          lucid.slotToUnixTime(await publication.synchronize()) < settledAtMs
        )
          await (publication.wait?.() ??
            new Promise((resolve) => setTimeout(resolve, 1_000)));
      },
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to synchronize past the recorded auth policy's last publication window",
          cause,
        }),
    });
    const utxos = yield* fetchReferenceScriptUtxosAt(
      lucid,
      address,
      `reference-script auth policy ${policy.policyId} UTxO fetch at ${address}`,
      `Failed to read what was published under auth policy ${policy.policyId}`,
    );
    return new Set(
      utxos.flatMap((utxo) =>
        Object.keys(utxo.assets).filter((unit) =>
          unit.startsWith(policy.policyId),
        ),
      ),
    ).size;
  });

/**
 * Refuses to create an auth policy whose window the publication of
 * `targets` cannot fit or the reference-script wallet cannot fund: its
 * plain balance must cover every deposit, fee and buffer publication will
 * spend, and the estimated publication time must fit between the policy's
 * creation and the minimum it must keep left.
 */
const assertPublicationFitsNewPolicy = ({
  lucid,
  address,
  targets,
  authPolicy,
  reserved,
  timelockDurationMs,
  minRemainingMs,
  mode,
}: {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly targets: readonly SDK.ReferenceScriptTarget[];
  readonly authPolicy: ReferenceScriptAuthPolicy;
  readonly reserved: ReadonlySet<string>;
  readonly timelockDurationMs: number;
  readonly minRemainingMs: number;
  readonly mode: ReferencePublicationOptions["mode"];
}): Effect.Effect<void, SDK.StateQueueError, IntentJournal> =>
  Effect.gen(function* () {
    const plain = (yield* awaitWalletViewUtxos(lucid, {
      scopeName: "reference-script auth policy creation",
      failureMessage:
        "Failed to read the reference-script wallet before creating an auth policy",
    })).filter(
      (utxo) => isPlainAdaOnlyUtxo(utxo) && !reserved.has(utxoOutRefKey(utxo)),
    );
    const balance = plain.reduce((sum, utxo) => sum + utxo.assets.lovelace, 0n);
    const required = referencePublicationPlainBalanceRequired(
      lucid,
      address,
      targets,
      authPolicy,
      mode,
    );
    const needed = referenceScriptWalletWorkingCapital(
      required.publication,
      required.preparationFeeAllowance,
      plain.length,
    );
    if (balance < needed) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: `Refusing to create a reference-script auth policy: the reference-script wallet holds ${formatAda(balance)} of plain ADA, but publishing its ${targets.length.toString()} reference scripts needs ${formatAda(needed)}; short by ${formatAda(needed - balance)}. Fund the wallet and retry; nothing was created.`,
          cause: `plain_balance=${balance.toString()},required=${needed.toString()}`,
        }),
      );
    }
    const estimatedMs = referencePublicationDurationEstimateMs(
      lucid,
      targets,
      mode,
    );
    const availableMs = timelockDurationMs - minRemainingMs;
    if (estimatedMs > availableMs) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: `Refusing to create a reference-script auth policy: publishing its ${targets.length.toString()} reference scripts is estimated to take ${minutes(estimatedMs)}, but the policy leaves ${minutes(availableMs)} (a ${minutes(timelockDurationMs)} timelock less the ${minutes(minRemainingMs)} it must keep left); nothing was created.`,
          cause: `estimated_ms=${estimatedMs.toString()},available_ms=${availableMs.toString()}`,
        }),
      );
    }
  });

/**
 * The auth policy the deployment publishes reference scripts under, and the
 * contracts built over it. A recorded policy with more than
 * `minRemainingMs` left is used as is. One with less is replaced by a new
 * policy only while nothing was published under it; otherwise the command
 * refuses, since a new policy changes every contract and so means
 * republishing all of them, which only `--fresh-redeploy` chooses. The
 * replacement is recorded as a fresh redeploy is, with its reason. No new
 * policy (first, replacement or fresh redeploy) is recorded unless the
 * reference-script wallet can fund its whole publication and the
 * publication fits its window.
 */
export const resolvePublishableReferenceScriptAuthPolicyProgram = <E>({
  deployment,
  lucid,
  referenceScriptsAddress,
  commandName,
  timelockDurationMs,
  minRemainingMs,
  contractsFor,
  publication = referencePublicationOptions(lucid),
}: {
  readonly deployment: ReferenceScriptAuthPolicyDeployment;
  /** Selects the reference-script wallet, which signs the policy. */
  readonly lucid: LucidEvolution;
  readonly referenceScriptsAddress: string;
  readonly commandName: ReferenceScriptCommandName;
  readonly timelockDurationMs: number;
  readonly minRemainingMs: number;
  readonly contractsFor: (
    policy: ReferenceScriptAuthPolicy,
  ) => Effect.Effect<SDK.MidgardValidators, E>;
  readonly publication?: ReferencePublicationOptions;
}): Effect.Effect<
  {
    readonly authPolicy: ReferenceScriptAuthPolicy;
    readonly contracts: SDK.MidgardValidators;
  },
  E | RunStateError | SDK.StateQueueError,
  IntentJournal
> =>
  Effect.gen(function* () {
    const recorded =
      yield* recordedReferenceScriptAuthPolicyProgram(deployment);
    const remainingMs =
      recorded === null
        ? undefined
        : SDK.referenceScriptAuthRemainingMs(
            recorded.policy,
            chainNowMs(lucid),
          );
    if (
      recorded !== null &&
      remainingMs !== undefined &&
      remainingMs > minRemainingMs
    ) {
      const authPolicy = yield* resolveReferenceScriptAuthPolicyProgram({
        ...deployment,
        lucid,
        timelockDurationMs,
      });
      return { authPolicy, contracts: yield* contractsFor(authPolicy) };
    }
    let reason: string | undefined;
    if (recorded !== null) {
      const { policyId } = recorded.policy;
      const published = recorded.inManifest
        ? undefined
        : yield* publishedUnderPolicy(
            lucid,
            referenceScriptsAddress,
            recorded.policy,
            minRemainingMs,
            publication,
          );
      if (published !== 0) {
        const roster = nodeRuntimeReferenceScriptTargets(
          yield* contractsFor(recorded.policy),
        );
        const locked = roster.reduce(
          (sum, target) =>
            sum +
            referenceScriptDepositLovelace(
              lucid,
              referenceScriptsAddress,
              target,
              recorded.policy,
            ),
          0n,
        );
        return yield* Effect.fail(
          new RunStateError(
            `Reference-script auth policy ${policyId} has ${minutes(remainingMs ?? 0)} left, at or below the ${minutes(minRemainingMs)} minimum publication needs, and ${
              published === undefined
                ? "the deployment manifest records it"
                : `${published.toString()} reference scripts are already published under it`
            }. A new policy changes every contract, so redoing means republishing all ${roster.length.toString()} reference scripts, which locks ${formatAda(locked)} in their outputs (plus fees). To choose that, rerun with --fresh-redeploy --fresh-redeploy-reason <reason>.`,
          ),
        );
      }
      reason = `auth policy ${policyId} had ${minutes(remainingMs ?? 0)} left (minimum ${minutes(minRemainingMs)}) and nothing was published under it; replaced automatically`;
    }
    const candidate = yield* Effect.tryPromise({
      try: () =>
        createReferenceScriptAuthPolicy(
          lucid,
          chainNowMs(lucid),
          timelockDurationMs,
        ),
      catch: (cause) =>
        new RunStateError("Failed to prepare a reference-script auth policy.", {
          cause,
        }),
    });
    const contracts = yield* contractsFor(candidate);
    yield* assertPublicationFitsNewPolicy({
      lucid,
      address: referenceScriptsAddress,
      targets: referenceScriptTargetsByCommand(contracts)[commandName],
      authPolicy: candidate,
      reserved: deploymentInitializationNonceOutRefKeys(contracts),
      timelockDurationMs,
      minRemainingMs,
      mode: publication.mode,
    });
    const authPolicy = yield* resolveReferenceScriptAuthPolicyProgram({
      ...deployment,
      lucid,
      timelockDurationMs,
      replacement: {
        policy: candidate,
        replacesPolicyId: recorded?.policy.policyId ?? null,
        ...(reason === undefined ? {} : { reason }),
      },
    });
    return { authPolicy, contracts };
  });
