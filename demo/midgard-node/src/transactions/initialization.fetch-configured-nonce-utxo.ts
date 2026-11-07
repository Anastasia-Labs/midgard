import * as SDK from "@al-ft/midgard-sdk";
import {
  LucidEvolution,
  type Network,
  type TxBuilder,
  type TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type InitProtocolAttempt } from "../e2e/run-state.js";
import { slotToUnixTimeForLucidOrEmulatorFallback } from "../lucid-time.js";
import {
  type IntentJournal,
  type IntentPlan,
  journaledIntent,
} from "../services/intent-journal.js";
import { outRefLabel } from "../tx-context.js";
import {
  DEFAULT_DEPLOYMENT_VALIDITY_BACKOFF_MS,
  DEFAULT_DEPLOYMENT_VALIDITY_WINDOW_MS,
} from "./initialization.atomic-protocol-init-reference-scripts-from-publications.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.js";
import {
  readSelectedWalletView,
  readSelectedWalletViewInputs,
} from "./utils.wallet-view.js";

/**
 * Resolves the configured one-shot hub-oracle nonce UTxO from the operator
 * wallet's view: absent while a live own intent (an initialization already
 * sent) holds it.
 */
export const fetchConfiguredNonceUtxo = (
  lucid: LucidEvolution,
  nodeConfig: {
    HUB_ORACLE_ONE_SHOT_TX_HASH: string;
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: number;
    L1_OPERATOR_SEED_PHRASE: string;
    NETWORK: Network;
    DA_COMMITTEE_HEX?: string;
    DA_THRESHOLD?: bigint | null;
  },
): Effect.Effect<UTxO, SDK.LucidError, IntentJournal> =>
  Effect.gen(function* () {
    const view = yield* readSelectedWalletView(lucid).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.LucidError({
            message:
              "Failed to read the operator wallet view for initialization",
            cause,
          }),
      ),
    );
    const walletUtxos = view.utxos;
    const configuredNonceUtxoLabel = `${nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH}#${nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX}`;
    const nonceUtxo = walletUtxos.find(
      (utxo) =>
        utxo.txHash === nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH &&
        utxo.outputIndex === nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
    );
    if (nonceUtxo === undefined) {
      const availableWalletUtxos = walletUtxos.map((utxo) => outRefLabel(utxo));
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Configured one-shot hub oracle UTxO is not available in the operator wallet",
          cause: `required=${configuredNonceUtxoLabel}, available=[${availableWalletUtxos.join(", ")}]`,
        }),
      );
    }
    return nonceUtxo;
  });

/**
 * Completes, signs, and submits the atomic protocol initialization with
 * local UPLC evaluation enforced. It creates every protocol list's root, so
 * it is journaled as a list insert keyed by the one-shot nonce it spends
 * (`list_insert:protocol_init:<nonce outref>`): S6 resends it while no live
 * output carries the state-queue policy, under `plan`, opened before the
 * initialization's first L1 read (S5). Before the follower starts (and in a
 * CLI process) it goes out unjournaled (`no_follower`). `recordSubmission`
 * receives the transaction's hash and validity bound before anything is
 * sent.
 */
export const completeAndSubmit = (
  lucid: LucidEvolution,
  txBuilder: TxBuilder,
  failureMessage: string,
  nonce: Pick<UTxO, "txHash" | "outputIndex">,
  plan: IntentPlan,
  recordSubmission: (
    attempt: InitProtocolAttempt,
  ) => Effect.Effect<void, SDK.LucidError>,
): Effect.Effect<
  string,
  SDK.LucidError | TxConfirmError | TxSignError | TxSubmitError,
  IntentJournal
> =>
  Effect.gen(function* () {
    const presetWalletInputs = yield* readSelectedWalletViewInputs(
      lucid,
      "protocol initialization",
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.LucidError({
            message: `${failureMessage}: ${cause.message}`,
            cause,
          }),
      ),
    );
    const unsignedTx = yield* Effect.tryPromise({
      try: () =>
        txBuilder.complete({ localUPLCEval: true, presetWalletInputs }),
      catch: (cause) =>
        new SDK.LucidError({
          message: `${failureMessage}: ${String(cause)}`,
          cause,
        }),
    });
    const ttl = unsignedTx.toTransaction().body().ttl();
    if (ttl === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: `${failureMessage}: the transaction has no validity upper bound`,
          cause: unsignedTx.toHash(),
        }),
      );
    }
    yield* recordSubmission({
      txHash: unsignedTx.toHash(),
      invalidHereafterSlot: Number(ttl),
    });
    return yield* handleSignSubmit(
      lucid,
      unsignedTx as TxSignBuilder,
      journaledIntent(
        "list_insert",
        `list_insert:protocol_init:${nonce.txHash}#${nonce.outputIndex.toString()}`,
        plan,
      ),
    );
  });

/**
 * Produces a conservative default validity start time for deployment
 * transactions.
 */
const resolveDeploymentStartTime = (lucid?: LucidEvolution): bigint => {
  if (lucid === undefined) {
    return BigInt(Date.now() - DEFAULT_DEPLOYMENT_VALIDITY_BACKOFF_MS);
  }
  const currentTime = slotToUnixTimeForLucidOrEmulatorFallback(
    lucid,
    lucid.currentSlot(),
  );
  const firstSlotTime = slotToUnixTimeForLucidOrEmulatorFallback(lucid, 0);
  return BigInt(
    Math.max(
      firstSlotTime,
      currentTime - DEFAULT_DEPLOYMENT_VALIDITY_BACKOFF_MS,
    ),
  );
};

export const resolveDefaultDeploymentDeadline = (
  lucid?: LucidEvolution,
): bigint => {
  const targetTime = Number(
    resolveDeploymentStartTime(lucid) + DEFAULT_DEPLOYMENT_VALIDITY_WINDOW_MS,
  );
  if (lucid === undefined) {
    return BigInt(targetTime);
  }
  const targetSlot = lucid.unixTimeToSlot(targetTime);
  const alignedTime = slotToUnixTimeForLucidOrEmulatorFallback(
    lucid,
    targetSlot,
  );
  if (alignedTime >= targetTime) {
    return BigInt(alignedTime);
  }
  return BigInt(
    slotToUnixTimeForLucidOrEmulatorFallback(lucid, targetSlot + 1),
  );
};

export const resolveDeploymentValidityBounds = (
  lucid?: LucidEvolution,
  validTo?: bigint,
): { validFrom: bigint; validTo: bigint } => {
  if (validTo !== undefined) {
    return {
      validFrom: validTo - DEFAULT_DEPLOYMENT_VALIDITY_WINDOW_MS,
      validTo,
    };
  }
  const upperBound = resolveDefaultDeploymentDeadline(lucid);
  return {
    validFrom: resolveDeploymentStartTime(lucid),
    validTo: upperBound,
  };
};
