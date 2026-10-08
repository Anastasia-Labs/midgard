import * as SDK from "@al-ft/midgard-sdk";
import {
  LucidEvolution,
  type Network,
  toUnit,
  type TxBuilder,
  type TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { slotToUnixTimeForLucidOrEmulatorFallback } from "../lucid-time.js";
import {
  type IntentJournal,
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

/**
 * Returns whether the canonical DA params UTxO is already present on-chain.
 */
export const isDaParamsInitialized = (
  lucid: LucidEvolution,
  daParamsGovernor: SDK.AuthenticatedValidator,
): Effect.Effect<boolean, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () => {
      const daParamsUtxos = await lucid.utxosAtWithUnit(
        daParamsGovernor.spendingScriptAddress,
        SDK.daParamsUnit(daParamsGovernor),
      );
      return daParamsUtxos.length > 0;
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to query DA params initialization state",
        cause,
      }),
  });

/**
 * Returns whether the state-queue root NFT is already present at the queue
 * address: one exact-unit lookup, never a scan of the address.
 */
export const isStateQueueInitialized = (
  lucid: LucidEvolution,
  stateQueue: SDK.AuthenticatedValidator,
): Effect.Effect<boolean, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () =>
      (
        await lucid.utxosAtWithUnit(
          stateQueue.spendingScriptAddress,
          toUnit(stateQueue.policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME),
        )
      ).length > 0,
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to query state-queue initialization state",
        cause,
      }),
  });

/**
 * Returns whether the DA bond pool NFT is already present at the pool address.
 */
export const isDaBondPoolInitialized = (
  lucid: LucidEvolution,
  daBondPool: SDK.AuthenticatedValidator,
): Effect.Effect<boolean, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () =>
      (
        await lucid.utxosAtWithUnit(
          daBondPool.spendingScriptAddress,
          SDK.daBondPoolUnit(daBondPool.policyId),
        )
      ).length > 0,
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to query DA bond pool initialization state",
        cause,
      }),
  });

/**
 * Resolves the configured one-shot hub-oracle nonce UTxO from the operator
 * wallet.
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
): Effect.Effect<UTxO, SDK.LucidError> =>
  Effect.gen(function* () {
    const walletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch operator wallet UTxOs for initialization",
          cause,
        }),
    });
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
 * output carries the state-queue policy. Before the follower starts (and in
 * a CLI process) it goes out unjournaled (`no_follower`).
 */
export const completeAndSubmit = (
  lucid: LucidEvolution,
  txBuilder: TxBuilder,
  failureMessage: string,
  nonce: Pick<UTxO, "txHash" | "outputIndex">,
): Effect.Effect<
  string,
  SDK.LucidError | TxConfirmError | TxSignError | TxSubmitError,
  IntentJournal
> =>
  Effect.gen(function* () {
    const unsignedTx = yield* Effect.tryPromise({
      try: () => txBuilder.complete({ localUPLCEval: true }),
      catch: (cause) =>
        new SDK.LucidError({
          message: `${failureMessage}: ${String(cause)}`,
          cause,
        }),
    });
    return yield* handleSignSubmit(
      lucid,
      unsignedTx as TxSignBuilder,
      journaledIntent(
        "list_insert",
        `list_insert:protocol_init:${nonce.txHash}#${nonce.outputIndex.toString()}`,
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

export const makePartialProtocolDeploymentError = (
  status: ProtocolDeploymentStatus,
): SDK.LucidError =>
  new SDK.LucidError({
    message:
      "Real protocol deployment is partial and cannot be completed in-place",
    cause: `missing_components=[${status.missingComponents.join(",")}]; the canonical Init validators require one atomic bootstrap transaction that mints the hub-oracle NFT and protocol root NFTs together; use a fresh one-shot hub-oracle nonce/deployment`,
  });

export type ProtocolDeploymentStatus = {
  readonly hubOracleWitness: UTxO | null;
  readonly correctionLockWitness: SDK.CorrectionLockUTxO | null;
  /** The state-queue root NFT is live (its queue's health is P1's, not init's). */
  readonly stateQueueInitialized: boolean;
  readonly depositHistoryInitialized: boolean;
  readonly withdrawalHistoryInitialized: boolean;
  readonly daParamsInitialized: boolean;
  readonly daBondPoolInitialized: boolean;
  readonly schedulerInitialized: boolean;
  readonly registeredOperatorsInitialized: boolean;
  readonly activeOperatorsInitialized: boolean;
  readonly retiredOperatorsInitialized: boolean;
  readonly fraudProofCatalogueInitialized: boolean;
  readonly phasMembershipRewardAddress: string;
  readonly phasMembershipScriptHash: string;
  readonly complete: boolean;
  readonly empty: boolean;
  readonly missingComponents: readonly string[];
};

export const fetchHistoryRootState = (
  lucid: LucidEvolution,
  contracts: SDK.EventHistoryContracts,
) =>
  Effect.tryPromise({
    try: async () => {
      const nodes = SDK.authenticateHistoryNodes(
        await lucid.utxosAt(contracts.list.spendingScriptAddress),
        {
          policyId: contracts.list.policyId,
          address: contracts.list.spendingScriptAddress,
          retentionAddress: contracts.retention.spendingScriptAddress,
          inlineLimitBytes: contracts.recipe.inlineLimitBytes,
        },
      );
      return {
        initialized: nodes.some(({ key }) => key === null),
        empty: nodes.length === 0,
      };
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to authenticate history deployment roots",
        cause,
      }),
  });
