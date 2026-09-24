import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type Assets,
  Data,
  getAddressDetails,
  type LucidEvolution,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type CredentialD, type MidgardValidators } from "../common.js";
import { fetchHubOracleUTxOProgram } from "../hub-oracle.js";
import {
  EVENT_HISTORY_MAX_PROTECTION_TIME,
  EventHistoryNode,
  type EventHistoryRecipe,
} from "./history.js";
import {
  type EventHistoryBuildContext,
  type EventHistoryPayloadInput,
} from "./history-build.js";
import { requireEventHistoryContracts } from "./history-deployment.js";
import {
  assertEventHistoryAdmissionFunding,
  eventHistoryMinimumNodeLovelace,
  eventHistoryMinimumOutputLovelace,
  eventHistoryWithdrawalFunding,
} from "./history-funding.js";
import {
  prepareEventHistoryPayload,
  prepareEventHistoryPayloadCbor,
} from "./history-payload.js";
import { resolveUserEventValidTo, UserEventBuildError } from "./internals.js";

export type UserHistoryContracts = Pick<
  MidgardValidators,
  "hubOracle" | "deposit" | "withdrawal" | "eventHistory"
>;

/** Unsigned callers retain the selected nonce before publishing data and supply
 * that same nonce and confirmed data output when building admission. */
export type UserHistoryBuildOptions = {
  readonly nonceInput?: Pick<UTxO, "txHash" | "outputIndex">;
  readonly externalData?: UTxO;
  readonly reclaimAuth?: CredentialD;
  readonly structuralRefundKey?: string;
  readonly validity?: { readonly validFrom: number; readonly validTo: number };
};

export const historyUserBuildError = (cause: unknown): UserEventBuildError =>
  new UserEventBuildError({
    message: `Failed to build authenticated event history transaction: ${String(cause)}`,
    cause,
  });

export const prepareUserHistoryContextProgram = (
  lucid: LucidEvolution,
  contracts: UserHistoryContracts,
  kind: EventHistoryRecipe["kind"],
  options: UserHistoryBuildOptions,
  scriptReference?: UTxO,
) =>
  Effect.gen(function* () {
    const prepared = yield* Effect.tryPromise({
      try: async () => {
        const pair = requireEventHistoryContracts(contracts);
        const history = kind === "Deposit" ? pair.deposit : pair.withdrawal;
        const event =
          kind === "Deposit" ? contracts.deposit : contracts.withdrawal;
        if (
          history.recipe.kind !== kind ||
          history.recipe.hubPolicyId !== contracts.hubOracle.policyId ||
          history.list.policyId !== event.policyId ||
          history.list.spendingScriptAddress !== event.spendingScriptAddress
        )
          throw new Error(
            "History deployment recipe does not match the configured event contract",
          );
        const network = lucid.config().network;
        if (network === undefined)
          throw new Error("Missing Cardano network for history submission");
        const walletAddress = await lucid.wallet().address();
        const credential = getAddressDetails(walletAddress).paymentCredential;
        if (credential === undefined)
          throw new Error("History funding wallet has no payment credential");
        const structuralRefundKey =
          options.structuralRefundKey ??
          (credential.type === "Key" ? credential.hash : undefined);
        if (
          structuralRefundKey === undefined ||
          !/^[0-9a-f]{56}$/u.test(structuralRefundKey)
        )
          throw new Error(
            "History structural funding requires an exact key refund credential",
          );
        const reclaimAuth: CredentialD =
          options.reclaimAuth ??
          (credential.type === "Key"
            ? { PublicKeyCredential: [credential.hash] }
            : { ScriptCredential: [credential.hash] });
        const walletInputs = (await lucid.wallet().getUtxos())
          .filter(
            (input) =>
              input.datum == null &&
              input.datumHash == null &&
              input.scriptRef == null &&
              !Object.keys(input.assets).some(
                (unit) =>
                  unit.startsWith(pair.deposit.list.policyId) ||
                  unit.startsWith(pair.withdrawal.list.policyId),
              ),
          )
          .sort(compareOutRefs);
        const requestedNonce = options.nonceInput;
        const nonce =
          requestedNonce === undefined
            ? walletInputs[0]
            : walletInputs.find(
                (input) => outRefLabel(input) === outRefLabel(requestedNonce),
              );
        if (nonce === undefined)
          throw new Error(
            "History nonce must be an available plain funding-wallet UTxO",
          );
        const applied: EventHistoryBuildContext["applied"] = {
          validator: history.list.spendingScript,
          policyId: history.list.policyId,
          address: history.list.spendingScriptAddress,
          rewardAddress: validatorToRewardAddress(
            network,
            history.list.withdrawalScript,
          ),
          retention: {
            validator: history.retention.spendingScript,
            address: history.retention.spendingScriptAddress,
          },
        };
        return {
          network,
          history,
          applied,
          nonce,
          fundingInputs: walletInputs.filter(
            (input) => outRefLabel(input) !== outRefLabel(nonce),
          ),
          reclaimAuth,
          structuralRefundKey,
        };
      },
      catch: historyUserBuildError,
    });
    const hub = yield* fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: contracts.hubOracle.spendingScriptAddress,
      hubOraclePolicyId: contracts.hubOracle.policyId,
    });
    const context: EventHistoryBuildContext = {
      lucid,
      applied: prepared.applied,
      recipe: prepared.history.recipe,
      hubReference: hub.utxo,
      scriptReference,
      fundingInputs: prepared.fundingInputs,
    };
    return { ...prepared, context };
  });

/** Use the largest funded encoding width, so choosing an amount cannot itself
 * increase the amount needed. Deposited Value is never increased by this quote. */
export const quoteUserHistoryFunding = ({
  context,
  payload,
  payloadCbor,
  reclaimAuth,
  structuralRefundKey,
  originalAssets,
  structuralLovelace,
  withdrawalLovelace,
}: {
  readonly context: EventHistoryBuildContext;
  readonly reclaimAuth: CredentialD;
  readonly structuralRefundKey: string;
  readonly originalAssets?: Assets;
  readonly structuralLovelace?: bigint;
  readonly withdrawalLovelace?: bigint;
} & EventHistoryPayloadInput) => {
  if (payload !== undefined && payloadCbor !== undefined)
    throw new Error("History payload must have exactly one encoding source");
  const plan =
    payloadCbor === undefined
      ? prepareEventHistoryPayload(payload, reclaimAuth, context.recipe)
      : prepareEventHistoryPayloadCbor(
          payloadCbor,
          reclaimAuth,
          context.recipe,
        );
  const deposit = "DepositPayload" in plan.payload;
  const id =
    "DepositPayload" in plan.payload
      ? plan.payload.DepositPayload.event.id
      : plan.payload.WithdrawalPayload.event.id;
  const envelope: EventHistoryNode = {
    position: { Key: [plan.key] },
    next: null,
    protected_until: EVENT_HISTORY_MAX_PROTECTION_TIME,
    payload: {
      Order: {
        facts: {
          event_id: id,
          inclusion_time: EVENT_HISTORY_MAX_PROTECTION_TIME,
          location: plan.location,
          structural_lovelace: deposit ? EVENT_HISTORY_MAX_PROTECTION_TIME : 0n,
          structural_refund_key: structuralRefundKey,
        },
      },
    },
  };
  const unit = context.applied.policyId + plan.key;
  const maximumAssets = {
    ...originalAssets,
    lovelace: EVENT_HISTORY_MAX_PROTECTION_TIME,
    [unit]: 1n,
  };
  const envelopeCbor = () =>
    plan.kind === "Inline"
      ? replacePlutusConstrFieldCbor(
          Data.to(envelope, EventHistoryNode),
          [3, 0, 2, 0],
          plan.payloadCbor,
        )
      : Data.to(envelope, EventHistoryNode);
  const nodeMinimum = eventHistoryMinimumNodeLovelace(
    maximumAssets,
    envelopeCbor(),
  );
  let structural: bigint;
  let assets: Assets;
  if (deposit) {
    if (originalAssets === undefined)
      throw new Error("Deposit original Value is required");
    const refundMinimum = eventHistoryMinimumOutputLovelace(
      { lovelace: EVENT_HISTORY_MAX_PROTECTION_TIME },
      "NoDatum",
    );
    const deficit = nodeMinimum - (originalAssets.lovelace ?? 0n);
    structural =
      structuralLovelace ??
      (deficit <= 0n ? 0n : deficit > refundMinimum ? deficit : refundMinimum);
    assets = {
      ...originalAssets,
      lovelace: (originalAssets.lovelace ?? 0n) + structural,
    };
  } else {
    const minimum = eventHistoryWithdrawalFunding(
      plan.payloadCbor,
      EVENT_HISTORY_MAX_PROTECTION_TIME,
    );
    const needed = [
      nodeMinimum,
      minimum.payoutMinimum,
      minimum.refundMinimum,
    ].reduce((left, right) => (left > right ? left : right));
    structural = 0n;
    assets = { lovelace: withdrawalLovelace ?? needed };
  }
  if (envelope.payload === "RootContent" || !("Order" in envelope.payload))
    throw new Error("Expected Order funding envelope");
  envelope.payload.Order.facts.structural_lovelace = structural;
  assertEventHistoryAdmissionFunding(
    envelopeCbor(),
    { ...assets, [unit]: 1n },
    context.applied.policyId,
    plan.payloadCbor,
    structural,
  );
  return { plan, assets, structuralLovelace: structural };
};

export const userHistoryValidity = (
  lucid: LucidEvolution,
  options: UserHistoryBuildOptions,
) =>
  options.validity ?? {
    validFrom: Date.now() - 60_000,
    validTo: resolveUserEventValidTo(lucid),
  };
