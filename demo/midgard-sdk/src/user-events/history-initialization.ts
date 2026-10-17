import {
  Data,
  type LucidEvolution,
  type TxBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { scriptRewardAddress } from "../cardano-addresses.js";
import { MAX_VALIDITY_RANGE_LENGTH_MS } from "../protocol-parameters.js";
import {
  requireInputIndex,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import {
  EVENT_HISTORY_MAX_PROTECTION_TIME,
  EventHistoryNode,
  EventHistoryObserve,
} from "./history.js";
import { type EventHistoryContractPair } from "./history-deployment.js";
import { eventHistoryMinimumNodeLovelace } from "./history-funding.js";

export type EventHistoryInitialization = {
  readonly contracts: EventHistoryContractPair;
  readonly nonce: UTxO;
  readonly validFrom: number;
  readonly validTo: number;
  readonly referenceScripts?: Readonly<{ deposit: UTxO; withdrawal: UTxO }>;
};

/** Compose both roots into the transaction which consumes the deployment nonce.
 * Register both list reward accounts before submitting this transaction.
 * Final transaction indexes are resolved after wallet funding and composition. */
export const appendEventHistoryInitialization = (
  lucid: LucidEvolution,
  tx: TxBuilder,
  params: EventHistoryInitialization,
): TxBuilder => {
  const network = lucid.config().network;
  if (network === undefined)
    throw new Error("History initialization requires a network");
  if (
    !Number.isSafeInteger(params.validFrom) ||
    !Number.isSafeInteger(params.validTo)
  )
    throw new Error("History initialization requires safe integer timestamps");
  const lower = BigInt(
    lucid.slotToUnixTime(lucid.unixTimeToSlot(params.validFrom)),
  );
  const upper =
    BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(params.validTo))) - 1n;
  if (
    lower < 0n ||
    upper < lower ||
    upper - lower > MAX_VALIDITY_RANGE_LENGTH_MS
  )
    throw new Error("Invalid history initialization validity interval");

  for (const [name, kind] of [
    ["deposit", "Deposit"],
    ["withdrawal", "Withdrawal"],
  ] as const) {
    const { recipe, list } = params.contracts[name];
    if (
      recipe.kind !== kind ||
      recipe.initializationNonce.transactionId !== params.nonce.txHash ||
      recipe.initializationNonce.outputIndex !==
        BigInt(params.nonce.outputIndex)
    )
      throw new Error(
        "History initialization must consume the declared shared nonce",
      );
    if (upper + recipe.protectionDurationMs > EVENT_HISTORY_MAX_PROTECTION_TIME)
      throw new Error(
        "History initialization exceeds the funded protection timestamp width",
      );
    const reference = params.referenceScripts?.[name];
    if (
      reference !== undefined &&
      (reference.scriptRef == null ||
        validatorToScriptHash(reference.scriptRef) !== list.policyId)
    )
      throw new Error(
        "History initialization reference script differs from its policy",
      );
  }

  tx.validFrom(params.validFrom).validTo(params.validTo);
  for (const name of ["deposit", "withdrawal"] as const) {
    const { recipe, list } = params.contracts[name];
    const node: EventHistoryNode = {
      position: "Root",
      next: null,
      protected_until: upper + recipe.protectionDurationMs,
      payload: "RootContent",
    };
    // Fund the largest future pointer/timestamp and an eight-byte ADA quantity.
    const lovelace = eventHistoryMinimumNodeLovelace(
      { lovelace: EVENT_HISTORY_MAX_PROTECTION_TIME, [list.policyId]: 1n },
      node,
    );
    tx.mintAssets({ [list.policyId]: 1n }, Data.void())
      .pay.ToContract(
        list.spendingScriptAddress,
        { kind: "inline", value: Data.to(node, EventHistoryNode) },
        { lovelace, [list.policyId]: 1n },
      )
      .withdraw(
        scriptRewardAddress(network, list.withdrawalScript),
        0n,
        (ctx) =>
          Data.to(
            {
              Initialize: {
                nonce_input_index: requireInputIndex(
                  ctx,
                  params.nonce,
                  "History initialization nonce",
                ),
                root_output_index: requireUniqueOutputIndex(
                  ctx.outputs,
                  (output) =>
                    output.address === list.spendingScriptAddress &&
                    output.assets[list.policyId] === 1n,
                  "History root",
                ),
              },
            },
            EventHistoryObserve,
          ),
      );
    const reference = params.referenceScripts?.[name];
    if (reference === undefined) tx.attach.Script(list.mintingScript);
    else tx.readFrom([reference]);
  }
  return tx;
};
