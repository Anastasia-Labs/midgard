import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { type OperatorLifecycleTxHashes } from "./register-active-operator.fetch-hub-oracle-ref-input.js";

export const toLifecycleResult = (
  txHashes: OperatorLifecycleTxHashes,
): Effect.Effect<OperatorLifecycleTxHashes, never> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `Operator lifecycle result: registerTxHash=${txHashes.registerTxHash ?? "skipped"}, activateTxHash=${txHashes.activateTxHash ?? "skipped"}, deregisterTxHash=${txHashes.deregisterTxHash ?? "skipped"}`,
    );
    return txHashes;
  });

/**
 * Activation on behalf of another operator. On-chain activation is
 * permissionless, so the wallet behind `lucid` only pays the fee while the
 * registered bond moves into the activated node unchanged. Registration and
 * deregistration still require the operator's own wallet.
 */
export type PermissionlessActivation = {
  readonly operatorKeyHash: string;
};

/**
 * A refusal decided from the directory before anything is built or spent:
 * the operator is already in the directory, is not registered when the flow
 * needs it to be, or its activation time has not arrived. Shares the
 * `StateQueueError` tag so the programs' error unions are unchanged; the CLI
 * prints it as one line.
 */
export class OperatorRegistrationRefusal extends SDK.StateQueueError {}

/** ISO-8601 UTC followed by the raw POSIX milliseconds, for refusal messages. */
export const describePosixTime = (posixMs: bigint): string =>
  `${new Date(Number(posixMs)).toISOString()} (${posixMs.toString()})`;
