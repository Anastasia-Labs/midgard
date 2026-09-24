import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { LedgerSnapshotOutput } from "../l1-ledger-snapshot.js";

/** Validates target evidence from an exact-point ledger capture. The caller
 * owns source authentication and generation fencing; this does not establish
 * signed-transaction absence or authorize a rollback. */
export const validateRecoveryStateQueue = ({
  outputs,
  contracts,
  expectedBase,
}: {
  readonly outputs: readonly LedgerSnapshotOutput[];
  readonly contracts: Pick<SDK.MidgardValidators, "stateQueue">;
  readonly expectedBase: {
    readonly outRef: string;
    readonly datumCbor: string;
    readonly utxosRoot: string;
  };
}): Effect.Effect<
  { readonly utxosRoot: string; readonly queueUTxO: SDK.StateQueueUTxO },
  SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const utxo = yield* Effect.try({
      try: (): UTxO => {
        const seen = new Set<string>();
        for (const output of outputs) {
          const outRef = `${output.txHash}#${output.outputIndex.toString()}`;
          if (seen.has(outRef))
            throw new Error("Recovery snapshot contains duplicate outrefs");
          seen.add(outRef);
        }
        const candidates = outputs.filter((output) =>
          Object.keys(output.assets).some((unit) =>
            unit.startsWith(contracts.stateQueue.policyId),
          ),
        );
        if (candidates.length !== 1)
          throw new Error("Recovery requires exactly one state-queue output");
        const output = candidates[0];
        if (output.address !== contracts.stateQueue.spendingScriptAddress)
          throw new Error("Recovery state queue has a foreign address");
        if (
          output.hasReferenceScript ||
          output.datum === undefined ||
          output.datumHash !== undefined
        )
          throw new Error("Recovery state queue requires only an inline datum");
        if (
          `${output.txHash}#${output.outputIndex.toString()}` !==
            expectedBase.outRef ||
          output.datum !== expectedBase.datumCbor
        )
          throw new Error("Recovery state queue does not match the exact base");
        return {
          txHash: output.txHash,
          outputIndex: output.outputIndex,
          address: output.address,
          assets: { ...output.assets },
          datum: output.datum,
        };
      },
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Invalid recovery state-queue snapshot",
          cause,
        }),
    });
    const queueUTxO = yield* SDK.utxoToStateQueueUTxO(
      utxo,
      contracts.stateQueue.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: "Failed to authenticate recovery state queue",
            cause,
          }),
      ),
    );
    if (
      queueUTxO.assetName !== SDK.STATE_QUEUE_ROOT_ASSET_NAME ||
      queueUTxO.datum.key !== "Empty" ||
      queueUTxO.datum.next !== "Empty"
    )
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Recovery requires a single confirmed state-queue root",
          cause: undefined,
        }),
      );
    const confirmed = yield* SDK.getConfirmedStateFromStateQueueDatum(
      queueUTxO.datum,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: "Invalid recovery confirmed-state datum",
            cause,
          }),
      ),
    );
    if (confirmed.data.utxoRoot !== expectedBase.utxosRoot)
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Recovery confirmed root does not match the exact base",
          cause: undefined,
        }),
      );
    return { utxosRoot: confirmed.data.utxoRoot, queueUTxO };
  });
