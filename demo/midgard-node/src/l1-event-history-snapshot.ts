import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type AcquiredLedgerSnapshot,
  type LedgerSnapshotOutput,
  readAcquiredLedgerSnapshot,
} from "./l1-ledger-snapshot.js";

export type EventHistoryLedgerSnapshot = Readonly<{
  ledger: AcquiredLedgerSnapshot;
  deposits: readonly SDK.DepositUTxO[];
  withdrawals: readonly SDK.WithdrawalUTxO[];
}>;

export type NodeEventHistoryDeployments = Readonly<{
  deposit: SDK.EventHistoryDeployment;
  withdrawal: SDK.EventHistoryDeployment;
}>;

const withoutReferenceScript = (output: LedgerSnapshotOutput): UTxO => ({
  txHash: output.txHash,
  outputIndex: output.outputIndex,
  address: output.address,
  assets: { ...output.assets },
  ...(output.datum === undefined ? {} : { datum: output.datum }),
  ...(output.datumHash === undefined ? {} : { datumHash: output.datumHash }),
});

/** Decode the two complete lists without a second provider read. These are
 * captured facts; origin/retirement provenance and live generation authority
 * belong to the node owner, not this SDK adapter. */
export const decodeEventHistoryLedgerSnapshot = (
  ledger: AcquiredLedgerSnapshot,
  deployments: NodeEventHistoryDeployments,
): Effect.Effect<EventHistoryLedgerSnapshot, SDK.LucidError> =>
  Effect.gen(function* () {
    const inputs = yield* Effect.try({
      try: () => {
        const at = (address: string) => {
          if (!ledger.addresses.includes(address))
            throw new Error(
              "Ledger snapshot did not capture a required history address",
            );
          return ledger.outputs.filter((output) => output.address === address);
        };
        const list = (deployment: SDK.EventHistoryDeployment) =>
          at(deployment.address).flatMap((output) => {
            const authenticated = Object.keys(output.assets).some((unit) =>
              unit.startsWith(deployment.policyId),
            );
            if (!authenticated) return [];
            if (output.hasReferenceScript)
              throw new Error(
                "Authenticated history output carries a reference script",
              );
            return [withoutReferenceScript(output)];
          });
        const retained = (deployment: SDK.EventHistoryDeployment) =>
          at(deployment.retentionAddress)
            .filter((output) => !output.hasReferenceScript)
            .map(withoutReferenceScript);
        return {
          deposit: list(deployments.deposit),
          depositData: retained(deployments.deposit),
          withdrawal: list(deployments.withdrawal),
          withdrawalData: retained(deployments.withdrawal),
        };
      },
      catch: (cause) =>
        new SDK.LucidError({
          message: "Invalid acquired history snapshot",
          cause,
        }),
    });
    const deposits = yield* SDK.utxosToDepositUTxOs(
      inputs.deposit,
      inputs.depositData,
      deployments.deposit,
    );
    const withdrawals = yield* SDK.utxosToWithdrawalUTxOs(
      inputs.withdrawal,
      inputs.withdrawalData,
      deployments.withdrawal,
    );
    return Object.freeze({
      ledger,
      deposits: Object.freeze(deposits),
      withdrawals: Object.freeze(withdrawals),
    });
  });

export const readEventHistoryLedgerSnapshot = async ({
  deployments,
  ...source
}: Omit<Parameters<typeof readAcquiredLedgerSnapshot>[0], "addresses"> & {
  readonly deployments: NodeEventHistoryDeployments;
}): Promise<EventHistoryLedgerSnapshot> => {
  const ledger = await readAcquiredLedgerSnapshot({
    ...source,
    addresses: Object.values(deployments).flatMap((deployment) => [
      deployment.address,
      deployment.retentionAddress,
    ]),
  });
  return await Effect.runPromise(
    decodeEventHistoryLedgerSnapshot(ledger, deployments),
  );
};
