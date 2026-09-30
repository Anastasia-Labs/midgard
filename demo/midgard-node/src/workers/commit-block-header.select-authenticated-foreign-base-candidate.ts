import * as SDK from "@al-ft/midgard-sdk";

import * as Ledger from "../database/utils/ledger.js";

export type ResolvedCommitBaseLedgerEntries = {
  readonly source: string;
  readonly entries?: readonly Ledger.MinimalEntry[];
  readonly root: string;
  readonly utxoPayloadAggregate: SDK.DaPayloadEntrySizeAggregate;
};

export const selectAuthenticatedForeignBaseCandidate = ({
  foreignUtxosRoot,
  requireEntries,
  candidates,
}: {
  readonly foreignUtxosRoot: string;
  readonly requireEntries: boolean;
  readonly candidates: readonly {
    readonly source: string;
    readonly root: string;
    readonly hasEntries: boolean;
  }[];
}):
  | { readonly type: "Ready"; readonly source: string }
  | { readonly type: "AwaitingForeignLedger"; readonly reason: string } => {
  const matching = candidates.find(
    (candidate) =>
      candidate.root === foreignUtxosRoot &&
      (!requireEntries || candidate.hasEntries),
  );
  return matching === undefined
    ? {
        type: "AwaitingForeignLedger",
        reason: candidates.some(
          (candidate) => candidate.root === foreignUtxosRoot,
        )
          ? "matching durable root has no authenticated entry snapshot"
          : "foreign UTxO root differs from every authenticated local base",
      }
    : { type: "Ready", source: matching.source };
};
