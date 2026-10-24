/**
 * Replay corpus block shapes consumed by the MPF probes.
 */

import * as SDK from "@al-ft/midgard-sdk";

export type CorpusHexEntry = { readonly key: string; readonly value: string };

export type CorpusLedgerOp =
  | { readonly type: "insert"; readonly key: string; readonly value: string }
  | { readonly type: "delete"; readonly key: string };

export type MpfReplayCorpusBlock = {
  readonly version: 1;
  readonly label: string;
  readonly initialLedgerEntries: readonly CorpusHexEntry[];
  readonly sourceEvents: readonly {
    readonly phase: SDK.TransitionPhase;
    readonly eventKeyCbor: string;
    readonly ledgerOps: readonly CorpusLedgerOp[];
  }[];
  readonly transactionOps: readonly CorpusHexEntry[];
  readonly deposits: readonly CorpusHexEntry[];
  readonly withdrawals: readonly CorpusHexEntry[];
  readonly forcedTransactions: readonly CorpusHexEntry[];
  readonly finalUtxoEntries: readonly CorpusHexEntry[];
  readonly expected: {
    readonly utxoRoot: string;
    readonly rawTxRoot: string;
    readonly txRoot: string;
    readonly transitionTraceRoot: string;
    readonly eventToStepRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
    readonly forcedTransactionsRoot: string;
    readonly transitionRoots: readonly {
      readonly pre: string;
      readonly post: string;
    }[];
  };
};
