import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import {
  countsFromLengths,
  headerFor,
  type JournalFixtureOptions,
  ledgerRoot,
  member,
  record,
  sourceRoot,
  type TestRoots,
} from "./da-payload.record.js";

const sourceRootOrEmpty = (
  domain: SDK.RootDomain,
  entries: readonly [Buffer, Buffer][],
) =>
  entries.length === 0
    ? Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT)
    : sourceRoot(domain, entries);

export const buildJournalFixture = async ({
  utxoEntries = [],
  depositEntries = [],
  forcedTransactionEntries = [],
  withdrawalEntries = [],
  txEntries = [],
  transitionTraceEntries = [],
  eventToStepEntries = [],
  rootOverrides = {},
  recordRootOverrides = {},
}: JournalFixtureOptions): Promise<{
  readonly roots: TestRoots;
  readonly header: SDK.Header;
  readonly headerHash: Buffer;
  readonly pending: PendingBlockFinalizationsDB.Record;
}> => {
  const counts = countsFromLengths({
    withdrawals: withdrawalEntries.length,
    forcedTransactions: forcedTransactionEntries.length,
    transactions: txEntries.length,
    deposits: depositEntries.length,
  });
  const roots: TestRoots = {
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    ...(await Effect.runPromise(
      Effect.all({
        utxosRoot:
          utxoEntries.length === 0
            ? Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT)
            : ledgerRoot(utxoEntries),
        forcedTransactionsRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.forcedTransactionsV1,
          forcedTransactionEntries,
        ),
        transactionsRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.transactionsV1,
          txEntries,
        ),
        depositsRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.deposits,
          depositEntries,
        ),
        withdrawalsRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.withdrawals,
          withdrawalEntries,
        ),
        transitionTraceRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.transitionTrace,
          transitionTraceEntries,
        ),
        eventToStepRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.eventToStep,
          eventToStepEntries,
        ),
      }),
    )),
    ...rootOverrides,
  };
  const header = headerFor(roots, counts);
  const headerHash = Buffer.from(
    await Effect.runPromise(SDK.hashBlockHeader(header)),
    "hex",
  );
  const memberRecords = (entries: readonly [Buffer, Buffer][]) =>
    entries.map(([key, value], index) => member(headerHash, key, value, index));

  return {
    roots,
    header,
    headerHash,
    pending: record({
      headerHash,
      utxoEntries,
      depositMembers: memberRecords(depositEntries),
      forcedTransactionMembers: memberRecords(forcedTransactionEntries),
      withdrawalMembers: memberRecords(withdrawalEntries),
      txMembers: memberRecords(txEntries),
      transitionTraceMembers: memberRecords(transitionTraceEntries),
      eventToStepMembers: memberRecords(eventToStepEntries),
      roots: {
        ...roots,
        ...recordRootOverrides,
      },
      counts,
      header,
    }),
  };
};
