/**
 * An own block's journal as commit submission prepares it: one
 * Midgard-native transaction from the mempool and, if given, one classified
 * withdrawal and one deposit, with a transition step per event and the transaction's
 * validation trace, under the hash of a header whose roots and counts
 * commit them, so the block's DA payload builds.
 */
import {
  computeHash32,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxWitnessSetCanonical,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { computeMidgardTxIdFromCanonicalCbor } from "@al-ft/midgard-validation";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DepositsDB,
  MempoolDB,
  type PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "../src/database/index.js";
import { Columns as TxColumns } from "../src/database/utils/tx.js";
import { encodeTransactionRootValue } from "../src/mpf/index.js";
import { buildAuthenticatedRootFromEncodedEntries } from "../src/workers/commit-block-header/transition-roots.js";
import { journalFixture } from "./helpers/own-journal-fixture.js";

const emptyList = Buffer.from([0x80]);

/** A Midgard-native transaction whose id is its payload's. */
const nativeTx = (fee: bigint) => {
  const body: MidgardNativeTxBodyCanonical = {
    spendInputsPreimageCbor: emptyList,
    referenceInputsPreimageCbor: emptyList,
    outputsPreimageCbor: emptyList,
    fee,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: emptyList,
    requiredSignersPreimageCbor: emptyList,
    mintPreimageCbor: emptyList,
    scriptIntegrityHash: computeHash32(Buffer.from([0xf6])),
    auxiliaryDataHash: computeHash32(Buffer.from([0xf6])),
    networkId: 0n,
  };
  const witnessSet: MidgardNativeTxWitnessSetCanonical = {
    addrTxWitsPreimageCbor: emptyList,
    scriptTxWitsPreimageCbor: emptyList,
    redeemerTxWitsPreimageCbor: emptyList,
  };
  const cbor = Buffer.from(
    encodeMidgardNativeTxCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body,
      witnessSet,
      compact: deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid"),
    }),
  );
  return { id: Buffer.from(computeMidgardTxIdFromCanonicalCbor(cbor)), cbor };
};

const rootOf = async (
  domain: SDK.RootDomain,
  entries: readonly (readonly [Buffer, Buffer])[],
) =>
  entries.length === 0
    ? SDK.EMPTY_MERKLE_TREE_ROOT
    : (
        await Effect.runPromise(
          buildAuthenticatedRootFromEncodedEntries(
            domain,
            entries.map(([key, value]) => ({ key, value })),
          ),
        )
      ).root;

/** `count` retained-root members of `label`. */
const retained = (label: number, count: number) =>
  Array.from({ length: count }, (_, index) => ({
    keyCbor: Buffer.from([label, index]),
    valueCbor: Buffer.from([0x41, label + index]),
  }));

const pairs = (members: ReturnType<typeof retained>) =>
  members.map((member) => [member.keyCbor, member.valueCbor] as const);

/**
 * The journal (ready for `preparePendingSubmission`) of an own block
 * including the transaction `tx` and, if given, the classified withdrawal
 * `withdrawal` and the deposit `deposit`.
 */
export const ownJournal = async (
  withdrawal?: WithdrawalsDB.Entry,
  deposit?: DepositsDB.Entry,
): Promise<{
  readonly journal: PendingBlockFinalizationsDB.PrepareInput;
  readonly tx: { readonly id: Buffer; readonly cbor: Buffer };
}> => {
  const tx = nativeTx(1n);
  const draft = journalFixture(Buffer.alloc(28));
  const withdrawals = withdrawal === undefined ? [] : [withdrawal];
  const deposits = deposit === undefined ? [] : [deposit];
  const events = 1 + withdrawals.length + deposits.length;
  const transitionTraceMembers = retained(0x01, events);
  const eventToStepMembers = retained(0x02, events);
  const validationTraceMembers = retained(0x03, 1);
  const roots = {
    transactionsRoot: await rootOf(SDK.ROOT_DOMAINS.transactionsV1, [
      [tx.id, encodeTransactionRootValue(tx.cbor, MIDGARD_CONSENSUS_PROFILE)],
    ]),
    depositsRoot: await rootOf(
      SDK.ROOT_DOMAINS.deposits,
      deposits.map((entry) => [
        entry[DepositsDB.Columns.ID],
        entry[DepositsDB.Columns.INFO],
      ]),
    ),
    withdrawalsRoot: await rootOf(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawals.map((entry) => [
        entry[WithdrawalsDB.Columns.ID],
        entry[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]!,
      ]),
    ),
    transitionTraceRoot: await rootOf(
      SDK.ROOT_DOMAINS.transitionTrace,
      pairs(transitionTraceMembers),
    ),
    eventToStepRoot: await rootOf(
      SDK.ROOT_DOMAINS.eventToStep,
      pairs(eventToStepMembers),
    ),
  };
  const counts = {
    l2TransactionCount: 1n,
    withdrawalCount: BigInt(withdrawals.length),
    depositCount: BigInt(deposits.length),
    totalEventCount: BigInt(events),
    transitionStepCount: BigInt(events),
  };
  // The header pins the validation-trace pair to the empty tree; the
  // journal keeps the built members.
  const validation = {
    validationTracesRoot: await rootOf(
      SDK.ROOT_DOMAINS.validationTraces,
      pairs(validationTraceMembers),
    ),
  };
  const header = {
    ...(LucidData.from(
      draft.headerCbor.toString("hex"),
      SDK.Header,
    ) as SDK.Header),
    ...roots,
    ...counts,
  };
  const headerHash = Buffer.from(
    await Effect.runPromise(SDK.hashBlockHeader(header)),
    "hex",
  );
  return {
    tx,
    journal: {
      ...draft,
      headerHash,
      headerCbor: Buffer.from(
        LucidData.to(header as never, SDK.Header as never),
        "hex",
      ),
      metadata: {
        ...draft.metadata,
        expectedRoots: {
          ...draft.metadata.expectedRoots,
          ...roots,
          ...validation,
        },
        expectedCounts: {
          ...draft.metadata.expectedCounts,
          ...counts,
          validationTraceCount: 1n,
        },
      },
      withdrawalEventIds: withdrawals.map(
        (entry) => entry[WithdrawalsDB.Columns.ID],
      ),
      withdrawalEntries: withdrawals,
      depositEventIds: deposits.map((entry) => entry[DepositsDB.Columns.ID]),
      depositEntries: deposits,
      mempoolTxIds: [tx.id],
      mempoolTxs: [
        {
          [TxColumns.TX_ID]: tx.id,
          [TxColumns.TX]: tx.cbor,
          [TxColumns.TIMESTAMPTZ]: draft.blockEndTime,
        },
      ],
      mempoolTxSourceTable: MempoolDB.tableName,
      transitionTraceMembers,
      eventToStepMembers,
      validationTraceMembers,
    },
  };
};
