import {
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";

import {
  buildJourneyHistoryTransaction,
  type HistoryPredecessor,
  JOURNEY_HISTORY_TRANSACTION_CATEGORIES,
  ownedHistoryLedgerEntries,
  retainHistoryTransactions,
  signHistoryTransaction,
} from "./history-cases.js";
import {
  journeyLedgerOwnerKey,
  publishJourneyDeposit,
  publishJourneyWithdrawals,
} from "./history-event-staging.js";
import {
  buildJourneyFabricatedDeposit,
  buildJourneyWithdrawalEvent,
  buildJourneyWithdrawnTransaction,
  journeyWithdrawalBody,
} from "./history-events.js";
import { buildAtJourneyMinFee } from "./journey-fee.js";
import {
  createPreparedJourneyFixture,
  createTransactionJourneyFixture,
  decodeJourneyRetainedBlock,
  type JourneyFaultPreparationInput,
} from "./staging.js";

const ownerKey = (input: JourneyFaultPreparationInput) =>
  journeyLedgerOwnerKey(input.ledgerOwnerSeedPhrase);

export const fabricatedDepositJourneyFixture = createPreparedJourneyFixture(
  "fabricatedDeposit",
  async (input) => {
    const deposit = await publishJourneyDeposit(input);
    return {
      buildFault: (timed) =>
        buildJourneyFabricatedDeposit({ ...timed, deposit }),
      buildSuccessor: (timed) =>
        buildJourneyFabricatedDeposit({ ...timed, deposit, honest: true }),
    };
  },
  { userEvents: 1 },
);

export const createWithdrawalJourneyFixture = (
  category: "fabricatedWithdrawal" | "withdrawalMistag" | "doubleWithdraw",
) =>
  createPreparedJourneyFixture(
    category,
    async (input) => {
      const body = journeyWithdrawalBody({
        predecessor: input.predecessor,
        owner: ownerKey(input).to_public().hash().to_hex(),
      });
      // doubleWithdraw publishes both withdrawals before one shared event
      // wait, so its staging fits one of operator-2's shifts.
      const withdrawals = await publishJourneyWithdrawals(
        input,
        body,
        category === "doubleWithdraw" ? 2 : 1,
      );
      return {
        buildFault: (timed) =>
          buildJourneyWithdrawalEvent({ ...timed, category, withdrawals }),
        buildSuccessor: (timed) =>
          buildJourneyWithdrawalEvent({
            ...timed,
            category,
            withdrawals,
            honest: true,
          }),
      };
    },
    { userEvents: category === "doubleWithdraw" ? 2 : 1 },
  );

/**
 * The owned output the withdrawn-input split divides: the one with the most
 * lovelace, so each half can fund a withdrawal Order whatever else the owner
 * holds. The first output by reference can be too small: on a fresh devnet it
 * is a 2 ADA output, below the Order's minimum.
 */
export const withdrawnInputSplitSource = (
  predecessor: HistoryPredecessor,
  owner: string,
) =>
  ownedHistoryLedgerEntries(predecessor, owner).reduce<
    ReturnType<typeof ownedHistoryLedgerEntries>[number] | undefined
  >((largest, entry) => {
    if (largest === undefined) return entry;
    const lovelace = (candidate: typeof entry) =>
      decodeMidgardTxOutput(candidate.output).value.lovelace;
    return lovelace(entry) > lovelace(largest) ? entry : largest;
  }, undefined);

/** The withdrawal of the split transaction's first output, the half it keeps
 * the original's Value in, not whichever owned output sorts first. */
export const withdrawnInputSplitBody = (
  split: HistoryPredecessor,
  owner: string,
) => {
  const transactions = split.payload.block_body.transactions;
  const [transaction] = transactions;
  if (transaction === undefined || transactions.length !== 1)
    throw new Error("Withdrawn-input split block must hold exactly the split");
  return journeyWithdrawalBody({
    predecessor: split,
    owner,
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.from(transaction[0], "hex"),
      outputIndex: 0,
    }),
  });
};

export const createWithdrawnInputJourneyFixture = (
  category: "withdrawnInput" | "withdrawnReferenceInput",
) =>
  createPreparedJourneyFixture(
    category,
    async (input) => {
      const split = await input.commitHistoryBlock(
        "withdrawal-ledger",
        async (timed) => {
          const selected = withdrawnInputSplitSource(
            timed.predecessor,
            ownerKey(input).to_public().hash().to_hex(),
          );
          const entry =
            selected === undefined
              ? undefined
              : [
                  selected.outRef.toString("hex"),
                  selected.output.toString("hex"),
                ];
          if (entry === undefined)
            throw new Error(
              "Withdrawn-input history requires a deposited output",
            );
          const output = decodeMidgardTxOutput(Buffer.from(entry[1], "hex"));
          const half = output.value.lovelace / 2n;
          const transaction = buildAtJourneyMinFee(
            timed.predecessor.header,
            (fee) =>
              signHistoryTransaction(
                {
                  spendInputs: [Buffer.from(entry[0], "hex")],
                  outputs: [
                    encodeMidgardTxOutput({
                      ...output,
                      value: { ...output.value, lovelace: half },
                    }),
                    encodeMidgardTxOutput({
                      ...output,
                      value: {
                        lovelace: output.value.lovelace - half - fee,
                        assets: new Map(),
                      },
                    }),
                  ],
                  fee,
                  networkId: timed.predecessor.header.expectedNetworkId,
                },
                ownerKey(input),
              ),
            output.value.lovelace - half,
          );
          const block = await retainHistoryTransactions({
            ...timed,
            transactions: [transaction],
          });
          if (block.replays[0]?.trace.verdict !== "accepted")
            throw new Error("Withdrawal prerequisite split is not valid");
          return block;
        },
      );
      const predecessor = await decodeJourneyRetainedBlock(
        split,
        input.ledgerAt,
      );
      const preparation = { ...input, predecessor };
      const body = withdrawnInputSplitBody(
        predecessor,
        ownerKey(input).to_public().hash().to_hex(),
      );
      const [withdrawal] = await publishJourneyWithdrawals(
        preparation,
        body,
        1,
      );
      return {
        predecessor: split,
        buildFault: (timed) =>
          buildJourneyWithdrawnTransaction({ ...timed, category, withdrawal }),
        buildSuccessor: (timed) =>
          buildJourneyWithdrawnTransaction({
            ...timed,
            category,
            withdrawal,
            honest: true,
          }),
      };
    },
    // The split of the ledger owner's output it withdraws, then the withdrawal.
    { historyBlocks: 1, userEvents: 1 },
  );

/** Locally verified candidates; the catalogue admits them after full-scope checks. */
export const JOURNEY_HISTORY_FIXTURES = [
  ...JOURNEY_HISTORY_TRANSACTION_CATEGORIES.map((category) =>
    createTransactionJourneyFixture(category, (input) =>
      buildJourneyHistoryTransaction({ ...input, category }),
    ),
  ),
  // Verified against real published L1 events on an isolated emulator chain
  // by history-event-cases.test.ts: exact installed selection and healthy control.
  createWithdrawnInputJourneyFixture("withdrawnInput"),
  createWithdrawnInputJourneyFixture("withdrawnReferenceInput"),
  fabricatedDepositJourneyFixture,
  createWithdrawalJourneyFixture("fabricatedWithdrawal"),
  createWithdrawalJourneyFixture("withdrawalMistag"),
  createWithdrawalJourneyFixture("doubleWithdraw"),
];
