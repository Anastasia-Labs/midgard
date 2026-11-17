import {
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";

import {
  buildJourneyHistoryTransaction,
  type HistoryPredecessor,
  JOURNEY_HISTORY_TRANSACTION_CATEGORIES,
  ownedHistoryLedgerEntries,
  retainHistoryTransactions,
} from "./history-cases.js";
import {
  journeyLedgerOwnerKey,
  journeyWithdrawalQuote,
  publishJourneyDeposit,
  publishJourneyWithdrawals,
} from "./history-event-staging.js";
import {
  buildJourneyFabricatedDeposit,
  buildJourneyWithdrawalEvent,
  buildJourneyWithdrawnTransaction,
  journeyWithdrawalBody,
  ownedOutputWithdrawalBody,
} from "./history-events.js";
import {
  createPreparedJourneyFixture,
  createTransactionJourneyFixture,
  decodeJourneyRetainedBlock,
  type JourneyFaultPreparationInput,
} from "./staging.js";
import {
  ownerOutputRequirement,
  signOwnerSplit,
  smallestQualifyingOutput,
  type WithdrawalFundingShape,
} from "./withdrawal-owner-funding.js";

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

/**
 * The ledger owner's smallest output on `predecessor` that funds a family
 * of `shape`, with what the family's withdrawn output then holds. The
 * family's funding before its timer (`ensureWithdrawalOwnerFunding`) tops the
 * owner up when no output on the run's tail does; before a devnet's first
 * block there is no tail, and nothing but the first deposit trace's own
 * deposit funds the owner.
 */
const fundingOwnerOutput = async (
  input: JourneyFaultPreparationInput,
  predecessor: HistoryPredecessor,
  shape: WithdrawalFundingShape,
) => {
  const key = ownerKey(input);
  const owner = key.to_public().hash().to_hex();
  const required = ownerOutputRequirement({
    shape,
    quote: journeyWithdrawalQuote({
      deployment: input.context.deployment,
      ownerKey: key,
    }),
    key,
    header: predecessor.header,
  });
  const requirements = new Map<string, bigint>();
  const entry = await smallestQualifyingOutput(
    ownedHistoryLedgerEntries(predecessor, owner),
    {
      requiredOf: async (candidate) => {
        const { total, orderLovelace } = await required(candidate);
        requirements.set(candidate.outRef.toString("hex"), orderLovelace);
        return total;
      },
    },
  );
  if (entry === undefined)
    throw new Error(
      `No output of the ledger owner's on the family's predecessor funds a ${shape}. A devnet's first family has no tail to top up before its timer, so only the first deposit trace's own deposit funds the owner; any later family's funding before its timer tops the owner up and checks the result`,
    );
  return {
    owner,
    key,
    entry,
    orderLovelace: requirements.get(entry.outRef.toString("hex"))!,
  };
};

export const createWithdrawalJourneyFixture = (
  category: "fabricatedWithdrawal" | "withdrawalMistag" | "doubleWithdraw",
) =>
  createPreparedJourneyFixture(
    category,
    async (input) => {
      const { owner, entry } = await fundingOwnerOutput(
        input,
        input.predecessor,
        "withdrawal",
      );
      const body = ownedOutputWithdrawalBody({
        owner,
        outRef: entry.outRef,
        output: decodeMidgardTxOutput(entry.output),
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

/** The withdrawal of the split transaction's first output, the one it keeps
 * the original's assets in, not whichever owned output sorts first. */
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
          // The first output holds exactly what its withdrawal needs, and the
          // rest stays with the owner as change for later families.
          const { key, entry, orderLovelace } = await fundingOwnerOutput(
            input,
            timed.predecessor,
            "split",
          );
          const block = await retainHistoryTransactions({
            ...timed,
            transactions: [
              signOwnerSplit({
                header: timed.predecessor.header,
                key,
                entry,
                orderLovelace,
              }),
            ],
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
