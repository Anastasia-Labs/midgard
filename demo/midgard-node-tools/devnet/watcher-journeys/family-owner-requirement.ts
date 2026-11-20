import * as SDK from "@al-ft/midgard-sdk/users";

import {
  journeyForcedTransactionSpend,
  journeyOwnerKey,
  type JourneyOwnerTransactionInput,
} from "./forced-transaction.js";
import {
  type HistoryPredecessor,
  JOURNEY_HISTORY_TRANSACTION_CATEGORIES,
  journeyHistoryTransactionSpend,
} from "./history-cases.js";
import {
  type OwnerSpend,
  ownerSpendLeastLovelace,
  ownerSpendShortfall,
  topUpTemplate,
} from "./owner-output-selection.js";
import { JOURNEY_SCRIPT_FORCED_CATEGORIES } from "./script-cases.js";
import { journeyValidationTraceSpend } from "./script-validation-trace-case.js";
import {
  JOURNEY_TRANSACTION_CATEGORIES,
  journeyTransactionSpend,
} from "./transaction-cases.js";
import { JOURNEY_FORCED_TRANSACTION_CATEGORIES } from "./transaction-forced-cases.js";
import { JOURNEY_TRANSACTION_SOURCE_CATEGORIES } from "./transaction-source-cases.js";
import {
  type OwnerFundingRequirement,
  withdrawalFundingShape,
  withdrawalOwnerRequirement,
  type WithdrawalQuote,
} from "./withdrawal-owner-funding.js";

const among = <T extends string>(
  categories: readonly T[],
  category: string,
): category is T => (categories as readonly string[]).includes(category);

/** Every output each of `spends` pays from one entry. */
const together =
  (...spends: OwnerSpend[]): OwnerSpend =>
  (entry) =>
    spends.flatMap((spend) => spend(entry));

/** How a family that spends an output of the ledger owner's on L2 builds. */
const spendKind = (category: string) =>
  among(JOURNEY_FORCED_TRANSACTION_CATEGORIES, category) ||
  among(JOURNEY_SCRIPT_FORCED_CATEGORIES, category)
    ? ("forced" as const)
    : among(JOURNEY_TRANSACTION_CATEGORIES, category)
      ? ("transaction" as const)
      : among(JOURNEY_TRANSACTION_SOURCE_CATEGORIES, category)
        ? ("source" as const)
        : category === "validationTraceDispute"
          ? ("validationTrace" as const)
          : among(JOURNEY_HISTORY_TRANSACTION_CATEGORIES, category)
            ? ("history" as const)
            : undefined;

/**
 * What a family of `category` that spends an output of the ledger owner's
 * pays from it, on `input.predecessor`: its fault and valid control and, for
 * a family whose successor is the default valid transaction
 * (`createTransactionJourneyFixture`), that successor too.
 */
const familySpend = (
  category: string,
  kind: NonNullable<ReturnType<typeof spendKind>>,
  input: JourneyOwnerTransactionInput,
): OwnerSpend => {
  const successor = journeyTransactionSpend(input, undefined);
  switch (kind) {
    case "forced":
      return journeyForcedTransactionSpend(input);
    case "transaction":
      return together(
        journeyTransactionSpend(
          input,
          category as (typeof JOURNEY_TRANSACTION_CATEGORIES)[number],
        ),
        successor,
      );
    case "source":
      return successor;
    case "validationTrace":
      return together(journeyValidationTraceSpend(input), successor);
    case "history":
      return together(
        journeyHistoryTransactionSpend({
          ...input,
          category:
            category as (typeof JOURNEY_HISTORY_TRANSACTION_CATEGORIES)[number],
        }),
        successor,
      );
  }
};

/**
 * The least lovelace the SDK deposits: a deposit's original Value must fund
 * its future reserve output (`assertEventHistoryAdmissionFunding`), whose
 * minimum grows only with the lovelace's own encoding width.
 */
const depositMinimumLovelace = () => {
  let lovelace = 0n;
  for (;;) {
    const minimum = SDK.eventHistoryMinimumOutputLovelace(
      { lovelace },
      "NoDatum",
    );
    if (minimum <= lovelace) return lovelace;
    lovelace = minimum;
  }
};

/**
 * Whether the family builds its blocks from an output of the ledger owner's,
 * withdrawn or spent on L2, so its funding before its timer must find one.
 */
export const spendsOwnerOutput = (category: string) =>
  withdrawalFundingShape(category) !== undefined ||
  spendKind(category) !== undefined;

/**
 * What the family needs of the ledger owner's outputs on a tail, for its
 * funding before its timer (`ensureWithdrawalOwnerFunding`): one output a
 * withdrawal family withdraws, or splits and withdraws (with change that
 * funds its control), or one that funds every transaction a spending family
 * builds from it (`familySpend`), each output at its minimum ADA after the
 * fee its schedule prices. A top-up is sized on a fresh output at
 * `topUpAddress` at the widest reference (`topUpTemplate`), and is never
 * below what the SDK deposits. Undefined for a family that needs none.
 */
export const familyOwnerRequirement = (
  category: string,
  input: Readonly<{ ledgerOwnerSeedPhrase: string; quote: WithdrawalQuote }>,
):
  | ((
      tail: HistoryPredecessor,
      topUpAddress: Buffer,
    ) => Promise<OwnerFundingRequirement>)
  | undefined => {
  const shape = withdrawalFundingShape(category);
  const kind = spendKind(category);
  if (shape === undefined && kind === undefined) return undefined;
  const deposited = (lovelace: bigint) => {
    const minimum = depositMinimumLovelace();
    return lovelace > minimum ? lovelace : minimum;
  };
  return async (tail, topUpAddress) => {
    if (shape !== undefined) {
      const requirement = await withdrawalOwnerRequirement({
        shape,
        quote: input.quote,
        key: journeyOwnerKey(input.ledgerOwnerSeedPhrase),
        header: tail.header,
        topUpAddress,
      });
      return {
        ...requirement,
        topUpLovelace: deposited(requirement.topUpLovelace),
      };
    }
    const spend = familySpend(category, kind!, {
      predecessor: tail,
      ledgerOwnerSeedPhrase: input.ledgerOwnerSeedPhrase,
    });
    return {
      purpose: `the ${category} family's transactions`,
      topUpLovelace: deposited(
        ownerSpendLeastLovelace(spend, topUpTemplate(topUpAddress)),
      ),
      funds: async (entry) => ownerSpendShortfall(spend, entry) === undefined,
    };
  };
};
