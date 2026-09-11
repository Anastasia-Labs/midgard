import { createForcedTransactionJourneyFixture } from "./forced-order.js";
import { createTransactionJourneyFixture } from "./staging.js";
import {
  buildJourneyTransactionFault,
  JOURNEY_TRANSACTION_CATEGORIES,
} from "./transaction-cases.js";
import {
  buildJourneyForcedTransaction,
  JOURNEY_FORCED_TRANSACTION_CATEGORIES,
  prepareJourneyForcedTransaction,
} from "./transaction-forced-cases.js";
import {
  buildJourneyTransactionSourceFault,
  JOURNEY_TRANSACTION_SOURCE_CATEGORIES,
} from "./transaction-source-cases.js";

/** Shared staging adapters; readiness is recorded separately by verification. */
export const JOURNEY_TRANSACTION_FIXTURES = [
  ...JOURNEY_TRANSACTION_CATEGORIES.map((category) =>
    createTransactionJourneyFixture(category, (input) =>
      buildJourneyTransactionFault({ ...input, category }),
    ),
  ),
  ...JOURNEY_FORCED_TRANSACTION_CATEGORIES.map((category) =>
    createForcedTransactionJourneyFixture(
      category,
      prepareJourneyForcedTransaction,
      {
        buildFault: (input) =>
          buildJourneyForcedTransaction({ ...input, category }),
        buildSuccessor: (input) =>
          buildJourneyForcedTransaction({ ...input, category, honest: true }),
      },
    ),
  ),
  ...JOURNEY_TRANSACTION_SOURCE_CATEGORIES.map((category) =>
    createTransactionJourneyFixture(category, (input) =>
      buildJourneyTransactionSourceFault({ ...input, category }),
    ),
  ),
];

/** Full installed selector, exact proof preparation and healthy control passed. */
const locallyVerifiedCategories = new Set([
  "inputSetUniqueness",
  "zeroInput",
  "invalidRange",
  "invalidSignature",
  "networkId",
  "mintAuthorization",
  "minFee",
  "spendInputSignerMissing",
  "protectedOutputSignerMissing",
  "observerOrderInvalid",
  "l2TxMistag",
  "minAda",
  "valueNotPreserved",
  "missingSignature",
  "transactionOutputNonCanonical",
  "resolvedOutputNonCanonical",
  "fieldPreimageLengthMismatch",
  "fieldItemWidthIllegal",
  "mintDeclaredAssetLimit",
  "distinctAssetAccumulationLimit",
  "observersForbiddenOnUntaggedNetwork",
  "daHashPreimage",
  "canonicalDecodability",
  "committedFieldShape",
]);

export const JOURNEY_LOCALLY_VERIFIED_TRANSACTION_FIXTURES =
  JOURNEY_TRANSACTION_FIXTURES.filter((fixture) =>
    locallyVerifiedCategories.has(fixture.category),
  );
