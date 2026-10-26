import { INSTALLED_FRAUD_PROOF_CATEGORY_ORDER } from "@al-ft/midgard-fault-proofs";

import type { JourneyCategory, JourneyFixture } from "./fixture.js";
import {
  JOURNEY_HISTORY_EVENT_FIXTURE_CANDIDATES,
  JOURNEY_HISTORY_FIXTURES,
} from "./history-fixtures.js";
import {
  JOURNEY_SCRIPT_FIXTURE_CANDIDATES,
  JOURNEY_SCRIPT_FIXTURES,
} from "./script-fixtures.js";
import {
  JOURNEY_LOCALLY_VERIFIED_TRANSACTION_FIXTURES,
  JOURNEY_TRANSACTION_FIXTURES,
} from "./transaction-fixtures.js";
import { transitionTraceJourneyFixture } from "./transition-trace-fixture.js";

export type JourneyFixtureOwner = "transaction" | "script" | "history";

/** Explicit work ownership; catalogue additions fail compilation until assigned. */
export const JOURNEY_FIXTURE_OWNERS = {
  invalidRange: "transaction",
  zeroInput: "transaction",
  daHashPreimage: "transaction",
  invalidSignature: "transaction",
  missingSignature: "transaction",
  canonicalDecodability: "transaction",
  committedFieldShape: "transaction",
  minFee: "transaction",
  l2TxMistag: "transaction",
  valueNotPreserved: "transaction",
  inputSetUniqueness: "transaction",
  mintAuthorization: "transaction",
  networkId: "transaction",
  minAda: "transaction",
  fieldPreimageLengthMismatch: "transaction",
  fieldItemWidthIllegal: "transaction",
  transactionOutputNonCanonical: "transaction",
  resolvedOutputNonCanonical: "transaction",
  mintDeclaredAssetLimit: "transaction",
  mintItemNonCanonical: "transaction",
  spendInputSignerMissing: "transaction",
  protectedOutputSignerMissing: "transaction",
  observersForbiddenOnUntaggedNetwork: "transaction",
  observerOrderInvalid: "transaction",
  nativeScriptDecoding: "script",
  nativeScriptInvalid: "script",
  witnessScriptDecoding: "script",
  scriptIntegrityHashMissing: "script",
  outputReferenceScriptDecoding: "script",
  redeemerCanonicity: "script",
  validationTraceDispute: "script",
  missingRedeemer: "script",
  missingScriptSource: "script",
  unusedRedeemer: "script",
  doubleSpend: "history",
  nonExistentInput: "history",
  nonExistentInputNoIndex: "history",
  transitionTrace: "history",
  noReferenceInput: "history",
  referenceInputNoIdx: "history",
  fabricatedDeposit: "history",
  fabricatedWithdrawal: "history",
  withdrawnReferenceInput: "history",
  withdrawalMistag: "history",
  doubleWithdraw: "history",
  withdrawnInput: "history",
} as const satisfies Record<JourneyCategory, JourneyFixtureOwner>;

export const JOURNEY_CATEGORIES: readonly JourneyCategory[] =
  INSTALLED_FRAUD_PROOF_CATEGORY_ORDER;

// Registration is separate from assignment: an assigned family is not evidence
// that its fixture or automatic live journey has passed.
export const JOURNEY_FIXTURES: readonly JourneyFixture[] = [
  transitionTraceJourneyFixture,
  ...JOURNEY_SCRIPT_FIXTURES,
  ...JOURNEY_LOCALLY_VERIFIED_TRANSACTION_FIXTURES,
  ...JOURNEY_HISTORY_FIXTURES,
];

export const JOURNEY_FIXTURE_CANDIDATES: readonly JourneyFixture[] = [
  transitionTraceJourneyFixture,
  ...JOURNEY_TRANSACTION_FIXTURES,
  ...JOURNEY_SCRIPT_FIXTURES,
  ...JOURNEY_SCRIPT_FIXTURE_CANDIDATES,
  ...JOURNEY_HISTORY_FIXTURES,
  ...JOURNEY_HISTORY_EVENT_FIXTURE_CANDIDATES,
];

/** Reject missing/duplicate selections before touching any chain or wallet. */
export const selectJourneyFixtures = (
  selection = "transitionTrace",
): JourneyFixture[] => {
  const categories = selection.split(",").map((value) => value.trim());
  if (
    categories.some((value) => value.length === 0) ||
    new Set(categories).size !== categories.length
  )
    throw new Error(
      "Journey selection must contain distinct nonempty categories",
    );
  return categories.map((category) => {
    const fixture = JOURNEY_FIXTURES.find(
      (value) => value.category === category,
    );
    if (fixture === undefined)
      throw new Error(`No live fixture registered for ${category}`);
    return fixture;
  });
};
