/**
 * The transaction-output-non-canonical builders the tests drive, each bound
 * into a function that builds its step and submits it with no journal (see
 * ./submit-unjournaled.ts).
 */
import { buildTransactionOutputNonCanonicalStep01Accepted } from "../../src/transaction-output-non-canonical/submit-step-01-accepted.js";
import { buildTransactionOutputNonCanonicalStep01Forced } from "../../src/transaction-output-non-canonical/submit-step-01-forced.js";
import { buildTransactionOutputNonCanonicalStep02 } from "../../src/transaction-output-non-canonical/submit-step-02.js";
import { buildTransactionOutputNonCanonicalStep03 } from "../../src/transaction-output-non-canonical/submit-step-03.js";
import { buildTransactionOutputNonCanonicalStep04 } from "../../src/transaction-output-non-canonical/submit-step-04.js";
import { unjournaled, unjournaledTurns } from "./submit-unjournaled.js";

export const submitTransactionOutputNonCanonicalStep01Accepted = unjournaled(
  buildTransactionOutputNonCanonicalStep01Accepted,
);
export const submitTransactionOutputNonCanonicalStep01Forced = unjournaled(
  buildTransactionOutputNonCanonicalStep01Forced,
);
export const submitTransactionOutputNonCanonicalStep02 = unjournaledTurns(
  buildTransactionOutputNonCanonicalStep02,
);
export const submitTransactionOutputNonCanonicalStep03 = unjournaled(
  buildTransactionOutputNonCanonicalStep03,
);
export const submitTransactionOutputNonCanonicalStep04 = unjournaled(
  buildTransactionOutputNonCanonicalStep04,
);
