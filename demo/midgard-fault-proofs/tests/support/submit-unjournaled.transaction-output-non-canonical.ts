/**
 * The transaction-output-non-canonical builders the tests drive, each bound
 * into a function that builds its step and submits it with no journal (see
 * ./submit-unjournaled.ts).
 */
import {
  buildTransactionOutputNonCanonicalStep01Accepted,
  buildTransactionOutputNonCanonicalStep01Forced,
  buildTransactionOutputNonCanonicalStep02,
  buildTransactionOutputNonCanonicalStep03,
  buildTransactionOutputNonCanonicalStep04,
} from "../../src/transaction-output-non-canonical/index.js";
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
