/** The validation-trace dispute runner from Init to conviction at the
 * deepest trace admission takes, the operator's thread at the trace's last
 * CEK builtin step, a direct builtin's failure (divideInteger by zero),
 * and the successor's at its last direct scalar builtin step, each resolved
 * through its arm chain
 * (validation-trace-dispute-max-shape-case.ts). The same transaction in an
 * honest block, which stamps it TxIsInvalid, starts no workflow and signs
 * nothing. */
import { describe } from "vitest";

import {
  deepestCase,
  honestBlock,
  LONGEST_FAILING_BUILTIN_SCRIPTS,
} from "./support/validation-trace-dispute-max-shape-case.js";

describe("validation-trace dispute runner at the deepest trace admission takes", () => {
  deepestCase({
    failingScript: LONGEST_FAILING_BUILTIN_SCRIPTS.failure,
    group: 9,
    successor: { group: 3 },
  });

  honestBlock({ failingScript: LONGEST_FAILING_BUILTIN_SCRIPTS.failure });
});
