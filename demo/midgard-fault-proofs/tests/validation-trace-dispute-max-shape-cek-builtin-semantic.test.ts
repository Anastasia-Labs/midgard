/** The validation-trace dispute runner from Init to conviction at the
 * deepest trace admission takes, the operator's thread at the trace's last
 * CEK builtin step, a semantic builtin (iData), and the successor's at its
 * map conversion's start (unMapData), each resolved through its arm chain
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
    failingScript: LONGEST_FAILING_BUILTIN_SCRIPTS.semantic,
    group: 5,
    successor: { group: 6 },
  });

  honestBlock({ failingScript: LONGEST_FAILING_BUILTIN_SCRIPTS.semantic });
});
