import { Trie } from "@aiken-lang/merkle-patricia-forestry";

import {
  applyValidationLedgerMutations,
  buildValidationMachineLedgerInsertOp,
} from "../dist/index.js";

/**
 * The steps `operations` make, in order, on the ledger holding exactly
 * `initialEntries`: a library trie keyed by out-ref, each value the output's
 * canonical descriptor (the value an insert of that output carries).
 */
export const mutationStepsOver = async ({ initialEntries, operations }) =>
  await applyValidationLedgerMutations(
    await Trie.fromList(
      initialEntries.map(({ outRef, output }) => ({
        key: outRef,
        value: buildValidationMachineLedgerInsertOp({
          key: outRef,
          outputCbor: output,
        }).value,
      })),
    ),
    operations,
  );
