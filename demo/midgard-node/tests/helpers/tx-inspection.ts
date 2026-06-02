import {
  compareOutRefs,
  type OutRefLike,
} from "@al-ft/midgard-core/out-ref";
import { CML } from "@lucid-evolution/lucid";

export const collectSortedInputOutRefs = (
  inputs: CML.TransactionInputList,
): readonly OutRefLike[] =>
  [...Array(inputs.len()).keys()]
    .map((index) => {
      const input = inputs.get(index);
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
    })
    .sort(compareOutRefs);
