import * as SDK from "@al-ft/midgard-sdk";

import {
  type DecodedTransactionMaterial,
  type NodeTransactionPayload,
  type PreparedTxInclusionJson,
} from "./prepare-double-spend.js";
import {
  type Candidate,
  nativeOutputItems,
  spendInputsOf,
} from "./prepare-input-no-idx.midgard-tx-output-from-canonical-cbor.js";
import {
  InputNoIdxRejection,
  reject,
} from "./prepare-input-no-idx.read-value.js";

export const findCandidate = ({
  decoded,
  byTxId,
  badTxId,
  badInputsIndex,
  headerHash,
}: {
  readonly decoded: readonly DecodedTransactionMaterial[];
  readonly byTxId: ReadonlyMap<string, DecodedTransactionMaterial>;
  readonly badTxId?: string;
  readonly badInputsIndex?: number;
  readonly headerHash: string;
}): Candidate => {
  const searched =
    badTxId === undefined
      ? decoded
      : (() => {
          const selected = byTxId.get(badTxId);
          if (selected === undefined) {
            return reject(
              "bad_tx_not_committed",
              `bad_tx_id=${badTxId} header_hash=${headerHash}`,
            );
          }
          return [selected];
        })();

  // The block is scanned input by input, so several inputs can fail for
  // different reasons. Report the most informative one: a real input that
  // exists in its producer (the valid-block negative) outranks a pinned index
  // that is out of range, which outranks an input whose producer simply is not
  // in this block (a `non-existent-input` claim, not this family's).
  const failureRank: Readonly<Record<string, number>> = {
    input_exists_in_producing_tx: 3,
    bad_input_index_out_of_range: 2,
    producing_tx_not_committed: 1,
  };
  let pinnedFailure: InputNoIdxRejection | undefined;
  const pinFailure = (failure: InputNoIdxRejection): void => {
    if (
      pinnedFailure === undefined ||
      (failureRank[failure.code] ?? 0) > (failureRank[pinnedFailure.code] ?? 0)
    ) {
      pinnedFailure = failure;
    }
  };
  for (const badTx of searched) {
    const inputs = spendInputsOf(badTx);
    const indices =
      badInputsIndex === undefined
        ? inputs.map((_, index) => index)
        : [badInputsIndex];
    for (const index of indices) {
      const badInput = inputs[index];
      if (badInput === undefined) {
        pinFailure(
          new InputNoIdxRejection(
            "bad_input_index_out_of_range",
            `bad_tx_id=${badTx.nodeTxId} bad_inputs_index=${index.toString()} input_count=${inputs.length.toString()}`,
          ),
        );
        continue;
      }
      const producingTx = byTxId.get(badInput.tx_id);
      if (producingTx === undefined) {
        pinFailure(
          new InputNoIdxRejection(
            "producing_tx_not_committed",
            `bad_tx_id=${badTx.nodeTxId} producing_tx_id=${badInput.tx_id}; the preimage of the input's transaction id is not in this block, so this is a non-existent-input claim, not input-no-idx`,
          ),
        );
        continue;
      }
      const producingOutputs = nativeOutputItems(producingTx);
      if (
        !SDK.isInputNoIdxViolation({
          badInputOutputIndex: badInput.output_index,
          producingTxOutputCount: producingOutputs.length,
        })
      ) {
        pinFailure(
          new InputNoIdxRejection(
            "input_exists_in_producing_tx",
            `bad_tx_id=${badTx.nodeTxId} producing_tx_id=${badInput.tx_id} output_index=${badInput.output_index.toString()} producing_output_count=${producingOutputs.length.toString()}; an existing transaction input cannot be proven non-existent`,
          ),
        );
        continue;
      }
      return {
        badTx,
        badInputsIndex: index,
        badInput,
        producingTx,
        producingOutputs,
      };
    }
  }
  if (pinnedFailure !== undefined) {
    throw pinnedFailure;
  }
  return reject(
    "no_violating_input",
    `header_hash=${headerHash} tx_count=${decoded.length.toString()}`,
  );
};

export const txInclusionOf = (
  tx: DecodedTransactionMaterial,
  transactionsPhasRoot: string,
  txMembershipProofCbor: string,
): PreparedTxInclusionJson => ({
  nativeTxId: tx.nodeTxId,
  nativeTx: tx.nativeTxCompact,
  nativeTxCompactCbor: tx.nativeCompactCbor,
  l2TransactionSourceCbor: tx.l2TransactionSourceCbor,
  transactionsPhasRoot,
  txMembershipProofCbor,
});

export type PrepareInputNoIdxFromTransactionsOptions = {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly expectedTransactionsRoot: string;
  /** Pin the challenged transaction; otherwise the first violation is used. */
  readonly badTxId?: string;
  /** Pin the challenged spend-input position inside that transaction. */
  readonly badInputsIndex?: string | number;
  readonly outputDir?: string;
};

export const parseIndex = (
  value: string | number | undefined,
): number | undefined => {
  if (value === undefined) {
    return undefined;
  }
  const parsed = typeof value === "number" ? value : Number(value);
  if (!Number.isInteger(parsed) || parsed < 0) {
    return reject(
      "bad_input_index_out_of_range",
      `--bad-inputs-index must be a non-negative integer, got "${String(value)}"`,
    );
  }
  return parsed;
};
