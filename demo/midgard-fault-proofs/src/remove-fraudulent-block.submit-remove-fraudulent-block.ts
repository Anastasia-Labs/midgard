import { formatUnknownError } from "@al-ft/midgard-core";

import { readFraudSlashFundingAuthority } from "./remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import { buildRemoveFraudulentBlock } from "./remove-fraudulent-block.build-remove-fraudulent-block.js";
import type {
  RemoveTransactionResult,
  StateQueueMutationLease,
  SubmitRemoveFraudulentBlockResult,
} from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import {
  type LocallyEvaluatedTransaction,
  submitUnjournaled,
} from "./workflow/transaction-boundary.js";

/**
 * Removes a fraud-proved block from the state queue by submitting each
 * removal directly: every successor removal in order, then the target
 * removal. It writes no workflow journal entry. Its callers are the
 * diagnostic CLI, tests, and the chain-driven proven-correction removal,
 * which needs no journal because the correction lock and fraud-proof record
 * on L1 say what is left to remove, so anyone can resume it.
 *
 * A non-tail removal requires `awaitConfirmation`, refused before any lease
 * or build, because each successor removal must be confirmed and the
 * topology refetched before the next transaction is built. The state-queue
 * mutation lease the first successor removal takes is renewed after every
 * submission, released once the target is removed, and failed on any error:
 * by the builder when the first build that acquired it throws, and here on
 * any later build, check or submission error.
 * `beforeSubmit` sees each signed removal before it is sent.
 */
export const submitRemoveFraudulentBlock = async ({
  awaitConfirmation = true,
  beforeSubmit,
  ...input
}: Omit<
  Parameters<typeof buildRemoveFraudulentBlock>[0],
  "heldStateQueueMutationLease" | "admitTarget"
> & {
  readonly awaitConfirmation?: boolean;
  readonly beforeSubmit?: (
    transaction: LocallyEvaluatedTransaction,
  ) => Promise<void> | void;
}): Promise<SubmitRemoveFraudulentBlockResult> => {
  const transactions: RemoveTransactionResult[] = [];
  let lease: StateQueueMutationLease | undefined;
  let context:
    | Awaited<ReturnType<typeof buildRemoveFraudulentBlock>>["context"]
    | undefined;
  while (true) {
    try {
      const {
        transaction,
        context: built,
        stateQueueMutationLease,
        ...removal
      } = await buildRemoveFraudulentBlock({
        ...input,
        ...(lease === undefined ? {} : { heldStateQueueMutationLease: lease }),
        ...(transactions.length > 0
          ? {}
          : {
              admitTarget: ({ hasSuccessor }) => {
                if (!awaitConfirmation && hasSuccessor) {
                  throw new Error(
                    "Removing a non-tail fraudulent block requires --await-confirmation so each successor removal can be confirmed and refetched before the next transaction.",
                  );
                }
              },
            }),
      });
      lease = stateQueueMutationLease ?? lease;
      context ??= built;
      await beforeSubmit?.(transaction);
      // The slash must reach the network exactly as it was evaluated.
      readFraudSlashFundingAuthority(transaction.signed);
      const txHash = await submitUnjournaled(input.lucid, transaction, {
        label: "remove-fraudulent-block",
        awaitConfirmation,
      });
      await lease?.renew();
      transactions.push({ txHash, ...removal });
      if (removal.kind === "remove-target") {
        await lease?.release();
        return {
          ...context,
          txHash,
          activeOperatorNodeOutRef:
            removal.slashingApproach === "SlashActiveOperator"
              ? removal.operatorNodeOutRef
              : null,
          registeredOperatorsElementOutRef:
            transactions.find(
              (tx) => tx.registeredOperatorsElementOutRef !== null,
            )?.registeredOperatorsElementOutRef ?? null,
          transactions,
          layout: removal.layout,
          awaitedConfirmation: awaitConfirmation,
          stateQueueMutationLease:
            lease === undefined
              ? null
              : { token: lease.token, source: lease.source, released: true },
        };
      }
    } catch (error) {
      // A first build that throws has already failed the lease it acquired,
      // and returned none, so `lease` is unset; from then on the lease is
      // held here, and the builder leaves failing it to this catch.
      await lease?.fail(formatUnknownError(error));
      throw error;
    }
  }
};
