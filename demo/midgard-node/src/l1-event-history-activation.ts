import * as SDK from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";

import {
  type BoundHistoryChainBlock,
  type EventHistorySourceBinding,
  verifyEventHistoryHubOutputs,
} from "./l1-event-history-source.js";
import type { HistoryTransition } from "./l1-event-history-transition.js";

const fail = (message: string): never => {
  throw new Error(`Invalid history initialization replay: ${message}`);
};

/** Establish the unique paired activation within a whole valid source block.
 * Authenticity of scripts and canonical block admission are owner preconditions.
 * The shared consumed nonce rules out earlier authenticated nodes on this branch.
 */
export const verifyEventHistoryActivation = ({
  block,
  binding,
  histories,
  transitions,
}: {
  readonly block: BoundHistoryChainBlock;
  readonly binding: EventHistorySourceBinding;
  readonly histories: SDK.EventHistoryContractPair;
  readonly transitions: readonly Readonly<{
    transactionIndex: number;
    transition: HistoryTransition;
  }>[];
}) => {
  const hubPolicy = binding.hubUnit.slice(0, 56);
  const nonce = histories.deposit.recipe.initializationNonce;
  const withdrawalNonce = histories.withdrawal.recipe.initializationNonce;
  if (
    nonce.transactionId !== withdrawalNonce.transactionId ||
    nonce.outputIndex !== withdrawalNonce.outputIndex
  )
    fail("both history recipes must share their initialization nonce");

  const initializations = transitions.filter(
    ({ transition }) => transition.operation === "Initialize",
  );
  const deposit = initializations.find(
    ({ transition }) => transition.kind === "deposit",
  );
  const withdrawal = initializations.find(
    ({ transition }) => transition.kind === "withdrawal",
  );
  if (
    initializations.length !== 2 ||
    deposit === undefined ||
    withdrawal === undefined ||
    deposit.transactionIndex !== withdrawal.transactionIndex
  )
    return fail("both histories must initialize in one activation transaction");
  const activationIndex = deposit.transactionIndex;
  const activation = block.transactions[activationIndex]!;
  if (
    activation.spends !== "inputs" ||
    activation.inputs.filter(
      (ref) =>
        ref.txHash === nonce.transactionId &&
        BigInt(ref.outputIndex) === nonce.outputIndex,
    ).length !== 1 ||
    transitions.some(
      ({ transactionIndex }) => transactionIndex < activationIndex,
    )
  )
    fail("activation must consume the exact ordinary deployment nonce first");
  const hubMint = Object.entries(activation.mint).filter(([unit]) =>
    unit.startsWith(hubPolicy),
  );
  const correctionUnit = toUnit(hubPolicy, SDK.CORRECTION_LOCK_ASSET_NAME);
  if (
    hubMint.length !== 2 ||
    activation.mint[binding.hubUnit] !== 1n ||
    activation.mint[correctionUnit] !== 1n
  )
    fail("activation must mint the exact hub and correction lock together");
  verifyEventHistoryHubOutputs(activation.outputs, binding);
  return Object.freeze({
    activationIndex,
    activationTransactionHash: activation.txHash,
  });
};
