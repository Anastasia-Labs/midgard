import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";

import {
  checkedJourneyConstruction,
  type JourneyLedgerPredecessor,
} from "./journey-ledger.js";
import { buildJourneyScriptTransaction } from "./script-cases.js";

export type JourneyValidationTraceInput = {
  predecessor: JourneyLedgerPredecessor;
  ledgerOwnerSeedPhrase: string;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
};

/** Fee rounds before the priced transaction stops growing. */
const FEE_ROUNDS = 32;

/**
 * The owner's signed mint under a Plutus V3 policy, the fee its size prices
 * under the predecessor's schedule paid from the spent output, committed
 * alone in a block on the predecessor. `failing` mints under a policy that
 * fails, which the block commits all the same; otherwise the policy passes.
 */
const buildJourneyPlutusMintBlock = async (
  input: JourneyValidationTraceInput & { failing: boolean },
) => {
  const { minFeeA, minFeeB } = input.predecessor.header;
  const build = (fee: bigint) =>
    buildJourneyScriptTransaction({
      predecessor: input.predecessor,
      ledgerOwnerSeedPhrase: input.ledgerOwnerSeedPhrase,
      variant: input.failing ? "failingPlutusMint" : "plutusMint",
      fee,
    }).transaction;
  let fee = minFeeB;
  let transaction = build(fee);
  for (let round = 0; ; round += 1) {
    const required =
      minFeeA * BigInt(transaction.canonicalCbor.length) + minFeeB;
    if (fee >= required) break;
    if (round === FEE_ROUNDS)
      throw new Error("Plutus mint journey fee did not settle");
    fee = required;
    transaction = build(fee);
  }
  const block = await retainedTransactionFixture({
    canonicalTransactionCbor: transaction.canonicalCbor,
    predecessor: input.predecessor,
    ledgerEntries: input.predecessor.ledger.entries,
    operatorVkey: input.operatorVkey,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
    minFeeB,
  });
  return checkedJourneyConstruction(block, block.postLedgerEntries);
};

/** A committed transaction whose Plutus policy fails: the operator accepts
 * what honest validation rejects. */
export const buildJourneyValidationTraceFault = async (
  input: JourneyValidationTraceInput,
) => {
  const block = await buildJourneyPlutusMintBlock({ ...input, failing: true });
  if (block.replay.trace.verdict !== "rejected")
    throw new Error("The failing Plutus mint passed honest validation");
  return block;
};

/** The same mint under a passing policy, against the same ledger and fees. */
export const buildJourneyValidationTraceControl = async (
  input: JourneyValidationTraceInput,
) => {
  const block = await buildJourneyPlutusMintBlock({ ...input, failing: false });
  if (block.replay.trace.verdict !== "accepted")
    throw new Error(
      `The passing Plutus mint failed honest validation: ${block.replay.trace.rejectionCode}`,
    );
  return block;
};
