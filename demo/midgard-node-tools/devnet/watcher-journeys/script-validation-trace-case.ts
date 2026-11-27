import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";

import {
  journeyOwnerEntries,
  type JourneyOwnerTransactionInput,
} from "./forced-transaction.js";
import { buildAtJourneyMinFee } from "./journey-fee.js";
import {
  checkedJourneyConstruction,
  type JourneyLedgerPredecessor,
} from "./journey-ledger.js";
import {
  type OwnedOutput,
  type OwnerSpend,
  selectOwnerOutput,
} from "./owner-output-selection.js";
import { buildJourneyScriptTransaction } from "./script-cases.js";

export type JourneyValidationTraceInput = {
  predecessor: JourneyLedgerPredecessor;
  ledgerOwnerSeedPhrase: string;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
};

/** The owner's Plutus mint spending `spent`, at its minimum fee. */
const plutusMintOf = (
  input: JourneyOwnerTransactionInput,
  failing: boolean,
  spent: OwnedOutput,
) =>
  buildAtJourneyMinFee(input.predecessor.header, (fee) => {
    const built = buildJourneyScriptTransaction({
      predecessor: input.predecessor,
      ledgerOwnerSeedPhrase: input.ledgerOwnerSeedPhrase,
      spent,
      variant: failing ? "failingPlutusMint" : "plutusMint",
      fee,
    });
    return { canonicalCbor: built.transaction.canonicalCbor, built };
  }).built;

/**
 * What the validation-trace family pays from an output: its failing mint and
 * its passing control, each carrying a reference script, so the fault and
 * the control spend the same output.
 */
export const journeyValidationTraceSpend =
  (input: JourneyOwnerTransactionInput): OwnerSpend =>
  (entry) => [
    ...plutusMintOf(input, true, entry).outputs,
    ...plutusMintOf(input, false, entry).outputs,
  ];

/**
 * The owner's signed mint under a Plutus V3 policy, the fee its size prices
 * under the predecessor's schedule paid from the owner's smallest output that
 * funds it (`selectOwnerOutput`), committed alone in a block on the
 * predecessor. `failing` mints under a policy that fails, which the block
 * commits all the same; otherwise the policy passes.
 */
const buildJourneyPlutusMintBlock = async (
  input: JourneyValidationTraceInput & { failing: boolean },
) => {
  const { minFeeB } = input.predecessor.header;
  const { transaction } = plutusMintOf(
    input,
    input.failing,
    selectOwnerOutput(
      journeyOwnerEntries(input),
      journeyValidationTraceSpend(input),
      "the validation-trace dispute's mints",
    ),
  );
  const block = await retainedTransactionFixture({
    canonicalTransactionCbor: transaction.canonicalCbor,
    predecessor: input.predecessor,
    ledgerEntries: input.predecessor.ledgerEntries,
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
