import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import type * as SDK from "@al-ft/midgard-sdk";

import { buildJourneyScriptTransaction } from "./script-cases.js";
import type { JourneyTransactionInput } from "./transaction-cases.js";

/** Wrongful operator reasons applied to a valid, signed forced transaction. */
const wrongfulReasons = {
  minAda: { OutputBelowMinAda: { output_index: 0n } },
  valueNotPreserved: "ValueNotPreserved",
  missingSignature: { RequiredSignerUnsigned: { signer_index: 0n } },
  transactionOutputNonCanonical: { OutputNonCanonical: { output_index: 0n } },
  resolvedOutputNonCanonical: {
    InputSpentOutputNonCanonical: { source_kind: 0n, input_index: 0n },
  },
  fieldPreimageLengthMismatch: {
    FieldPreimageLengthMismatch: { field_index: 0n },
  },
  fieldItemWidthIllegal: {
    FieldItemWidthIllegal: { field_index: 5n, item_index: 0n },
  },
  mintDeclaredAssetLimit: { MintDeclaredAssetLimit: { policy_index: 0n } },
  distinctAssetAccumulationLimit: {
    MintAssetAccumulationLimit: { mint_index: 0n },
  },
} satisfies Partial<
  Record<SDK.FraudProofCatalogueCategoryName, SDK.RejectionReason>
>;

export type JourneyForcedTransactionCategory = keyof typeof wrongfulReasons;
export const JOURNEY_FORCED_TRANSACTION_CATEGORIES = Object.keys(
  wrongfulReasons,
) as JourneyForcedTransactionCategory[];

/** Exact bytes to place in the real L1 order before building its claimed verdict. */
export const prepareJourneyForcedTransaction = buildJourneyScriptTransaction;

export const buildJourneyForcedTransaction = async (
  input: JourneyTransactionInput & {
    category: JourneyForcedTransactionCategory;
    orderKey: SDK.OutputReference;
    honest?: boolean;
  },
) => {
  const material = prepareJourneyForcedTransaction(input);
  const block = await retainedTransactionFixture({
    canonicalTransactionCbor: material.transaction.canonicalCbor,
    programMaterial: material.programMaterial,
    predecessor: input.predecessor,
    ledgerEntries: input.predecessor.payload.block_body.utxos.map(
      ([key, value]) => ({
        outRef: Buffer.from(key, "hex"),
        output: Buffer.from(value, "hex"),
      }),
    ),
    operatorVkey: input.operatorVkey,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
    source: {
      kind: "forced",
      orderKey: input.orderKey,
      verdict: input.honest
        ? "ForcedTxValid"
        : { ForcedTxInvalid: { reason: wrongfulReasons[input.category] } },
    },
  });
  if (block.replay.trace.verdict !== "accepted")
    throw new Error(
      `The valid forced ${input.category} control failed validation: ${block.replay.trace.rejectionCode}`,
    );
  return block;
};
