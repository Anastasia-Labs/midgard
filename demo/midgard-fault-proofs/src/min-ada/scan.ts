import {
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";

import {
  prepareTransactionOutputEvidence,
  transactionOutputScanControlData,
} from "../transaction-output-non-canonical/transaction-output-non-canonical.js";
export { transactionOutputScanControlData };
export const minAdaOutputScanEvidence = (
  transactionId: string,
  outputIndex: bigint,
  itemCbors: readonly string[],
) => {
  const field = encodeMidgardFieldPreimage(
    itemCbors.map((item) => Buffer.from(item, "hex")),
  );
  return prepareTransactionOutputEvidence({
    finding: {
      subject: acceptedVerdictSubject(transactionId),
      fieldIndex: 2,
      itemIndex: Number(outputIndex),
    },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
};
export const minAdaInitialScanState = (
  evidence: ReturnType<typeof minAdaOutputScanEvidence>,
) => ({
  subject: evidence.subject,
  output_index: BigInt(evidence.itemIndex),
  item_length: BigInt(evidence.itemLength),
  item_hash: evidence.itemHash,
  chunk_hashes: [...evidence.chunkHashes],
  control: transactionOutputScanControlData(evidence.scanControls[0]!),
  outcome: 0n,
});
