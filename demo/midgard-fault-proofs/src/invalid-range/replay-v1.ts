import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import { nativeTxFromCoreCompact } from "../submit-step-01.js";
import {
  invalidRangeEvidenceCloses,
  prepareInvalidRangeEvidence,
} from "./family-v1.js";

export const detectInvalidRangeForcedReplay = (block: CanonicalBlockEvidence) =>
  Object.freeze(
    block.reconstruction.forcedTransactions.flatMap((transaction, index) => {
      const verdict = transaction.value.verdict;
      if (verdict === "ForcedTxValid") return [];
      const reason = verdict.ForcedTxInvalid.reason;
      if (
        reason !== "ValidityIntervalMalformed" &&
        reason !== "ValidityIntervalExcludesBlockSlot"
      )
        return [];
      const compact = decodeMidgardNativeTxCompact(
        Buffer.from(transaction.value.source.compact_cbor, "hex"),
      );
      const evidence = prepareInvalidRangeEvidence({
        subject: SDK.forcedVerdictSubject({
          transactionId: transaction.value.tx_id,
          sourceKey: transaction.key,
          rejectionReason: reason,
        }),
        blockSlot: block.header.blockSlot,
        txBody: nativeTxFromCoreCompact(compact).body,
      });
      return invalidRangeEvidenceCloses(evidence)
        ? [
            Object.freeze({
              detectionId: `invalid-range:forced:${index.toString()}:${transaction.value.tx_id}:${reason}`,
              headerHash: block.headerHash,
              violationId: "invalid-range" as const,
              position: BigInt(index),
              forcedIndex: index,
              evidence,
            }),
          ]
        : [];
    }),
  );
