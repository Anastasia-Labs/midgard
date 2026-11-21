import type { MidgardLedgerOutputProofRefusalKind } from "@al-ft/midgard-core";
import type { MidgardRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";

import {
  forcedTxOutputFaultRejection,
  outputProofRefusalClass,
} from "../ledger-tx/codec.locate-forced-tx-item-fault.js";
import { rejectionReasonOfSubject } from "../rejection-reason.js";

/**
 * The reason the machine stops on when its output proof refuses the output
 * at `outputIndex` with `refusal`.
 */
export const refusedOutputStopReason = (
  outputIndex: number,
  refusal: MidgardLedgerOutputProofRefusalKind,
): MidgardRejectionReasonV1 =>
  rejectionReasonOfSubject(
    forcedTxOutputFaultRejection({
      itemIndex: outputIndex,
      itemClass: outputProofRefusalClass(refusal),
    }).subject,
    "machine",
  );
