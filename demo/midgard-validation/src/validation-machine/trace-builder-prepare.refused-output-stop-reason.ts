import { isDeepStrictEqual } from "node:util";

import type { MidgardLedgerOutputProofRefusalKind } from "@al-ft/midgard-core";
import type { MidgardRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";

import {
  forcedTxOutputFaultRejection,
  outputProofRefusalClass,
} from "../ledger-tx/codec.locate-forced-tx-item-fault.js";
import { rejectionReasonOfSubject } from "../rejection-reason.js";
import type { RejectedTx } from "../types.js";

/**
 * The reason the machine stops on when its output proof refuses the output
 * at `outputIndex` with `refusal`, or an error when replay did not reject the
 * transaction there with that same reason.
 */
export const refusedOutputStopReason = ({
  outputIndex,
  refusal,
  rejection,
  terminalPhase,
}: {
  readonly outputIndex: number;
  readonly refusal: MidgardLedgerOutputProofRefusalKind;
  readonly rejection: RejectedTx | null;
  readonly terminalPhase: string;
}): MidgardRejectionReasonV1 | Error => {
  const expected = forcedTxOutputFaultRejection({
    itemIndex: outputIndex,
    itemClass: outputProofRefusalClass(refusal),
  });
  const reason = rejectionReasonOfSubject(expected.subject, "machine");
  return terminalPhase === "scriptSources" &&
    rejection?.code === expected.code &&
    rejection.subject !== undefined &&
    isDeepStrictEqual(
      rejectionReasonOfSubject(rejection.subject, "machine"),
      reason,
    )
    ? reason
    : new Error(
        `output ${outputIndex} fails its output proof (${refusal}) but replay rejected at ${terminalPhase}/${rejection?.code ?? "none"}`,
      );
};
