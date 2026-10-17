import { encodeMidgardNativeTxCanonical } from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  Proof,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  prepareResolvedOutputNonCanonicalEvidence,
  type ResolvedOutputCoordinate,
  type ResolvedOutputEvidence,
  type ResolvedOutputForcedSource,
} from "../../src/resolved-output-non-canonical/index.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import {
  type PriorLedgerFixture,
  resolvedOutputReason,
} from "./resolved-output-non-canonical-emulator.build-prior-ledger.js";
import { type CommittedBlock } from "./resolved-output-non-canonical-emulator.commit-block.js";
import { transitionTraceOutRef } from "./submit-init-emulator-shared.js";

/**
 * Evidence from the retained prior ledger and the committed transaction. For
 * the honest cases (where `prepare` refuses because the output agrees with
 * the verdict) `claim` builds the evidence under the contradicting subject
 * and then swaps in the honest subject, which is what a lying prover holds.
 */
export const resolvedOutputEvidence = ({
  block,
  prior,
  coordinate,
  claim,
}: {
  readonly block: CommittedBlock;
  readonly prior: PriorLedgerFixture;
  readonly coordinate: ResolvedOutputCoordinate;
  readonly claim?: "lying";
}): ResolvedOutputEvidence => {
  const subject: VerdictSubject =
    block.forced === undefined
      ? acceptedVerdictSubject(block.nativeTxId)
      : forcedVerdictSubject({
          transactionId: block.nativeTxId,
          sourceKey: block.forced.membership.key,
          rejectionReason: block.forced.reason,
        });
  const resolved = {
    priorRoot: prior.priorRoot,
    transactionId: prior.priorTxId,
    outputIndex: prior.outputIndex,
    descriptorCborHex: prior.descriptorCbor.toString("hex"),
    outputCborHex: prior.output.toString("hex"),
    membershipProofCborHex: prior.proofCborHex,
    membershipProof: Data.from(prior.proofCborHex, Proof),
  };
  if (claim === undefined) {
    return prepareResolvedOutputNonCanonicalEvidence({
      subject,
      coordinate,
      canonicalTransactionCbor: block.canonicalCbor,
      resolved,
    });
  }
  const contradicting: VerdictSubject =
    block.forced === undefined
      ? forcedVerdictSubject({
          transactionId: block.nativeTxId,
          sourceKey: transitionTraceOutRef("f1"),
          rejectionReason: resolvedOutputReason(coordinate),
        })
      : acceptedVerdictSubject(block.nativeTxId);
  const prepared = prepareResolvedOutputNonCanonicalEvidence({
    subject: contradicting,
    coordinate,
    canonicalTransactionCbor: (contradicting.source_kind === 1n
      ? encodeMidgardForcedTxCanonical
      : encodeMidgardNativeTxCanonical)(block.nativeTx),
    resolved,
  });
  return Object.freeze({
    ...prepared,
    subject,
    canonicalTransactionCborHex: block.canonicalCbor.toString("hex"),
  });
};

export const forcedSourceOf = (
  block: CommittedBlock,
): ResolvedOutputForcedSource => {
  if (block.forced === undefined)
    throw new Error("block carries no forced leaf");
  return {
    header: block.header,
    membership: block.forced.membership,
    direction: 1n,
  };
};

// ---------------------------------------------------------------------------
// Stages
// ---------------------------------------------------------------------------

export type Captured<T> = Awaited<
  ReturnType<typeof captureEmulatorSubmission<T>>
>;
