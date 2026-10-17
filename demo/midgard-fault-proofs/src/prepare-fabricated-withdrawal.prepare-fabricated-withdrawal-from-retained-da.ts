import * as SDK from "@al-ft/midgard-sdk";

import {
  type FabricatedWithdrawalL1Witness,
  type PreparedFabricatedWithdrawalOutput,
} from "./prepare-fabricated-withdrawal.classify-fabricated-withdrawal-fault.js";
import {
  fabricatedWithdrawalBlockEvidenceFromVerifiedPayload,
  prepareFabricatedWithdrawalFromCommittedLeaves,
} from "./prepare-fabricated-withdrawal.prepare-fabricated-withdrawal-from-committed-leaves.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "./transition-trace/fetch.js";

/**
 * The security-grade entry point: authenticated L1 header observation + public
 * retained-DA payload + an authenticated L1 withdrawal-identity witness -> a
 * submittable `fabricated-withdrawal` proof plan.
 */
export const prepareFabricatedWithdrawalFromRetainedDa = async ({
  observation,
  sources,
  witness,
  retries,
  minimumConfirmationDepth,
  committedWithdrawalIdCbor,
  outputDir,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly witness: FabricatedWithdrawalL1Witness;
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
  readonly committedWithdrawalIdCbor?: string;
  readonly outputDir?: string;
}): Promise<PreparedFabricatedWithdrawalOutput> => {
  if (sources.length === 0) {
    throw new SDK.CanonicalEvidenceRejection(
      "da_evidence_wrong_trust_class",
      "no public DA source was configured",
    );
  }
  const admittedObservation =
    await SDK.admitAuthenticatedStateQueueHeaderObservation({
      observation,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
  const fetched = await fetchRetainedDaPayloadByHeaderHash({
    headerHash: admittedObservation.headerHash,
    sources,
    ...(retries === undefined ? {} : { retries }),
  });
  const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload({
    observation: admittedObservation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance: SDK.assertSecurityGradeEvidence(
      SDK.admitEvidenceProvenance({ provenance: fetched.provenance }),
    ),
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  return await prepareFabricatedWithdrawalFromCommittedLeaves({
    headerHash: evidence.headerHash,
    committedWithdrawalsRoot: evidence.committedWithdrawalsRoot,
    withdrawalCount: evidence.withdrawalCount,
    headerStartTime: evidence.headerStartTime,
    headerEndTime: evidence.headerEndTime,
    entries: evidence.entries,
    witness,
    ...(committedWithdrawalIdCbor === undefined
      ? {}
      : { committedWithdrawalIdCbor }),
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};
