import * as SDK from "@al-ft/midgard-sdk";

import {
  type FabricatedDepositL1Witness,
  type PreparedFabricatedDepositOutput,
} from "./prepare-fabricated-deposit.classify-fabricated-deposit-fault.js";
import {
  fabricatedDepositBlockEvidenceFromVerifiedPayload,
  prepareFabricatedDepositFromCommittedLeaves,
} from "./prepare-fabricated-deposit.prepare-fabricated-deposit-from-committed-leaves.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "./transition-trace/fetch.js";

/**
 * The security-grade entry point: authenticated L1 header observation + public
 * retained-DA payload + an authenticated L1 deposit-identity witness -> a
 * submittable `fabricated-deposit` proof plan.
 */
export const prepareFabricatedDepositFromRetainedDa = async ({
  observation,
  sources,
  witness,
  retries,
  minimumConfirmationDepth,
  committedDepositIdCbor,
  outputDir,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly witness: FabricatedDepositL1Witness;
  readonly retries?: number;
  readonly minimumConfirmationDepth?: number;
  readonly committedDepositIdCbor?: string;
  readonly outputDir?: string;
}): Promise<PreparedFabricatedDepositOutput> => {
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
  const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayload({
    observation: admittedObservation,
    payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
    daProvenance: SDK.assertSecurityGradeEvidence(
      SDK.admitEvidenceProvenance({ provenance: fetched.provenance }),
    ),
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  return await prepareFabricatedDepositFromCommittedLeaves({
    headerHash: evidence.headerHash,
    committedDepositsRoot: evidence.committedDepositsRoot,
    depositCount: evidence.depositCount,
    headerStartTime: evidence.headerStartTime,
    headerEndTime: evidence.headerEndTime,
    entries: evidence.entries,
    witness,
    ...(committedDepositIdCbor === undefined ? {} : { committedDepositIdCbor }),
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};
