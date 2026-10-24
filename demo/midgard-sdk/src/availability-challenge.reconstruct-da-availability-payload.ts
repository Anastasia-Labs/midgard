import { Data, fromHex } from "@lucid-evolution/lucid";

import { parseDaAvailabilityChallengeRecordCbor } from "./availability-challenge.assert-canonical-da-availability-commitment.js";
import {
  assertCanonicalDaAvailabilityParameters,
  daAvailabilityTrancheStartAccumulator,
} from "./availability-challenge.assert-canonical-da-availability-parameters.js";
import {
  type DaAvailabilityTranchePublicationPlan,
  verifyDaAvailabilityPayloadCommitment,
} from "./availability-challenge.assert-canonical-da-availability-publication-datum.js";
import {
  daAvailabilityChallengeAssetName,
  DaAvailabilityCommitmentError,
} from "./availability-challenge.da-availability-mint-redeemer-schema.js";
import {
  DaAvailabilityChallengeRecord,
  DaAvailabilityParameters,
  DaAvailabilityTrancheDatum,
  DaAvailabilityTrancheDescriptorSchema,
  HASH_32,
} from "./availability-challenge.da-availability-tranche-datum-schema.js";
import {
  advanceDaAvailabilityTranche,
  type DaAvailabilityTrancheEvidence,
  planDaAvailabilityPublications,
} from "./availability-challenge.plan-da-availability-publications.js";
import { type OutputReference } from "./common.js";

/**
 * Authenticated challenge-record evidence, read from the admitted raw-L1
 * `OpenChallenge` transaction.
 */
export type DaAvailabilityChallengeRecordEvidence = Readonly<{
  /** Exact inline datum read from the challenge record output. */
  datumCborHex: string;
  /** Challenger funding input consumed by `OpenChallenge`; derives DACH. */
  challengerFundingOutRef: OutputReference;
  /** Challenge record output carrying the datum and the DACH token. */
  recordOutputOutRef: OutputReference;
}>;

const assertCanonicalDaAvailabilityEvidenceOutRef = (
  outRef: OutputReference,
  field: string,
): void => {
  if (
    typeof outRef !== "object" ||
    outRef === null ||
    Object.getPrototypeOf(outRef) !== Object.prototype ||
    Reflect.ownKeys(outRef).length !== 2 ||
    !Reflect.has(outRef, "transactionId") ||
    !Reflect.has(outRef, "outputIndex") ||
    !HASH_32.test(outRef.transactionId) ||
    outRef.outputIndex < 0n ||
    outRef.outputIndex > 65_535n
  ) {
    throw new DaAvailabilityCommitmentError(
      `${field} must be a canonical bounded Cardano output reference`,
    );
  }
};

const challengeRecordFromEvidence = (
  evidence: DaAvailabilityChallengeRecordEvidence,
  parameters: DaAvailabilityParameters,
): DaAvailabilityChallengeRecord => {
  assertCanonicalDaAvailabilityParameters(parameters);
  if (
    typeof evidence !== "object" ||
    evidence === null ||
    Object.getPrototypeOf(evidence) !== Object.prototype ||
    Reflect.ownKeys(evidence).length !== 3 ||
    !Reflect.has(evidence, "datumCborHex") ||
    !Reflect.has(evidence, "challengerFundingOutRef") ||
    !Reflect.has(evidence, "recordOutputOutRef")
  ) {
    throw new DaAvailabilityCommitmentError(
      "challengeRecord evidence must contain exactly datum and input/output identities",
    );
  }
  assertCanonicalDaAvailabilityEvidenceOutRef(
    evidence.challengerFundingOutRef,
    "challengeRecord.challengerFundingOutRef",
  );
  assertCanonicalDaAvailabilityEvidenceOutRef(
    evidence.recordOutputOutRef,
    "challengeRecord.recordOutputOutRef",
  );
  if (
    evidence.challengerFundingOutRef.transactionId ===
      evidence.recordOutputOutRef.transactionId &&
    evidence.challengerFundingOutRef.outputIndex ===
      evidence.recordOutputOutRef.outputIndex
  ) {
    throw new DaAvailabilityCommitmentError(
      "challenge record output cannot equal its consumed challenger funding input",
    );
  }
  const record = parseDaAvailabilityChallengeRecordCbor(
    evidence.datumCborHex,
    parameters,
  );
  const expectedChallengeAssetName = daAvailabilityChallengeAssetName(
    evidence.challengerFundingOutRef,
  );
  if (record.challenge_asset_name !== expectedChallengeAssetName) {
    throw new DaAvailabilityCommitmentError(
      "challenge record does not carry the DACH identity derived from its consumed challenger funding input",
    );
  }
  return record;
};

/**
 * Response planner whose identity, commitment and deadline originate in the
 * exact challenge-record datum. Production callers must obtain the evidence
 * from the admitted raw-L1 `OpenChallenge` transaction.
 */
export const planDaAvailabilityPublicationsFromChallengeRecord = (input: {
  readonly challengeRecord: DaAvailabilityChallengeRecordEvidence;
  readonly parameters: DaAvailabilityParameters;
  readonly payload: Uint8Array;
}): readonly DaAvailabilityTranchePublicationPlan[] => {
  const record = challengeRecordFromEvidence(
    input.challengeRecord,
    input.parameters,
  );
  return planDaAvailabilityPublications({
    commitment: record.commitment,
    payload: input.payload,
    challengeAssetName: record.challenge_asset_name,
  });
};

/**
 * Public-evidence reconstruction from authenticated L1 publication history.
 * The caller supplies chain-ordered observations; this verifier never sorts or
 * repairs them, so a missing/reordered/replayed chunk fails closed.
 */
export const reconstructDaAvailabilityPayload = (input: {
  readonly challengeRecord: DaAvailabilityChallengeRecordEvidence;
  readonly parameters: DaAvailabilityParameters;
  readonly tranches: readonly DaAvailabilityTrancheEvidence[];
}): Uint8Array => {
  const challenged = challengeRecordFromEvidence(
    input.challengeRecord,
    input.parameters,
  );
  const commitment = challenged.commitment;
  if (input.tranches.length !== commitment.tranche_descriptors.length) {
    throw new DaAvailabilityCommitmentError(
      "public evidence tranche count does not equal the signed descriptor count",
    );
  }
  const payloadParts: Uint8Array[] = [];
  for (const [index, descriptor] of commitment.tranche_descriptors.entries()) {
    const evidence = input.tranches[index];
    if (
      evidence === undefined ||
      Data.to(
        evidence.descriptor as never,
        DaAvailabilityTrancheDescriptorSchema as never,
      ) !==
        Data.to(
          descriptor as never,
          DaAvailabilityTrancheDescriptorSchema as never,
        )
    ) {
      throw new DaAvailabilityCommitmentError(
        `public evidence tranche ${index.toString()} is missing or reordered`,
      );
    }
    let state: DaAvailabilityTrancheDatum = {
      Active: {
        deployment_identity: commitment.deployment_identity,
        header_hash: commitment.header_hash,
        challenge_asset_name: challenged.challenge_asset_name,
        descriptor,
        next_offset: descriptor.start_offset,
        accumulator: daAvailabilityTrancheStartAccumulator({
          deploymentIdentity: commitment.deployment_identity,
          headerHash: commitment.header_hash,
          trancheIndex: Number(descriptor.tranche_index),
          startOffset: Number(descriptor.start_offset),
          byteLength: Number(descriptor.byte_length),
        }),
        latest_carrier_output_index: null,
        response_deadline: challenged.response_deadline,
        challenger: challenged.challenger,
      },
    };
    for (const observation of evidence.publications) {
      state = advanceDaAvailabilityTranche({
        active: state,
        publication: observation.publication,
        responseGeometry: commitment.response_geometry,
        inclusiveValidityUpper: observation.inclusiveValidityUpper,
        carrierOutputIndex: observation.carrierOutputIndex,
      });
      payloadParts.push(fromHex(observation.publication.chunk));
    }
    if (typeof state !== "object" || !("Receipt" in state)) {
      throw new DaAvailabilityCommitmentError(
        `public evidence tranche ${index.toString()} is incomplete`,
      );
    }
  }
  const payload = Uint8Array.from(
    Buffer.concat(payloadParts.map((part) => Buffer.from(part))),
  );
  if (
    !verifyDaAvailabilityPayloadCommitment({
      commitment,
      payload,
    })
  ) {
    throw new DaAvailabilityCommitmentError(
      "reconstructed public evidence does not equal the signed payload commitment",
    );
  }
  return payload;
};
