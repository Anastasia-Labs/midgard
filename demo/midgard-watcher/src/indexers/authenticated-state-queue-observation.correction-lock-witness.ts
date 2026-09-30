import { type FraudProofRawL1Transaction } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import { type QueueNode } from "./authenticated-state-queue-observation.parse-persisted-header.js";
import { outputReferences } from "./authenticated-state-queue-observation.parse-persisted-observation.js";
import { lockOutput } from "./authenticated-state-queue-observation.queue-output.js";
import {
  decodeLockOutputs,
  fraudProofIdentity,
  orderedResolved,
} from "./authenticated-state-queue-observation.reconstruct-queue.js";

export const correctionLockWitness = ({
  raw,
  body,
  mintPolicies,
  redeemers,
  stateQueuePolicyId,
  correctionLockAddress,
  hubOraclePolicyId,
  fraudProofPolicyId,
  fraudProofAddress,
  availabilityChallengePolicyId,
}: {
  raw: FraudProofRawL1Transaction;
  body: CML.TransactionBody;
  mintPolicies: readonly string[];
  redeemers: readonly SDK.StateQueueTransitionRedeemer[];
  stateQueuePolicyId: string;
  correctionLockAddress: string;
  hubOraclePolicyId: string;
  fraudProofPolicyId: string;
  fraudProofAddress: string;
  availabilityChallengePolicyId: string;
}): SDK.StateQueueCorrectionLockWitness => {
  const spentRefs = outputReferences(body.inputs());
  const referenceRefs = outputReferences(body.reference_inputs());
  const spentResolved = orderedResolved(spentRefs, raw.resolvedInputs);
  const referenceResolved = orderedResolved(
    referenceRefs,
    raw.resolvedReferenceInputs,
  );
  const locksIn = spentResolved.flatMap((input) => {
    const decoded = lockOutput({
      output: CML.TransactionOutput.from_cbor_hex(input.outputCbor),
      outRef: input.outRef,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return decoded === null ? [] : [decoded];
  });
  const locksReferenced = referenceResolved.flatMap((input) => {
    const decoded = lockOutput({
      output: CML.TransactionOutput.from_cbor_hex(input.outputCbor),
      outRef: input.outRef,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return decoded === null ? [] : [decoded];
  });
  const locksOut = decodeLockOutputs({
    body,
    transactionHash: raw.txHash,
    correctionLockAddress,
    hubOraclePolicyId,
  });
  const policyIndex = mintPolicies.indexOf(stateQueuePolicyId);
  if (policyIndex < 0) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      locksOut.length !== 0
    ) {
      throw new Error("non-mint queue update touched CorrectionLock");
    }
    return { kind: "none" };
  }
  const mintRedeemer = redeemers.filter(
    ({ purpose, index }) =>
      purpose === "mint" && index === policyIndex.toString(),
  );
  if (mintRedeemer.length !== 1)
    throw new Error("state-queue mint redeemer is not unique");
  const decoded = Data.from(
    mintRedeemer[0]!.cborHex,
    SDK.StateQueueRedeemer,
  ) as SDK.StateQueueRedeemer;
  if (typeof decoded === "object" && decoded !== null && "InitV1" in decoded) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      locksOut.length !== 1 ||
      locksOut[0]!.datum !== "Idle"
    ) {
      throw new Error("state-queue Init has invalid CorrectionLock topology");
    }
    return {
      kind: "genesis",
      producedOutRef: locksOut[0]!.outRef,
      nextDatum: "Idle",
    };
  }
  if (decoded === "Deinit") {
    if (
      locksIn.length !== 1 ||
      locksIn[0]!.datum !== "Idle" ||
      locksReferenced.length !== 0 ||
      locksOut.length !== 0
    ) {
      throw new Error("state-queue Deinit has invalid CorrectionLock topology");
    }
    return {
      kind: "deinit",
      consumedOutRef: locksIn[0]!.outRef,
      previousDatum: "Idle",
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    ("CommitBlockHeader" in decoded || "MergeToConfirmedStateV1" in decoded)
  ) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 1 ||
      locksReferenced[0]!.datum !== "Idle" ||
      locksOut.length !== 0
    ) {
      throw new Error("append/merge has invalid CorrectionLock topology");
    }
    return {
      kind: "idle_reference",
      referenceOutRef: locksReferenced[0]!.outRef,
      datum: "Idle",
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    ("RemoveUnattestedBlockAfterTimeout" in decoded ||
      "RemoveFraudulentBlockHeader" in decoded)
  ) {
    if (
      locksIn.length !== 1 ||
      locksReferenced.length !== 0 ||
      locksOut.length !== 1
    ) {
      throw new Error("correction has invalid CorrectionLock topology");
    }
    const timeout = "RemoveUnattestedBlockAfterTimeout" in decoded;
    const targetHeaderHash = timeout
      ? decoded.RemoveUnattestedBlockAfterTimeout.timed_out_header_hash
      : decoded.RemoveFraudulentBlockHeader.fraudulent_blocks_header_hash;
    const identity: SDK.CorrectionIdentity = timeout
      ? "AttestationTimeout"
      : fraudProofIdentity({
          proof:
            referenceResolved[
              Number(
                decoded.RemoveFraudulentBlockHeader.fraud_proof_ref_input_index,
              )
            ] ??
            (() => {
              throw new Error("fraud proof reference index is out of bounds");
            })(),
          fraudProofPolicyId,
          fraudProofAddress,
          targetHeaderHash,
        });
    return {
      kind: "correction_transition",
      consumedOutRef: locksIn[0]!.outRef,
      continuedOutRef: locksOut[0]!.outRef,
      targetHeaderHash,
      correctionIdentity: identity,
      previousDatum: locksIn[0]!.datum,
      nextDatum: locksOut[0]!.datum,
    };
  }
  if (
    typeof decoded === "object" &&
    decoded !== null &&
    "RemoveUnavailableBlockAfterTimeout" in decoded
  ) {
    if (
      locksIn.length !== 1 ||
      locksReferenced.length !== 0 ||
      locksOut.length !== 1
    ) {
      throw new Error(
        "availability timeout has invalid CorrectionLock topology",
      );
    }
    const timeout = decoded.RemoveUnavailableBlockAfterTimeout;
    const identity: SDK.CorrectionIdentity = {
      AvailabilityChallenge: {
        challenge_asset_name: timeout.challenge_asset_name,
      },
    };
    const previousDatum = locksIn[0]!.datum;
    if (previousDatum === "Idle") {
      // The step that takes the lock is the Timeout itself: it must burn the
      // exact challenge token the redeemer names, so the identity carried by
      // the lock is the settled-and-timed-out challenge, not a label.
      const mint = body.mint();
      const burned = mint
        ?.get_assets(CML.ScriptHash.from_hex(availabilityChallengePolicyId))
        ?.get(CML.AssetName.from_hex(timeout.challenge_asset_name));
      if (burned !== -1n)
        throw new Error(
          "availability timeout does not burn the challenge it names",
        );
    } else {
      const locked = previousDatum.Locked;
      const lockedIdentity = locked.correction_identity;
      if (
        locked.target_header_hash !== timeout.unavailable_header_hash ||
        typeof lockedIdentity !== "object" ||
        lockedIdentity === null ||
        !("AvailabilityChallenge" in lockedIdentity) ||
        lockedIdentity.AvailabilityChallenge.challenge_asset_name !==
          timeout.challenge_asset_name
      )
        throw new Error(
          "availability timeout continues a CorrectionLock held by another correction",
        );
    }
    return {
      kind: "correction_transition",
      consumedOutRef: locksIn[0]!.outRef,
      continuedOutRef: locksOut[0]!.outRef,
      targetHeaderHash: timeout.unavailable_header_hash,
      correctionIdentity: identity,
      previousDatum,
      nextDatum: locksOut[0]!.datum,
    };
  }
  throw new Error(
    "state-queue mint redeemer has no admitted CorrectionLock topology",
  );
};

export const unsafeCorrectionLockWitnessForTest = correctionLockWitness;

export const sameQueue = (
  left: readonly QueueNode[],
  right: readonly QueueNode[],
): boolean =>
  left.length === right.length &&
  left.every(
    (node, index) =>
      node.headerHash === right[index]?.headerHash &&
      node.outRef === right[index]?.outRef,
  );

export const sameLockDatum = (
  left: SDK.CorrectionLockDatum,
  right: SDK.CorrectionLockDatum,
): boolean =>
  Data.to(left, SDK.CorrectionLockDatum) ===
  Data.to(right, SDK.CorrectionLockDatum);
