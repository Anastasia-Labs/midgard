import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { L1SourceIntegrityError } from "./source-integrity.js";
import {
  decodeCorrectionLockOutput,
  fetchResolvedOutput,
  fraudProofAssetName,
} from "./state-queue-replay-provider.decode-correction-lock-output.js";
import {
  type HistoricalOutput,
  type Queue,
  splitOutRef,
  type StateQueueReplayFetch,
  type Transaction,
} from "./state-queue-replay-provider.open-rpc.js";
import { type CorrectionLockOutput } from "./state-queue-replay-provider.parse-transaction.js";

export const correctionLockWitness = async ({
  transaction,
  outputs,
  stateQueuePolicyId,
  hubOraclePolicyId,
  correctionLockAddress,
  fraudProofPolicyId,
  fraudProofAddress,
  kupoUrl,
  fetchImpl,
}: {
  readonly transaction: Transaction;
  readonly outputs: readonly CorrectionLockOutput[];
  readonly stateQueuePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly correctionLockAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly kupoUrl: string;
  readonly fetchImpl: StateQueueReplayFetch;
}): Promise<SDK.StateQueueCorrectionLockWitness> => {
  const spentResolved = await Promise.all(
    transaction.spentInputOutRefs.map(async (reference) => ({
      reference,
      value: await fetchResolvedOutput(kupoUrl, reference, fetchImpl),
    })),
  );
  const referenceResolved = await Promise.all(
    transaction.referenceInputOutRefs.map(async (reference) => ({
      reference,
      value: await fetchResolvedOutput(kupoUrl, reference, fetchImpl),
    })),
  );
  const locksIn = spentResolved.flatMap(({ reference, value }) => {
    const { txHash, index } = splitOutRef(reference);
    const lock = decodeCorrectionLockOutput(
      value,
      txHash,
      index,
      correctionLockAddress,
      hubOraclePolicyId,
    );
    return lock === null ? [] : [lock];
  });
  const locksReferenced = referenceResolved.flatMap(({ reference, value }) => {
    const { txHash, index } = splitOutRef(reference);
    const lock = decodeCorrectionLockOutput(
      value,
      txHash,
      index,
      correctionLockAddress,
      hubOraclePolicyId,
    );
    return lock === null ? [] : [lock];
  });
  const policyIndex = transaction.mintPolicyIds.indexOf(stateQueuePolicyId);
  if (policyIndex < 0) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      outputs.length !== 0
    ) {
      throw new L1SourceIntegrityError(
        "non-mint checkpoint unexpectedly carries CorrectionLock",
      );
    }
    return { kind: "none" };
  }
  const mint = transaction.redeemers.filter(
    ({ purpose, index }) =>
      purpose === "mint" && index === policyIndex.toString(),
  );
  if (mint.length !== 1)
    throw new L1SourceIntegrityError("state-queue mint redeemer is not unique");
  let decoded: SDK.StateQueueRedeemer;
  try {
    decoded = Data.from(mint[0]!.cborHex, SDK.StateQueueRedeemer);
  } catch (cause) {
    throw new L1SourceIntegrityError("state-queue mint redeemer is invalid", {
      cause,
    });
  }
  if (typeof decoded === "object" && decoded !== null && "InitV1" in decoded) {
    if (
      locksIn.length !== 0 ||
      locksReferenced.length !== 0 ||
      outputs.length !== 1 ||
      outputs[0]!.datum !== "Idle"
    ) {
      throw new L1SourceIntegrityError(
        "state-queue init CorrectionLock topology is invalid",
      );
    }
    return {
      kind: "genesis",
      producedOutRef: outputs[0]!.outRef,
      nextDatum: "Idle",
    };
  }
  if (decoded === "Deinit") {
    if (
      locksIn.length !== 1 ||
      locksIn[0]!.datum !== "Idle" ||
      locksReferenced.length !== 0 ||
      outputs.length !== 0
    ) {
      throw new L1SourceIntegrityError(
        "state-queue deinit CorrectionLock topology is invalid",
      );
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
      outputs.length !== 0
    ) {
      throw new L1SourceIntegrityError(
        "append/merge CorrectionLock topology is invalid",
      );
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
      "RemoveUnavailableBlockAfterTimeout" in decoded ||
      "RemoveFraudulentBlockHeader" in decoded)
  ) {
    if (
      locksIn.length !== 1 ||
      locksReferenced.length !== 0 ||
      outputs.length !== 1
    ) {
      throw new L1SourceIntegrityError(
        "correction CorrectionLock topology is invalid",
      );
    }
    const targetHeaderHash =
      "RemoveUnattestedBlockAfterTimeout" in decoded
        ? decoded.RemoveUnattestedBlockAfterTimeout.timed_out_header_hash
        : "RemoveUnavailableBlockAfterTimeout" in decoded
          ? decoded.RemoveUnavailableBlockAfterTimeout.unavailable_header_hash
          : decoded.RemoveFraudulentBlockHeader.fraudulent_blocks_header_hash;
    const identity: SDK.CorrectionIdentity =
      "RemoveUnattestedBlockAfterTimeout" in decoded
        ? "AttestationTimeout"
        : "RemoveUnavailableBlockAfterTimeout" in decoded
          ? {
              AvailabilityChallenge: {
                challenge_asset_name:
                  decoded.RemoveUnavailableBlockAfterTimeout
                    .challenge_asset_name,
              },
            }
          : (() => {
              const reference =
                referenceResolved[
                  Number(
                    decoded.RemoveFraudulentBlockHeader
                      .fraud_proof_ref_input_index,
                  )
                ];
              const assetName =
                reference === undefined
                  ? null
                  : fraudProofAssetName(
                      reference.value,
                      fraudProofAddress,
                      fraudProofPolicyId,
                      targetHeaderHash,
                    );
              if (assetName === null)
                throw new L1SourceIntegrityError(
                  "fraud proof CorrectionLock identity is invalid",
                );
              return { FraudProof: { fraud_proof_asset_name: assetName } };
            })();
    return {
      kind: "correction_transition",
      consumedOutRef: locksIn[0]!.outRef,
      continuedOutRef: outputs[0]!.outRef,
      targetHeaderHash,
      correctionIdentity: identity,
      previousDatum: locksIn[0]!.datum,
      nextDatum: outputs[0]!.datum,
    };
  }
  throw new L1SourceIntegrityError(
    "state-queue checkpoint has no CorrectionLock topology",
  );
};

export const reconstruct = (
  previousQueue: Queue,
  transaction: Transaction,
  outputs: readonly HistoricalOutput[],
): Queue => {
  const spent = new Set(transaction.spentInputOutRefs);
  const previousByIdentity = new Map(
    previousQueue.map((node) => [node.headerHash, node]),
  );
  const outputByIdentity = new Map(
    outputs.map(({ node }) => [node.headerHash, node]),
  );
  if (
    outputs.length === 0 ||
    !previousQueue.some(({ outRef }) => spent.has(outRef)) ||
    outputs.some(
      ({ node }) =>
        previousByIdentity.has(node.headerHash) &&
        !spent.has(previousByIdentity.get(node.headerHash)!.outRef),
    )
  ) {
    throw new L1SourceIntegrityError(
      "state-queue replay outputs do not follow their inputs",
    );
  }
  const retained = previousQueue.flatMap((node) => {
    if (!spent.has(node.outRef)) return [node];
    const continuation = outputByIdentity.get(node.headerHash);
    return continuation === undefined ? [] : [continuation];
  });
  const introduced = outputs
    .map(({ node }) => node)
    .filter(({ headerHash }) => !previousByIdentity.has(headerHash));
  if (
    introduced.length > 1 ||
    introduced.some(({ headerHash }) => headerHash === null)
  ) {
    throw new L1SourceIntegrityError(
      "state-queue replay introduced invalid identities",
    );
  }
  const nextQueue = [...retained, ...introduced];
  if (
    nextQueue.length === 0 ||
    nextQueue[0]?.headerHash !== null ||
    new Set(nextQueue.map(({ headerHash }) => headerHash)).size !==
      nextQueue.length ||
    new Set(nextQueue.map(({ outRef }) => outRef)).size !== nextQueue.length
  ) {
    throw new L1SourceIntegrityError(
      "state-queue replay reconstructed an invalid queue",
    );
  }
  const expectedLinks = new Map(
    nextQueue.map((node, index) => [
      node.headerHash,
      nextQueue[index + 1]?.headerHash ?? null,
    ]),
  );
  if (
    outputs.some(
      ({ node, nextHeaderHash }) =>
        expectedLinks.get(node.headerHash) !== nextHeaderHash,
    )
  ) {
    throw new L1SourceIntegrityError(
      "state-queue replay linked-list outputs disagree",
    );
  }
  return nextQueue;
};
