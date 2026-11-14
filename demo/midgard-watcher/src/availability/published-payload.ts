import {
  type FraudProofRawL1Transaction,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import {
  observationPoint,
  readAtPoint,
  unitHistoryTransactions,
  type WatcherAvailabilityL1,
} from "./follower-reads.js";

const admitted = new WeakMap<object, VerifiedWatcherDeploymentIdentity>();
export const assertWatcherL1AvailabilityPayloadSource = (
  source: RetainedDaPayloadSource,
  identity: VerifiedWatcherDeploymentIdentity,
): void => {
  if (admitted.get(source) !== identity)
    throw new Error(
      "L1 availability source was not admitted from this deployment's canonical publication history",
    );
};

const outputs = (transaction: FraudProofRawL1Transaction): UTxO[] => {
  const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
  return Array.from({ length: body.outputs().len() }, (_, outputIndex) => {
    const output = coreToTxOutput(body.outputs().get(outputIndex));
    // The ledger admits equivalent CBOR encodings. Re-encode the decoded
    // Plutus Data before passing it to the SDK's canonical wire codecs.
    return {
      ...output,
      ...(output.datum == null
        ? {}
        : { datum: Data.to(Data.from(output.datum)) }),
      txHash: transaction.txHash,
      outputIndex,
    };
  });
};
const ref = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex}`;

/** The outrefs a transaction spends, read from its body. */
const spentOutRefs = (transaction: FraudProofRawL1Transaction): string[] => {
  const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
  try {
    const inputs = body.inputs();
    return Array.from({ length: inputs.len() }, (_, index) => {
      const input = inputs.get(index);
      return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
    });
  } finally {
    body.free();
  }
};

/** Pure history decoding; only the concrete source below admits its provenance. */
export const reconstructWatcherAvailabilityPublishedPayload = async (input: {
  headerHash: string;
  terminalCommitment: string;
  deploymentIdentity: string;
  availabilityAddress: string;
  availabilityPolicyId: string;
  stateQueuePolicyId: string;
  parameters: SDK.DaAvailabilityParameters;
  readHistory(unit: string): Promise<readonly FraudProofRawL1Transaction[]>;
  slotToUnixTime(slot: number): number;
}): Promise<Buffer> => {
  const history = await input.readHistory(
    input.stateQueuePolicyId +
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      input.headerHash,
  );
  const address = input.availabilityAddress;
  const policy = input.availabilityPolicyId;
  // OpenChallenge creates exactly one ChallengeRecordV1 output (the DACH
  // token plus the record datum) naming this header. Later transactions in
  // the node's history re-output the node, never a second record.
  let opened:
    | {
        transaction: FraudProofRawL1Transaction;
        output: UTxO;
        record: SDK.DaAvailabilityChallengeRecord;
      }
    | undefined;
  for (const transaction of history) {
    for (const output of outputs(transaction)) {
      if (
        output.address !== address ||
        output.datum == null ||
        !Object.keys(output.assets).some((unit) =>
          unit.startsWith(
            policy + SDK.DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX,
          ),
        )
      )
        continue;
      const record = SDK.parseDaAvailabilityChallengeRecordCbor(
        output.datum,
        input.parameters,
      );
      if (record.commitment.header_hash !== input.headerHash) continue;
      if (output.assets[policy + record.challenge_asset_name] !== 1n)
        throw new Error(
          "Challenge record output does not carry its own DACH identity",
        );
      if (opened !== undefined)
        throw new Error("Canonical history repeats challenge record creation");
      opened = { transaction, output, record };
    }
  }
  if (opened === undefined)
    throw new Error(
      "Published header has no canonical challenge-record history",
    );
  const record = opened.record;
  if (
    record.commitment.deployment_identity !== input.deploymentIdentity ||
    SDK.daAvailabilityPublishedTerminalCommitment(record.commitment) !==
      input.terminalCommitment
  ) {
    throw new Error(
      "Published queue commitment differs from canonical challenge history",
    );
  }
  // The DACH identity derives from the challenger funding input OpenChallenge
  // consumed; exactly one spent input may derive it.
  const fundingInputs = spentOutRefs(opened.transaction).filter((outRef) => {
    const [transactionId, outputIndex] = outRef.split("#");
    return (
      SDK.daAvailabilityChallengeAssetName({
        transactionId: transactionId!,
        outputIndex: BigInt(outputIndex!),
      }) === record.challenge_asset_name
    );
  });
  if (fundingInputs.length !== 1)
    throw new Error(
      "Challenge history has no unique challenger funding input for its DACH identity",
    );
  const [transactionId, outputIndex] = fundingInputs[0]!.split("#");
  const challengeRecord: SDK.DaAvailabilityChallengeRecordEvidence = {
    datumCborHex: opened.output.datum!,
    challengerFundingOutRef: {
      transactionId: transactionId!,
      outputIndex: BigInt(outputIndex!),
    },
    recordOutputOutRef: SDK.outputReferenceFromUTxO(opened.output),
  };
  const tranches: SDK.DaAvailabilityTrancheEvidence[] = [];
  for (const descriptor of record.commitment.tranche_descriptors) {
    const unit =
      policy +
      SDK.daAvailabilityTrancheAssetName({
        challengeAssetName: record.challenge_asset_name,
        trancheIndex: Number(descriptor.tranche_index),
      });
    const transactions = await input.readHistory(unit);
    const initial = outputs(opened.transaction).filter(
      (output) => output.address === address && output.assets[unit] === 1n,
    );
    if (initial.length !== 1)
      throw new Error("Challenge has no unique initial tranche output");
    let thread = initial[0]!;
    const publications: SDK.DaAvailabilityTrancheEvidence["publications"][number][] =
      [];
    const visited = new Set<string>();
    while (true) {
      if (thread.datum == null || visited.has(ref(thread)))
        throw new Error("L1 tranche history is malformed or cyclic");
      visited.add(ref(thread));
      const state = SDK.parseDaAvailabilityTrancheDatumCbor(thread.datum);
      if ("Receipt" in state) break;
      const successors = transactions.filter((transaction) =>
        spentOutRefs(transaction).includes(ref(thread)),
      );
      if (successors.length !== 1)
        throw new Error(
          "Published tranche has missing or conflicting canonical successor",
        );
      const transaction = successors[0]!;
      const next = outputs(transaction).filter(
        (output) => output.address === address && output.assets[unit] === 1n,
      );
      if (next.length !== 1 || next[0]!.datum == null)
        throw new Error(
          "Published tranche was settled without complete public bytes",
        );
      thread = next[0]!;
      const nextDatum = SDK.parseDaAvailabilityTrancheDatumCbor(thread.datum!);
      const carrierIndex =
        "Active" in nextDatum
          ? nextDatum.Active.latest_carrier_output_index
          : nextDatum.Receipt.terminal_carrier_output_index;
      if (carrierIndex === null)
        throw new Error(
          "Published tranche continuation lacks its carrier index",
        );
      const carrier = outputs(transaction)[Number(carrierIndex)];
      if (
        carrier?.address !== address ||
        carrier.datum == null ||
        Object.keys(carrier.assets).some((unit) => unit !== "lovelace")
      ) {
        throw new Error(
          "Canonical publication carrier is not the exact protected output",
        );
      }
      const ttl = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor).ttl();
      if (ttl === undefined || ttl > BigInt(Number.MAX_SAFE_INTEGER))
        throw new Error("Canonical publication lacks bounded validity");
      publications.push({
        publication: SDK.parseDaAvailabilityPublicationDatumCbor(
          carrier.datum,
          record.commitment.response_geometry,
          descriptor,
        ),
        carrierOutputIndex: carrierIndex,
        // The ledger ttl is the EXCLUSIVE upper validity end; the validator
        // and the SDK builder bound publications by the inclusive upper,
        // ttl - 1 ms.
        inclusiveValidityUpper: BigInt(input.slotToUnixTime(Number(ttl))) - 1n,
      });
    }
    tranches.push({ descriptor, publications });
  }
  return Buffer.from(
    SDK.reconstructDaAvailabilityPayload({
      challengeRecord,
      parameters: input.parameters,
      tranches,
    }),
  );
};

/**
 * Reconstruct public bytes from spent carrier history, read from the
 * watcher's chain follower; no operator storage is consulted.
 * `minimumConfirmationDepth` is 1 when the observation is the tip view, and
 * the deployment's release depth otherwise.
 */
export const createWatcherL1AvailabilityPayloadSource = (input: {
  identity: VerifiedWatcherDeploymentIdentity;
  deployment: SDK.DaAvailabilityDeployment;
  l1: WatcherAvailabilityL1;
  minimumConfirmationDepth: number;
  lucid: Pick<LucidEvolution, "slotToUnixTime">;
  currentObservation(): WatcherAuthenticatedStateQueueObservation | null;
  scope?: SDK.DaAvailabilityReadScope;
}): RetainedDaPayloadSource => {
  assertVerifiedWatcherDeploymentIdentity(input.identity);
  const { minimumConfirmationDepth } = input;
  if (
    !Number.isSafeInteger(minimumConfirmationDepth) ||
    minimumConfirmationDepth < 1
  )
    throw new Error("L1 payload history depth must be positive");
  const sourceId = `watcher-l1-availability/${input.identity.manifestId}`;
  const sourcePeerId = "cardano-l1";
  const cache = new Map<
    string,
    { terminalCommitment: string; payload: Buffer }
  >();
  const source: RetainedDaPayloadSource = {
    sourceId,
    fetchPayloadByHeaderHash: async (headerHash) => {
      input.scope?.assertCurrent();
      const observation = input.currentObservation();
      if (observation === null) return { ok: false, sourceId, attempts: [] };
      assertWatcherStateQueueObservation(observation);
      if (
        observation.deploymentIdentityDigest !== input.identity.manifestId ||
        BigInt(observation.nativePoint.finalityDepth) <
          BigInt(minimumConfirmationDepth)
      ) {
        throw new Error(
          "L1 payload history requires the authenticated deployment observation",
        );
      }
      const header = observation.finalizedHeaders.find(
        (header) => header.headerHash === headerHash,
      );
      if (
        header === undefined ||
        header.daAvailability === "Unattested" ||
        !("Published" in header.daAvailability)
      ) {
        return { ok: false, sourceId, attempts: [] };
      }
      const terminalCommitment =
        header.daAvailability.Published.terminal_commitment;
      for (const cachedHeader of cache.keys()) {
        if (
          !observation.finalizedHeaders.some(
            (header) => header.headerHash === cachedHeader,
          )
        )
          cache.delete(cachedHeader);
      }
      let payload =
        cache.get(headerHash)?.terminalCommitment === terminalCommitment
          ? cache.get(headerHash)!.payload
          : undefined;
      if (payload === undefined) {
        const point = observationPoint(observation);
        payload = await readAtPoint(input.l1, point, input.scope, async () =>
          reconstructWatcherAvailabilityPublishedPayload({
            headerHash,
            terminalCommitment,
            deploymentIdentity: input.deployment.hubOraclePolicyId,
            availabilityAddress:
              input.deployment.contracts.availabilityChallenge
                .spendingScriptAddress,
            availabilityPolicyId:
              input.deployment.contracts.availabilityChallenge.policyId,
            stateQueuePolicyId: input.deployment.contracts.stateQueue.policyId,
            parameters: input.deployment.parameters,
            readHistory: async (unit) =>
              await unitHistoryTransactions(
                input.l1.reads,
                unit,
                point,
                minimumConfirmationDepth,
              ),
            slotToUnixTime: input.lucid.slotToUnixTime,
          }),
        );
      }
      input.scope?.assertCurrent();
      if (
        input.currentObservation()?.observationDigest !==
        observation.observationDigest
      ) {
        throw new Error(
          "L1 availability observation changed during reconstruction",
        );
      }
      // Each cached value still requires a matching admitted Published marker on use.
      cache.set(headerHash, { terminalCommitment, payload });
      return {
        ok: true,
        sourceId,
        sourcePeerId,
        payloadEnvelopeCbor: Buffer.from(payload),
        attempts: [],
        provenance: SDK.assertSecurityGradeEvidence(
          SDK.admitEvidenceProvenance({
            provenance: {
              trustClass: "public_or_permissionless_da",
              sourceId: `${sourceId}/${sourcePeerId}`,
              grade: "security",
            },
          }),
        ),
      };
    },
  };
  admitted.set(source, input.identity);
  return source;
};
