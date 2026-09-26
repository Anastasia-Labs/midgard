import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Transaction,
  type LocalKupmiosFraudProofRawSource,
  localKupmiosHttpOgmiosRawSourceDetails,
  pinAdmittedLocalKupmiosBoundaryAtPoint,
  readAdmittedLocalKupmiosRawTransaction,
  readAdmittedLocalKupmiosUnitHistoryAtPoint,
  type RetainedDaPayloadSource,
  settleLocalKupmiosReads,
  withLocalKupmiosSourceCapture,
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
  let challenged:
    | {
        transaction: FraudProofRawL1Transaction;
        output: UTxO;
        datum: Extract<
          SDK.DaAvailabilityBondDatum,
          { ChallengedBond: unknown }
        >;
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
      const datum = SDK.parseDaAvailabilityBondDatumCbor(
        output.datum,
        input.parameters,
      );
      if (
        !("ChallengedBond" in datum) ||
        datum.ChallengedBond.commitment.header_hash !== input.headerHash
      )
        continue;
      if (challenged !== undefined)
        throw new Error("Canonical history repeats challenged bond creation");
      challenged = { transaction, output, datum };
    }
  }
  if (challenged === undefined)
    throw new Error(
      "Published header has no canonical challenged-bond history",
    );
  const bond = challenged.datum.ChallengedBond;
  if (
    bond.commitment.deployment_identity !== input.deploymentIdentity ||
    SDK.daAvailabilityPublishedTerminalCommitment(bond.commitment) !==
      input.terminalCommitment
  ) {
    throw new Error(
      "Published queue commitment differs from canonical challenge history",
    );
  }
  const bondInputs = challenged.transaction.resolvedInputs.filter((raw) => {
    const output = coreToTxOutput(
      CML.TransactionOutput.from_cbor_hex(raw.outputCbor),
    );
    return (
      output.address === address &&
      output.assets[policy + bond.da_bond_asset_name] === 1n
    );
  });
  if (bondInputs.length !== 1)
    throw new Error("Challenge history has no unique available-bond input");
  const [transactionId, outputIndex] = bondInputs[0]!.outRef.split("#");
  const challengedBond: SDK.DaAvailabilityChallengedBondEvidence = {
    datumCborHex: challenged.output.datum!,
    bondInputOutRef: {
      transactionId: transactionId!,
      outputIndex: BigInt(outputIndex!),
    },
    challengedBondOutputOutRef: SDK.outputReferenceFromUTxO(challenged.output),
  };
  const tranches: SDK.DaAvailabilityTrancheEvidence[] = [];
  for (const descriptor of bond.commitment.tranche_descriptors) {
    const unit =
      policy +
      SDK.daAvailabilityTrancheAssetName({
        challengeAssetName: bond.challenge_asset_name,
        trancheIndex: Number(descriptor.tranche_index),
      });
    const transactions = await input.readHistory(unit);
    const initial = outputs(challenged.transaction).filter(
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
        transaction.resolvedInputs.some((raw) => raw.outRef === ref(thread)),
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
          bond.commitment.response_geometry,
          descriptor,
        ),
        carrierOutputIndex: carrierIndex,
        inclusiveValidityUpper: BigInt(input.slotToUnixTime(Number(ttl))),
      });
    }
    tranches.push({ descriptor, publications });
  }
  return Buffer.from(
    SDK.reconstructDaAvailabilityPayload({
      challengedBond,
      parameters: input.parameters,
      tranches,
    }),
  );
};

/** Reconstruct public bytes from spent carrier history; no operator storage is consulted. */
export const createWatcherL1AvailabilityPayloadSource = (input: {
  identity: VerifiedWatcherDeploymentIdentity;
  deployment: SDK.DaAvailabilityDeployment;
  rawSource: LocalKupmiosFraudProofRawSource;
  lucid: Pick<LucidEvolution, "slotToUnixTime">;
  currentObservation(): WatcherAuthenticatedStateQueueObservation | null;
}): RetainedDaPayloadSource => {
  assertVerifiedWatcherDeploymentIdentity(input.identity);
  const details = localKupmiosHttpOgmiosRawSourceDetails(input.rawSource);
  if (
    details?.deploymentIdentityDigest !== input.identity.manifestId ||
    details.blueprintHash !== input.identity.blueprintHash
  )
    throw new Error("L1 payload history differs from verified deployment");
  const minimumConfirmationDepth =
    details.observationDepth === "inclusion" ? 1 : details.confirmationDepth;
  const sourceId = `watcher-l1-availability/${input.identity.manifestId}`;
  const sourcePeerId = "cardano-l1";
  const cache = new Map<
    string,
    { terminalCommitment: string; payload: Buffer }
  >();
  const source: RetainedDaPayloadSource = {
    sourceId,
    fetchPayloadByHeaderHash: async (headerHash) => {
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
        payload = await withLocalKupmiosSourceCapture(
          input.rawSource,
          async () => {
            const { blockHash, slot, blockNo } = observation.nativePoint;
            const point = {
              blockHash,
              slot,
              blockNo,
              pointId: computeFraudProofRawL1PointId({
                blockHash,
                slot,
                blockNo,
              }),
            };
            await pinAdmittedLocalKupmiosBoundaryAtPoint({
              source: input.rawSource,
              point,
            });
            const readHistory = async (unit: string) => {
              const history = await readAdmittedLocalKupmiosUnitHistoryAtPoint({
                source: input.rawSource,
                unit,
                point,
              });
              return await settleLocalKupmiosReads(
                history.transactions.map(({ txHash, inclusionPoint }) =>
                  readAdmittedLocalKupmiosRawTransaction({
                    source: input.rawSource,
                    txHash,
                    expectedInclusionPoint: inclusionPoint,
                    minimumConfirmationDepth,
                  }),
                ),
              );
            };
            return await reconstructWatcherAvailabilityPublishedPayload({
              headerHash,
              terminalCommitment,
              deploymentIdentity: input.deployment.hubOraclePolicyId,
              availabilityAddress:
                input.deployment.contracts.availabilityChallenge
                  .spendingScriptAddress,
              availabilityPolicyId:
                input.deployment.contracts.availabilityChallenge.policyId,
              stateQueuePolicyId:
                input.deployment.contracts.stateQueue.policyId,
              parameters: input.deployment.parameters,
              readHistory,
              slotToUnixTime: input.lucid.slotToUnixTime,
            });
          },
        );
      }
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
