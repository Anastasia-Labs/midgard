import type { AvailabilityOperationIntent } from "@al-ft/midgard-core/availability-operation-journal";
import {
  computeFraudProofRawL1PointId,
  type LocalKupmiosFraudProofRawSource,
  localKupmiosHttpOgmiosRawSourceDetails,
  pinAdmittedLocalKupmiosBoundaryAtPoint,
  readAdmittedLocalKupmiosAddressUtxosAtPoint,
  readAdmittedLocalKupmiosRawTransaction,
  readAdmittedLocalKupmiosTransactionInclusion,
  readAdmittedLocalKupmiosUtxosByOutRefAtPoint,
  settleLocalKupmiosReads,
  withLocalKupmiosSourceCapture,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, type UTxO } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";

const outRef = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex}`;

/** Every snapshot and inclusion witness belongs to one canonical finalized point. */
export const createWatcherAvailabilityObservation = (input: {
  identity: VerifiedWatcherDeploymentIdentity;
  source: LocalKupmiosFraudProofRawSource;
  deployment: SDK.DaAvailabilityDeployment;
}) => {
  const details = localKupmiosHttpOgmiosRawSourceDetails(input.source);
  if (
    details?.deploymentIdentityDigest !== input.identity.manifestId ||
    details.blueprintHash !== input.identity.blueprintHash
  ) {
    throw new Error(
      "Availability raw source differs from the signed deployment",
    );
  }
  const capture = <T>(
    observation: WatcherAuthenticatedStateQueueObservation,
    read: () => Promise<T>,
  ): Promise<T> =>
    withLocalKupmiosSourceCapture(input.source, async () => {
      assertWatcherStateQueueObservation(observation);
      if (
        observation.deploymentIdentityDigest !== input.identity.manifestId ||
        BigInt(observation.nativePoint.finalityDepth) < 30n
      ) {
        throw new Error(
          "Availability intake requires the exact finalized deployment observation",
        );
      }
      const { blockHash, slot, blockNo } = observation.nativePoint;
      await pinAdmittedLocalKupmiosBoundaryAtPoint({
        source: input.source,
        point: {
          blockHash,
          slot,
          blockNo,
          pointId: computeFraudProofRawL1PointId({ blockHash, slot, blockNo }),
        },
      });
      return await read();
    });
  const readAddress = async (
    observation: WatcherAuthenticatedStateQueueObservation,
    address: string,
  ): Promise<UTxO[]> => {
    const { blockHash, slot, blockNo } = observation.nativePoint;
    const point = {
      blockHash,
      slot,
      blockNo,
      pointId: computeFraudProofRawL1PointId({ blockHash, slot, blockNo }),
    };
    const raw = await readAdmittedLocalKupmiosAddressUtxosAtPoint({
      source: input.source,
      address,
      point,
    });
    return raw.map((output) => {
      const [txHash, index] = output.outRef.split("#");
      return {
        ...coreToTxOutput(
          CML.TransactionOutput.from_cbor_hex(output.outputCbor),
        ),
        txHash: txHash!,
        outputIndex: Number(index),
      };
    });
  };
  return {
    async snapshot(
      observation: WatcherAuthenticatedStateQueueObservation,
      headerHash: string,
    ): Promise<SDK.DaAvailabilityChallengeSnapshot> {
      return await capture(observation, async () => {
        const [availabilityUtxos, stateQueueUtxos, correctionLockUtxos] =
          await settleLocalKupmiosReads([
            readAddress(
              observation,
              input.deployment.contracts.availabilityChallenge
                .spendingScriptAddress,
            ),
            readAddress(
              observation,
              input.deployment.contracts.stateQueue.spendingScriptAddress,
            ),
            readAddress(
              observation,
              input.deployment.contracts.correctionLock.spendingScriptAddress,
            ),
          ]);
        const snapshot = await SDK.daAvailabilityChallengeSnapshotFromUtxos(
          input.deployment,
          headerHash,
          { availabilityUtxos, stateQueueUtxos, correctionLockUtxos },
        );
        const admittedHeader = observation.finalizedHeaders.find(
          (header) => header.headerHash === headerHash,
        );
        if (
          admittedHeader !== undefined &&
          (snapshot.queue === undefined ||
            outRef(snapshot.queue.utxo) !== admittedHeader.queueOutRef ||
            snapshot.queue.utxo.datum !== admittedHeader.linkedListDatumCborHex)
        ) {
          throw new Error(
            "Availability queue snapshot differs from its authenticated header",
          );
        }
        return snapshot;
      });
    },
    async operation(
      observation: WatcherAuthenticatedStateQueueObservation,
      intent: AvailabilityOperationIntent,
    ): Promise<SDK.DaAvailabilityOperationObservation> {
      return await capture(observation, async () => {
        const signed = CML.Transaction.from_cbor_hex(intent.signedCbor);
        const inclusion = await readAdmittedLocalKupmiosTransactionInclusion({
          source: input.source,
          txHash: intent.txHash,
        });
        if (inclusion !== null) {
          if (
            BigInt(inclusion.blockNo) > BigInt(observation.nativePoint.blockNo)
          ) {
            return {
              status: "unknown",
              reason:
                "Availability transaction has not reached the admitted finalized point",
            };
          }
          const transaction = await readAdmittedLocalKupmiosRawTransaction({
            source: input.source,
            txHash: intent.txHash,
            expectedInclusionPoint: inclusion,
            minimumConfirmationDepth: 30,
          });
          if (
            CML.TransactionBody.from_cbor_hex(
              transaction.bodyCbor,
            ).to_canonical_cbor_hex() !== signed.body().to_canonical_cbor_hex()
          ) {
            throw new Error(
              "Canonical transaction body differs from signed availability intent",
            );
          }
          return {
            status: "included",
            txHash: intent.txHash,
            inclusionPoint: inclusion.pointId,
            confirmationDepth: transaction.confirmationDepth,
          };
        }
        const consumed = [...intent.spentOutRefs, ...intent.collateralOutRefs];
        const { blockHash, slot, blockNo } = observation.nativePoint;
        const observed = await readAdmittedLocalKupmiosUtxosByOutRefAtPoint({
          source: input.source,
          outRefs: consumed,
          point: {
            blockHash,
            slot,
            blockNo,
            pointId: computeFraudProofRawL1PointId({
              blockHash,
              slot,
              blockNo,
            }),
          },
        });
        const unspent = new Set(observed.map(({ outRef }) => outRef));
        if (consumed.every((ref) => unspent.has(ref))) {
          return {
            status: "unspent",
            currentSlot: Number(observation.nativePoint.slot),
          };
        }
        return {
          status: "inputs_missing",
          currentSlot: Number(slot),
          missingOutRefs: consumed.filter((ref) => !unspent.has(ref)),
        };
      });
    },
  };
};
