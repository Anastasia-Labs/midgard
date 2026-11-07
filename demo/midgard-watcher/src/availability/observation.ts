import type {
  AvailabilityOperationIntent,
  AvailabilityOperationRecord,
} from "@al-ft/midgard-core/availability-operation-journal";
import { type FraudProofRawL1Point } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import {
  refusalError,
  required,
} from "../l1-follower/fault-proof-l1-source.chain.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentProtocolScriptAuthority,
} from "../runtime/deployment-identity.js";
import {
  recoverWatcherAttestedCommitment,
  watcherRawTransactionCbor,
} from "./commitment-source.js";
import {
  observationPoint,
  rawTransactionAt,
  readAtPoint,
  settleAll,
  unitHistoryTransactions,
  type WatcherAvailabilityL1,
} from "./follower-reads.js";
import { authenticWatcherDaBondPool } from "./pool-observation.js";

const outRef = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex}`;

/**
 * Every snapshot and inclusion witness belongs to one canonical finalized
 * point, read from the watcher's chain follower. `confirmationDepth` is the
 * verified deployment's release depth.
 */
export const createWatcherAvailabilityObservation = (input: {
  identity: VerifiedWatcherDeploymentIdentity;
  l1: WatcherAvailabilityL1;
  confirmationDepth: number;
  deployment: SDK.DaAvailabilityDeployment;
  scope?: SDK.DaAvailabilityReadScope;
  /** Resolves hashed DAAT datums during commitment recovery (E1). */
  lucid?: Pick<LucidEvolution, "config">;
}) => {
  const { confirmationDepth } = input;
  if (!Number.isSafeInteger(confirmationDepth) || confirmationDepth < 1)
    throw new Error("Availability confirmation depth must be positive");
  const reads = input.l1.reads;
  const capture = async <T>(
    observation: WatcherAuthenticatedStateQueueObservation,
    read: () => Promise<T>,
  ): Promise<T> => {
    assertWatcherStateQueueObservation(observation);
    if (
      observation.deploymentIdentityDigest !== input.identity.manifestId ||
      BigInt(observation.nativePoint.finalityDepth) < BigInt(confirmationDepth)
    ) {
      throw new Error(
        "Availability intake requires the exact finalized deployment observation",
      );
    }
    return await readAtPoint(
      input.l1,
      observationPoint(observation),
      input.scope,
      read,
    );
  };
  const pointOf = observationPoint;
  // An Attested node's commitment never changes, so each verified recovery is
  // kept for the node's lifetime in the finalized queue.
  const commitments = new Map<string, SDK.DaAvailabilityCommitment>();
  const readAddress = async (
    observation: WatcherAuthenticatedStateQueueObservation,
    address: string,
  ): Promise<UTxO[]> => {
    const point = pointOf(observation);
    const raw = required(await reads.addressUtxosAtPoint(address, point));
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
    confirmationDepth,
    /**
     * The pooled DA bond at the finalized point, read on its own so that it is
     * reported whether or not any header is pending. `undefined` means no
     * output holds the pool NFT; a malformed pool fails closed.
     */
    async pool(
      observation: WatcherAuthenticatedStateQueueObservation,
    ): Promise<UTxO | undefined> {
      return await capture(observation, async () => {
        const { policyId, spendingScriptAddress } =
          input.deployment.contracts.daBondPool;
        return authenticWatcherDaBondPool({
          utxos: await readAddress(observation, spendingScriptAddress),
          policyId,
          address: spendingScriptAddress,
        });
      });
    },
    async snapshot(
      observation: WatcherAuthenticatedStateQueueObservation,
      headerHash: string,
    ): Promise<SDK.DaAvailabilityChallengeSnapshot> {
      return await capture(observation, async () => {
        const [
          availabilityUtxos,
          stateQueueUtxos,
          correctionLockUtxos,
          poolUtxos,
        ] = await settleAll([
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
          // The pooled DA bond a Timeout slashes, at the same finalized point.
          readAddress(
            observation,
            input.deployment.contracts.daBondPool.spendingScriptAddress,
          ),
        ]);
        input.scope?.assertCurrent();
        const snapshot = await SDK.daAvailabilityChallengeSnapshotFromUtxos(
          input.deployment,
          headerHash,
          {
            availabilityUtxos,
            stateQueueUtxos,
            correctionLockUtxos,
            poolUtxos,
          },
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
    /**
     * The full commitment behind an `Attested{commitment_hash}` node, recovered
     * from the node's canonical Apply transaction at the finalized point and
     * verified against that hash (spec #685 E1).
     */
    async attestedCommitment(
      observation: WatcherAuthenticatedStateQueueObservation,
      headerHash: string,
      expectedCommitmentHash: string,
    ): Promise<SDK.DaAvailabilityCommitment> {
      input.scope?.assertCurrent();
      const key = `${headerHash}:${expectedCommitmentHash}`;
      const cached = commitments.get(key);
      if (cached !== undefined) return cached;
      const recovered = await capture(observation, async () => {
        const point = pointOf(observation);
        return await recoverWatcherAttestedCommitment({
          headerHash,
          expectedCommitmentHash,
          stateQueuePolicyId: input.deployment.contracts.stateQueue.policyId,
          daAttestationPolicyId: watcherDeploymentProtocolScriptAuthority(
            input.identity,
          ).protocolScriptHashes.daAttestationMint,
          lucid: input.lucid ?? {
            config: () =>
              ({}) as ReturnType<Pick<LucidEvolution, "config">["config"]>,
          },
          readHistory: async (unit) =>
            await unitHistoryTransactions(
              reads,
              unit,
              point,
              confirmationDepth,
            ),
          readTransaction: async (txHash) => {
            const inclusion = required(
              await reads.transactionInclusion(txHash),
            );
            if (
              inclusion === null ||
              BigInt(inclusion.blockNo) > BigInt(point.blockNo)
            )
              return undefined;
            return await rawTransactionAt(
              reads,
              txHash,
              inclusion,
              confirmationDepth,
            );
          },
        });
      });
      input.scope?.assertCurrent();
      for (const cachedKey of commitments.keys()) {
        const [cachedHeader] = cachedKey.split(":");
        if (
          !observation.finalizedHeaders.some(
            (header) => header.headerHash === cachedHeader,
          )
        )
          commitments.delete(cachedKey);
      }
      commitments.set(key, recovered.commitment);
      return recovered.commitment;
    },
    /**
     * Whether this actor's challenge workflow for `headerHash` ended in a
     * terminal step someone else landed (P20), walked from its confirmed Open
     * at the finalized point with the same follower reads as the foreign
     * spends of {@link operation}. Every spend is verified from its raw bytes
     * and counted `finalityDepth` deeper than the point it lies below.
     */
    async workflowRelease(
      observation: WatcherAuthenticatedStateQueueObservation,
      openIntent: AvailabilityOperationRecord,
      headerHash: string,
    ): Promise<SDK.DaAvailabilityWorkflowRelease | undefined> {
      return await capture(observation, async () => {
        const point = pointOf(observation);
        const spendPoints = new Map<string, FraudProofRawL1Point>();
        const readers: SDK.DaAvailabilityForeignSpendReaders = {
          // Native finality includes the point itself; SDK depth counts
          // only blocks after inclusion, so expose the actual tip height.
          readBoundary: async () => ({
            pointId: point.pointId,
            blockNo:
              Number(point.blockNo) +
              Number(observation.nativePoint.finalityDepth) -
              1,
          }),
          fetchSpend: async (ref) => {
            const outRef = `${ref.txHash}#${ref.outputIndex.toString()}`;
            const observed = required(
              await reads.utxosByOutRefAtPoint([outRef], point),
            );
            if (observed.beyondRetention.includes(outRef))
              throw refusalError(
                "beyond_retention",
                `the spend of ${outRef} may have been pruned`,
              );
            const spend = observed.spends.find(
              (entry) => entry.outRef === outRef,
            );
            if (spend === undefined) return undefined;
            spendPoints.set(spend.spendPoint.pointId, spend.spendPoint);
            return {
              transactionId: spend.spendingTxHash,
              point: {
                slot: Number(spend.spendPoint.slot),
                blockHash: spend.spendPoint.blockHash,
              },
            };
          },
          fetchAncestor: async (slot) => {
            const spendPoint = [...spendPoints.values()].find(
              (entry) => Number(entry.slot) === slot,
            );
            if (spendPoint === undefined)
              throw new Error(
                "Availability workflow release has no spend at the requested slot",
              );
            const predecessorPoint = required(
              await reads.predecessorPoint(spendPoint),
            );
            return {
              slot: Number(predecessorPoint.slot),
              blockHash: predecessorPoint.blockHash,
            };
          },
          readTransaction: async ({ point: spendPoint, txHash }) => {
            const inclusion = required(
              await reads.transactionInclusion(txHash),
            );
            if (
              inclusion === null ||
              Number(inclusion.slot) !== spendPoint.slot ||
              inclusion.blockHash !== spendPoint.blockHash
            )
              return undefined;
            if (BigInt(inclusion.blockNo) > BigInt(point.blockNo))
              throw new Error(
                "Availability input spend lies above the canonical boundary",
              );
            const raw = await rawTransactionAt(
              reads,
              txHash,
              inclusion,
              confirmationDepth,
            );
            return {
              txHash: raw.txHash,
              point: {
                slot: Number(inclusion.slot),
                blockHash: inclusion.blockHash,
                blockNo: Number(inclusion.blockNo),
              },
              cbor: watcherRawTransactionCbor(raw),
            };
          },
        };
        return await SDK.resolveDaAvailabilityWorkflowRelease(
          readers,
          openIntent,
          headerHash,
          confirmationDepth,
        );
      });
    },
    async operation(
      observation: WatcherAuthenticatedStateQueueObservation,
      intent: AvailabilityOperationIntent,
    ): Promise<SDK.DaAvailabilityOperationObservation> {
      return await capture(observation, async () => {
        const signed = CML.Transaction.from_cbor_hex(intent.signedCbor);
        const inclusion = required(
          await reads.transactionInclusion(intent.txHash),
        );
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
          const transaction = await rawTransactionAt(
            reads,
            intent.txHash,
            inclusion,
            confirmationDepth,
          );
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
            // Raw admission counts inclusion itself; SDK evidence does not.
            confirmationDepth: transaction.confirmationDepth - 1,
            currentSlot: Number(observation.nativePoint.slot),
            currentBlockNo: Number(observation.nativePoint.blockNo),
          };
        }
        const consumed = [...intent.spentOutRefs, ...intent.collateralOutRefs];
        const { slot, blockNo } = observation.nativePoint;
        const observed = required(
          await reads.utxosByOutRefAtPoint(consumed, pointOf(observation)),
        );
        // A pruned row cannot be told from a spend: never read it as missing.
        if (observed.beyondRetention.length > 0)
          throw refusalError(
            "beyond_retention",
            `availability inputs ${observed.beyondRetention.join(", ")} may have been pruned`,
          );
        const unspent = new Set(observed.outputs.map(({ outRef }) => outRef));
        if (consumed.every((ref) => unspent.has(ref))) {
          return {
            status: "unspent",
            currentSlot: Number(observation.nativePoint.slot),
          };
        }
        // SDK evidence counts blocks after inclusion. Native finality
        // includes the observed block, so subtract it from the combined depth.
        const foreignSpends = observed.spends.map((spend) => ({
          outRef: spend.outRef,
          spendingTxHash: spend.spendingTxHash,
          spendPoint: spend.spendPoint.pointId,
          confirmationDepth:
            Number(blockNo) -
            Number(spend.spendPoint.blockNo) +
            Number(observation.nativePoint.finalityDepth) -
            1,
        }));
        return {
          status: "inputs_missing",
          currentSlot: Number(slot),
          missingOutRefs: consumed.filter((ref) => !unspent.has(ref)),
          ...(foreignSpends.length === 0 ? {} : { foreignSpends }),
        };
      });
    },
  };
};
