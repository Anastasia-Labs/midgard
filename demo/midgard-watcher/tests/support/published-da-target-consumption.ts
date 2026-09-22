import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, type UTxO } from "@lucid-evolution/lucid";

/** Current caught-up native coverage contains no transaction with this hash.
 * This is an observation of absence, not proof that signed bytes cannot land. */
export class NativeTransactionNotIncludedError extends Error {
  constructor(readonly txHash: string) {
    super(`Native node did not include transaction ${txHash}`);
    this.name = "NativeTransactionNotIncludedError";
  }
}

export type PublishedDaTransactionStep = "init" | "signatures" | "apply";

/** A DA transaction the actor signed and submitted, recorded before submission. */
export type PublishedDaTransactionRecord = {
  readonly step: PublishedDaTransactionStep;
  readonly txHash: string;
  readonly signedCbor: string;
};

export type PublishedDaTransactionOutcome = PublishedDaTransactionRecord & {
  readonly disposition: "included" | "absent" | "pending";
  readonly reason?: string;
};

/**
 * An indexed spend of an output that carried the header's state queue unit.
 * The index is a hint only; reconciliation authenticates both transactions
 * through the independent confirmed-transaction reader.
 */
export type PublishedHeaderConsumption = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly spentByTxHash: string;
  readonly spentAtSlot: number;
};

/** The staged fault header was removed by its own fraud correction before DA apply. */
export type PublishedDaTargetCorrection = {
  readonly kind: "corrected";
  readonly headerHash: string;
  readonly removalTxHash: string;
  readonly removedStateQueueOutRef: string;
  readonly fraudProofOutRef: string;
  readonly submittedDaTransactions: readonly PublishedDaTransactionOutcome[];
  /** The bonded attestation output left behind when init landed but apply never could. */
  readonly orphanedAttestationOutRef: string | null;
};

const outRefLabel = (outRef: {
  readonly txHash: string;
  readonly outputIndex: number;
}) => `${outRef.txHash}#${outRef.outputIndex.toString()}`;

const inputLabels = (inputs: CML.TransactionInputList | undefined) =>
  inputs === undefined
    ? []
    : Array.from({ length: inputs.len() }, (_, index) => {
        const input = inputs.get(index);
        return {
          txHash: input.transaction_id().to_hex(),
          outputIndex: Number(input.index()),
        };
      });

const transactionOutputs = (transaction: CML.Transaction, txHash: string) => {
  const outputs = transaction.body().outputs();
  return Array.from({ length: outputs.len() }, (_, outputIndex) => ({
    ...coreToTxOutput(outputs.get(outputIndex)),
    txHash,
    outputIndex,
  }));
};

const authenticatedTransaction = (
  txHash: string,
  cbor: string,
  label: string,
) => {
  const transaction = CML.Transaction.from_cbor_hex(cbor);
  if (
    !transaction.is_valid() ||
    CML.hash_transaction(transaction.body()).to_hex() !== txHash
  )
    throw new Error(`${label} ${txHash} is not the exact valid included body`);
  return transaction;
};

/**
 * Reconcile a state queue target that disappeared while its DA attestation
 * was in progress. The only acceptable consumption is a fraud correction of
 * this exact header: the authenticated spender burns the header unit,
 * references a fraud-proof token for this header, and consumed an output
 * that actually carried this header. Every DA transaction already submitted
 * is preserved with its authenticated disposition so the orphaned bond and
 * the lost apply are visible rather than silently forgotten.
 */
export const reconcilePublishedDaTargetConsumption = async (input: {
  readonly headerHash: string;
  readonly stateQueueAddress: string;
  readonly stateQueuePolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly consumptions: readonly PublishedHeaderConsumption[];
  readonly attestationOutputs: readonly UTxO[];
  readonly submitted: readonly PublishedDaTransactionRecord[];
  readConfirmedTransaction(txHash: string): Promise<{ cbor: string }>;
}): Promise<PublishedDaTargetCorrection> => {
  const stateQueueAssetName =
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + input.headerHash;
  const stateQueueUnit = input.stateQueuePolicyId + stateQueueAssetName;
  const latest = [...input.consumptions].sort(
    (a, b) => a.spentAtSlot - b.spentAtSlot,
  )[input.consumptions.length - 1];
  if (latest === undefined)
    throw new Error(
      `State queue target ${input.headerHash} has no indexed consumption`,
    );
  const removedOutRef = outRefLabel(latest);

  // The consumed output must have actually been this header's state queue node.
  const creating = authenticatedTransaction(
    latest.txHash,
    (await input.readConfirmedTransaction(latest.txHash)).cbor,
    "State queue target creation",
  );
  const removedOutput = transactionOutputs(creating, latest.txHash).find(
    ({ outputIndex }) => outputIndex === latest.outputIndex,
  );
  if (
    removedOutput === undefined ||
    removedOutput.address !== input.stateQueueAddress ||
    removedOutput.assets[stateQueueUnit] !== 1n
  )
    throw new Error(
      `Consumed output ${removedOutRef} did not carry state queue header ${input.headerHash}`,
    );

  // The spender must be a removal: it spends the node, burns its unit and
  // continues no output carrying it.
  const removal = authenticatedTransaction(
    latest.spentByTxHash,
    (await input.readConfirmedTransaction(latest.spentByTxHash)).cbor,
    "State queue target removal",
  );
  const spent = inputLabels(removal.body().inputs()).some(
    (ref) => outRefLabel(ref) === removedOutRef,
  );
  const burned = removal
    .body()
    .mint()
    ?.get(
      CML.ScriptHash.from_hex(input.stateQueuePolicyId),
      CML.AssetName.from_hex(stateQueueAssetName),
    );
  const continued = transactionOutputs(removal, latest.spentByTxHash).some(
    ({ assets }) => assets[stateQueueUnit] !== undefined,
  );
  if (!spent || burned !== -1n || continued)
    throw new Error(
      `Transaction ${latest.spentByTxHash} consumed state queue header ${input.headerHash} without removing it`,
    );

  // A removal of this header is a fraud correction only when it references a
  // fraud-proof token minted for this header; a timeout removal never does.
  const references = inputLabels(removal.body().reference_inputs());
  const resolved = await Promise.all(
    references.map(async (reference) => {
      const creating = authenticatedTransaction(
        reference.txHash,
        (await input.readConfirmedTransaction(reference.txHash)).cbor,
        "Fraud proof reference creation",
      );
      return transactionOutputs(creating, reference.txHash)[
        reference.outputIndex
      ];
    }),
  );
  const fraudProof = resolved.find(
    (utxo) =>
      utxo !== undefined &&
      Object.entries(utxo.assets).some(
        ([unit, quantity]) =>
          quantity === 1n &&
          unit.startsWith(input.fraudProofPolicyId) &&
          unit.endsWith(input.headerHash) &&
          unit.length ===
            input.fraudProofPolicyId.length + 8 + input.headerHash.length &&
          /^[0-9a-f]{8}$/.test(
            unit.slice(
              input.fraudProofPolicyId.length,
              -input.headerHash.length,
            ),
          ),
      ),
  );
  if (fraudProof === undefined)
    throw new Error(
      `Removal ${latest.spentByTxHash} of header ${input.headerHash} referenced no fraud proof for it`,
    );

  if (input.attestationOutputs.length > 1)
    throw new Error(
      `Ambiguous DA attestation output for header ${input.headerHash}`,
    );
  const orphanedAttestation = input.attestationOutputs[0];

  const submittedDaTransactions = await Promise.all(
    input.submitted.map(
      async (record): Promise<PublishedDaTransactionOutcome> => {
        const signed = CML.Transaction.from_cbor_hex(record.signedCbor);
        if (CML.hash_transaction(signed.body()).to_hex() !== record.txHash)
          throw new Error(
            `Recorded DA ${record.step} transaction bytes changed their hash`,
          );
        try {
          authenticatedTransaction(
            record.txHash,
            (await input.readConfirmedTransaction(record.txHash)).cbor,
            `DA ${record.step}`,
          );
          return { ...record, disposition: "included" };
        } catch (cause) {
          if (
            !(cause instanceof NativeTransactionNotIncludedError) ||
            cause.txHash !== record.txHash
          )
            throw cause;
          // Only native absence permits the competing valid spend to prove
          // this body lost. A phase-two-invalid inclusion still spent collateral.
          if (
            inputLabels(signed.body().inputs()).some(
              (ref) => outRefLabel(ref) === removedOutRef,
            )
          )
            return {
              ...record,
              disposition: "absent",
              reason: `removal ${latest.spentByTxHash} spent its state queue input ${removedOutRef}`,
            };
          return {
            ...record,
            disposition: "pending",
            reason: cause.message,
          };
        }
      },
    ),
  );

  return {
    kind: "corrected",
    headerHash: input.headerHash,
    removalTxHash: latest.spentByTxHash,
    removedStateQueueOutRef: removedOutRef,
    fraudProofOutRef: outRefLabel(fraudProof),
    submittedDaTransactions,
    orphanedAttestationOutRef:
      orphanedAttestation === undefined
        ? null
        : outRefLabel(orphanedAttestation),
  };
};
