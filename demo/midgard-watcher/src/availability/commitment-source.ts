import type { FraudProofRawL1Transaction } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  type LucidEvolution,
} from "@lucid-evolution/lucid";

/** Re-assembles the full transaction CBOR from an admitted raw-L1 read. */
export const watcherRawTransactionCbor = (
  raw: Pick<
    FraudProofRawL1Transaction,
    "bodyCbor" | "witnessSetCbor" | "isValid"
  >,
): string => {
  const body = CML.TransactionBody.from_cbor_hex(raw.bodyCbor);
  const witnessSet = CML.TransactionWitnessSet.from_cbor_hex(
    raw.witnessSetCbor,
  );
  const transaction = CML.Transaction.new(
    body,
    witnessSet,
    raw.isValid,
    undefined,
  );
  try {
    return transaction.to_cbor_hex();
  } finally {
    transaction.free();
    witnessSet.free();
    body.free();
  }
};

const burnsUnit = (
  raw: FraudProofRawL1Transaction,
  policyId: string,
  assetName: string,
): boolean => {
  const body = CML.TransactionBody.from_cbor_hex(raw.bodyCbor);
  try {
    const tokens = body.mint()?.get_assets(CML.ScriptHash.from_hex(policyId));
    return tokens?.get(CML.AssetName.from_hex(assetName)) === -1n;
  } finally {
    body.free();
  }
};

const holdsUnit = (outputCbor: string, unit: string): boolean => {
  const output = CML.TransactionOutput.from_cbor_hex(outputCbor);
  try {
    return coreToTxOutput(output).assets[unit] === 1n;
  } finally {
    output.free();
  }
};

/**
 * The transactions that produced Apply's inputs whose outputs, as the
 * follower resolved them for Apply, all hold no `unit`. The SDK reads a
 * producer only to find the output holding `unit`, so such a producer
 * changes nothing and is never read: its creating transaction may be one
 * the follower never stored, such as the submitter's fee UTxO, which a
 * pruned store refuses to place.
 */
const producersWithout = (
  apply: FraudProofRawL1Transaction,
  unit: string,
): Set<string> => {
  const resolved = new Map(
    apply.resolvedInputs.map(({ outRef, outputCbor }) => [outRef, outputCbor]),
  );
  const holds = new Map<string, boolean>();
  const body = CML.TransactionBody.from_cbor_hex(apply.bodyCbor);
  try {
    const inputs = body.inputs();
    for (let i = 0; i < inputs.len(); i++) {
      const input = inputs.get(i);
      const txHash = input.transaction_id().to_hex();
      const cbor = resolved.get(`${txHash}#${input.index().toString()}`);
      const holding = cbor === undefined || holdsUnit(cbor, unit);
      holds.set(txHash, (holds.get(txHash) ?? false) || holding);
    }
  } finally {
    body.free();
  }
  return new Set(
    [...holds].flatMap(([txHash, holding]) => (holding ? [] : [txHash])),
  );
};

/**
 * Sources the full attested commitment of an `Attested{commitment_hash}` node
 * (spec #685 E1). The only on-chain copy after Apply is the DAAT datum Apply
 * spent, so this finds the unique valid Apply transaction in the node's
 * canonical unit history (the one that burns the header's DAAT token) and
 * lets the SDK read the spent DAAT output from its producing transaction.
 *
 * The commitment must hash to the node's `commitment_hash`; anything else is
 * refused with the SDK's typed `commitment-hash-mismatch` error, never used.
 */
export const recoverWatcherAttestedCommitment = async (input: {
  headerHash: string;
  expectedCommitmentHash: string;
  stateQueuePolicyId: string;
  daAttestationPolicyId: string;
  lucid: Pick<LucidEvolution, "config">;
  /** The admitted canonical history of one unit, in chain order. */
  readHistory(unit: string): Promise<readonly FraudProofRawL1Transaction[]>;
  /** One admitted transaction by id, or `undefined` when it is unknown. */
  readTransaction(
    txHash: string,
  ): Promise<FraudProofRawL1Transaction | undefined>;
}): Promise<SDK.RecoveredDaAvailabilityCommitment> => {
  const history = await input.readHistory(
    input.stateQueuePolicyId +
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      input.headerHash,
  );
  const attestationName =
    SDK.DA_ATTESTATION_ASSET_NAME_PREFIX + input.headerHash;
  const applies = history.filter(
    (transaction) =>
      transaction.isValid &&
      burnsUnit(transaction, input.daAttestationPolicyId, attestationName),
  );
  if (applies.length !== 1)
    throw new SDK.DaAvailabilityTransactionError(
      `Attested header ${input.headerHash} has ${applies.length.toString()} canonical Apply transactions; exactly one is required`,
      "apply-commitment-unrecoverable",
    );
  const known = new Map(
    history.map((transaction) => [transaction.txHash, transaction]),
  );
  const unread = producersWithout(
    applies[0]!,
    input.daAttestationPolicyId + attestationName,
  );
  const recovered = await SDK.recoverDaAvailabilityCommitmentFromApplyTx(
    input.lucid,
    {
      applyTxHash: applies[0]!.txHash,
      daAttestationPolicyId: input.daAttestationPolicyId,
      expectedCommitmentHash: input.expectedCommitmentHash,
      fetchTransactionCbor: async (txHash) => {
        const raw =
          known.get(txHash) ??
          (unread.has(txHash)
            ? undefined
            : await input.readTransaction(txHash));
        return raw === undefined ? undefined : watcherRawTransactionCbor(raw);
      },
    },
  );
  if (recovered.headerHash !== input.headerHash)
    throw new SDK.DaAvailabilityTransactionError(
      "Recovered commitment names a different block",
      "apply-commitment-unrecoverable",
    );
  return recovered;
};
