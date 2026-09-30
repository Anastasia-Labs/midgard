import {
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  getHeaderFromStateQueueDatum,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  getAddressDetails,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type FraudProofRawL1TerminalDefinition,
  mintQuantity,
  outputQuantity,
  rawToUtxo,
  stateQueueHeaderHash,
  StateQueueHeaderNotLiveError,
  stateQueueTopology,
} from "./raw-l1-family-derivation.state-queue-topology.js";
import type {
  FraudProofRawL1Snapshot,
  FraudProofRawL1Transaction,
  FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";

/**
 * Derives the canonical evidence header observation from the same admitted
 * state-queue bytes used by the live family state machine. Production callers
 * therefore never assert their own `authenticated_cardano_l1` provenance.
 */
export const deriveAuthenticatedStateQueueHeaderObservationFromRawL1 = async ({
  snapshot,
  definition,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
}): Promise<AuthenticatedStateQueueHeaderObservation> => {
  const topology = await stateQueueTopology({ snapshot, definition });
  if (topology.target === undefined) {
    throw new StateQueueHeaderNotLiveError();
  }
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(topology.target.datum),
  );
  // The header was committed where its NFT was minted. Later transactions
  // re-create the live output without changing the header (a DA attestation
  // does), so binding evidence to the current output's creating transaction
  // would change the observation under a prepared workflow.
  const mint = uniqueStateQueueHeaderMint({ snapshot, definition });
  return {
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    sourceMode: "local_node",
    provenance: {
      trustClass: "authenticated_cardano_l1",
      sourceId: snapshot.provenance.sourceId,
      grade: "security",
    },
    chainPoint: {
      slot: BigInt(mint.inclusionPoint.slot),
      blockHash: mint.inclusionPoint.blockHash,
    },
    confirmationDepth: mint.confirmationDepth,
    headerHash: definition.headerHash,
    header,
  };
};

/** The one authenticated mint of the selected header's state-queue NFT. */
const uniqueStateQueueHeaderMint = ({
  snapshot,
  definition,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
}): FraudProofRawL1Transaction => {
  const unit = toUnit(
    definition.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + definition.headerHash,
  );
  const mints = snapshot.transactions.filter(
    (transaction) =>
      mintQuantity(
        CML.TransactionBody.from_cbor_hex(transaction.bodyCbor),
        unit,
      ) === 1n,
  );
  if (mints.length !== 1)
    throw new Error("state-queue header requires one authenticated NFT mint");
  return mints[0]!;
};

/** Recovery-only observation of the unique authenticated mint of a header NFT.
 * The complete release-final unit history remains available after removal.
 * This does not assert that the header is live or authorize a new proof action. */
export const deriveRetainedStateQueueHeaderObservationFromRawL1 = async ({
  snapshot,
  definition,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
}): Promise<AuthenticatedStateQueueHeaderObservation> => {
  const unit = toUnit(
    definition.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + definition.headerHash,
  );
  const transaction = uniqueStateQueueHeaderMint({ snapshot, definition });
  const outputs = CML.TransactionBody.from_cbor_hex(
    transaction.bodyCbor,
  ).outputs();
  const matching: UTxO[] = [];
  for (let index = 0; index < outputs.len(); index++) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.assets[unit] === 1n &&
      output.address === definition.stateQueue.address
    )
      matching.push({
        ...output,
        txHash: transaction.txHash,
        outputIndex: index,
      });
  }
  if (matching.length !== 1)
    throw new Error("Retained header mint has no unique state-queue output");
  const node = await Effect.runPromise(
    utxoToStateQueueUTxO(matching[0]!, definition.stateQueue.policyId),
  );
  if ((await stateQueueHeaderHash(node)) !== definition.headerHash)
    throw new Error("Retained header source differs from selected header");
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(node.datum),
  );
  return {
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    sourceMode: "local_node",
    provenance: {
      trustClass: "authenticated_cardano_l1",
      sourceId: snapshot.provenance.sourceId,
      grade: "security",
    },
    chainPoint: {
      slot: BigInt(transaction.inclusionPoint.slot),
      blockHash: transaction.inclusionPoint.blockHash,
    },
    confirmationDepth: transaction.confirmationDepth,
    headerHash: definition.headerHash,
    header,
  };
};

export const bodyOutputsContainUnit = (
  body: CML.TransactionBody,
  unit: string,
): boolean => {
  const outputs = body.outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    if ((coreToTxOutput(outputs.get(index)).assets[unit] ?? 0n) !== 0n) {
      return true;
    }
  }
  return false;
};

export const historicalTransactions = ({
  snapshot,
  unit,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly unit: string;
}): readonly FraudProofRawL1Transaction[] => {
  const history = snapshot.history.find((candidate) => candidate.unit === unit);
  if (history === undefined) {
    throw new Error(`raw L1 family snapshot omitted history for ${unit}`);
  }
  const transactionByHash = new Map(
    snapshot.transactions.map(
      (candidate) => [candidate.txHash, candidate] as const,
    ),
  );
  return history.transactionHashes.map((hash) => {
    const transaction = transactionByHash.get(hash);
    if (transaction === undefined) {
      throw new Error(`raw L1 family snapshot omitted transaction ${hash}`);
    }
    return transaction;
  });
};

export const exactRemoval = ({
  snapshot,
  stateUnit,
  proofOutRef,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly stateUnit: string;
  readonly proofOutRef: string;
}): {
  readonly transaction: FraudProofRawL1Transaction;
  readonly removed: FraudProofRawL1Utxo;
} => {
  const matches = historicalTransactions({ snapshot, unit: stateUnit }).flatMap(
    (transaction) => {
      const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
      const removed = transaction.resolvedInputs.filter(
        (candidate) => outputQuantity(candidate, stateUnit) === 1n,
      );
      return removed.length === 1 &&
        !bodyOutputsContainUnit(body, stateUnit) &&
        mintQuantity(body, stateUnit) === -1n &&
        transaction.resolvedReferenceInputs.some(
          (candidate) => candidate.outRef === proofOutRef,
        )
        ? [{ transaction, removed: removed[0]! }]
        : [];
    },
  );
  if (matches.length !== 1) {
    throw new Error(
      "raw L1 history does not prove exactly one proof-referenced state removal",
    );
  }
  return matches[0]!;
};

export const operatorFromRemovedState = async ({
  removed,
  definition,
}: {
  readonly removed: FraudProofRawL1Utxo;
  readonly definition: FraudProofRawL1TerminalDefinition;
}): Promise<string> => {
  if (rawToUtxo(removed).address !== definition.stateQueue.address) {
    throw new Error("removed state-queue input came from another address");
  }
  const decoded = await Effect.runPromise(
    utxoToStateQueueUTxO(rawToUtxo(removed), definition.stateQueue.policyId),
  );
  if ((await stateQueueHeaderHash(decoded)) !== definition.headerHash) {
    throw new Error("removal consumed a different state-queue header");
  }
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(decoded.datum),
  );
  return header.operatorVkey;
};

export const transactionOutputs = (
  transaction: FraudProofRawL1Transaction,
): readonly {
  readonly outRef: string;
  readonly output: CML.TransactionOutput;
}[] => {
  const outputs = CML.TransactionBody.from_cbor_hex(
    transaction.bodyCbor,
  ).outputs();
  const result: { outRef: string; output: CML.TransactionOutput }[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    result.push({
      outRef: `${transaction.txHash}#${index.toString()}`,
      output: outputs.get(index),
    });
  }
  return result;
};

export const isProverEnterpriseOutput = (
  output: CML.TransactionOutput,
  proverCredential: string,
): boolean => {
  const details = getAddressDetails(output.address().to_bech32());
  return (
    details.paymentCredential?.type === "Key" &&
    details.paymentCredential.hash === proverCredential &&
    details.stakeCredential === undefined
  );
};

export const isExactRewardOutput = ({
  output,
  proverCredential,
  reward,
}: {
  readonly output: CML.TransactionOutput;
  readonly proverCredential: string;
  readonly reward: bigint;
}): boolean => {
  if (!isProverEnterpriseOutput(output, proverCredential)) return false;
  const decoded = coreToTxOutput(output);
  const nonzero = Object.entries(decoded.assets).filter(
    ([, quantity]) => quantity !== 0n,
  );
  return (
    nonzero.length === 1 &&
    nonzero[0]?.[0] === "lovelace" &&
    nonzero[0]?.[1] === reward &&
    output.datum() === undefined &&
    output.datum_hash() === undefined &&
    output.script_ref() === undefined
  );
};

export const operatorBondInputs = ({
  transaction,
  definition,
  activeUnit,
  retiredUnit,
}: {
  readonly transaction: FraudProofRawL1Transaction;
  readonly definition: FraudProofRawL1TerminalDefinition;
  readonly activeUnit: string;
  readonly retiredUnit: string;
}): readonly FraudProofRawL1Utxo[] =>
  transaction.resolvedInputs.filter((candidate) => {
    const output = coreToTxOutput(
      CML.TransactionOutput.from_cbor_hex(candidate.outputCbor),
    );
    return (
      (output.address === definition.operatorDirectory.activeAddress &&
        (output.assets[activeUnit] ?? 0n) === 1n) ||
      (output.address === definition.operatorDirectory.retiredAddress &&
        (output.assets[retiredUnit] ?? 0n) === 1n)
    );
  });
