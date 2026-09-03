import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  type FraudProofCatalogueCategoryName,
  FraudProofTokenDatum,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  sortStateQueueUTxOs,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  type StateQueueUTxO,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  getAddressDetails,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
} from "./journal-v1.js";
import type {
  FraudProofRawL1ComputationStepRole,
  FraudProofRawL1ScopeRole,
  FraudProofRawL1Snapshot,
  FraudProofRawL1SnapshotRequest,
  FraudProofRawL1Transaction,
  FraudProofRawL1Utxo,
} from "./raw-l1-snapshot-v1.js";
import {
  validateVerifiedFraudProofReleaseEconomicsPolicy,
  type VerifiedFraudProofReleaseEconomicsPolicy,
} from "./release-economics-policy-v1.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy-v1.js";

type LucidDataSchema = Parameters<typeof Data.from>[1];

export type FraudProofRawL1FamilyDefinition = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly categoryId: string;
  readonly headerHash: string;
  readonly proverCredential: string;
  readonly stateQueue: {
    readonly policyId: string;
    readonly address: string;
  };
  readonly computationThread: {
    readonly policyId: string;
    readonly steps: readonly {
      readonly role: FraudProofRawL1ComputationStepRole;
      readonly address: string;
      readonly datumSchema: LucidDataSchema;
    }[];
  };
  readonly proofToken: {
    readonly policyId: string;
    readonly address: string;
  };
  readonly operatorDirectory: {
    readonly activePolicyId: string;
    readonly activeAddress: string;
    readonly retiredPolicyId: string;
    readonly retiredAddress: string;
  };
  readonly schedulerAddress: string;
};

export type FraudProofRawL1FamilyStage =
  | {
      readonly kind: "not_started";
      readonly stateQueueBlockOutRef: string;
    }
  | {
      readonly kind: "step";
      readonly step: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13;
      readonly threadOutRef: string;
      readonly stateQueueBlockOutRef: string;
    }
  | {
      readonly kind: "proof_token";
      readonly fraudProofOutRef: string;
      readonly stateQueueBlockOutRef: string;
      readonly nextRemovalOutRef: string;
    }
  | {
      readonly kind: "removed";
      readonly terminal: FraudProofWorkflowTerminal;
    };

const HEX_4 = /^[0-9a-f]{8}$/u;
const HEX_28 = /^[0-9a-f]{56}$/u;
const COMPUTATION_STEP_ROLES = Object.freeze([
  "computation_thread_step_01",
  "computation_thread_step_02",
  "computation_thread_step_03",
  "computation_thread_step_04",
  "computation_thread_step_05",
  "computation_thread_step_06",
  "computation_thread_step_07",
  "computation_thread_step_08",
  "computation_thread_step_09",
] as const satisfies readonly FraudProofRawL1ComputationStepRole[]);

const assertCanonicalComputationSteps = (
  definition: FraudProofRawL1FamilyDefinition,
): void => {
  const steps = definition.computationThread.steps;
  if (
    steps.length === 0 ||
    steps.length > COMPUTATION_STEP_ROLES.length ||
    steps.some((step, index) => step.role !== COMPUTATION_STEP_ROLES[index])
  ) {
    throw new Error(
      "raw L1 family definition requires one to nine canonically ordered computation steps",
    );
  }
};

const rawToUtxo = (raw: FraudProofRawL1Utxo): UTxO => {
  const [txHash, outputIndex] = raw.outRef.split("#");
  return {
    txHash: txHash!,
    outputIndex: Number(outputIndex),
    ...coreToTxOutput(CML.TransactionOutput.from_cbor_hex(raw.outputCbor)),
  };
};

const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const scope = (
  snapshot: FraudProofRawL1Snapshot,
  role: FraudProofRawL1ScopeRole,
): FraudProofRawL1Snapshot["scopes"][number] => {
  const matches = snapshot.scopes.filter(
    (candidate) => candidate.role === role,
  );
  if (matches.length !== 1) {
    throw new Error(
      `raw L1 family snapshot requires exactly one ${role} scope`,
    );
  }
  return matches[0]!;
};

const outputQuantity = (raw: FraudProofRawL1Utxo, unit: string): bigint =>
  coreToTxOutput(CML.TransactionOutput.from_cbor_hex(raw.outputCbor)).assets[
    unit
  ] ?? 0n;

const currentUnit = ({
  snapshot,
  role,
  unit,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly role: FraudProofRawL1ScopeRole;
  readonly unit: string;
}): FraudProofRawL1Utxo | undefined => {
  const matches = scope(snapshot, role).utxos.filter(
    (candidate) => outputQuantity(candidate, unit) !== 0n,
  );
  if (matches.length > 1) {
    throw new Error(`raw L1 family snapshot contains duplicate ${role} tokens`);
  }
  const match = matches[0];
  if (match !== undefined && outputQuantity(match, unit) !== 1n) {
    throw new Error(`raw L1 family snapshot ${role} token quantity is not one`);
  }
  return match;
};

const requireThreadDatum = ({
  raw,
  schema,
  proverCredential,
  label,
}: {
  readonly raw: FraudProofRawL1Utxo;
  readonly schema: LucidDataSchema;
  readonly proverCredential: string;
  readonly label: string;
}): void => {
  if (raw.datumCbor === null) {
    throw new Error(`${label} is missing its inline computation datum`);
  }
  const datum = Data.from(raw.datumCbor, schema) as {
    readonly fraud_prover?: unknown;
  };
  if (datum.fraud_prover !== proverCredential) {
    throw new Error(`${label} is owned by another fraud prover`);
  }
};

const requireProofDatum = ({
  raw,
  proverCredential,
}: {
  readonly raw: FraudProofRawL1Utxo;
  readonly proverCredential: string;
}): void => {
  if (raw.datumCbor === null) {
    throw new Error("permanent proof token is missing its inline datum");
  }
  const datum = Data.from(raw.datumCbor, FraudProofTokenDatum);
  if (datum.fraud_prover !== proverCredential) {
    throw new Error("permanent proof token is owned by another fraud prover");
  }
};

const stateQueueHeaderHash = async (
  candidate: StateQueueUTxO,
): Promise<string | null> => {
  if (candidate.datum.key === "Empty") return null;
  if (!candidate.assetName.startsWith(STATE_QUEUE_NODE_ASSET_NAME_PREFIX)) {
    throw new Error("state-queue block has an invalid token prefix");
  }
  const assetHash = candidate.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  if (candidate.datum.key.Key.key !== assetHash) {
    throw new Error("state-queue block token and linked-list key disagree");
  }
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(candidate.datum),
  );
  const computedHash = await Effect.runPromise(hashBlockHeader(header));
  if (computedHash !== assetHash) {
    throw new Error(
      "state-queue block datum and authentication token disagree",
    );
  }
  return assetHash;
};

const stateQueueTopology = async ({
  snapshot,
  definition,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1FamilyDefinition;
}): Promise<{
  readonly ordered: readonly StateQueueUTxO[];
  readonly target: StateQueueUTxO | undefined;
  readonly successor: StateQueueUTxO | undefined;
}> => {
  const scoped = scope(snapshot, "state_queue");
  const stateOutputs = scoped.utxos.filter((candidate) =>
    Object.entries(
      coreToTxOutput(CML.TransactionOutput.from_cbor_hex(candidate.outputCbor))
        .assets,
    ).some(
      ([unit, quantity]) =>
        unit.startsWith(definition.stateQueue.policyId) && quantity !== 0n,
    ),
  );
  const decoded = await Promise.all(
    stateOutputs.map((candidate) =>
      Effect.runPromise(
        utxoToStateQueueUTxO(
          rawToUtxo(candidate),
          definition.stateQueue.policyId,
        ),
      ),
    ),
  );
  const ordered = await Effect.runPromise(sortStateQueueUTxOs(decoded));
  const hashes = await Promise.all(ordered.map(stateQueueHeaderHash));
  const targetIndex = hashes.indexOf(definition.headerHash);
  return {
    ordered,
    target: targetIndex < 0 ? undefined : ordered[targetIndex],
    successor:
      targetIndex < 0 || targetIndex + 1 >= ordered.length
        ? undefined
        : ordered[targetIndex + 1],
  };
};

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
  readonly definition: FraudProofRawL1FamilyDefinition;
}): Promise<AuthenticatedStateQueueHeaderObservation> => {
  const topology = await stateQueueTopology({ snapshot, definition });
  if (topology.target === undefined) {
    throw new Error(
      "authenticated state-queue header observation requires a live target",
    );
  }
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(topology.target.datum),
  );
  const targetOutRef = outRef(topology.target.utxo);
  const creatingTxHash = targetOutRef.split("#")[0]!;
  const creatingTransaction = snapshot.transactions.find(
    (transaction) => transaction.txHash === creatingTxHash,
  );
  if (creatingTransaction === undefined) {
    throw new Error(
      "raw L1 snapshot omitted the state-queue header creation transaction",
    );
  }
  return {
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    sourceMode: "local_node",
    provenance: {
      trustClass: "authenticated_cardano_l1",
      sourceId: snapshot.provenance.sourceId,
      grade: "security",
    },
    chainPoint: {
      slot: BigInt(creatingTransaction.inclusionPoint.slot),
      blockHash: creatingTransaction.inclusionPoint.blockHash,
    },
    confirmationDepth: creatingTransaction.confirmationDepth,
    headerHash: definition.headerHash,
    header,
  };
};

const bodyOutputsContainUnit = (
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

const mintQuantity = (body: CML.TransactionBody, unit: string): bigint => {
  const minted = body
    .mint()
    ?.get_assets(CML.ScriptHash.from_hex(unit.slice(0, 56)));
  return minted?.get(CML.AssetName.from_hex(unit.slice(56))) ?? 0n;
};

const historicalTransactions = ({
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

const exactRemoval = ({
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

const operatorFromRemovedState = async ({
  removed,
  definition,
}: {
  readonly removed: FraudProofRawL1Utxo;
  readonly definition: FraudProofRawL1FamilyDefinition;
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

const transactionOutputs = (
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

const isProverEnterpriseOutput = (
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

const isExactRewardOutput = ({
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

const operatorBondInputs = ({
  transaction,
  definition,
  activeUnit,
  retiredUnit,
}: {
  readonly transaction: FraudProofRawL1Transaction;
  readonly definition: FraudProofRawL1FamilyDefinition;
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

const deriveTerminal = async ({
  snapshot,
  definition,
  stateUnit,
  proofUnit,
  proof,
  releaseEconomics,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1FamilyDefinition;
  readonly stateUnit: string;
  readonly proofUnit: string;
  readonly proof: FraudProofRawL1Utxo;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
}): Promise<FraudProofWorkflowTerminal> => {
  const verifiedEconomics =
    validateVerifiedFraudProofReleaseEconomicsPolicy(releaseEconomics);
  if (
    verifiedEconomics.deploymentIdentityDigest !==
      snapshot.deploymentIdentityDigest ||
    verifiedEconomics.releaseIdentityDigest !== snapshot.releaseIdentityDigest
  ) {
    throw new Error(
      "release economics identity does not match the raw L1 snapshot",
    );
  }
  requireProofDatum({
    raw: proof,
    proverCredential: definition.proverCredential,
  });
  const removal = exactRemoval({
    snapshot,
    stateUnit,
    proofOutRef: proof.outRef,
  });
  const operatorCredential = await operatorFromRemovedState({
    removed: removal.removed,
    definition,
  });
  const activeUnit = toUnit(
    definition.operatorDirectory.activePolicyId,
    `${ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX}${operatorCredential}`,
  );
  const retiredUnit = toUnit(
    definition.operatorDirectory.retiredPolicyId,
    `${RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX}${operatorCredential}`,
  );
  const stateHistory = historicalTransactions({ snapshot, unit: stateUnit });
  const proofReferencing = stateHistory.filter((transaction) =>
    transaction.resolvedReferenceInputs.some(
      (candidate) => candidate.outRef === proof.outRef,
    ),
  );
  const slashCandidates = proofReferencing.flatMap((transaction) => {
    const inputs = operatorBondInputs({
      transaction,
      definition,
      activeUnit,
      retiredUnit,
    });
    if (inputs.length > 1) {
      throw new Error(
        "one correction transaction consumed multiple operator bonds",
      );
    }
    return inputs.length === 1 ? [{ transaction, bondInput: inputs[0]! }] : [];
  });
  if (slashCandidates.length !== 1) {
    throw new Error(
      "raw L1 history does not prove exactly one bond-consuming proof-referenced slash",
    );
  }
  const slash = slashCandidates[0]!;
  const currentOperatorTokens = [
    ...scope(snapshot, "active_operator_directory").utxos.filter(
      (candidate) => outputQuantity(candidate, activeUnit) !== 0n,
    ),
    ...scope(snapshot, "retired_operator_directory").utxos.filter(
      (candidate) => outputQuantity(candidate, retiredUnit) !== 0n,
    ),
  ];
  if (currentOperatorTokens.length > 0) {
    throw new Error(
      "fraudulent operator remains in an authenticated directory",
    );
  }
  const policy = verifiedEconomics.policy;
  const requiredBond = BigInt(policy.requiredBondLovelace);
  const penalty = BigInt(policy.slashingPenaltyLovelace);
  const reward = BigInt(policy.fraudProverRewardLovelace);
  const inactivityPenalty = BigInt(policy.inactivitySlashingPenaltyLovelace);
  const bondLovelace =
    coreToTxOutput(
      CML.TransactionOutput.from_cbor_hex(slash.bondInput.outputCbor),
    ).assets.lovelace ?? 0n;
  const slashFee = CML.TransactionBody.from_cbor_hex(
    slash.transaction.bodyCbor,
  ).fee();
  const fullTranche = bondLovelace === requiredBond && slashFee === penalty;
  const partialTranche =
    bondLovelace === requiredBond - inactivityPenalty &&
    slashFee === penalty - inactivityPenalty;
  if (!fullTranche && !partialTranche) {
    throw new Error(
      "bond-consuming slash does not match the release-bound full or partial tranche",
    );
  }
  const enterpriseOutputs = transactionOutputs(slash.transaction).filter(
    ({ output }) =>
      isProverEnterpriseOutput(output, definition.proverCredential),
  );
  if (
    enterpriseOutputs.length !== 1 ||
    !isExactRewardOutput({
      output: enterpriseOutputs[0]!.output,
      proverCredential: definition.proverCredential,
      reward,
    })
  ) {
    throw new Error(
      "bond-consuming slash does not carry one exact ADA-only enterprise reward",
    );
  }
  const rewardOutput = enterpriseOutputs[0]!;
  const allRewardOutputs = stateHistory.flatMap((transaction) =>
    transactionOutputs(transaction).filter(({ output }) =>
      isExactRewardOutput({
        output,
        proverCredential: definition.proverCredential,
        reward,
      }),
    ),
  );
  if (
    allRewardOutputs.length !== 1 ||
    allRewardOutputs[0]!.outRef !== rewardOutput.outRef
  ) {
    throw new Error(
      "raw L1 history contains a missing or duplicate prover reward",
    );
  }
  const proofTxHash = proof.outRef.split("#")[0]!;
  return {
    schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
    category: definition.category,
    headerHash: definition.headerHash,
    proofToken: {
      unit: proofUnit,
      outRef: proof.outRef,
      createdByTxHash: proofTxHash,
      retainedAtFinalState: true,
    },
    correction: {
      removalTxHash: removal.transaction.txHash,
      removedStateQueueOutRef: removal.removed.outRef,
      fraudulentHeaderAbsent: true,
      referencedProofTokenOutRef: proof.outRef,
    },
    economics: {
      operatorCredential,
      proverCredential: definition.proverCredential,
      operatorBondInputOutRef: slash.bondInput.outRef,
      operatorBondInputLovelace: bondLovelace.toString(),
      slashedLovelace: slashFee.toString(),
      proverRewardOutputOutRef: rewardOutput.outRef,
      proverRewardLovelace: reward.toString(),
      removalFeeLovelace: slashFee.toString(),
      duplicateRewardAbsent: true,
    },
    observedAt: {
      slot: removal.transaction.inclusionPoint.slot,
      blockHash: removal.transaction.inclusionPoint.blockHash,
      confirmationDepth: removal.transaction.confirmationDepth,
    },
  };
};

export const fraudProofRawL1SnapshotRequestForFamily = ({
  definition,
  releaseFinality,
}: {
  readonly definition: FraudProofRawL1FamilyDefinition;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): FraudProofRawL1SnapshotRequest => {
  assertCanonicalComputationSteps(definition);
  if (
    !HEX_4.test(definition.categoryId) ||
    !HEX_28.test(definition.headerHash) ||
    !HEX_28.test(definition.proverCredential)
  ) {
    throw new Error(
      "raw L1 family definition has invalid category/header/prover bytes",
    );
  }
  const assetName = `${definition.categoryId}${definition.headerHash}`;
  return {
    deploymentIdentityDigest: releaseFinality.deploymentIdentityDigest,
    releaseIdentityDigest: releaseFinality.releaseIdentityDigest,
    finalityPolicyDigest: releaseFinality.policyDigest,
    headerHash: definition.headerHash,
    scopes: [
      { role: "state_queue", address: definition.stateQueue.address },
      ...definition.computationThread.steps.map(({ role, address }) => ({
        role,
        address,
      })),
      {
        role: "permanent_proof_token",
        address: definition.proofToken.address,
      },
      {
        role: "active_operator_directory",
        address: definition.operatorDirectory.activeAddress,
      },
      {
        role: "retired_operator_directory",
        address: definition.operatorDirectory.retiredAddress,
      },
      { role: "scheduler", address: definition.schedulerAddress },
    ],
    historyUnits: [
      toUnit(
        definition.stateQueue.policyId,
        `${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${definition.headerHash}`,
      ),
      toUnit(definition.computationThread.policyId, assetName),
      toUnit(definition.proofToken.policyId, assetName),
    ],
  };
};

export const deriveFraudProofRawL1FamilyStage = async ({
  snapshot,
  definition,
  releaseEconomics,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1FamilyDefinition;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
}): Promise<FraudProofRawL1FamilyStage> => {
  assertCanonicalComputationSteps(definition);
  if (
    snapshot.headerHash !== definition.headerHash ||
    scope(snapshot, "state_queue").address !== definition.stateQueue.address ||
    scope(snapshot, "permanent_proof_token").address !==
      definition.proofToken.address
  ) {
    throw new Error("raw L1 snapshot does not match its family definition");
  }
  const assetName = `${definition.categoryId}${definition.headerHash}`;
  const stateUnit = toUnit(
    definition.stateQueue.policyId,
    `${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${definition.headerHash}`,
  );
  const threadUnit = toUnit(definition.computationThread.policyId, assetName);
  const proofUnit = toUnit(definition.proofToken.policyId, assetName);
  const expectedHistory = new Set([stateUnit, threadUnit, proofUnit]);
  if (
    snapshot.historyUnits.length !== expectedHistory.size ||
    snapshot.historyUnits.some((unit) => !expectedHistory.has(unit))
  ) {
    throw new Error("raw L1 snapshot changed the family authentication units");
  }
  const threads = definition.computationThread.steps.flatMap((step, index) => {
    if (scope(snapshot, step.role).address !== step.address) {
      throw new Error(
        `raw L1 snapshot changed step ${(index + 1).toString()} address`,
      );
    }
    const current = currentUnit({
      snapshot,
      role: step.role,
      unit: threadUnit,
    });
    if (current === undefined) return [];
    requireThreadDatum({
      raw: current,
      schema: step.datumSchema,
      proverCredential: definition.proverCredential,
      label: `computation step ${(index + 1).toString()}`,
    });
    return [
      {
        step: (index + 1) as
          | 1
          | 2
          | 3
          | 4
          | 5
          | 6
          | 7
          | 8
          | 9
          | 10
          | 11
          | 12
          | 13,
        raw: current,
      },
    ];
  });
  if (threads.length > 1) {
    throw new Error("raw L1 snapshot has more than one live computation step");
  }
  const proof = currentUnit({
    snapshot,
    role: "permanent_proof_token",
    unit: proofUnit,
  });
  if (proof !== undefined) {
    requireProofDatum({
      raw: proof,
      proverCredential: definition.proverCredential,
    });
  }
  if (proof !== undefined && threads.length > 0) {
    throw new Error(
      "raw L1 snapshot has both a computation thread and proof token",
    );
  }
  const topology = await stateQueueTopology({ snapshot, definition });
  if (topology.target === undefined) {
    if (proof === undefined || threads.length > 0) {
      throw new Error(
        "fraudulent header disappeared without a retained proof token",
      );
    }
    return {
      kind: "removed",
      terminal: await deriveTerminal({
        snapshot,
        definition,
        stateUnit,
        proofUnit,
        proof,
        releaseEconomics,
      }),
    };
  }
  const stateQueueBlockOutRef = outRef(topology.target.utxo);
  if (proof !== undefined) {
    return {
      kind: "proof_token",
      fraudProofOutRef: proof.outRef,
      stateQueueBlockOutRef,
      nextRemovalOutRef:
        topology.successor === undefined
          ? stateQueueBlockOutRef
          : outRef(topology.successor.utxo),
    };
  }
  const thread = threads[0];
  if (thread !== undefined) {
    return {
      kind: "step",
      step: thread.step,
      threadOutRef: thread.raw.outRef,
      stateQueueBlockOutRef,
    };
  }
  return { kind: "not_started", stateQueueBlockOutRef };
};
