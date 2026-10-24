import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, toUnit } from "@lucid-evolution/lucid";

import { fraudSlashFeeLovelace } from "../remove-fraudulent-block.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
} from "./journal.js";
import {
  bodyOutputsContainUnit,
  exactRemoval,
  historicalTransactions,
  isExactRewardOutput,
  isProverEnterpriseOutput,
  operatorBondInputs,
  operatorFromRemovedState,
  transactionOutputs,
} from "./raw-l1-family-derivation.derive-retained-state-queue-header-observation-from-raw-l1.js";
import {
  foreignReceiptSlash,
  type FraudProofRawL1HistoryReader,
  lostDefenseReceiptSlash,
} from "./raw-l1-family-derivation.lost-defense-slash.js";
import {
  assertCanonicalComputationSteps,
  type FraudProofRawL1TerminalDefinition,
  HEX_4,
  HEX_28,
  mintQuantity,
  outputQuantity,
  requireProofDatum,
  scope,
} from "./raw-l1-family-derivation.state-queue-topology.js";
import type {
  FraudProofRawL1Snapshot,
  FraudProofRawL1SnapshotRequest,
  FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";
import {
  validateVerifiedFraudProofReleaseEconomicsPolicy,
  type VerifiedFraudProofReleaseEconomicsPolicy,
} from "./release-economics-policy.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";

/**
 * The header is gone but no terminal of ours stands: another prover's
 * record removed it, or the operator's bond ended in a slash that pays
 * another prover. Read-only verification waits on it; a run cannot complete.
 */
export type FraudProofRawL1TerminalNotOurs = Readonly<{
  kind: "terminal_not_ours";
  reason: string;
}>;

const notOurs = (reason: string): FraudProofRawL1TerminalNotOurs => ({
  kind: "terminal_not_ours",
  reason,
});

export const deriveTerminal = async ({
  snapshot,
  definition,
  stateUnit,
  proofUnit,
  proof,
  releaseEconomics,
  history,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
  readonly stateUnit: string;
  readonly proofUnit: string;
  readonly proof: FraudProofRawL1Utxo;
  readonly releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy;
  /** Further token histories, for a bond a lost-defense receipt slashed. */
  readonly history?: FraudProofRawL1HistoryReader | undefined;
}): Promise<
  | Readonly<{ kind: "ours"; terminal: FraudProofWorkflowTerminal }>
  | FraudProofRawL1TerminalNotOurs
> => {
  const verifiedEconomics =
    validateVerifiedFraudProofReleaseEconomicsPolicy(releaseEconomics);
  if (
    verifiedEconomics.deploymentIdentityDigest !==
      snapshot.deploymentIdentityDigest ||
    verifiedEconomics.blueprintHash !== snapshot.blueprintHash
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
  if (removal === undefined)
    return notOurs("another prover's record removed the header");
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
  // With no proof-referenced slash, the operator's bond can only have ended
  // in its slash by the receipt of this case's thread it lost; a slash by
  // another prover's receipt paid that prover, not us.
  const receiptSlash = {
    snapshot,
    definition,
    proofUnit,
    proofOutRef: proof.outRef,
    removal: removal.transaction,
    operatorCredential,
    activeUnit,
    retiredUnit,
    history,
  };
  const slash =
    slashCandidates.length === 1
      ? slashCandidates[0]
      : slashCandidates.length === 0
        ? await lostDefenseReceiptSlash(receiptSlash)
        : undefined;
  if (slash === undefined) {
    if (
      slashCandidates.length === 0 &&
      (await foreignReceiptSlash(receiptSlash))
    )
      return notOurs("another prover's receipt slashed the operator's bond");
    throw new Error(
      "raw L1 history does not prove exactly one bond-consuming proof-referenced slash",
    );
  }
  const slashBody = CML.TransactionBody.from_cbor_hex(
    slash.transaction.bodyCbor,
  );
  const bondUnit =
    outputQuantity(slash.bondInput, activeUnit) === 1n
      ? activeUnit
      : retiredUnit;
  // Directory tokens identify the operator, not a permanent bond lifetime.
  // A later registration can mint the same unit again. Authenticate the end
  // of this bond in its slash transaction instead of banning that identity
  // from today's directory while the historical correction gains depth.
  if (mintQuantity(slashBody, bondUnit) !== -1n) {
    throw new Error("slash did not burn the consumed operator bond token");
  }
  if (
    bodyOutputsContainUnit(slashBody, activeUnit) ||
    bodyOutputsContainUnit(slashBody, retiredUnit)
  ) {
    throw new Error("slash continued an operator bond token");
  }
  if (
    (["active_operator_directory", "retired_operator_directory"] as const).some(
      (role) =>
        scope(snapshot, role).utxos.some(
          (candidate) => candidate.outRef === slash.bondInput.outRef,
        ),
    )
  ) {
    throw new Error(
      "consumed operator bond remains live in the raw L1 snapshot",
    );
  }
  const policy = verifiedEconomics.policy;
  const reward = BigInt(policy.fraudProverRewardLovelace);
  const bondLovelace =
    coreToTxOutput(
      CML.TransactionOutput.from_cbor_hex(slash.bondInput.outputCbor),
    ).assets.lovelace ?? 0n;
  const slashFee = slashBody.fee();
  if (slashFee !== fraudSlashFeeLovelace(policy, bondLovelace)) {
    throw new Error(
      "bond-consuming slash fee does not match the release-bound slash of its bond",
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
  // Reward uniqueness is scoped to this operator's authenticated bond slash.
  // Earlier child splices can pay the same prover for other operator bonds.
  const proofTxHash = proof.outRef.split("#")[0]!;
  const terminal: FraudProofWorkflowTerminal = {
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
  return { kind: "ours", terminal };
};

export const fraudProofRawL1SnapshotRequestForFamily = ({
  definition,
  releaseFinality,
}: {
  readonly definition: FraudProofRawL1TerminalDefinition;
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
    blueprintHash: releaseFinality.blueprintHash,
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

export const authenticateFamilySnapshot = ({
  snapshot,
  definition,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
}) => {
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
  return { stateUnit, threadUnit, proofUnit };
};
