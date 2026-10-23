import {
  type FraudProofCatalogueCategoryName,
  FraudProofTokenDatum,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  sortStateQueueUTxOs,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  type StateQueueUTxO,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { datumProver } from "../fraud-proof-record-selection.js";
import {
  COMPUTATION_THREAD_STEP_ROLES,
  type ComputationThreadStep,
} from "./computation-thread-steps.js";
import { type FraudProofWorkflowTerminal } from "./journal.js";
import type {
  FraudProofRawL1ComputationStepRole,
  FraudProofRawL1ScopeRole,
  FraudProofRawL1Snapshot,
  FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";

type LucidDataSchema = Parameters<typeof Data.from>[1];

export type FraudProofRawL1TerminalDefinition = {
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
  /**
   * A family whose cases can be defended: its case token's policy and the
   * award policy of the lost-defense receipts its threads mint.
   */
  readonly lostDefense?: {
    readonly casePolicyId: string;
    readonly receiptPolicyId: string;
  };
};

/** Executable observation additionally authenticates each live thread datum. */
export type FraudProofRawL1FamilyDefinition = Omit<
  FraudProofRawL1TerminalDefinition,
  "computationThread"
> & {
  readonly computationThread: {
    readonly policyId: string;
    readonly steps: readonly (FraudProofRawL1TerminalDefinition["computationThread"]["steps"][number] & {
      readonly datumSchema: LucidDataSchema;
    })[];
  };
};

export type FraudProofRawL1FamilyStage =
  | {
      readonly kind: "not_started";
      readonly stateQueueBlockOutRef: string;
    }
  | {
      readonly kind: "step";
      readonly step: ComputationThreadStep;
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

export const HEX_4 = /^[0-9a-f]{8}$/u;

export const HEX_28 = /^[0-9a-f]{56}$/u;

const COMPUTATION_STEP_ROLES = COMPUTATION_THREAD_STEP_ROLES;

export const assertCanonicalComputationSteps = (
  definition: FraudProofRawL1TerminalDefinition,
): void => {
  const steps = definition.computationThread.steps;
  if (
    steps.length === 0 ||
    steps.length > COMPUTATION_STEP_ROLES.length ||
    steps.some((step, index) => step.role !== COMPUTATION_STEP_ROLES[index])
  ) {
    throw new Error(
      `raw L1 family definition requires one to ${COMPUTATION_STEP_ROLES.length} canonically ordered computation steps`,
    );
  }
};

export const rawToUtxo = (raw: FraudProofRawL1Utxo): UTxO => {
  const [txHash, outputIndex] = raw.outRef.split("#");
  return {
    txHash: txHash!,
    outputIndex: Number(outputIndex),
    ...coreToTxOutput(CML.TransactionOutput.from_cbor_hex(raw.outputCbor)),
  };
};

export const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export const scope = (
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

export const outputQuantity = (
  raw: FraudProofRawL1Utxo,
  unit: string,
): bigint =>
  coreToTxOutput(CML.TransactionOutput.from_cbor_hex(raw.outputCbor)).assets[
    unit
  ] ?? 0n;

/**
 * Every output of `role`'s scope holding exactly one `unit`, in out-ref
 * order. The thread and proof units name a category and a header only, so
 * any number of cases or proofs on one header can each hold one; an output
 * holding another quantity holds no thread or proof of this family.
 */
export const unitHolders = ({
  snapshot,
  role,
  unit,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly role: FraudProofRawL1ScopeRole;
  readonly unit: string;
}): readonly FraudProofRawL1Utxo[] =>
  scope(snapshot, role)
    .utxos.filter((candidate) => outputQuantity(candidate, unit) === 1n)
    .sort((left, right) =>
      left.outRef < right.outRef ? -1 : left.outRef > right.outRef ? 1 : 0,
    );

/** Whether `raw`'s inline datum decodes under `schema` and names us. */
export const ownsThread = ({
  raw,
  schema,
  proverCredential,
}: {
  readonly raw: FraudProofRawL1Utxo;
  readonly schema: LucidDataSchema;
  readonly proverCredential: string;
}): boolean => datumProver(raw.datumCbor, schema) === proverCredential;

export const requireProofDatum = ({
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

export const stateQueueHeaderHash = async (
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

export const stateQueueTopology = async ({
  snapshot,
  definition,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
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
 * The family stage could not be derived from an admitted raw L1 snapshot.
 * The snapshot is the chain as it stands, so the same view fails the same
 * way: the caller records it against the one workflow and observes again,
 * never failing anything else. Matched by tag: the package ships more than
 * one entry bundle.
 */
export class FraudProofRawL1DerivationError extends Error {
  readonly _tag = "FraudProofRawL1DerivationError";

  constructor(category: string, cause: unknown) {
    super(
      `${category} raw L1 family stage derivation failed: ${cause instanceof Error ? cause.message : String(cause)}`,
      { cause },
    );
    this.name = "FraudProofRawL1DerivationError";
  }
}

/** Whether `error` is a derivation failure. The orchestrator catches it as
 * thrown, unwrapped. */
export const isFraudProofRawL1DerivationError = (
  error: unknown,
): error is FraudProofRawL1DerivationError =>
  typeof error === "object" &&
  error !== null &&
  (error as { readonly _tag?: unknown })._tag ===
    "FraudProofRawL1DerivationError";

/** The selected header is no longer in the state queue. */
export class StateQueueHeaderNotLiveError extends Error {
  constructor() {
    super(
      "authenticated state-queue header observation requires a live target",
    );
    this.name = "StateQueueHeaderNotLiveError";
  }
}

export const mintQuantity = (
  body: CML.TransactionBody,
  unit: string,
): bigint => {
  const minted = body
    .mint()
    ?.get_assets(CML.ScriptHash.from_hex(unit.slice(0, 56)));
  return minted?.get(CML.AssetName.from_hex(unit.slice(56))) ?? 0n;
};
