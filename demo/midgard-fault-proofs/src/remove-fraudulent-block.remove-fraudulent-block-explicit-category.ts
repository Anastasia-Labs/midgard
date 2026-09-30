import {
  incompleteRemoveLastFraudulentBlockHeaderTxProgram,
  type LinkedListNodeView,
  requireUniqueOutputIndex,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  type Script,
  type TxOutput,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { type ContractDeploymentInfo } from "./inspect-contracts.js";
import {
  type DeploymentScriptName,
  type ReferenceScriptName,
  type RemoveFraudulentBlockCategoryLabel,
  type RemoveFraudulentBlockContracts,
  type RemoveFraudulentBlockFraudCategory,
  type RemoveFraudulentBlockLayout,
} from "./remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import {
  requireMatchingScriptHash,
  type SubmitProviderConfig,
} from "./runtime.js";

const REMOVE_LAYOUT_KEYS = [
  "fraudProofRefInputIndex",
  "stateQueueRedeemerTxInfoIndex",
  "activeOperatorsRedeemerTxInfoIndex",
  "retiredOperatorsRedeemerTxInfoIndex",
  "activeOperatorsElementRefInputIndex",
  "retiredOperatorsElementRefInputIndex",
  "anchorElementOutputIndex",
  "fraudulentNodeOutputIndex",
] as const satisfies readonly (keyof RemoveFraudulentBlockLayout)[];

export type OperatorListEntry = {
  readonly utxo: UTxO;
  readonly view: LinkedListNodeView;
};

export type OperatorListRemovalPlan = {
  readonly anchor: OperatorListEntry;
  readonly node: OperatorListEntry;
  readonly lastNodeAfterRemoval?: OperatorListEntry;
};

export type StateQueueTopology = {
  readonly root: StateQueueUTxO;
  readonly ordered: readonly StateQueueUTxO[];
  readonly nodeByHeaderHash: ReadonlyMap<string, StateQueueUTxO>;
  readonly predecessorByHeaderHash: ReadonlyMap<string, StateQueueUTxO>;
  readonly successorByHeaderHash: ReadonlyMap<string, StateQueueUTxO>;
};

export type RemoveTransactionKind = "remove-successor" | "remove-target";

export type RemoveTransactionResult = {
  readonly kind: RemoveTransactionKind;
  readonly txHash: string;
  readonly removedHeaderHash: string;
  readonly removedOperator: string;
  readonly stateQueueBlockOutRef: string;
  readonly operatorNodeOutRef: string | null;
  readonly registeredOperatorsElementOutRef: string | null;
  readonly slashingApproach:
    | "SlashActiveOperator"
    | "SlashRetiredOperator"
    | "OperatorAlreadySlashed";
  readonly layout: Record<keyof RemoveFraudulentBlockLayout, string | null>;
};

export type SchedulerRemovalPlan =
  | {
      readonly kind: "inactive";
    }
  | {
      readonly kind: "goToAnchor";
      readonly newOperator: string;
      readonly removedNodeIsLast: boolean;
    }
  | {
      readonly kind: "rewind";
      readonly newOperator?: string;
      readonly removedNodeIsLast: boolean;
    };

export type RemoveFraudulentBlockSlashing = Parameters<
  typeof incompleteRemoveLastFraudulentBlockHeaderTxProgram
>[2]["slashing"];

export type OperatorSlashingLayoutContext =
  | {
      readonly operatorDirectoryAnchor: UTxO;
      readonly operatorDirectoryNode: UTxO;
      readonly scheduler: UTxO;
      readonly schedulerPlan: SchedulerRemovalPlan;
      readonly hubOracle: UTxO;
      readonly registeredOperatorsElement?: UTxO;
      readonly activeOperatorsLastNode?: UTxO;
      readonly operatorDirectoryAnchorUnit: string;
      readonly slashedOperatorDirectory: "active";
      readonly schedulerUnit: string;
      readonly contracts: RemoveFraudulentBlockContracts;
    }
  | {
      readonly operatorDirectoryAnchor: UTxO;
      readonly operatorDirectoryNode: UTxO;
      readonly hubOracle: UTxO;
      readonly operatorDirectoryAnchorUnit: string;
      readonly slashedOperatorDirectory: "retired";
      readonly contracts: RemoveFraudulentBlockContracts;
    };

export type RemoveFraudulentBlockCliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly fraudCategory?: RemoveFraudulentBlockFraudCategory;
  readonly fraudulentHeaderHash: string;
  readonly awaitConfirmation?: boolean;
};

/**
 * Explicit already-resolved category record for fault-proof families that
 * predate their catalogue registration (none at present; the mechanism stays
 * for the next pre-registration family).
 * These families have no SDK contract-chain builder and no category id in any
 * deployment manifest yet, so removal cannot resolve them the canonical way;
 * per the families' submitter convention the caller supplies the
 * already-resolved facts instead, and every fail-closed check the canonical
 * path runs still runs: the shared fraud-proof pair is checked against the
 * `fraudProofMint`/`fraudProofSpend` deployment entries, the step-01 hash
 * against the named deployment entry, and the category id against collisions
 * with every canonical registered id. The on-chain removal handler is
 * category-agnostic — it authenticates the fraud-proof reference input by
 * policy id and reads the header hash off the asset-name suffix — so no
 * category-specific script participates in the removal transaction itself.
 */
export type RemoveFraudulentBlockExplicitCategory = {
  /** Category label used in failure messages and the result payload. */
  readonly name: string;
  /**
   * The 4-byte hex category id the family's computation thread and
   * fraud-proof token were minted under.
   */
  readonly categoryId: string;
  /**
   * Deployment-manifest entry whose `scriptHash` pins the family's step-01
   * spending script.
   */
  readonly firstStepDeploymentEntry: string;
  /** The step-01 spending-script hash of the already-resolved family chain. */
  readonly firstStepScriptHash: string;
  /** The shared fraud-proof pair the family chain was parameterized with. */
  readonly fraudProof: {
    readonly policyId: string;
    readonly spendingScriptHash: string;
    readonly spendingScriptAddress: string;
  };
};

export type StateQueueMutationLease = {
  readonly token: string;
  readonly source: string;
  readonly renew: () => Promise<void>;
  readonly release: () => Promise<void>;
  readonly fail: (error: string) => Promise<void>;
};

export type StateQueueMutationLeaseIdentity = Pick<
  StateQueueMutationLease,
  "token" | "source"
>;

export type StateQueueMutationLeaseCoordinator = {
  readonly acquire: () => Promise<StateQueueMutationLease>;
  /** Reconstructs the exact journaled fencing lease; never acquires a new one. */
  readonly resume?: (
    identity: StateQueueMutationLeaseIdentity,
  ) => Promise<StateQueueMutationLease>;
};

export type SubmitRemoveFraudulentBlockResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly fraudCategory: RemoveFraudulentBlockCategoryLabel;
  readonly fraudCategoryId: string;
  readonly fraudulentHeaderHash: string;
  readonly stateQueueBlockOutRef: string;
  readonly stateQueueRootOutRef: string;
  readonly fraudProofOutRef: string;
  readonly activeOperatorsRootOutRef: string | null;
  readonly activeOperatorNodeOutRef: string | null;
  readonly schedulerOutRef: string;
  readonly hubOracleOutRef: string;
  readonly registeredOperatorsElementOutRef: string | null;
  readonly referenceScriptOutRefs: Readonly<
    Record<ReferenceScriptName, string | null>
  >;
  readonly transactions: readonly RemoveTransactionResult[];
  readonly layout: Record<keyof RemoveFraudulentBlockLayout, string | null>;
  readonly awaitedConfirmation: boolean;
  readonly stateQueueMutationLease: {
    readonly token: string;
    readonly source: string;
    readonly released: boolean;
  } | null;
};

/**
 * Lease identity recorded by the local coordinator, the only coordination mode.
 * A journaled in-flight removal resumes only under this exact identity; a
 * journal written under a Midgard node's HTTP lease (a node URL as `source`)
 * is refused rather than silently continued.
 */
export const LOCAL_STATE_QUEUE_MUTATION_LEASE_SOURCE = "local";

export const LOCAL_STATE_QUEUE_MUTATION_LEASE_TOKEN =
  "local-retry-until-confirmed";

const refuseLeaseCoordinationModeSwitch = (
  expectedSource: string,
  actualSource: string,
): Error =>
  new Error(
    `Refusing state-queue mutation lease from a different coordinator: expected=${expectedSource} actual=${actualSource}. A journaled in-flight removal cannot switch coordination mode; a journal started by a coordinator that no longer exists must be abandoned.`,
  );

/**
 * Local state-queue mutation coordination. The prover fences no Midgard node's
 * commitment or merge workers, so the "lease" is a frozen no-op: each peel of a
 * non-tail removal is confirmed and the state-queue topology refetched before
 * the next one, and a peel that loses to a competing commit or merge throws.
 * Retry-until-confirmed lives in the watcher's workflow orchestrator, which
 * reconciles against authenticated L1 state and rebuilds the removal from the
 * fresh view; a bare CLI run fails on a lost race and must be re-run. The
 * identity is still journaled so a resume cannot silently pick up a removal
 * journaled under a different coordinator.
 */
export const createLocalStateQueueMutationLeaseCoordinator =
  (): StateQueueMutationLeaseCoordinator => {
    const lease: StateQueueMutationLease = Object.freeze({
      token: LOCAL_STATE_QUEUE_MUTATION_LEASE_TOKEN,
      source: LOCAL_STATE_QUEUE_MUTATION_LEASE_SOURCE,
      renew: async () => {},
      release: async () => {},
      fail: async () => {},
    });
    return {
      acquire: async () => lease,
      resume: async ({ token, source }) => {
        if (source !== LOCAL_STATE_QUEUE_MUTATION_LEASE_SOURCE) {
          throw refuseLeaseCoordinationModeSwitch(
            LOCAL_STATE_QUEUE_MUTATION_LEASE_SOURCE,
            source,
          );
        }
        if (token !== LOCAL_STATE_QUEUE_MUTATION_LEASE_TOKEN) {
          throw new Error(
            `Refusing state-queue mutation lease with an unknown local token: expected=${LOCAL_STATE_QUEUE_MUTATION_LEASE_TOKEN} actual=${token}`,
          );
        }
        return lease;
      },
    };
  };

export const requireOutputIndexByUnit = ({
  outputs,
  address,
  unit,
  label,
}: {
  readonly outputs: readonly TxOutput[];
  readonly address: string;
  readonly unit: string;
  readonly label: string;
}): bigint =>
  requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === address && (output.assets[unit] ?? 0n) === 1n,
    label,
  );

export const layoutToJson = (
  layout: RemoveFraudulentBlockLayout,
): Record<keyof RemoveFraudulentBlockLayout, string | null> =>
  Object.fromEntries(
    REMOVE_LAYOUT_KEYS.map((key) => [
      key,
      layout[key] === undefined ? null : layout[key].toString(),
    ]),
  ) as Record<keyof RemoveFraudulentBlockLayout, string | null>;

export const requireDeploymentScript = (
  deploymentInfo: ContractDeploymentInfo,
  name: DeploymentScriptName,
): Script => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  if (entry.contract === undefined) {
    throw new Error(
      `Deployment info entry "${name}" is missing contract CBOR; regenerate deployment info from the current live deployment.`,
    );
  }
  const script = {
    type: entry.contract.type,
    script: entry.contract.cborHex,
  } as Script;
  requireMatchingScriptHash({
    label: `${name} script`,
    deployed: entry.scriptHash,
    derived: validatorToScriptHash(script),
  });
  return script;
};
