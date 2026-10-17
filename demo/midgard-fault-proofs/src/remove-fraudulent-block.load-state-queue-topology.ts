import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  type LinkedListNodeView,
  SchedulerDatum,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  type StateQueueRemoveReferenceScriptUTxOs,
  type StateQueueUTxO,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type ContractDeploymentInfo } from "./inspect-contracts.js";
import { requireDeploymentReferenceScript } from "./remove-fraudulent-block.assemble-removal-contracts.js";
import {
  type ReferenceScriptName,
  REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES,
  STATE_QUEUE_REMOVE_REFERENCE_SCRIPT_NAMES,
} from "./remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import { type StateQueueTopology } from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import { outRefLabel } from "./runtime.js";

export const resolveReferenceScripts = async ({
  lucid,
  deploymentInfo,
  requireReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly requireReferenceScripts: boolean;
}): Promise<
  StateQueueRemoveReferenceScriptUTxOs & {
    readonly stateQueueFraudRemovalWithdraw: UTxO;
  }
> => {
  const stateQueueFraudRemovalWithdraw = await requireDeploymentReferenceScript(
    {
      lucid,
      deploymentInfo,
      name: "stateQueueFraudRemovalWithdraw",
    },
  );
  if (!requireReferenceScripts) {
    return { stateQueueFraudRemovalWithdraw };
  }
  const [
    correctionLockSpend,
    stateQueueSpend,
    stateQueueMint,
    activeOperatorsSpend,
    activeOperatorsMint,
    retiredOperatorsSpend,
    retiredOperatorsMint,
    schedulerSpend,
  ] = await Promise.all(
    STATE_QUEUE_REMOVE_REFERENCE_SCRIPT_NAMES.map((name) =>
      requireDeploymentReferenceScript({ lucid, deploymentInfo, name }),
    ),
  );
  return {
    stateQueueFraudRemovalWithdraw,
    correctionLockSpend,
    stateQueueSpend,
    stateQueueMint,
    activeOperatorsSpend,
    activeOperatorsMint,
    retiredOperatorsSpend,
    retiredOperatorsMint,
    schedulerSpend,
  };
};

export const referenceScriptOutRefs = (
  referenceScripts:
    | (StateQueueRemoveReferenceScriptUTxOs & {
        readonly stateQueueFraudRemovalWithdraw: UTxO;
      })
    | undefined,
): Readonly<Record<ReferenceScriptName, string | null>> =>
  Object.fromEntries(
    REMOVE_FRAUDULENT_BLOCK_REFERENCE_SCRIPT_NAMES.map((name) => {
      const utxo = referenceScripts?.[name];
      return [name, utxo === undefined ? null : outRefLabel(utxo)] as const;
    }),
  ) as Readonly<Record<ReferenceScriptName, string | null>>;

type SchedulerDatumValue =
  | "NoActiveOperators"
  | {
      readonly ActiveOperator: {
        readonly operator: string;
        readonly start_time: bigint;
      };
    };

export const decodeSchedulerDatum = (
  schedulerUtxo: UTxO,
): SchedulerDatumValue => {
  if (schedulerUtxo.datum == null) {
    throw new Error(
      `Scheduler UTxO ${outRefLabel(schedulerUtxo)} is missing datum.`,
    );
  }
  return Data.from(schedulerUtxo.datum, SchedulerDatum) as SchedulerDatumValue;
};

export const nodeKeyValue = (
  nodeKey: LinkedListNodeView["key"],
): string | null => (nodeKey === "Empty" ? null : nodeKey.Key.key);

export const nextKeyValue = (nodeView: LinkedListNodeView): string | null =>
  nodeView.next === "Empty" ? null : nodeView.next.Key.key;

export const compareHexByteStrings = (left: string, right: string): number =>
  Buffer.from(left, "hex").compare(Buffer.from(right, "hex"));

export const requireStateQueueHeaderHash = async (
  stateQueueNode: StateQueueUTxO,
): Promise<string> => {
  if (stateQueueNode.datum.key === "Empty") {
    throw new Error(
      `State-queue UTxO ${outRefLabel(stateQueueNode.utxo)} is the confirmed-state root, not a block node.`,
    );
  }
  if (
    !stateQueueNode.assetName.startsWith(STATE_QUEUE_NODE_ASSET_NAME_PREFIX)
  ) {
    throw new Error(
      `State-queue block ${outRefLabel(stateQueueNode.utxo)} has unexpected asset name ${stateQueueNode.assetName}.`,
    );
  }
  const assetHeaderHash = stateQueueNode.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  if (stateQueueNode.datum.key.Key.key !== assetHeaderHash) {
    throw new Error(
      `State-queue block ${outRefLabel(stateQueueNode.utxo)} has key ${stateQueueNode.datum.key.Key.key} but asset header hash ${assetHeaderHash}.`,
    );
  }
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(stateQueueNode.datum),
  );
  const computedHeaderHash = await Effect.runPromise(hashBlockHeader(header));
  if (computedHeaderHash !== assetHeaderHash) {
    throw new Error(
      `State-queue block datum hash mismatch for ${outRefLabel(stateQueueNode.utxo)}: asset=${assetHeaderHash}, computed=${computedHeaderHash}.`,
    );
  }
  return assetHeaderHash;
};

const hasStateQueuePolicyAsset = (
  utxo: UTxO,
  stateQueuePolicyId: string,
): boolean =>
  Object.entries(utxo.assets).some(
    ([unit, quantity]) =>
      quantity !== 0n &&
      unit !== "lovelace" &&
      unit.startsWith(stateQueuePolicyId),
  );

export const loadStateQueueTopology = async ({
  lucid,
  stateQueueAddress,
  stateQueuePolicyId,
}: {
  readonly lucid: LucidEvolution;
  readonly stateQueueAddress: string;
  readonly stateQueuePolicyId: string;
}): Promise<StateQueueTopology> => {
  const allUtxos = await lucid.utxosAt(stateQueueAddress);
  const stateQueueUtxos = await Promise.all(
    allUtxos
      .filter((utxo) => hasStateQueuePolicyAsset(utxo, stateQueuePolicyId))
      .map((utxo) =>
        Effect.runPromise(utxoToStateQueueUTxO(utxo, stateQueuePolicyId)),
      ),
  );
  const roots = stateQueueUtxos.filter((entry) => entry.datum.key === "Empty");
  if (roots.length !== 1) {
    throw new Error(
      `Expected exactly one state-queue root UTxO, found ${roots.length.toString()}.`,
    );
  }

  const root = roots[0]!;
  if (root.assetName !== STATE_QUEUE_ROOT_ASSET_NAME) {
    throw new Error(
      `State-queue root ${outRefLabel(root.utxo)} has unexpected asset name ${root.assetName}.`,
    );
  }
  const nodeByHeaderHash = new Map<string, StateQueueUTxO>();
  for (const entry of stateQueueUtxos) {
    if (entry.datum.key === "Empty") {
      continue;
    }
    const headerHash = await requireStateQueueHeaderHash(entry);
    if (nodeByHeaderHash.has(headerHash)) {
      throw new Error(`State queue contains duplicate block ${headerHash}.`);
    }
    nodeByHeaderHash.set(headerHash, entry);
  }

  const ordered: StateQueueUTxO[] = [root];
  const predecessorByHeaderHash = new Map<string, StateQueueUTxO>();
  const successorByHeaderHash = new Map<string, StateQueueUTxO>();
  const visited = new Set<string>();
  let predecessor = root;
  let currentKey = nextKeyValue(root.datum);
  while (currentKey !== null) {
    if (visited.has(currentKey)) {
      throw new Error(`State queue contains a cycle at ${currentKey}.`);
    }
    visited.add(currentKey);
    const current = nodeByHeaderHash.get(currentKey);
    if (current === undefined) {
      throw new Error(`State queue points to missing block ${currentKey}.`);
    }
    ordered.push(current);
    predecessorByHeaderHash.set(currentKey, predecessor);
    const successorKey = nextKeyValue(current.datum);
    if (successorKey !== null) {
      const successor = nodeByHeaderHash.get(successorKey);
      if (successor === undefined) {
        throw new Error(
          `State queue block ${currentKey} points to missing block ${successorKey}.`,
        );
      }
      successorByHeaderHash.set(currentKey, successor);
    }
    predecessor = current;
    currentKey = successorKey;
  }

  if (visited.size !== nodeByHeaderHash.size) {
    const unreachable = [...nodeByHeaderHash.keys()].filter(
      (headerHash) => !visited.has(headerHash),
    );
    throw new Error(
      `State queue contains unreachable block(s): ${unreachable.join(", ")}.`,
    );
  }

  return {
    root,
    ordered,
    nodeByHeaderHash,
    predecessorByHeaderHash,
    successorByHeaderHash,
  };
};

export const activeOperatorUnit = (
  policyId: string,
  operator: string,
): string =>
  toUnit(policyId, ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operator);

const isOperatorListUnit = ({
  unit,
  policyId,
  rootAssetName,
  nodeAssetNamePrefix,
}: {
  readonly unit: string;
  readonly policyId: string;
  readonly rootAssetName: string;
  readonly nodeAssetNamePrefix: string;
}): boolean =>
  unit.startsWith(policyId) &&
  (unit.slice(policyId.length) === rootAssetName ||
    unit.slice(policyId.length).startsWith(nodeAssetNamePrefix));

export const hasOperatorListToken = ({
  utxo,
  policyId,
  rootAssetName,
  nodeAssetNamePrefix,
}: {
  readonly utxo: UTxO;
  readonly policyId: string;
  readonly rootAssetName: string;
  readonly nodeAssetNamePrefix: string;
}): boolean =>
  Object.entries(utxo.assets).some(
    ([unit, quantity]) =>
      quantity === 1n &&
      isOperatorListUnit({
        unit,
        policyId,
        rootAssetName,
        nodeAssetNamePrefix,
      }),
  );
