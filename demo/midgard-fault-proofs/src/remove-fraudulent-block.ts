import {
  CML,
  Data,
  coreToTxOutput,
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
  validatorToAddress,
  validatorToScriptHash,
  type LucidEvolution,
  type Network,
  type Script,
  type SpendingValidator,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  FraudProofTokenDatum,
  HUB_ORACLE_ASSET_NAME,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  SCHEDULER_ASSET_NAME,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  SchedulerDatum,
  buildDoubleSpendFaultProofContracts,
  encodeLinkedListNodeView,
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  getRedeemerPointersInContextOrder,
  hashBlockHeader,
  incompleteRemoveLastFraudulentBlockHeaderTxProgram,
  parseFaultProofBlueprint,
  resolveMintPolicyRedeemerTxInfoIndex,
  resolveMintPolicyTxInfoRedeemerIndexFromPolicySet,
  resolveRedeemerTxInfoIndex,
  utxoToStateQueueUTxO,
  type ActiveOperatorMintRedeemer as ActiveOperatorMintRedeemerData,
  type LinkedListNodeView,
  type SchedulerSpendRedeemer as SchedulerSpendRedeemerData,
  type StateQueueRemoveReferenceScriptUTxOs,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import {
  parseContractDeploymentInfo,
  type ContractDeploymentInfo,
} from "./inspect-contracts.js";
import {
  compareOutRefs,
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parsedOutRefFromUtxo,
  readJsonFile,
  requireDeploymentScriptHash,
  requireSingletonUtxo,
  resolveProverSigner,
  type ParsedOutRef,
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { requireMatchingScriptHash, selectFeeInput } from "./submit-step-01.js";
import { parseHex } from "./json-file.js";

const STATE_QUEUE_ANCHOR_OUTPUT_INDEX = 0n;
const ACTIVE_OPERATORS_ANCHOR_OUTPUT_INDEX = 1n;
const SCHEDULER_OUTPUT_INDEX = 2n;
const DEFAULT_REMOVE_VALIDITY_WINDOW_MS = 300_000n;
const DEFAULT_REMOVE_VALIDITY_BACKDATE_MS = 120_000n;

type ReferenceScriptName =
  | "stateQueueSpend"
  | "stateQueueMint"
  | "activeOperatorsSpend"
  | "activeOperatorsMint"
  | "schedulerSpend";

type DeploymentScriptName = ReferenceScriptName | "registeredOperatorsSpend";

type RemoveFraudulentBlockContracts = {
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly stateQueueSpendingScript: Script;
  readonly stateQueueMintingScript: Script;
  readonly activeOperatorsPolicyId: string;
  readonly activeOperatorsAddress: string;
  readonly activeOperatorsSpendingScript: Script;
  readonly activeOperatorsMintingScript: Script;
  readonly schedulerPolicyId: string;
  readonly schedulerAddress: string;
  readonly schedulerSpendingScript: Script;
  readonly hubOraclePolicyId: string;
  readonly registeredOperatorsPolicyId: string;
  readonly registeredOperatorsAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly doubleSpendCategoryId: string;
};

type RemoveFraudulentBlockLayout = {
  readonly anchorElementInputIndex: bigint;
  readonly fraudulentNodeInputIndex: bigint;
  readonly fraudProofRefInputIndex: bigint;
  readonly activeOperatorsRedeemerTxInfoIndex: bigint;
  readonly stateQueueRedeemerTxInfoIndex: bigint;
  readonly activeOperatorAnchorInputIndex: bigint;
  readonly activeOperatorNodeInputIndex: bigint;
  readonly activeOperatorAnchorOutputIndex: bigint;
  readonly schedulerRefInputIndex?: bigint;
  readonly schedulerInputIndex?: bigint;
  readonly schedulerOutputIndex?: bigint;
  readonly schedulerRedeemerTxInfoIndex?: bigint;
  readonly activeOperatorsLastNodeRefInputIndex?: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly registeredOperatorsRootRefInputIndex: bigint;
  readonly stateQueueAnchorOutputIndex: bigint;
};

const REMOVE_LAYOUT_KEYS = [
  "anchorElementInputIndex",
  "fraudulentNodeInputIndex",
  "fraudProofRefInputIndex",
  "activeOperatorsRedeemerTxInfoIndex",
  "stateQueueRedeemerTxInfoIndex",
  "activeOperatorAnchorInputIndex",
  "activeOperatorNodeInputIndex",
  "activeOperatorAnchorOutputIndex",
  "schedulerRefInputIndex",
  "schedulerInputIndex",
  "schedulerOutputIndex",
  "schedulerRedeemerTxInfoIndex",
  "activeOperatorsLastNodeRefInputIndex",
  "hubOracleRefInputIndex",
  "registeredOperatorsRootRefInputIndex",
  "stateQueueAnchorOutputIndex",
] as const satisfies readonly (keyof RemoveFraudulentBlockLayout)[];

type ActiveOperatorListEntry = {
  readonly utxo: UTxO;
  readonly view: LinkedListNodeView;
};

type ActiveOperatorRemovalPlan = {
  readonly root: ActiveOperatorListEntry;
  readonly anchor: ActiveOperatorListEntry;
  readonly node: ActiveOperatorListEntry;
  readonly lastNodeAfterRemoval?: ActiveOperatorListEntry;
};

type SchedulerRemovalPlan =
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

export type RemoveFraudulentBlockCliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly fraudulentHeaderHash: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitRemoveFraudulentBlockResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly fraudulentHeaderHash: string;
  readonly stateQueueBlockOutRef: string;
  readonly stateQueueRootOutRef: string;
  readonly fraudProofOutRef: string;
  readonly activeOperatorsRootOutRef: string;
  readonly activeOperatorNodeOutRef: string;
  readonly schedulerOutRef: string;
  readonly hubOracleOutRef: string;
  readonly registeredOperatorsRootOutRef: string;
  readonly referenceScriptOutRefs: Readonly<
    Record<ReferenceScriptName, string | null>
  >;
  readonly layout: Record<keyof RemoveFraudulentBlockLayout, string | null>;
  readonly awaitedConfirmation: boolean;
};

const sortUtxosByLedgerOrder = (utxos: readonly UTxO[]): readonly UTxO[] =>
  [...utxos].sort((left, right) =>
    compareOutRefs(parsedOutRefFromUtxo(left), parsedOutRefFromUtxo(right)),
  );

const dedupeUtxos = (utxos: readonly UTxO[]): readonly UTxO[] => [
  ...new Map(utxos.map((utxo) => [outRefLabel(utxo), utxo])).values(),
];

const orderedIndex = (
  utxos: readonly UTxO[],
  target: UTxO,
  label: string,
): bigint => {
  const sorted = sortUtxosByLedgerOrder(utxos);
  const targetLabel = outRefLabel(target);
  const index = sorted.findIndex((utxo) => outRefLabel(utxo) === targetLabel);
  if (index < 0) {
    throw new Error(`Missing ${label} ${targetLabel} in transaction layout.`);
  }
  return BigInt(index);
};

const cmlInputs = (
  tx: CML.Transaction,
  kind: "inputs" | "referenceInputs",
): readonly ParsedOutRef[] => {
  const inputs =
    kind === "inputs" ? tx.body().inputs() : tx.body().reference_inputs();
  if (inputs === undefined) {
    return [];
  }
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort(compareOutRefs);
};

const findCmlInputIndex = (
  tx: CML.Transaction,
  target: UTxO,
  label: string,
): bigint => {
  const targetRef = parsedOutRefFromUtxo(target);
  const index = cmlInputs(tx, "inputs").findIndex(
    (candidate) =>
      candidate.txHash === targetRef.txHash &&
      candidate.outputIndex === targetRef.outputIndex,
  );
  if (index < 0) {
    throw new Error(
      `Balanced transaction does not spend ${label} ${outRefLabel(target)}.`,
    );
  }
  return BigInt(index);
};

const findCmlReferenceInputIndex = (
  tx: CML.Transaction,
  target: UTxO,
  label: string,
): bigint => {
  const targetRef = parsedOutRefFromUtxo(target);
  const index = cmlInputs(tx, "referenceInputs").findIndex(
    (candidate) =>
      candidate.txHash === targetRef.txHash &&
      candidate.outputIndex === targetRef.outputIndex,
  );
  if (index < 0) {
    throw new Error(
      `Balanced transaction does not reference ${label} ${outRefLabel(target)}.`,
    );
  }
  return BigInt(index);
};

const findOutputIndexByUnit = ({
  tx,
  address,
  unit,
  label,
}: {
  readonly tx: CML.Transaction;
  readonly address: string;
  readonly unit: string;
  readonly label: string;
}): bigint => {
  const outputs = tx.body().outputs();
  const matches: number[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (output.address === address && (output.assets[unit] ?? 0n) === 1n) {
      matches.push(index);
    }
  }
  if (matches.length !== 1) {
    throw new Error(
      `Balanced transaction must contain exactly one ${label} output for ${unit}; found ${matches.length.toString()}.`,
    );
  }
  return BigInt(matches[0]!);
};

const resolveSpendRedeemerTxInfoIndex = ({
  tx,
  scriptInputs,
  target,
  label,
}: {
  readonly tx: CML.Transaction;
  readonly scriptInputs: readonly UTxO[];
  readonly target: UTxO;
  readonly label: string;
}): bigint => {
  const pointers = getRedeemerPointersInContextOrder(tx);
  const fullInputIndex = findCmlInputIndex(tx, target, label);
  try {
    return resolveRedeemerTxInfoIndex({
      pointers,
      target: { tag: CML.RedeemerTag.Spend, index: fullInputIndex },
      label,
    });
  } catch {
    return resolveRedeemerTxInfoIndex({
      pointers,
      target: {
        tag: CML.RedeemerTag.Spend,
        index: orderedIndex(scriptInputs, target, label),
      },
      label,
    });
  }
};

const layoutToJson = (
  layout: RemoveFraudulentBlockLayout,
): Record<keyof RemoveFraudulentBlockLayout, string | null> =>
  Object.fromEntries(
    REMOVE_LAYOUT_KEYS.map((key) => [
      key,
      layout[key] === undefined ? null : layout[key].toString(),
    ]),
  ) as Record<keyof RemoveFraudulentBlockLayout, string | null>;

const sameLayout = (
  left: RemoveFraudulentBlockLayout,
  right: RemoveFraudulentBlockLayout,
): boolean =>
  [...new Set([...Object.keys(left), ...Object.keys(right)])].every(
    (key) =>
      left[key as keyof RemoveFraudulentBlockLayout] ===
      right[key as keyof RemoveFraudulentBlockLayout],
  );

const requireDeploymentScript = (
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

const buildRemovalContracts = async ({
  blueprint,
  deploymentInfo,
  network,
}: {
  readonly blueprint: unknown;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly network: Network;
}): Promise<RemoveFraudulentBlockContracts> => {
  const hubOraclePolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "hubOracleMint",
  );
  const fraudProofCataloguePolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "fraudProofCatalogueMint",
  );
  const doubleSpendContracts = await Effect.runPromise(
    buildDoubleSpendFaultProofContracts({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId,
      fraudProofCataloguePolicyId,
    }),
  );
  requireMatchingScriptHash({
    label: "fraudProofMint policy",
    deployed: requireDeploymentScriptHash(deploymentInfo, "fraudProofMint"),
    derived: doubleSpendContracts.fraudProof.policyId,
  });
  requireMatchingScriptHash({
    label: "fraudProofSpend script",
    deployed: requireDeploymentScriptHash(deploymentInfo, "fraudProofSpend"),
    derived: doubleSpendContracts.fraudProof.spendingScriptHash,
  });
  requireMatchingScriptHash({
    label: "fraudProofDoubleSpend step-01 script",
    deployed: requireDeploymentScriptHash(
      deploymentInfo,
      "fraudProofDoubleSpend",
    ),
    derived: doubleSpendContracts.doubleSpend.firstStep.spendingScriptHash,
  });

  const stateQueueSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueSpend",
  );
  const stateQueueMintingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueMint",
  );
  const activeOperatorsSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "activeOperatorsSpend",
  );
  const activeOperatorsMintingScript = requireDeploymentScript(
    deploymentInfo,
    "activeOperatorsMint",
  );
  const schedulerSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "schedulerSpend",
  );
  const activeOperatorsPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "activeOperatorsMint",
  );
  const schedulerPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "schedulerMint",
  );
  const registeredOperatorsPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "registeredOperatorsMint",
  );

  return {
    stateQueuePolicyId: requireDeploymentScriptHash(
      deploymentInfo,
      "stateQueueMint",
    ),
    stateQueueAddress: validatorToAddress(
      network,
      stateQueueSpendingScript as SpendingValidator,
    ),
    stateQueueSpendingScript,
    stateQueueMintingScript,
    activeOperatorsPolicyId,
    activeOperatorsAddress: validatorToAddress(
      network,
      activeOperatorsSpendingScript as SpendingValidator,
    ),
    activeOperatorsSpendingScript,
    activeOperatorsMintingScript,
    schedulerPolicyId,
    schedulerAddress: validatorToAddress(
      network,
      schedulerSpendingScript as SpendingValidator,
    ),
    schedulerSpendingScript,
    hubOraclePolicyId,
    registeredOperatorsPolicyId,
    registeredOperatorsAddress: validatorToAddress(
      network,
      requireDeploymentScript(
        deploymentInfo,
        "registeredOperatorsSpend",
      ) as SpendingValidator,
    ),
    fraudProofPolicyId: doubleSpendContracts.fraudProof.policyId,
    fraudProofAddress: doubleSpendContracts.fraudProof.spendingScriptAddress,
    doubleSpendCategoryId:
      deploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue?.categories
        .doubleSpend.categoryId ??
      (() => {
        throw new Error(
          "Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.categories.doubleSpend.",
        );
      })(),
  };
};

const requireDeploymentReferenceScript = async ({
  lucid,
  deploymentInfo,
  name,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly name: ReferenceScriptName;
}): Promise<UTxO> => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${name}" is missing refScriptUTxO; publish reference scripts and regenerate deployment info before live removal.`,
    );
  }
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef: entry.refScriptUTxO,
    label: `${name} reference-script UTxO`,
  });
  if (utxo.scriptRef == null) {
    throw new Error(
      `${name} reference-script UTxO ${outRefLabel(utxo)} does not carry a reference script.`,
    );
  }
  const scriptRef = utxo.scriptRef;
  requireMatchingScriptHash({
    label: `${name} reference script`,
    deployed: entry.scriptHash,
    derived: validatorToScriptHash(scriptRef),
  });
  return utxo;
};

const resolveReferenceScripts = async ({
  lucid,
  deploymentInfo,
  requireReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly requireReferenceScripts: boolean;
}): Promise<StateQueueRemoveReferenceScriptUTxOs | undefined> => {
  if (!requireReferenceScripts) {
    return undefined;
  }
  const [
    stateQueueSpend,
    stateQueueMint,
    activeOperatorsSpend,
    activeOperatorsMint,
    schedulerSpend,
  ] = await Promise.all(
    (
      [
        "stateQueueSpend",
        "stateQueueMint",
        "activeOperatorsSpend",
        "activeOperatorsMint",
        "schedulerSpend",
      ] as const
    ).map((name) =>
      requireDeploymentReferenceScript({ lucid, deploymentInfo, name }),
    ),
  );
  return {
    stateQueueSpend,
    stateQueueMint,
    activeOperatorsSpend,
    activeOperatorsMint,
    schedulerSpend,
  };
};

const referenceScriptOutRefs = (
  referenceScripts: StateQueueRemoveReferenceScriptUTxOs | undefined,
): Readonly<Record<ReferenceScriptName, string | null>> => ({
  stateQueueSpend:
    referenceScripts?.stateQueueSpend === undefined
      ? null
      : outRefLabel(referenceScripts.stateQueueSpend),
  stateQueueMint:
    referenceScripts?.stateQueueMint === undefined
      ? null
      : outRefLabel(referenceScripts.stateQueueMint),
  activeOperatorsSpend:
    referenceScripts?.activeOperatorsSpend === undefined
      ? null
      : outRefLabel(referenceScripts.activeOperatorsSpend),
  activeOperatorsMint:
    referenceScripts?.activeOperatorsMint === undefined
      ? null
      : outRefLabel(referenceScripts.activeOperatorsMint),
  schedulerSpend:
    referenceScripts?.schedulerSpend === undefined
      ? null
      : outRefLabel(referenceScripts.schedulerSpend),
});

type SchedulerDatumValue =
  | "NoActiveOperators"
  | {
      readonly ActiveOperator: {
        readonly operator: string;
        readonly start_time: bigint;
      };
    };

const decodeSchedulerDatum = (schedulerUtxo: UTxO): SchedulerDatumValue => {
  if (schedulerUtxo.datum == null) {
    throw new Error(
      `Scheduler UTxO ${outRefLabel(schedulerUtxo)} is missing datum.`,
    );
  }
  return Data.from(schedulerUtxo.datum, SchedulerDatum) as SchedulerDatumValue;
};

const nodeKeyValue = (nodeKey: LinkedListNodeView["key"]): string | null =>
  nodeKey === "Empty" ? null : nodeKey.Key.key;

const nextKeyValue = (nodeView: LinkedListNodeView): string | null =>
  nodeView.next === "Empty" ? null : nodeView.next.Key.key;

const activeOperatorUnit = (policyId: string, operator: string): string =>
  toUnit(policyId, ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operator);

const isActiveOperatorListUnit = (
  unit: string,
  activeOperatorsPolicyId: string,
): boolean =>
  unit.startsWith(activeOperatorsPolicyId) &&
  (unit.slice(activeOperatorsPolicyId.length) ===
    ACTIVE_OPERATORS_ROOT_ASSET_NAME ||
    unit
      .slice(activeOperatorsPolicyId.length)
      .startsWith(ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX));

const hasActiveOperatorListToken = (
  utxo: UTxO,
  activeOperatorsPolicyId: string,
): boolean =>
  Object.entries(utxo.assets).some(
    ([unit, quantity]) =>
      quantity === 1n &&
      isActiveOperatorListUnit(unit, activeOperatorsPolicyId),
  );

const loadActiveOperatorList = async ({
  lucid,
  activeOperatorsAddress,
  activeOperatorsPolicyId,
}: {
  readonly lucid: LucidEvolution;
  readonly activeOperatorsAddress: string;
  readonly activeOperatorsPolicyId: string;
}): Promise<readonly ActiveOperatorListEntry[]> => {
  const utxos = await lucid.utxosAt(activeOperatorsAddress);
  return await Promise.all(
    utxos
      .filter((utxo) =>
        hasActiveOperatorListToken(utxo, activeOperatorsPolicyId),
      )
      .map(async (utxo) => ({
        utxo,
        view: await Effect.runPromise(getLinkedListNodeViewFromUTxO(utxo)),
      })),
  );
};

const resolveActiveOperatorRemovalPlan = ({
  entries,
  operator,
}: {
  readonly entries: readonly ActiveOperatorListEntry[];
  readonly operator: string;
}): ActiveOperatorRemovalPlan => {
  const roots = entries.filter((entry) => entry.view.key === "Empty");
  if (roots.length !== 1) {
    throw new Error(
      `Expected exactly one active-operators root UTxO, found ${roots.length.toString()}.`,
    );
  }
  const root = roots[0]!;
  const nodesByKey = new Map<string, ActiveOperatorListEntry>();
  for (const entry of entries) {
    const key = nodeKeyValue(entry.view.key);
    if (key === null) {
      continue;
    }
    if (nodesByKey.has(key)) {
      throw new Error(`Active-operators list contains duplicate key ${key}.`);
    }
    nodesByKey.set(key, entry);
  }

  let anchor: ActiveOperatorListEntry = root;
  let currentKey = nextKeyValue(root.view);
  let removed: ActiveOperatorListEntry | undefined;
  let lastNode: ActiveOperatorListEntry | undefined;
  const visited = new Set<string>();

  while (currentKey !== null) {
    if (visited.has(currentKey)) {
      throw new Error(
        `Active-operators list contains a cycle at key ${currentKey}.`,
      );
    }
    visited.add(currentKey);
    const current = nodesByKey.get(currentKey);
    if (current === undefined) {
      throw new Error(
        `Active-operators list points to missing node ${currentKey}.`,
      );
    }
    if (currentKey === operator) {
      removed = current;
    }
    const nextKey = nextKeyValue(current.view);
    if (nextKey === null) {
      lastNode = current;
    }
    if (removed === undefined) {
      anchor = current;
    }
    currentKey = nextKey;
  }

  if (visited.size !== nodesByKey.size) {
    const unreachable = [...nodesByKey.keys()].filter(
      (key) => !visited.has(key),
    );
    throw new Error(
      `Active-operators list contains unreachable node(s): ${unreachable.join(", ")}.`,
    );
  }
  if (removed === undefined) {
    throw new Error(
      `Fraudulent operator ${operator} is not present in the active-operators list.`,
    );
  }

  const lastNodeAfterRemoval =
    lastNode === undefined || nodeKeyValue(lastNode.view.key) === operator
      ? undefined
      : lastNode;

  return { root, anchor, node: removed, lastNodeAfterRemoval };
};

const resolveSchedulerRemovalPlan = ({
  schedulerUtxo,
  operator,
  removalPlan,
}: {
  readonly schedulerUtxo: UTxO;
  readonly operator: string;
  readonly removalPlan: ActiveOperatorRemovalPlan;
}): SchedulerRemovalPlan => {
  const schedulerDatum = decodeSchedulerDatum(schedulerUtxo);
  if (
    schedulerDatum === "NoActiveOperators" ||
    schedulerDatum.ActiveOperator.operator !== operator
  ) {
    return { kind: "inactive" };
  }
  const anchorKey = nodeKeyValue(removalPlan.anchor.view.key);
  const removedNodeIsLast = nextKeyValue(removalPlan.node.view) === null;
  if (anchorKey !== null) {
    return { kind: "goToAnchor", newOperator: anchorKey, removedNodeIsLast };
  }
  return {
    kind: "rewind",
    newOperator:
      removalPlan.lastNodeAfterRemoval === undefined
        ? undefined
        : (nodeKeyValue(removalPlan.lastNodeAfterRemoval.view.key) ??
          undefined),
    removedNodeIsLast,
  };
};

const buildKnownLayout = ({
  feeInput,
  stateQueueRoot,
  stateQueueBlock,
  fraudProof,
  activeOperatorAnchor,
  activeOperatorNode,
  scheduler,
  schedulerPlan,
  hubOracle,
  registeredOperatorsRoot,
  activeOperatorsLastNode,
  referenceScripts,
  stateQueuePolicyId,
  activeOperatorsPolicyId,
}: {
  readonly feeInput: UTxO;
  readonly stateQueueRoot: UTxO;
  readonly stateQueueBlock: UTxO;
  readonly fraudProof: UTxO;
  readonly activeOperatorAnchor: UTxO;
  readonly activeOperatorNode: UTxO;
  readonly scheduler: UTxO;
  readonly schedulerPlan: SchedulerRemovalPlan;
  readonly hubOracle: UTxO;
  readonly registeredOperatorsRoot: UTxO;
  readonly activeOperatorsLastNode?: UTxO;
  readonly referenceScripts: StateQueueRemoveReferenceScriptUTxOs | undefined;
  readonly stateQueuePolicyId: string;
  readonly activeOperatorsPolicyId: string;
}): RemoveFraudulentBlockLayout => {
  const scriptInputs = [
    stateQueueRoot,
    stateQueueBlock,
    activeOperatorNode,
    activeOperatorAnchor,
    ...(schedulerPlan.kind === "inactive" ? [] : [scheduler]),
  ];
  const allInputs = [feeInput, ...scriptInputs];
  const referenceInputs = sortUtxosByLedgerOrder(
    dedupeUtxos([
      fraudProof,
      hubOracle,
      registeredOperatorsRoot,
      ...(schedulerPlan.kind === "inactive" ? [scheduler] : []),
      ...(activeOperatorsLastNode === undefined
        ? []
        : [activeOperatorsLastNode]),
      ...Object.values(referenceScripts ?? {}).filter(
        (utxo): utxo is UTxO => utxo !== undefined,
      ),
    ]),
  );
  const mintPolicies = [stateQueuePolicyId, activeOperatorsPolicyId];
  const stateQueueRedeemerTxInfoIndex =
    resolveMintPolicyTxInfoRedeemerIndexFromPolicySet({
      policyIds: mintPolicies,
      targetPolicyId: stateQueuePolicyId,
      precedingSpendRedeemerCount: scriptInputs.length,
    });
  const activeOperatorsRedeemerTxInfoIndex =
    resolveMintPolicyTxInfoRedeemerIndexFromPolicySet({
      policyIds: mintPolicies,
      targetPolicyId: activeOperatorsPolicyId,
      precedingSpendRedeemerCount: scriptInputs.length,
    });

  return {
    anchorElementInputIndex: orderedIndex(
      allInputs,
      stateQueueRoot,
      "state-queue root input",
    ),
    fraudulentNodeInputIndex: orderedIndex(
      allInputs,
      stateQueueBlock,
      "fraudulent state-queue block input",
    ),
    fraudProofRefInputIndex: orderedIndex(
      referenceInputs,
      fraudProof,
      "fraud-proof reference input",
    ),
    activeOperatorsRedeemerTxInfoIndex,
    stateQueueRedeemerTxInfoIndex,
    activeOperatorAnchorInputIndex: orderedIndex(
      allInputs,
      activeOperatorAnchor,
      "active-operator anchor input",
    ),
    activeOperatorNodeInputIndex: orderedIndex(
      allInputs,
      activeOperatorNode,
      "active-operator node input",
    ),
    activeOperatorAnchorOutputIndex: ACTIVE_OPERATORS_ANCHOR_OUTPUT_INDEX,
    ...(schedulerPlan.kind === "inactive"
      ? {
          schedulerRefInputIndex: orderedIndex(
            referenceInputs,
            scheduler,
            "scheduler reference input",
          ),
        }
      : {
          schedulerInputIndex: orderedIndex(
            allInputs,
            scheduler,
            "scheduler input",
          ),
          schedulerOutputIndex: SCHEDULER_OUTPUT_INDEX,
          schedulerRedeemerTxInfoIndex: orderedIndex(
            scriptInputs,
            scheduler,
            "scheduler spend redeemer",
          ),
        }),
    ...(activeOperatorsLastNode === undefined
      ? {}
      : {
          activeOperatorsLastNodeRefInputIndex: orderedIndex(
            referenceInputs,
            activeOperatorsLastNode,
            "active-operators last-node reference input",
          ),
        }),
    hubOracleRefInputIndex: orderedIndex(
      referenceInputs,
      hubOracle,
      "hub-oracle reference input",
    ),
    registeredOperatorsRootRefInputIndex: orderedIndex(
      referenceInputs,
      registeredOperatorsRoot,
      "registered-operators root reference input",
    ),
    stateQueueAnchorOutputIndex: STATE_QUEUE_ANCHOR_OUTPUT_INDEX,
  };
};

const deriveLayoutFromTx = ({
  tx,
  stateQueueRoot,
  stateQueueBlock,
  fraudProof,
  activeOperatorAnchor,
  activeOperatorNode,
  scheduler,
  schedulerPlan,
  hubOracle,
  registeredOperatorsRoot,
  activeOperatorsLastNode,
  stateQueueRootUnit,
  activeOperatorAnchorUnit,
  schedulerUnit,
  contracts,
}: {
  readonly tx: CML.Transaction;
  readonly stateQueueRoot: UTxO;
  readonly stateQueueBlock: UTxO;
  readonly fraudProof: UTxO;
  readonly activeOperatorAnchor: UTxO;
  readonly activeOperatorNode: UTxO;
  readonly scheduler: UTxO;
  readonly schedulerPlan: SchedulerRemovalPlan;
  readonly hubOracle: UTxO;
  readonly registeredOperatorsRoot: UTxO;
  readonly activeOperatorsLastNode?: UTxO;
  readonly stateQueueRootUnit: string;
  readonly activeOperatorAnchorUnit: string;
  readonly schedulerUnit: string;
  readonly contracts: RemoveFraudulentBlockContracts;
}): RemoveFraudulentBlockLayout => {
  const scriptInputs = [
    stateQueueRoot,
    stateQueueBlock,
    activeOperatorNode,
    activeOperatorAnchor,
    ...(schedulerPlan.kind === "inactive" ? [] : [scheduler]),
  ];
  const base = {
    anchorElementInputIndex: findCmlInputIndex(
      tx,
      stateQueueRoot,
      "state-queue root input",
    ),
    fraudulentNodeInputIndex: findCmlInputIndex(
      tx,
      stateQueueBlock,
      "fraudulent state-queue block input",
    ),
    fraudProofRefInputIndex: findCmlReferenceInputIndex(
      tx,
      fraudProof,
      "fraud-proof reference input",
    ),
    activeOperatorsRedeemerTxInfoIndex: resolveMintPolicyRedeemerTxInfoIndex({
      tx,
      policyIds: [
        contracts.stateQueuePolicyId,
        contracts.activeOperatorsPolicyId,
      ],
      targetPolicyId: contracts.activeOperatorsPolicyId,
    }),
    stateQueueRedeemerTxInfoIndex: resolveMintPolicyRedeemerTxInfoIndex({
      tx,
      policyIds: [
        contracts.stateQueuePolicyId,
        contracts.activeOperatorsPolicyId,
      ],
      targetPolicyId: contracts.stateQueuePolicyId,
    }),
    activeOperatorAnchorInputIndex: findCmlInputIndex(
      tx,
      activeOperatorAnchor,
      "active-operator anchor input",
    ),
    activeOperatorNodeInputIndex: findCmlInputIndex(
      tx,
      activeOperatorNode,
      "active-operator node input",
    ),
    activeOperatorAnchorOutputIndex: findOutputIndexByUnit({
      tx,
      address: contracts.activeOperatorsAddress,
      unit: activeOperatorAnchorUnit,
      label: "active-operator anchor continuation",
    }),
    hubOracleRefInputIndex: findCmlReferenceInputIndex(
      tx,
      hubOracle,
      "hub-oracle reference input",
    ),
    registeredOperatorsRootRefInputIndex: findCmlReferenceInputIndex(
      tx,
      registeredOperatorsRoot,
      "registered-operators root reference input",
    ),
    stateQueueAnchorOutputIndex: findOutputIndexByUnit({
      tx,
      address: contracts.stateQueueAddress,
      unit: stateQueueRootUnit,
      label: "state-queue root continuation",
    }),
  };
  return {
    ...base,
    ...(schedulerPlan.kind === "inactive"
      ? {
          schedulerRefInputIndex: findCmlReferenceInputIndex(
            tx,
            scheduler,
            "scheduler reference input",
          ),
        }
      : {
          schedulerInputIndex: findCmlInputIndex(
            tx,
            scheduler,
            "scheduler input",
          ),
          schedulerOutputIndex: findOutputIndexByUnit({
            tx,
            address: contracts.schedulerAddress,
            unit: schedulerUnit,
            label: "scheduler continuation",
          }),
          schedulerRedeemerTxInfoIndex: resolveSpendRedeemerTxInfoIndex({
            tx,
            scriptInputs,
            target: scheduler,
            label: "scheduler spend redeemer",
          }),
        }),
    ...(activeOperatorsLastNode === undefined
      ? {}
      : {
          activeOperatorsLastNodeRefInputIndex: findCmlReferenceInputIndex(
            tx,
            activeOperatorsLastNode,
            "active-operators last-node reference input",
          ),
        }),
  };
};

export const submitRemoveFraudulentBlock = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  fraudulentHeaderHash,
  awaitConfirmation = true,
  requireReferenceScripts = true,
  validFrom,
  validTo,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly fraudulentHeaderHash: string;
  readonly awaitConfirmation?: boolean;
  readonly requireReferenceScripts?: boolean;
  readonly validFrom?: bigint;
  readonly validTo?: bigint;
}): Promise<SubmitRemoveFraudulentBlockResult> => {
  const headerHash = parseHex(
    fraudulentHeaderHash,
    "--fraudulent-header-hash",
    28,
  );
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const contracts = await buildRemovalContracts({
    blueprint,
    deploymentInfo: parsedDeploymentInfo,
    network,
  });
  if (
    contracts.doubleSpendCategoryId.length !==
    FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT * 2
  ) {
    throw new Error("Double-spend fraud-proof category id has invalid length.");
  }

  const referenceScripts = await resolveReferenceScripts({
    lucid,
    deploymentInfo: parsedDeploymentInfo,
    requireReferenceScripts,
  });
  signer.selectWallet(lucid);

  const stateQueueBlockUnit = toUnit(
    contracts.stateQueuePolicyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  );
  const stateQueueRootUnit = toUnit(
    contracts.stateQueuePolicyId,
    STATE_QUEUE_ROOT_ASSET_NAME,
  );
  const fraudProofAssetName = contracts.doubleSpendCategoryId + headerHash;
  const fraudProofUnit = toUnit(
    contracts.fraudProofPolicyId,
    fraudProofAssetName,
  );
  const activeOperatorsRootUnit = toUnit(
    contracts.activeOperatorsPolicyId,
    ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  );
  const schedulerUnit = toUnit(
    contracts.schedulerPolicyId,
    SCHEDULER_ASSET_NAME,
  );
  const hubOracleUnit = toUnit(
    contracts.hubOraclePolicyId,
    HUB_ORACLE_ASSET_NAME,
  );
  const registeredOperatorsRootUnit = toUnit(
    contracts.registeredOperatorsPolicyId,
    REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  );

  const [
    stateQueueBlockUtxo,
    stateQueueRootUtxo,
    fraudProofUtxo,
    activeOperatorsRootUtxo,
    schedulerUtxo,
    hubOracleUtxo,
    registeredOperatorsRootUtxo,
  ] = await Promise.all([
    requireSingletonUtxo({
      lucid,
      address: contracts.stateQueueAddress,
      unit: stateQueueBlockUnit,
      label: "fraudulent state-queue block",
    }),
    requireSingletonUtxo({
      lucid,
      address: contracts.stateQueueAddress,
      unit: stateQueueRootUnit,
      label: "state-queue root",
    }),
    requireSingletonUtxo({
      lucid,
      address: contracts.fraudProofAddress,
      unit: fraudProofUnit,
      label: "fraud-proof token",
    }),
    requireSingletonUtxo({
      lucid,
      address: contracts.activeOperatorsAddress,
      unit: activeOperatorsRootUnit,
      label: "active-operators root",
    }),
    requireSingletonUtxo({
      lucid,
      address: contracts.schedulerAddress,
      unit: schedulerUnit,
      label: "scheduler",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: hubOracleUnit,
      label: "hub oracle",
    }),
    requireSingletonUtxo({
      lucid,
      address: contracts.registeredOperatorsAddress,
      unit: registeredOperatorsRootUnit,
      label: "registered-operators root",
    }),
  ]);

  const stateQueueBlock = await Effect.runPromise(
    utxoToStateQueueUTxO(stateQueueBlockUtxo, contracts.stateQueuePolicyId),
  );
  const stateQueueRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(stateQueueRootUtxo, contracts.stateQueuePolicyId),
  );
  const blockHeader = await Effect.runPromise(
    getHeaderFromStateQueueDatum(stateQueueBlock.datum),
  );
  const computedHeaderHash = await Effect.runPromise(
    hashBlockHeader(blockHeader),
  );
  if (computedHeaderHash !== headerHash) {
    throw new Error(
      `State-queue block datum hash mismatch: requested=${headerHash}, computed=${computedHeaderHash}.`,
    );
  }
  const fraudulentOperator = blockHeader.operatorVkey;
  const activeOperatorNodeUnit = toUnit(
    contracts.activeOperatorsPolicyId,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + fraudulentOperator,
  );
  const activeOperatorNodeUtxo = await requireSingletonUtxo({
    lucid,
    address: contracts.activeOperatorsAddress,
    unit: activeOperatorNodeUnit,
    label: "active-operator node",
  });
  const activeOperatorRemoval = resolveActiveOperatorRemovalPlan({
    entries: await loadActiveOperatorList({
      lucid,
      activeOperatorsAddress: contracts.activeOperatorsAddress,
      activeOperatorsPolicyId: contracts.activeOperatorsPolicyId,
    }),
    operator: fraudulentOperator,
  });
  if (
    outRefLabel(activeOperatorRemoval.root.utxo) !==
    outRefLabel(activeOperatorsRootUtxo)
  ) {
    throw new Error(
      "Resolved active-operators root does not match root NFT lookup.",
    );
  }
  if (
    outRefLabel(activeOperatorRemoval.node.utxo) !==
    outRefLabel(activeOperatorNodeUtxo)
  ) {
    throw new Error(
      "Resolved active-operator node does not match node NFT lookup.",
    );
  }
  const schedulerPlan = resolveSchedulerRemovalPlan({
    schedulerUtxo,
    operator: fraudulentOperator,
    removalPlan: activeOperatorRemoval,
  });
  const activeOperatorAnchorKey = nodeKeyValue(
    activeOperatorRemoval.anchor.view.key,
  );
  const activeOperatorAnchorUnit =
    activeOperatorAnchorKey === null
      ? activeOperatorsRootUnit
      : activeOperatorUnit(
          contracts.activeOperatorsPolicyId,
          activeOperatorAnchorKey,
        );
  const activeOperatorsLastNode =
    schedulerPlan.kind === "rewind" && schedulerPlan.newOperator !== undefined
      ? activeOperatorRemoval.lastNodeAfterRemoval?.utxo
      : undefined;
  const now = BigInt(Date.now());
  const txValidFrom = validFrom ?? now - DEFAULT_REMOVE_VALIDITY_BACKDATE_MS;
  const txValidTo = validTo ?? now + DEFAULT_REMOVE_VALIDITY_WINDOW_MS;

  if (fraudProofUtxo.datum == null) {
    throw new Error(
      `Fraud-proof token UTxO ${outRefLabel(fraudProofUtxo)} is missing datum.`,
    );
  }
  const fraudProofDatum = Data.from(fraudProofUtxo.datum, FraudProofTokenDatum);
  if (fraudProofDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Fraud-proof token prover ${fraudProofDatum.fraud_prover} does not match signer ${signer.paymentKeyHash}.`,
    );
  }

  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const initialLayout = buildKnownLayout({
    feeInput,
    stateQueueRoot: stateQueueRoot.utxo,
    stateQueueBlock: stateQueueBlock.utxo,
    fraudProof: fraudProofUtxo,
    activeOperatorAnchor: activeOperatorRemoval.anchor.utxo,
    activeOperatorNode: activeOperatorRemoval.node.utxo,
    scheduler: schedulerUtxo,
    schedulerPlan,
    hubOracle: hubOracleUtxo,
    registeredOperatorsRoot: registeredOperatorsRootUtxo,
    activeOperatorsLastNode,
    referenceScripts,
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    activeOperatorsPolicyId: contracts.activeOperatorsPolicyId,
  });

  const requireLayoutIndex = (
    value: bigint | undefined,
    label: keyof RemoveFraudulentBlockLayout,
  ): bigint => {
    if (value === undefined) {
      throw new Error(`Missing ${label} in remove-fraudulent-block layout.`);
    }
    return value;
  };

  const makeActiveOperatorsMintRedeemer = (
    layout: RemoveFraudulentBlockLayout,
  ): ActiveOperatorMintRedeemerData => {
    const operatorRemovalSchedulerSync =
      schedulerPlan.kind === "inactive"
        ? {
            ShowOperatorIsInactive: {
              scheduler_ref_input_index: requireLayoutIndex(
                layout.schedulerRefInputIndex,
                "schedulerRefInputIndex",
              ),
            },
          }
        : {
            ShowSchedulerIsAdvancing: {
              scheduler_input_index: requireLayoutIndex(
                layout.schedulerInputIndex,
                "schedulerInputIndex",
              ),
              scheduler_redeemer_index: requireLayoutIndex(
                layout.schedulerRedeemerTxInfoIndex,
                "schedulerRedeemerTxInfoIndex",
              ),
              removing_operators_anchor_element_key:
                activeOperatorAnchorKey === null
                  ? null
                  : activeOperatorAnchorKey,
              removing_operator_is_the_last_member:
                schedulerPlan.removedNodeIsLast,
            },
          };

    return {
      SlashOperator: {
        slashing_arguments: {
          slashed_operator: fraudulentOperator,
          hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
          slashed_operator_anchor_element_input_index:
            layout.activeOperatorAnchorInputIndex,
          slashed_operator_node_input_index:
            layout.activeOperatorNodeInputIndex,
          slashed_operator_anchor_element_output_index:
            layout.activeOperatorAnchorOutputIndex,
          slashing_reason: {
            SlashOperatorForBadState: {
              state_queue_redeemer_index: layout.stateQueueRedeemerTxInfoIndex,
            },
          },
        },
        operator_removal_scheduler_sync: operatorRemovalSchedulerSync,
      },
    };
  };

  const makeSchedulerSpendRedeemer = (
    layout: RemoveFraudulentBlockLayout,
  ): SchedulerSpendRedeemerData => {
    if (schedulerPlan.kind === "inactive") {
      throw new Error("Inactive scheduler plan does not spend scheduler.");
    }
    const scheduler_input_index = requireLayoutIndex(
      layout.schedulerInputIndex,
      "schedulerInputIndex",
    );
    const scheduler_output_index = requireLayoutIndex(
      layout.schedulerOutputIndex,
      "schedulerOutputIndex",
    );
    return {
      scheduler_input_index,
      scheduler_output_index,
      advancing_approach:
        schedulerPlan.kind === "goToAnchor"
          ? {
              GoToNextDueToOperatorRemoval: {
                active_operators_mint_redeemer_index:
                  layout.activeOperatorsRedeemerTxInfoIndex,
                removal_reason: "OperatorSlashing",
              },
            }
          : {
              RewindDueToOperatorRemoval: {
                active_operators_mint_redeemer_index:
                  layout.activeOperatorsRedeemerTxInfoIndex,
                m_active_operators_last_node_ref_input_index:
                  layout.activeOperatorsLastNodeRefInputIndex ?? null,
                removal_reason: "OperatorSlashing",
                registered_element_ref_input_index:
                  layout.registeredOperatorsRootRefInputIndex,
              },
            },
    };
  };

  const schedulerSpendForLayout = (layout: RemoveFraudulentBlockLayout) => {
    if (schedulerPlan.kind === "inactive") {
      return undefined;
    }
    const nextScheduledOperator =
      schedulerPlan.kind === "goToAnchor"
        ? schedulerPlan.newOperator
        : schedulerPlan.newOperator;
    const datum =
      nextScheduledOperator === undefined
        ? Data.to("NoActiveOperators", SchedulerDatum)
        : Data.to(
            {
              ActiveOperator: {
                operator: nextScheduledOperator,
                start_time: txValidTo,
              },
            },
            SchedulerDatum,
          );
    return {
      input: schedulerUtxo,
      redeemer: makeSchedulerSpendRedeemer(layout),
      script: contracts.schedulerSpendingScript,
      continuedOutput: {
        address: contracts.schedulerAddress,
        datum,
        assets: schedulerUtxo.assets,
      },
    };
  };

  const makeTx = (layout: RemoveFraudulentBlockLayout) =>
    incompleteRemoveLastFraudulentBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueueAddress,
        stateQueuePolicyId: contracts.stateQueuePolicyId,
      },
      {
        anchorUTxO: stateQueueRoot,
        fraudulentBlockUTxO: stateQueueBlock,
        additionalInputs: [feeInput],
        validFrom: txValidFrom,
        validTo: txValidTo,
        fraudulentOperator,
        fraudulentBlocksHeaderHash: headerHash,
        fraudProofRefInput: fraudProofUtxo,
        fraudProofRefInputIndex: layout.fraudProofRefInputIndex,
        additionalRefInputs: [
          hubOracleUtxo,
          registeredOperatorsRootUtxo,
          ...(schedulerPlan.kind === "inactive" ? [schedulerUtxo] : []),
          ...(activeOperatorsLastNode === undefined
            ? []
            : [activeOperatorsLastNode]),
        ],
        slashing: {
          kind: "slashActiveOperator",
          activeOperatorsRedeemerTxInfoIndex:
            layout.activeOperatorsRedeemerTxInfoIndex,
          activeOperatorsAssetsToBurn: {
            [activeOperatorNodeUnit]: -1n,
          },
          activeOperatorsMintRedeemer: makeActiveOperatorsMintRedeemer(layout),
          activeOperatorsMintingScript: contracts.activeOperatorsMintingScript,
          activeOperatorInputs: [
            activeOperatorRemoval.anchor.utxo,
            activeOperatorRemoval.node.utxo,
          ],
          activeOperatorSpendingScript: contracts.activeOperatorsSpendingScript,
          activeOperatorSpendRedeemer: "ListStateTransition",
          continuedActiveOperatorAnchorOutput: {
            address: contracts.activeOperatorsAddress,
            datum: encodeLinkedListNodeView({
              ...activeOperatorRemoval.anchor.view,
              next: activeOperatorRemoval.node.view.next,
            }),
            assets: activeOperatorRemoval.anchor.utxo.assets,
          },
          schedulerSpend: schedulerSpendForLayout(layout),
        },
        anchorElementInputIndex: layout.anchorElementInputIndex,
        anchorElementOutputIndex: layout.stateQueueAnchorOutputIndex,
        fraudulentNodeInputIndex: layout.fraudulentNodeInputIndex,
        stateQueueSpendingScript: contracts.stateQueueSpendingScript,
        stateQueueMintingScript: contracts.stateQueueMintingScript,
        referenceScripts,
      },
    ).addSignerKey(signer.paymentKeyHash);

  const draft = await makeTx(initialLayout).complete({ localUPLCEval: true });
  const deriveLayout = (tx: CML.Transaction): RemoveFraudulentBlockLayout =>
    deriveLayoutFromTx({
      tx,
      stateQueueRoot: stateQueueRoot.utxo,
      stateQueueBlock: stateQueueBlock.utxo,
      fraudProof: fraudProofUtxo,
      activeOperatorAnchor: activeOperatorRemoval.anchor.utxo,
      activeOperatorNode: activeOperatorRemoval.node.utxo,
      scheduler: schedulerUtxo,
      schedulerPlan,
      hubOracle: hubOracleUtxo,
      registeredOperatorsRoot: registeredOperatorsRootUtxo,
      activeOperatorsLastNode,
      stateQueueRootUnit,
      activeOperatorAnchorUnit,
      schedulerUnit,
      contracts,
    });
  const resolvedLayout = deriveLayout(draft.toTransaction());
  const unsigned: TxSignBuilder = await makeTx(resolvedLayout).complete({
    localUPLCEval: true,
  });
  const finalLayout = deriveLayout(unsigned.toTransaction());
  if (!sameLayout(resolvedLayout, finalLayout)) {
    throw new Error(
      `remove-fraudulent-block layout was unstable: resolved=${JSON.stringify(layoutToJson(resolvedLayout))}, final=${JSON.stringify(layoutToJson(finalLayout))}.`,
    );
  }

  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    fraudulentHeaderHash: headerHash,
    stateQueueBlockOutRef: outRefLabel(stateQueueBlock.utxo),
    stateQueueRootOutRef: outRefLabel(stateQueueRoot.utxo),
    fraudProofOutRef: outRefLabel(fraudProofUtxo),
    activeOperatorsRootOutRef: outRefLabel(activeOperatorsRootUtxo),
    activeOperatorNodeOutRef: outRefLabel(activeOperatorNodeUtxo),
    schedulerOutRef: outRefLabel(schedulerUtxo),
    hubOracleOutRef: outRefLabel(hubOracleUtxo),
    registeredOperatorsRootOutRef: outRefLabel(registeredOperatorsRootUtxo),
    referenceScriptOutRefs: referenceScriptOutRefs(referenceScripts),
    layout: layoutToJson(finalLayout),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitRemoveFraudulentBlockFromFiles = async (
  config: RemoveFraudulentBlockCliConfig,
): Promise<SubmitRemoveFraudulentBlockResult> => {
  const [lucid, blueprint, deploymentInfo] = await Promise.all([
    makeLucidForSubmit(config),
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
  ]);
  const signer = resolveProverSigner(config);
  return await submitRemoveFraudulentBlock({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    fraudulentHeaderHash: config.fraudulentHeaderHash,
    awaitConfirmation: config.awaitConfirmation,
    requireReferenceScripts: true,
  });
};
