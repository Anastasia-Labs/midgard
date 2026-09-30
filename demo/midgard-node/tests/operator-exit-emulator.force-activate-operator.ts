import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  BOND_LOVELACE,
  describeFailure,
  type OperatorExitFixture,
  REGISTRATION_DURATION_MS,
  runProgram,
  SLASHING_PENALTY_LOVELACE,
} from "./operator-exit-emulator.build-operator-exit-snapshot.js";
import {
  alignMs,
  completeTx,
  fetchDirectorySnapshot,
  resyncWallet,
  submitSigned,
} from "./operator-exit-emulator.build-retire-tx.js";

/**
 * Registers an operator by calling the SDK builder directly, which is how a
 * duplicate registration is reachable at all: the node refuses it locally, and
 * `RegisterOperator` only proves non-membership of the active and retired
 * lists, never of the registered list.
 */
export const forceRegisterOperator = async ({
  fixture,
  operatorLucid,
  operatorKeyHash,
}: {
  readonly fixture: OperatorExitFixture;
  readonly operatorLucid: LucidEvolution;
  readonly operatorKeyHash: string;
}): Promise<{ readonly nodeKey: string; readonly txHash: string }> => {
  const { contracts, emulator, scriptRefs } = fixture;
  await resyncWallet(operatorLucid);
  const snapshot = await fetchDirectorySnapshot(operatorLucid, contracts);
  const registeredRootNode = SDK.findRootNode(snapshot.registered);
  const activeNotMemberWitness = snapshot.active.find(({ datum }) =>
    SDK.orderedNotMemberWitness(datum, operatorKeyHash),
  );
  const retiredNotMemberWitness = snapshot.retired.find(({ datum }) =>
    SDK.orderedNotMemberWitness(datum, operatorKeyHash),
  );
  if (
    registeredRootNode === undefined ||
    activeNotMemberWitness === undefined ||
    retiredNotMemberWitness === undefined
  ) {
    throw new Error("Missing witnesses for a forced registration");
  }
  const registerValidTo = alignMs(
    operatorLucid,
    BigInt(emulator.now()) + 120_000n,
  );
  const registrationTime = registerValidTo - 1n + REGISTRATION_DURATION_MS;
  const nodeKey = SDK.posixTimeToRegisteredNodeKey(registrationTime);
  const registeredNodeUnit = toUnit(
    contracts.registeredOperators.policyId,
    SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + nodeKey,
  );
  const baseConfig = {
    lucid: operatorLucid,
    contracts,
    operatorKeyHash,
    registeredOperatorScriptRefs: scriptRefs.registeredOperators,
    hubOracleRefInput: snapshot.hubOracle.utxo,
    activeNotMemberWitness,
    retiredNotMemberWitness,
    registeredRootNode,
    registerFundingInputs: await operatorLucid.wallet().getUtxos(),
    registerMintAssets: { [registeredNodeUnit]: 1n },
    prependedNodeDatum: {
      key: { Key: { key: nodeKey } },
      next: registeredRootNode.datum.next,
      data: SDK.encodeRegisteredOperatorDatumValue(
        operatorKeyHash,
      ) as SDK.LinkedListNodeView["data"],
    },
    prependedNodeAssets: {
      lovelace: BOND_LOVELACE,
      [registeredNodeUnit]: 1n,
    },
    updatedRegisteredRootDatum: {
      ...registeredRootNode.datum,
      next: { Key: { key: nodeKey } },
    },
    registerValidTo,
  } satisfies SDK.RegisterOperatorTxConfig;
  let layout: SDK.RegisterRedeemerLayout | undefined;
  await completeTx(
    SDK.buildRegisterOperatorTx({
      ...baseConfig,
      onLayout: (resolved) => {
        layout = resolved;
      },
    }),
  );
  if (layout === undefined) {
    throw new Error("Forced registration did not resolve a redeemer layout");
  }
  const completed = await completeTx(
    SDK.buildRegisterOperatorTx({ ...baseConfig, layout }),
  );
  return {
    nodeKey,
    txHash: await submitSigned(operatorLucid, completed),
  };
};

/**
 * Activates one of several registered nodes for the same key. The node's own
 * program refuses to act at all once a key holds more than one registration,
 * so the duplicate scenarios drive the SDK builder directly.
 */
export const forceActivateOperator = async ({
  fixture,
  operatorLucid,
  operatorKeyHash,
  registeredNodeKey,
}: {
  readonly fixture: OperatorExitFixture;
  readonly operatorLucid: LucidEvolution;
  readonly operatorKeyHash: string;
  readonly registeredNodeKey: string;
}): Promise<string> => {
  const { contracts, emulator, scriptRefs } = fixture;
  await resyncWallet(operatorLucid);
  const snapshot = await fetchDirectorySnapshot(operatorLucid, contracts);
  const registeredNode = SDK.findNodeByKey(
    snapshot.registered,
    registeredNodeKey,
  );
  const registeredAnchor = SDK.findAnchorNodeForKey(
    snapshot.registered,
    registeredNodeKey,
  );
  const retiredNotMemberWitness = snapshot.retired.find(({ datum }) =>
    SDK.orderedNotMemberWitness(datum, operatorKeyHash),
  );
  const activeInsertionAnchor = snapshot.active.find(({ datum }) =>
    SDK.orderedNotMemberWitness(datum, operatorKeyHash),
  );
  if (
    registeredNode === undefined ||
    registeredAnchor === undefined ||
    retiredNotMemberWitness === undefined ||
    activeInsertionAnchor === undefined
  ) {
    throw new Error("Missing witnesses for a forced activation");
  }
  const activationTime = SDK.registeredNodeKeyToPosixTime(
    registeredNode.datum.key,
  );
  if (activationTime === undefined) {
    throw new Error("Expected a registered node, not the root");
  }
  const activeNodeUnit = SDK.activeOperatorNodeUnit(
    contracts.activeOperators.policyId,
    operatorKeyHash,
  );
  const baseConfig = {
    lucid: operatorLucid,
    contracts,
    operatorKeyHash,
    registeredOperatorScriptRefs: scriptRefs.registeredOperators,
    activeOperatorScriptRefs: scriptRefs.activeOperators,
    hubOracleRefInput: snapshot.hubOracle.utxo,
    retiredNotMemberWitness,
    registeredNode,
    registeredAnchor,
    activeInsertionAnchor,
    activationFundingInputs: await operatorLucid.wallet().getUtxos(),
    validFrom: alignMs(operatorLucid, activationTime + 1_000n),
    registeredNodeUnit: toUnit(
      contracts.registeredOperators.policyId,
      SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + registeredNodeKey,
    ),
    activeNodeUnit,
    transferredOperatorAssets: {
      lovelace: BOND_LOVELACE,
      [activeNodeUnit]: 1n,
    },
    updatedRegisteredAnchorDatum: {
      ...registeredAnchor.datum,
      next: registeredNode.datum.next,
    },
  } satisfies SDK.ActivateOperatorTxConfig;
  emulator.awaitSlot(5);
  let layout: SDK.ActivateRedeemerLayout | undefined;
  await completeTx(
    SDK.buildActivateOperatorTx({
      ...baseConfig,
      onLayout: (resolved) => {
        layout = resolved;
      },
    }),
  );
  if (layout === undefined) {
    throw new Error("Forced activation did not resolve a redeemer layout");
  }
  const completed = await completeTx(
    SDK.buildActivateOperatorTx({ ...baseConfig, layout }),
  );
  return submitSigned(operatorLucid, completed);
};

export const slashDuplicateOperator = async ({
  fixture,
  submitterLucid,
  operatorKeyHash,
  removedRegisteredNodeKey,
  duplicateProof,
  overrides = {},
}: {
  readonly fixture: OperatorExitFixture;
  readonly submitterLucid: LucidEvolution;
  readonly operatorKeyHash: string;
  readonly removedRegisteredNodeKey: string;
  readonly duplicateProof: SDK.DuplicateProof;
  readonly overrides?: Partial<SDK.SlashDuplicateOperatorTxConfig>;
}): Promise<{ readonly txHash: string; readonly fee: bigint }> => {
  const { contracts, scriptRefs } = fixture;
  await resyncWallet(submitterLucid);
  const snapshot = await fetchDirectorySnapshot(submitterLucid, contracts);
  const duplicateRegisteredNode = SDK.findNodeByKey(
    snapshot.registered,
    removedRegisteredNodeKey,
  );
  const registeredAnchor = SDK.findAnchorNodeForKey(
    snapshot.registered,
    removedRegisteredNodeKey,
  );
  if (duplicateRegisteredNode === undefined || registeredAnchor === undefined) {
    throw new Error("Missing witnesses for duplicate-operator slashing");
  }
  const { tx } = await runProgram(
    SDK.buildUnsignedSlashDuplicateOperatorTxProgram({
      lucid: submitterLucid,
      contracts,
      operatorKeyHash,
      registeredOperatorScriptRefs: scriptRefs.registeredOperators,
      duplicateRegisteredNode,
      registeredAnchor,
      duplicateRegisteredNodeUnit: toUnit(
        contracts.registeredOperators.policyId,
        SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX +
          removedRegisteredNodeKey,
      ),
      duplicateProof,
      slashingPenaltyLovelace: SLASHING_PENALTY_LOVELACE,
      ...overrides,
    }),
  );
  const fee = tx.toTransaction().body().fee();
  return { txHash: await submitSigned(submitterLucid, tx), fee };
};

/**
 * Refusals raised by the builders themselves, before any validator runs. A
 * negative test that trips one of these proves nothing about the chain.
 */
export const BUILDER_REFUSAL_MARKERS = [
  "expected exactly one matching redeemer purpose",
  "is missing from final tx inputs",
  "is missing from final tx reference inputs",
  "output selector matched multiple outputs",
  "output is missing from final tx outputs",
  "expected own spend purpose",
  "did not resolve a redeemer layout",
  "disagree on the scheduler route",
  "must reserve the inactivity penalty",
  "Failed to balance the pinned fee",
] as const;

/**
 * Asserts the deployed validator refused the transaction, rather than the
 * builder refusing to assemble it, and returns the failure text. Local UPLC
 * evaluation runs the deployed validators while the transaction is completed,
 * so an on-chain refusal surfaces as a script execution failure.
 */
export const expectOnChainRefusal = async (
  attempt: () => Promise<unknown>,
): Promise<string> => {
  let message: string | undefined;
  try {
    await attempt();
  } catch (cause) {
    message = describeFailure(cause);
  }
  if (message === undefined) {
    throw new Error("Expected the transaction to be refused");
  }
  for (const marker of BUILDER_REFUSAL_MARKERS) {
    if (message.includes(marker)) {
      throw new Error(
        `The transaction failed in the builder rather than on chain: ${message}`,
      );
    }
  }
  expect(message).toMatch(/failed script execution (Spend|Mint)\[\d+\]/);
  return message;
};

export const fetchRetiredNodes = async (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Promise<readonly SDK.RetiredOperatorNode[]> =>
  (await fetchDirectorySnapshot(lucid, contracts)).retired;
