import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  buildActivateOperatorTx,
  buildRegisterOperatorTx,
  getProtocolParameters,
  Header,
  HUB_ORACLE_ASSET_NAME,
  REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  RegisteredOperatorDatum,
  REGISTRATION_DURATION_MS,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";

import { network } from "./blueprints.js";
import {
  largestWalletUtxo,
  requireUtxoWithUnit,
  runEmulatorLifecycleStage,
} from "./emulator-context.js";
import {
  type SetupContracts,
  type SetupLucid,
  type SetupUnits,
} from "./setup-tx.setup-units.js";
import {
  directoryNodes,
  nodeWithDatum,
  orderedInsertionAnchor,
} from "./setup-tx.submit-initial-mint-tx.js";

/**
 * Genuinely register an operator from the selected wallet, then move that
 * authenticated node into the active-operators set. The registration's
 * validity ends `registrationSlots` ahead; activation becomes legal once the
 * chain has moved past the derived activation time, which `awaitActivation`
 * arranges for onboardings that join a non-empty active set.
 */
export const onboardEmulatorOperator = async ({
  lucid,
  contracts,
  operatorKeyHash,
  registrationSlots = 120,
  awaitActivation = () => {},
}: {
  readonly lucid: SetupLucid;
  readonly contracts: SetupContracts;
  readonly operatorKeyHash: string;
  readonly registrationSlots?: number;
  readonly awaitActivation?: (activationTime: bigint) => void | Promise<void>;
}): Promise<{ registeredNodeUnit: string; activeNodeUnit: string }> => {
  const hubOracleUnit = toUnit(
    contracts.hubOracle.policyId,
    HUB_ORACLE_ASSET_NAME,
  );
  const registeredRootUnit = toUnit(
    contracts.registeredOperators.policyId,
    REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  );
  const activeNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
  );
  const hubOracleUtxo = await requireUtxoWithUnit(
    lucid,
    credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    hubOracleUnit,
    "hub oracle before operator onboarding",
  );
  const [activeNodes, retiredNodes, registeredRootUtxo] = await Promise.all([
    directoryNodes(
      lucid,
      contracts.activeOperators.spendingScriptAddress,
      contracts.activeOperators.policyId,
      "active-operators node",
    ),
    directoryNodes(
      lucid,
      contracts.retiredOperators.spendingScriptAddress,
      contracts.retiredOperators.policyId,
      "retired-operators node",
    ),
    requireUtxoWithUnit(
      lucid,
      contracts.registeredOperators.spendingScriptAddress,
      registeredRootUnit,
      "registered-operators root before registration",
    ),
  ]);
  const activeNotMemberWitness = orderedInsertionAnchor(
    activeNodes,
    operatorKeyHash,
    "active-operators set",
  );
  const retiredNotMemberWitness = orderedInsertionAnchor(
    retiredNodes,
    operatorKeyHash,
    "retired-operators set",
  );
  const registeredRoot = await nodeWithDatum({
    utxo: registeredRootUtxo,
    policyId: contracts.registeredOperators.policyId,
    label: "registered-operators root",
  });
  const lifecycleReferences = contracts.operatorLifecycleReferenceScripts;
  if (lifecycleReferences === undefined) {
    throw new Error(
      "operator lifecycle reference scripts must be published before the header clock is sampled",
    );
  }
  const registerValidTo = BigInt(
    lucid.slotToUnixTime(lucid.currentSlot() + registrationSlots),
  );
  const activationTime = registerValidTo - 1n + REGISTRATION_DURATION_MS;
  const activationTimeHex = activationTime.toString(16);
  const registrationNodeKey =
    activationTimeHex.length % 2 === 0
      ? activationTimeHex
      : `0${activationTimeHex}`;
  const registeredNodeUnit = toUnit(
    contracts.registeredOperators.policyId,
    REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + registrationNodeKey,
  );
  const prependedNodeDatum = {
    key: { Key: { key: registrationNodeKey } },
    next: registeredRoot.datum.next,
    data: Data.castTo({ operator: operatorKeyHash }, RegisteredOperatorDatum),
  } as const;
  const updatedRegisteredRootDatum = {
    ...registeredRoot.datum,
    next: { Key: { key: registrationNodeKey } },
  } as const;
  const registrationFunding = [
    await largestWalletUtxo(lucid, "operator registration funding"),
  ];
  let registerLayout: Parameters<typeof buildRegisterOperatorTx>[0]["layout"];
  const registerTx = (layout = registerLayout) =>
    buildRegisterOperatorTx({
      lucid,
      contracts,
      operatorKeyHash,
      registeredOperatorScriptRefs: lifecycleReferences.registered,
      hubOracleRefInput: hubOracleUtxo,
      activeNotMemberWitness,
      retiredNotMemberWitness,
      registeredRootNode: registeredRoot,
      registerFundingInputs: registrationFunding,
      registerMintAssets: { [registeredNodeUnit]: 1n },
      prependedNodeDatum,
      prependedNodeAssets: {
        lovelace: getProtocolParameters(network).required_bond,
        [registeredNodeUnit]: 1n,
      },
      updatedRegisteredRootDatum,
      registerValidTo,
      ...(layout === undefined ? {} : { layout }),
      onLayout: (resolved) => {
        registerLayout = resolved;
      },
    });
  await runEmulatorLifecycleStage(
    `setup.operator-registration.preflight policy=${contracts.registeredOperators.policyId}`,
    () =>
      registerTx().complete({
        localUPLCEval: true,
        presetWalletInputs: [...registrationFunding],
      }),
  );
  if (registerLayout === undefined) {
    throw new Error("operator registration layout was not resolved");
  }
  const registrationUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-registration.complete",
    () =>
      registerTx(registerLayout).complete({
        localUPLCEval: true,
        presetWalletInputs: [...registrationFunding],
      }),
  );
  const registrationSigned = await registrationUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-registration", async () =>
    lucid.awaitTx(await registrationSigned.submit()),
  );
  await awaitActivation(activationTime);

  const [registeredNodeUtxo, continuedRegisteredRootUtxo] = await Promise.all([
    requireUtxoWithUnit(
      lucid,
      contracts.registeredOperators.spendingScriptAddress,
      registeredNodeUnit,
      "registered operator node",
    ),
    requireUtxoWithUnit(
      lucid,
      contracts.registeredOperators.spendingScriptAddress,
      registeredRootUnit,
      "registered-operators root after registration",
    ),
  ]);
  const [registeredNode, continuedRegisteredRoot] = await Promise.all([
    nodeWithDatum({
      utxo: registeredNodeUtxo,
      policyId: contracts.registeredOperators.policyId,
      label: "registered operator node",
    }),
    nodeWithDatum({
      utxo: continuedRegisteredRootUtxo,
      policyId: contracts.registeredOperators.policyId,
      label: "continued registered-operators root",
    }),
  ]);
  // The registered node sits at the list head, so the root is its anchor.
  if (
    continuedRegisteredRoot.datum.next === "Empty" ||
    continuedRegisteredRoot.datum.next.Key.key !== registrationNodeKey
  )
    throw new Error("registered-operators root no longer links the new node");
  const activeInsertionAnchor = orderedInsertionAnchor(
    await directoryNodes(
      lucid,
      contracts.activeOperators.spendingScriptAddress,
      contracts.activeOperators.policyId,
      "active-operators node",
    ),
    operatorKeyHash,
    "active-operators set",
  );
  const activationFunding = [
    await largestWalletUtxo(lucid, "operator activation funding"),
  ];
  const transferredOperatorAssets = {
    ...registeredNode.utxo.assets,
    [activeNodeUnit]: 1n,
  };
  delete transferredOperatorAssets[registeredNodeUnit];
  let activateLayout: Parameters<typeof buildActivateOperatorTx>[0]["layout"];
  const activateTx = (layout = activateLayout) =>
    buildActivateOperatorTx({
      lucid,
      contracts,
      operatorKeyHash,
      registeredOperatorScriptRefs: lifecycleReferences.registered,
      activeOperatorScriptRefs: lifecycleReferences.active,
      hubOracleRefInput: hubOracleUtxo,
      retiredNotMemberWitness,
      registeredNode,
      registeredAnchor: continuedRegisteredRoot,
      activeInsertionAnchor,
      activationFundingInputs: activationFunding,
      validFrom: BigInt(lucid.slotToUnixTime(lucid.currentSlot())),
      registeredNodeUnit,
      activeNodeUnit,
      transferredOperatorAssets,
      updatedRegisteredAnchorDatum: {
        ...continuedRegisteredRoot.datum,
        next: registeredNode.datum.next,
      },
      ...(layout === undefined ? {} : { layout }),
      onLayout: (resolved) => {
        activateLayout = resolved;
      },
    });
  const activationMintPolicyOrder = [
    ["active-operators", contracts.activeOperators.policyId],
    ["registered-operators", contracts.registeredOperators.policyId],
  ]
    .sort((left, right) => left[1]!.localeCompare(right[1]!))
    .map(([label]) => label)
    .join(",");
  await runEmulatorLifecycleStage(
    `setup.operator-activation.preflight mint-policy-order=[${activationMintPolicyOrder}]`,
    () =>
      activateTx().complete({
        localUPLCEval: true,
        presetWalletInputs: [...activationFunding],
      }),
  );
  if (activateLayout === undefined) {
    throw new Error("operator activation layout was not resolved");
  }
  const activationUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-activation.complete",
    () =>
      activateTx(activateLayout).complete({
        localUPLCEval: true,
        presetWalletInputs: [...activationFunding],
      }),
  );
  const activationSigned = await activationUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-activation", async () =>
    lucid.awaitTx(await activationSigned.submit()),
  );
  return { registeredNodeUnit, activeNodeUnit };
};

/**
 * Transactions 2 and 3: genuinely register the header's operator, then move
 * that authenticated node into the active-operators set.
 */
export const submitOperatorActivationTx = async ({
  lucid,
  contracts,
  header,
  units,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: SetupContracts;
  readonly header: Header;
  readonly units: SetupUnits;
}): Promise<void> => {
  const onboarded = await onboardEmulatorOperator({
    lucid,
    contracts,
    operatorKeyHash: header.operatorVkey,
  });
  if (onboarded.activeNodeUnit !== units.activeOperatorNode)
    throw new Error(
      "setup activated a different operator node than the header names",
    );
};
