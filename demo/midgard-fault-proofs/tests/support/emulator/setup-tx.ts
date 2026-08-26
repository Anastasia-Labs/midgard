import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  ConfirmedState,
  EMPTY_MERKLE_TREE_ROOT,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FraudProofCatalogueDatum,
  type FraudProofCatalogueDeploymentInfo,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  getHeaderV1FromStateQueueDatum,
  hashBlockHeaderV1,
  HeaderV1,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  makeHubOracleDatum,
  type MidgardValidators,
  outputReferenceFromUTxO,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  RegisteredOperatorMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  RetiredOperatorMintRedeemer,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SchedulerMintRedeemer,
  SchedulerSpendRedeemer,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  Lucid,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { network } from "./blueprints.js";
import { ledgerOrderedIndex } from "./catalogue.js";
import {
  firstWalletUtxo,
  requireUtxoWithUnit,
  runEmulatorLifecycleStage,
} from "./emulator-context.js";
import {
  ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX,
  SCHEDULER_APPOINTMENT_OUTPUT_INDEX,
  SETUP_OUTPUT_INDEX,
} from "./header-fixtures.js";

type SetupLucid = Awaited<ReturnType<typeof Lucid>>;

/** Every asset unit the four setup transactions mint or track. */
const setupUnits = (
  contracts: MidgardValidators,
  header: HeaderV1,
  headerHash: string,
) => ({
  hubOracle: toUnit(contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME),
  fraudProofCatalogue: toUnit(
    contracts.fraudProofCatalogue.policyId,
    FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  ),
  stateQueueBlock: toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  ),
  stateQueueRoot: toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_ROOT_ASSET_NAME,
  ),
  scheduler: toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
  activeOperatorsRoot: toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ),
  retiredOperatorsRoot: toUnit(
    contracts.retiredOperators.policyId,
    RETIRED_OPERATORS_ROOT_ASSET_NAME,
  ),
  activeOperatorNode: toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + header.operatorVkey,
  ),
  registeredOperatorsRoot: toUnit(
    contracts.registeredOperators.policyId,
    REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  ),
});

type SetupUnits = ReturnType<typeof setupUnits>;

/**
 * Transaction 1: mint the hub oracle, scheduler, state-queue root, operator
 * roots, and fraud-proof catalogue in one authored output order.
 */
const submitInitialMintTx = async ({
  lucid,
  contracts,
  nonceUtxo,
  catalogue,
  header,
  units,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: MidgardValidators;
  readonly nonceUtxo: UTxO;
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly header: HeaderV1;
  readonly units: SetupUnits;
}): Promise<void> => {
  const hubOracleDatum = await Effect.runPromise(makeHubOracleDatum(contracts));
  const confirmedState = {
    headerHash: GENESIS_HEADER_HASH,
    prevHeaderHash: GENESIS_HEADER_HASH,
    utxoRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: header.startTime,
    endTime: header.startTime,
    protocolVersion: GENESIS_PROTOCOL_VERSION,
  };
  const unsigned = await lucid
    .newTx()
    .validFrom(Number(header.startTime - 120_000n))
    .validTo(Number(header.startTime + 1n))
    .collectFrom([nonceUtxo])
    .mintAssets({ [units.hubOracle]: 1n }, Data.void())
    .pay.ToAddressWithData(
      credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      ),
      {
        kind: "inline",
        value: Data.to(hubOracleDatum, HubOracleDatum),
      },
      { [units.hubOracle]: 1n },
    )
    .mintAssets(
      { [units.scheduler]: 1n },
      Data.to("Init", SchedulerMintRedeemer),
    )
    .pay.ToContract(
      contracts.scheduler.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to("NoActiveOperators", SchedulerDatum),
      },
      { [units.scheduler]: 1n },
    )
    // Fixed by the authored setup output order: hub oracle, scheduler,
    // state-queue root, active-operators root, retired-operators root, then
    // registered-operators root.
    .mintAssets(
      { [units.stateQueueRoot]: 1n },
      Data.to(
        { InitV1: { output_index: SETUP_OUTPUT_INDEX.stateQueueRoot } },
        StateQueueRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.stateQueue.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: Data.castTo(confirmedState, ConfirmedState),
        }),
      },
      { [units.stateQueueRoot]: 1n },
    )
    .mintAssets(
      { [units.activeOperatorsRoot]: 1n },
      Data.to(
        { Init: { output_index: SETUP_OUTPUT_INDEX.activeOperatorsRoot } },
        ActiveOperatorMintRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.activeOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [units.activeOperatorsRoot]: 1n },
    )
    .mintAssets(
      { [units.retiredOperatorsRoot]: 1n },
      Data.to(
        { Init: { output_index: SETUP_OUTPUT_INDEX.retiredOperatorsRoot } },
        RetiredOperatorMintRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.retiredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [units.retiredOperatorsRoot]: 1n },
    )
    .mintAssets({ [units.registeredOperatorsRoot]: 1n }, Data.void())
    .pay.ToContract(
      contracts.registeredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [units.registeredOperatorsRoot]: 1n },
    )
    .mintAssets({ [units.fraudProofCatalogue]: 1n }, Data.void())
    .pay.ToAddressWithData(
      contracts.fraudProofCatalogue.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(catalogue.root, FraudProofCatalogueDatum),
      },
      { [units.fraudProofCatalogue]: 1n },
    )
    .attach.MintingPolicy(contracts.hubOracle.mintingScript)
    .attach.MintingPolicy(contracts.fraudProofCatalogue.mintingScript)
    .attach.MintingPolicy(contracts.scheduler.mintingScript)
    .attach.MintingPolicy(contracts.stateQueue.mintingScript)
    .attach.MintingPolicy(contracts.activeOperators.mintingScript)
    .attach.MintingPolicy(contracts.retiredOperators.mintingScript)
    .attach.MintingPolicy(contracts.registeredOperators.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  await runEmulatorLifecycleStage("setup.initial", async () =>
    lucid.awaitTx(await signed.submit()),
  );
};

/**
 * Transaction 2: activate the header's operator — insert its node into the
 * active-operators list and consume its registration.
 */
const submitOperatorActivationTx = async ({
  lucid,
  contracts,
  header,
  units,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: MidgardValidators;
  readonly header: HeaderV1;
  readonly units: SetupUnits;
}): Promise<void> => {
  const initialActiveOperatorsRoot = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorsRoot,
    "active-operators root after the setup mint",
  );
  const registeredOperatorActivationUnit = toUnit(
    contracts.registeredOperators.policyId,
    "00",
  );
  const activeRootWithOperatorDatum = encodeLinkedListNodeView({
    key: "Empty",
    next: { Key: { key: header.operatorVkey } },
    data: "",
  });
  const activeOperatorInitialDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      { bond_unlock_time: null, inactivity_strikes: 0n },
      ActiveOperatorDatum,
    ),
  });
  const activeOperatorsActivateRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.activeOperators.policyId,
      "test active-operators activation mint",
    );
    return Data.to(
      {
        ActivateOperator: {
          new_active_operator_key: header.operatorVkey,
          active_operator_anchor_element_output_index:
            ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.root,
          active_operator_inserted_node_output_index:
            ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.insertedNode,
          registered_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.registeredOperators.policyId,
            "test registered-operators activation mint",
          ),
          active_operators_set_was_empty: true,
        },
      },
      ActiveOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const registeredOperatorsActivateRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.registeredOperators.policyId,
      "test registered-operators activation mint",
    );
    return Data.to(
      {
        ActivateOperator: {
          activating_operator: header.operatorVkey,
          anchor_element_input_outref: outputReferenceFromUTxO(
            initialActiveOperatorsRoot,
          ),
          anchor_element_output_index:
            ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.root,
          hub_oracle_ref_input_index: 0n,
          retired_operators_element_ref_input_index: 0n,
          active_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.activeOperators.policyId,
            "test active-operators activation mint",
          ),
        },
      },
      RegisteredOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const activationUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-activation.complete",
    () =>
      lucid
        .newTx()
        .collectFrom(
          [initialActiveOperatorsRoot],
          Data.to("ListStateTransition", ActiveOperatorSpendRedeemer),
        )
        .mintAssets(
          { [units.activeOperatorNode]: 1n },
          activeOperatorsActivateRedeemer,
        )
        .mintAssets(
          { [registeredOperatorActivationUnit]: 1n },
          registeredOperatorsActivateRedeemer,
        )
        .pay.ToContract(
          contracts.activeOperators.spendingScriptAddress,
          { kind: "inline", value: activeRootWithOperatorDatum },
          initialActiveOperatorsRoot.assets,
        )
        .pay.ToContract(
          contracts.activeOperators.spendingScriptAddress,
          { kind: "inline", value: activeOperatorInitialDatum },
          { lovelace: 20_000_000n, [units.activeOperatorNode]: 1n },
        )
        .attach.MintingPolicy(contracts.activeOperators.mintingScript)
        .attach.Script(contracts.activeOperators.spendingScript)
        .attach.MintingPolicy(contracts.registeredOperators.mintingScript)
        .complete({ localUPLCEval: true }),
  );
  const activationSigned = await activationUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-activation", async () =>
    lucid.awaitTx(await activationSigned.submit()),
  );
};

/**
 * Transaction 3: appoint the activated operator as the scheduler's first
 * shift, returning the appointed scheduler UTxO.
 */
const submitSchedulerAppointmentTx = async ({
  lucid,
  contracts,
  header,
  units,
  schedulerUtxo,
  activeOperatorNode,
  registeredOperatorsRoot,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: MidgardValidators;
  readonly header: HeaderV1;
  readonly units: SetupUnits;
  readonly schedulerUtxo: UTxO;
  readonly activeOperatorNode: UTxO;
  readonly registeredOperatorsRoot: UTxO;
}): Promise<UTxO> => {
  const schedulerAppointmentFeeInput = await firstWalletUtxo(
    lucid,
    "scheduler appointment fee input",
  );
  const appointmentInputs = [schedulerAppointmentFeeInput, schedulerUtxo];
  const appointmentRefs = [activeOperatorNode, registeredOperatorsRoot];
  const schedulerAppointmentRedeemer: SchedulerSpendRedeemer = {
    scheduler_input_index: ledgerOrderedIndex(
      appointmentInputs,
      schedulerUtxo,
      "scheduler appointment input",
    ),
    scheduler_output_index: SCHEDULER_APPOINTMENT_OUTPUT_INDEX.scheduler,
    advancing_approach: {
      AppointFirstOperator: {
        new_shifts_operator_node_ref_input_index: ledgerOrderedIndex(
          appointmentRefs,
          activeOperatorNode,
          "active-operator node appointment reference input",
        ),
        registered_element_ref_input_index: ledgerOrderedIndex(
          appointmentRefs,
          registeredOperatorsRoot,
          "registered-operators root appointment reference input",
        ),
      },
    },
  };
  const appointmentUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-appointment.complete",
    () =>
      lucid
        .newTx()
        .collectFrom([schedulerAppointmentFeeInput])
        .collectFrom(
          [schedulerUtxo],
          Data.to(schedulerAppointmentRedeemer, SchedulerSpendRedeemer),
        )
        .readFrom(appointmentRefs)
        .pay.ToContract(
          contracts.scheduler.spendingScriptAddress,
          {
            kind: "inline",
            value: Data.to(
              {
                ActiveOperator: {
                  operator: header.operatorVkey,
                  start_time: header.startTime,
                },
              },
              SchedulerDatum,
            ),
          },
          schedulerUtxo.assets,
        )
        .attach.Script(contracts.scheduler.spendingScript)
        .validFrom(Number(header.startTime - 120_000n))
        .validTo(Number(header.startTime + 1n))
        .complete({ localUPLCEval: true }),
  );
  const appointmentSigned = await appointmentUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-appointment", async () =>
    lucid.awaitTx(await appointmentSigned.submit()),
  );

  const appointedSchedulerUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.scheduler.spendingScriptAddress,
    units.scheduler,
    "scheduler after the appointment transaction",
  );
  expect(Data.from(appointedSchedulerUtxo.datum!, SchedulerDatum)).toEqual({
    ActiveOperator: {
      operator: header.operatorVkey,
      start_time: header.startTime,
    },
  });
  return appointedSchedulerUtxo;
};

/**
 * Transaction 4: commit the (fraudulent) header onto the state queue behind
 * the root, holding the operator's bond.
 */
const submitHeaderCommitTx = async ({
  lucid,
  contracts,
  header,
  headerHash,
  units,
  hubOracleUtxo,
  stateQueueRootUtxo,
  appointedSchedulerUtxo,
  activeOperatorNode,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: MidgardValidators;
  readonly header: HeaderV1;
  readonly headerHash: string;
  readonly units: SetupUnits;
  readonly hubOracleUtxo: UTxO;
  readonly stateQueueRootUtxo: UTxO;
  readonly appointedSchedulerUtxo: UTxO;
  readonly activeOperatorNode: UTxO;
}): Promise<{
  readonly fraudulentBlockUtxo: UTxO;
  readonly continuedActiveOperatorNode: UTxO;
}> => {
  const stateQueueRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(stateQueueRootUtxo, contracts.stateQueue.policyId),
  );
  const commitFeeInput = await firstWalletUtxo(lucid, "commit fee input");
  const commitValidFrom = header.startTime - 60_000n;
  const commitValidTo = header.endTime + 1n;
  const continuedActiveOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      {
        bond_unlock_time:
          commitValidTo -
          1n +
          BigInt(MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs),
        inactivity_strikes: 0n,
      },
      ActiveOperatorDatum,
    ),
  });
  const activeOperatorCommitRedeemer = ((ctx) =>
    Data.to(
      {
        UpdateBondHoldNewState: {
          active_operator: header.operatorVkey,
          active_node_input_index: requireInputIndex(
            ctx,
            activeOperatorNode,
            "commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              (output.assets[units.activeOperatorNode] ?? 0n) === 1n,
            "commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            "commit hub-oracle reference input",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "commit state-queue mint redeemer",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const commitTx = await Effect.runPromise(
    incompleteEmulatorCommitBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: stateQueueRoot,
        newHeader: header,
        additionalInputs: [commitFeeInput],
        validFrom: commitValidFrom,
        validTo: commitValidTo,
        schedulerRefInput: appointedSchedulerUtxo,
        additionalRefInputs: [hubOracleUtxo],
        activeOperatorInput: activeOperatorNode,
        activeOperatorSpendRedeemer: activeOperatorCommitRedeemer,
        activeOperatorSpendingScript: contracts.activeOperators.spendingScript,
        continuedActiveOperatorOutput: {
          address: contracts.activeOperators.spendingScriptAddress,
          datum: continuedActiveOperatorDatum,
          assets: activeOperatorNode.assets,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
      },
    ),
  );
  const commitUnsigned = await runEmulatorLifecycleStage(
    "setup.header-commit.complete",
    () => commitTx.complete({ localUPLCEval: true }),
  );
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await runEmulatorLifecycleStage("setup.header-commit", async () =>
    lucid.awaitTx(await commitSigned.submit()),
  );

  const fraudulentBlockUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueBlock,
    "committed block after the header commit",
  );
  const continuedRootUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueRoot,
    "state-queue root after the header commit",
  );
  const continuedActiveOperatorNode = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorNode,
    "active-operator node after the header commit",
  );
  const committedBlock = await Effect.runPromise(
    utxoToStateQueueUTxO(fraudulentBlockUtxo, contracts.stateQueue.policyId),
  );
  const committedHeader = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(committedBlock.datum),
  );
  expect(committedHeader.transactionsRoot).toBe(header.transactionsRoot);
  const continuedRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(continuedRootUtxo, contracts.stateQueue.policyId),
  );
  expect(continuedRoot.datum.next).toEqual({ Key: { key: headerHash } });
  return { fraudulentBlockUtxo, continuedActiveOperatorNode };
};

/**
 * The four-transaction setup journey every emulator suite starts from:
 * initial mint, operator activation, scheduler appointment, then the
 * fraudulent header commit.
 */
export const submitSetupTx = async ({
  lucid,
  contracts,
  nonceUtxo,
  catalogue,
  header,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: MidgardValidators;
  readonly nonceUtxo: UTxO;
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly header: HeaderV1;
}): Promise<{
  readonly fraudulentBlockOutRef: string;
  readonly headerHash: string;
  readonly stateQueueBlockUnit: string;
  readonly stateQueueRootUnit: string;
  readonly hubOracle: UTxO;
  readonly scheduler: UTxO;
  readonly activeOperatorsRoot: UTxO;
  readonly activeOperatorsRootUnit: string;
  readonly retiredOperatorsRoot: UTxO;
  readonly retiredOperatorsRootUnit: string;
  readonly activeOperatorNode: UTxO;
  readonly activeOperatorNodeUnit: string;
  readonly registeredOperatorsRoot: UTxO;
}> => {
  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
  const units = setupUnits(contracts, header, headerHash);

  await submitInitialMintTx({
    lucid,
    contracts,
    nonceUtxo,
    catalogue,
    header,
    units,
  });
  await submitOperatorActivationTx({ lucid, contracts, header, units });

  const hubOracleUtxo = await requireUtxoWithUnit(
    lucid,
    credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    units.hubOracle,
    "hub oracle after setup",
  );
  const stateQueueRootUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueRoot,
    "state-queue root after setup",
  );
  const schedulerUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.scheduler.spendingScriptAddress,
    units.scheduler,
    "scheduler after setup",
  );
  const activeOperatorNode = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorNode,
    "active-operator node after activation",
  );
  const activeOperatorsRoot = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorsRoot,
    "active-operators root after activation",
  );
  const retiredOperatorsRoot = await requireUtxoWithUnit(
    lucid,
    contracts.retiredOperators.spendingScriptAddress,
    units.retiredOperatorsRoot,
    "retired-operators root after setup",
  );
  const registeredOperatorsRoot = await requireUtxoWithUnit(
    lucid,
    contracts.registeredOperators.spendingScriptAddress,
    units.registeredOperatorsRoot,
    "registered-operators root after setup",
  );

  const appointedSchedulerUtxo = await submitSchedulerAppointmentTx({
    lucid,
    contracts,
    header,
    units,
    schedulerUtxo,
    activeOperatorNode,
    registeredOperatorsRoot,
  });
  const { fraudulentBlockUtxo, continuedActiveOperatorNode } =
    await submitHeaderCommitTx({
      lucid,
      contracts,
      header,
      headerHash,
      units,
      hubOracleUtxo,
      stateQueueRootUtxo,
      appointedSchedulerUtxo,
      activeOperatorNode,
    });

  return {
    fraudulentBlockOutRef: `${fraudulentBlockUtxo.txHash}#${fraudulentBlockUtxo.outputIndex.toString()}`,
    headerHash,
    stateQueueBlockUnit: units.stateQueueBlock,
    stateQueueRootUnit: units.stateQueueRoot,
    hubOracle: hubOracleUtxo,
    scheduler: appointedSchedulerUtxo,
    activeOperatorsRoot,
    activeOperatorsRootUnit: units.activeOperatorsRoot,
    retiredOperatorsRoot,
    retiredOperatorsRootUnit: units.retiredOperatorsRoot,
    activeOperatorNode: continuedActiveOperatorNode,
    activeOperatorNodeUnit: units.activeOperatorNode,
    registeredOperatorsRoot,
  };
};
