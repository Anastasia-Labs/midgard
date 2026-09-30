import {
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  computeInactivityThreshold,
  encodeLinkedListNodeView,
  getProtocolParameters,
  outputReferenceFromUTxO,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  RetiredOperatorDatum,
  RetiredOperatorMintRedeemer,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SchedulerSpendRedeemer,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { type ProvedDoubleSpendFixture } from "./submit-init-emulator-fixtures.instrument-lucid-for-removal.js";
import {
  activeOperatorDatumFromUtxo,
  cleanAdaOnlyWalletFeeInput,
  expectReferenceScriptOnlyTransaction,
  minimumLovelaceForInlineValue,
  requireCurrentUnitUtxo,
} from "./submit-init-emulator-fixtures.minimum-lovelace-for-inline-value.js";
import {
  captureEmulatorSubmission,
  network,
} from "./submit-init-emulator-shared.js";

/**
 * Drive the fixture's sole active operator through the canonical five-strike
 * inactivity path and transfer its exact remaining bond to the retired set.
 * This is deliberately a real validator lifecycle, not a fabricated retired
 * datum, so the partial Q53 tranche reaches fraud removal with authenticated
 * provenance from the active-operator and scheduler contracts.
 */
export const retireFixtureOperatorAfterInactivity = async (
  fixture: ProvedDoubleSpendFixture,
): Promise<UTxO> => {
  const { activeOperators, retiredOperators, scheduler, stateQueue } =
    fixture.contracts;
  const operator = fixture.fraudulentHeader.operatorVkey;
  const schedulerUnit = toUnit(scheduler.policyId, SCHEDULER_ASSET_NAME);
  const retiredOperatorNodeUnit = toUnit(
    retiredOperators.policyId,
    RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX + operator,
  );
  const tailStateQueueUnit =
    fixture.successors.at(-1)?.successorBlockUnit ??
    fixture.setup.stateQueueBlockUnit;
  const protocolParameters = getProtocolParameters(network);

  for (
    let expectedInputStrikes = 0n;
    expectedInputStrikes < 5n;
    expectedInputStrikes += 1n
  ) {
    const currentScheduler = await requireCurrentUnitUtxo({
      lucid: fixture.funderLucid,
      address: scheduler.spendingScriptAddress,
      unit: schedulerUnit,
      label: "inactivity strike scheduler",
    });
    const currentActiveOperator = await requireCurrentUnitUtxo({
      lucid: fixture.funderLucid,
      address: activeOperators.spendingScriptAddress,
      unit: fixture.setup.activeOperatorNodeUnit,
      label: "inactivity strike active operator",
    });
    const activeOperatorsRoot = await requireCurrentUnitUtxo({
      lucid: fixture.funderLucid,
      address: activeOperators.spendingScriptAddress,
      unit: fixture.setup.activeOperatorsRootUnit,
      label: "inactivity strike active-operators root",
    });
    const registeredOperatorsRoot = await requireCurrentUnitUtxo({
      lucid: fixture.funderLucid,
      address: fixture.contracts.registeredOperators.spendingScriptAddress,
      unit: toUnit(
        fixture.contracts.registeredOperators.policyId,
        REGISTERED_OPERATORS_ROOT_ASSET_NAME,
      ),
      label: "inactivity strike registered-operators root",
    });
    const tailStateQueueNode = await requireCurrentUnitUtxo({
      lucid: fixture.funderLucid,
      address: stateQueue.spendingScriptAddress,
      unit: tailStateQueueUnit,
      label: "inactivity strike terminal state-queue node",
    });
    const schedulerDatum = Data.from(currentScheduler.datum!, SchedulerDatum);
    if (
      !(
        typeof schedulerDatum === "object" && "ActiveOperator" in schedulerDatum
      )
    ) {
      throw new Error("Inactivity strike expected an appointed scheduler.");
    }
    expect(schedulerDatum.ActiveOperator.operator).toBe(operator);
    const currentActiveDatum = await activeOperatorDatumFromUtxo(
      currentActiveOperator,
    );
    expect(currentActiveDatum.inactivity_strikes).toBe(expectedInputStrikes);

    const threshold = computeInactivityThreshold({
      shiftStartMs: BigInt(schedulerDatum.ActiveOperator.start_time),
      stateQueueTailEndTimeMs: BigInt(
        fixture.successors.at(-1)?.header.endTime ??
          fixture.fraudulentHeader.endTime,
      ),
    });
    if (threshold.kind !== "threshold") {
      throw new Error(
        "Inactivity strike threshold does not fall before the shift ends.",
      );
    }
    const inactivityThreshold = Number(threshold.thresholdMs);
    const firstValidSlot =
      fixture.funderLucid.unixTimeToSlot(inactivityThreshold) + 2;
    const slotsToAdvance = firstValidSlot - fixture.funderLucid.currentSlot();
    if (slotsToAdvance > 0) {
      fixture.emulator.awaitSlot(slotsToAdvance);
    }
    const validFrom = fixture.funderLucid.slotToUnixTime(
      fixture.funderLucid.currentSlot(),
    );
    const validTo = validFrom + 60_000;
    const nextShiftStart = BigInt(validTo - 1);
    const feeInput = await cleanAdaOnlyWalletFeeInput(
      fixture.proverLucid,
      "inactivity strike fee input",
    );
    const schedulerSpendRedeemer = ((ctx) =>
      Data.to(
        {
          scheduler_input_index: requireInputIndex(
            ctx,
            currentScheduler,
            "inactivity strike scheduler input",
          ),
          scheduler_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) => (output.assets[schedulerUnit] ?? 0n) === 1n,
            "inactivity strike scheduler output",
          ),
          advancing_approach: {
            RewindDueToSkippedOperator: {
              active_operators_root_ref_input_index: requireReferenceInputIndex(
                ctx,
                activeOperatorsRoot,
                "inactivity strike active-operators root",
              ),
              skipped_operator_node_input_index: requireInputIndex(
                ctx,
                currentActiveOperator,
                "inactivity strike active-operator input",
              ),
              active_operators_spend_redeemer_index: requireSpendRedeemerIndex(
                ctx,
                currentActiveOperator,
                "inactivity strike active-operator redeemer",
              ),
              state_queue_ref_input_index: requireReferenceInputIndex(
                ctx,
                tailStateQueueNode,
                "inactivity strike state-queue tail",
              ),
              hub_oracle_ref_input_index: requireReferenceInputIndex(
                ctx,
                fixture.setup.hubOracle,
                "inactivity strike hub oracle",
              ),
              m_active_operators_last_node_ref_input_index: null,
              registered_element_ref_input_index: requireReferenceInputIndex(
                ctx,
                registeredOperatorsRoot,
                "inactivity strike registered-operators root",
              ),
              neglected_user_event: "NoNeglectedUserEvent",
            },
          },
        } satisfies SchedulerSpendRedeemer,
        SchedulerSpendRedeemer,
      )) satisfies BuildTxWithRedeemer;
    const activeOperatorSpendRedeemer = ((ctx) =>
      Data.to(
        {
          StrikeForInactivity: {
            active_node_input_index: requireInputIndex(
              ctx,
              currentActiveOperator,
              "inactivity strike active-operator input",
            ),
            active_node_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                (output.assets[fixture.setup.activeOperatorNodeUnit] ?? 0n) ===
                1n,
              "inactivity strike active-operator output",
            ),
            operator,
            active_node_link: null,
            scheduler_input_index: requireInputIndex(
              ctx,
              currentScheduler,
              "inactivity strike scheduler input",
            ),
            scheduler_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              currentScheduler,
              "inactivity strike scheduler redeemer",
            ),
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              fixture.setup.hubOracle,
              "inactivity strike hub oracle",
            ),
          },
        } satisfies ActiveOperatorSpendRedeemer,
        ActiveOperatorSpendRedeemer,
      )) satisfies BuildTxWithRedeemer;
    const strikeUnsigned = await fixture.proverLucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom([currentScheduler], schedulerSpendRedeemer)
      .collectFrom([currentActiveOperator], activeOperatorSpendRedeemer)
      .readFrom([
        fixture.setup.hubOracle,
        activeOperatorsRoot,
        registeredOperatorsRoot,
        tailStateQueueNode,
        fixture.removalReferenceScriptPublications.published
          .activeOperatorsSpend,
        fixture.removalReferenceScriptPublications.published.schedulerSpend,
      ])
      .pay.ToContract(
        scheduler.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(
            {
              ActiveOperator: {
                operator,
                start_time: nextShiftStart,
              },
            },
            SchedulerDatum,
          ),
        },
        currentScheduler.assets,
      )
      .pay.ToContract(
        activeOperators.spendingScriptAddress,
        {
          kind: "inline",
          value: encodeLinkedListNodeView({
            key: { Key: { key: operator } },
            next: "Empty",
            data: Data.castTo(
              {
                bond_unlock_time: currentActiveDatum.bond_unlock_time,
                inactivity_strikes: expectedInputStrikes + 1n,
              },
              ActiveOperatorDatum,
            ),
          }),
        },
        currentActiveOperator.assets,
      )
      .validFrom(validFrom)
      .validTo(validTo)
      .complete({ localUPLCEval: true });
    const strikeSigned = await strikeUnsigned.sign.withWallet().complete();
    const expectedStrikeReferenceInputs = [
      fixture.setup.hubOracle,
      activeOperatorsRoot,
      registeredOperatorsRoot,
      tailStateQueueNode,
      fixture.removalReferenceScriptPublications.published.activeOperatorsSpend,
      fixture.removalReferenceScriptPublications.published.schedulerSpend,
    ];
    const strikeCapture = await captureEmulatorSubmission(
      fixture.emulator,
      async () => {
        const txHash = await strikeSigned.submit();
        await fixture.proverLucid.awaitTx(txHash);
        return txHash;
      },
    );
    expectReferenceScriptOnlyTransaction({
      signed: strikeSigned,
      measurement: strikeCapture.measurement,
      expectedReferenceInputs: expectedStrikeReferenceInputs,
    });
  }

  const activeOperatorsRoot = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: activeOperators.spendingScriptAddress,
    unit: fixture.setup.activeOperatorsRootUnit,
    label: "inactivity retirement active-operators root",
  });
  const activeOperator = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: activeOperators.spendingScriptAddress,
    unit: fixture.setup.activeOperatorNodeUnit,
    label: "inactivity retirement active operator",
  });
  const retiredOperatorsRoot = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: retiredOperators.spendingScriptAddress,
    unit: fixture.setup.retiredOperatorsRootUnit,
    label: "inactivity retirement retired-operators root",
  });
  const currentScheduler = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: scheduler.spendingScriptAddress,
    unit: schedulerUnit,
    label: "inactivity retirement scheduler",
  });
  const registeredOperatorsRoot = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: fixture.contracts.registeredOperators.spendingScriptAddress,
    unit: toUnit(
      fixture.contracts.registeredOperators.policyId,
      REGISTERED_OPERATORS_ROOT_ASSET_NAME,
    ),
    label: "inactivity retirement registered-operators root",
  });
  const activeDatum = await activeOperatorDatumFromUtxo(activeOperator);
  expect(activeDatum.inactivity_strikes).toBe(5n);
  const retirementValidFrom = fixture.funderLucid.slotToUnixTime(
    fixture.funderLucid.currentSlot(),
  );
  const retirementValidTo = retirementValidFrom + 60_001;
  const activeOperatorsMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      activeOperators.policyId,
      "inactivity retirement active-operators mint",
    );
    return Data.to(
      {
        RetireOperator: {
          active_operator_key: operator,
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            fixture.setup.hubOracle,
            "inactivity retirement hub oracle",
          ),
          active_operator_anchor_element_input_outref:
            outputReferenceFromUTxO(activeOperatorsRoot),
          active_operator_anchor_element_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              (output.assets[fixture.setup.activeOperatorsRootUnit] ?? 0n) ===
              1n,
            "inactivity retirement active-operators root output",
          ),
          retired_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            retiredOperators.policyId,
            "inactivity retirement retired-operators mint",
          ),
          penalize_for_inactivity: true,
          operator_removal_scheduler_sync: {
            ShowSchedulerIsAdvancing: {
              scheduler_input_index: requireInputIndex(
                ctx,
                currentScheduler,
                "inactivity retirement scheduler input",
              ),
              scheduler_redeemer_index: requireSpendRedeemerIndex(
                ctx,
                currentScheduler,
                "inactivity retirement scheduler redeemer",
              ),
              removing_operators_anchor_element_key: null,
              removing_operator_is_the_last_member: true,
            },
          },
        },
      } satisfies ActiveOperatorMintRedeemer,
      ActiveOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const retiredOperatorsMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      retiredOperators.policyId,
      "inactivity retirement retired-operators mint",
    );
    return Data.to(
      {
        RetireOperator: {
          new_retired_operator_key: operator,
          bond_unlock_time: activeDatum.bond_unlock_time,
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            fixture.setup.hubOracle,
            "inactivity retirement hub oracle",
          ),
          retired_operator_anchor_element_output_index:
            requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                (output.assets[fixture.setup.retiredOperatorsRootUnit] ??
                  0n) === 1n,
              "inactivity retirement retired-operators root output",
            ),
          retired_operator_inserted_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) => (output.assets[retiredOperatorNodeUnit] ?? 0n) === 1n,
            "inactivity retirement retired-operator node output",
          ),
          active_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            activeOperators.policyId,
            "inactivity retirement active-operators mint",
          ),
        },
      } satisfies RetiredOperatorMintRedeemer,
      RetiredOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const schedulerSpendRedeemer = ((ctx) =>
    Data.to(
      {
        scheduler_input_index: requireInputIndex(
          ctx,
          currentScheduler,
          "inactivity retirement scheduler input",
        ),
        scheduler_output_index: requireUniqueOutputIndex(
          ctx.outputs,
          (output) => (output.assets[schedulerUnit] ?? 0n) === 1n,
          "inactivity retirement scheduler output",
        ),
        advancing_approach: {
          RewindDueToOperatorRemoval: {
            active_operators_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              activeOperators.policyId,
              "inactivity retirement active-operators mint",
            ),
            m_active_operators_last_node_ref_input_index: null,
            removal_reason: "OperatorRetirement",
            registered_element_ref_input_index: requireReferenceInputIndex(
              ctx,
              registeredOperatorsRoot,
              "inactivity retirement registered-operators root",
            ),
          },
        },
      } satisfies SchedulerSpendRedeemer,
      SchedulerSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const partialBond =
    protocolParameters.required_bond -
    protocolParameters.inactivity_slashing_penalty;
  const activeOperatorsRootDatum = encodeLinkedListNodeView({
    key: "Empty",
    next: "Empty",
    data: "",
  });
  const retiredOperatorsRootDatum = encodeLinkedListNodeView({
    key: "Empty",
    next: { Key: { key: operator } },
    data: "",
  });
  const retiredOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: operator } },
    next: "Empty",
    data: Data.castTo(
      { bond_unlock_time: activeDatum.bond_unlock_time },
      RetiredOperatorDatum,
    ),
  });
  const retiredRootInputLovelace = retiredOperatorsRoot.assets.lovelace ?? 0n;
  const retiredRootOutputLovelace = minimumLovelaceForInlineValue({
    address: retiredOperators.spendingScriptAddress,
    datum: retiredOperatorsRootDatum,
    assets: retiredOperatorsRoot.assets,
  });
  const retiredRootRentTopUp =
    retiredRootOutputLovelace - retiredRootInputLovelace;
  if (retiredRootRentTopUp < 0n) {
    throw new Error(
      "Retired-operators root min-ADA top-up cannot be negative.",
    );
  }
  const retirementRentInput = await cleanAdaOnlyWalletFeeInput(
    fixture.proverLucid,
    "inactivity retirement linked-list rent",
  );
  const retirementRentInputLovelace = retirementRentInput.assets.lovelace ?? 0n;
  const retirementRentRefund =
    retirementRentInputLovelace - retiredRootRentTopUp;
  if (retirementRentRefund <= 1_000_000n) {
    throw new Error(
      "Inactivity retirement rent input cannot fund a canonical change output.",
    );
  }
  const retirementRefundAddress = await fixture.proverLucid.wallet().address();
  const retirementProtocolInputs =
    (activeOperatorsRoot.assets.lovelace ?? 0n) +
    (activeOperator.assets.lovelace ?? 0n) +
    retiredRootInputLovelace +
    (currentScheduler.assets.lovelace ?? 0n) +
    retirementRentInputLovelace;
  const retirementProtocolOutputsAndFee =
    (activeOperatorsRoot.assets.lovelace ?? 0n) +
    retiredRootOutputLovelace +
    partialBond +
    (currentScheduler.assets.lovelace ?? 0n) +
    retirementRentRefund +
    protocolParameters.inactivity_slashing_penalty;
  expect(retirementProtocolInputs).toBe(retirementProtocolOutputsAndFee);
  const retirementUnsigned = await fixture.proverLucid
    .newTx()
    .collectFrom([retirementRentInput])
    .collectFrom(
      [activeOperatorsRoot, activeOperator],
      Data.to("ListStateTransition", ActiveOperatorSpendRedeemer),
    )
    .collectFrom([retiredOperatorsRoot], Data.void())
    .collectFrom([currentScheduler], schedulerSpendRedeemer)
    .readFrom([
      fixture.setup.hubOracle,
      registeredOperatorsRoot,
      fixture.removalReferenceScriptPublications.published.activeOperatorsSpend,
      fixture.removalReferenceScriptPublications.published.activeOperatorsMint,
      fixture.removalReferenceScriptPublications.published
        .retiredOperatorsSpend,
      fixture.removalReferenceScriptPublications.published.retiredOperatorsMint,
      fixture.removalReferenceScriptPublications.published.schedulerSpend,
    ])
    .mintAssets(
      { [fixture.setup.activeOperatorNodeUnit]: -1n },
      activeOperatorsMintRedeemer,
    )
    .mintAssets({ [retiredOperatorNodeUnit]: 1n }, retiredOperatorsMintRedeemer)
    .pay.ToContract(
      activeOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: activeOperatorsRootDatum,
      },
      activeOperatorsRoot.assets,
    )
    .pay.ToContract(
      retiredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: retiredOperatorsRootDatum,
      },
      {
        ...retiredOperatorsRoot.assets,
        lovelace: retiredRootOutputLovelace,
      },
    )
    .pay.ToContract(
      retiredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: retiredOperatorDatum,
      },
      { lovelace: partialBond, [retiredOperatorNodeUnit]: 1n },
    )
    .pay.ToContract(
      scheduler.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to("NoActiveOperators", SchedulerDatum),
      },
      currentScheduler.assets,
    )
    .pay.ToAddress(retirementRefundAddress, {
      lovelace: retirementRentRefund,
    })
    .validFrom(retirementValidFrom)
    .validTo(retirementValidTo)
    .setMinFee(protocolParameters.inactivity_slashing_penalty)
    .complete({ coinSelection: false, localUPLCEval: true });
  const retirementSigned = await retirementUnsigned.sign
    .withWallet()
    .complete();
  const expectedRetirementReferenceInputs = [
    fixture.setup.hubOracle,
    registeredOperatorsRoot,
    fixture.removalReferenceScriptPublications.published.activeOperatorsSpend,
    fixture.removalReferenceScriptPublications.published.activeOperatorsMint,
    fixture.removalReferenceScriptPublications.published.retiredOperatorsSpend,
    fixture.removalReferenceScriptPublications.published.retiredOperatorsMint,
    fixture.removalReferenceScriptPublications.published.schedulerSpend,
  ];
  const retirementCapture = await captureEmulatorSubmission(
    fixture.emulator,
    async () => {
      const txHash = await retirementSigned.submit();
      await fixture.proverLucid.awaitTx(txHash);
      return txHash;
    },
  );
  expectReferenceScriptOnlyTransaction({
    signed: retirementSigned,
    measurement: retirementCapture.measurement,
    expectedReferenceInputs: expectedRetirementReferenceInputs,
  });
  expect(retirementSigned.toTransaction().body().fee()).toBe(
    protocolParameters.inactivity_slashing_penalty,
  );

  await expect(
    fixture.funderLucid.utxosAtWithUnit(
      activeOperators.spendingScriptAddress,
      fixture.setup.activeOperatorNodeUnit,
    ),
  ).resolves.toHaveLength(0);
  const retiredOperator = await requireCurrentUnitUtxo({
    lucid: fixture.funderLucid,
    address: retiredOperators.spendingScriptAddress,
    unit: retiredOperatorNodeUnit,
    label: "partially inactivity-slashed retired operator",
  });
  expect(retiredOperator.assets.lovelace).toBe(partialBond);
  return retiredOperator;
};
