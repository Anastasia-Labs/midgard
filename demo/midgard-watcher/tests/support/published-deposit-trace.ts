import { computeHash28, MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { transitionTraceDepositRetainedFixture } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  paymentCredentialOf,
  toUnit,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  committeeSignerIndex,
  type DaLocalSignerConfig,
  daLocalSigners,
} from "midgard-node/da/local-signers";
import { availabilityParametersFromManifest } from "midgard-node/services/midgard-contracts";
import type { publishWorkflowDeployment } from "midgard-node/tests/helpers/published-workflow-deployment";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "midgard-node/transactions/register-active-operator";

type Published = Awaited<ReturnType<typeof publishWorkflowDeployment>>;

const seal = async (payload: SDK.DaPayload, header: SDK.Header) => {
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const updated = {
    ...payload,
    block_body: { ...payload.block_body, header, header_hash: headerHash },
  };
  return {
    payload: updated,
    header,
    headerHash,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(updated), {
      mode: "identity",
    }),
  };
};

/** Publish an ordinary deposit and retain the existing deposit-trace fixture. */
export const stagePublishedDepositTrace = async (
  deployment: Published,
  {
    daSignerConfig,
    honest = false,
    onStage = () => {},
  }: {
    daSignerConfig: DaLocalSignerConfig;
    honest?: boolean;
    onStage?: (name: string) => void;
  },
) => {
  const { contracts, emulator, references } = deployment;
  let lucid = deployment.operatorLucid;
  let address = await lucid.wallet().address();
  const awaitConfirmed = async (txHash: string) => {
    await lucid.awaitTx(txHash);
    // Operator onboarding maintains an explicit wallet snapshot. Refresh it
    // after direct SDK transactions so their successors use the live ledger.
    lucid.overrideUTxOs(await lucid.utxosAt(address));
  };
  let operatorVkey = paymentCredentialOf(address).hash;
  const one = async (scriptAddress: string, unit: string): Promise<UTxO> => {
    const values = await lucid.utxosAtWithUnit(scriptAddress, unit);
    if (values.length !== 1)
      throw new Error(`Expected one actual published state: ${unit}`);
    return values[0]!;
  };
  const reference = (name: string) => {
    const value = references.get(name);
    if (value === undefined)
      throw new Error(`Missing actual publication ${name}`);
    return value;
  };
  const plain = async () => {
    const value = (await lucid.wallet().getUtxos()).find(
      (u) =>
        u.datum == null &&
        u.datumHash == null &&
        u.scriptRef == null &&
        Object.keys(u.assets).every((unit) => unit === "lovelace"),
    );
    if (value === undefined)
      throw new Error("No ordinary wallet funding input");
    return value;
  };
  const rootUnit = toUnit(
    contracts.stateQueue.policyId,
    SDK.STATE_QUEUE_ROOT_ASSET_NAME,
  );
  const root = await one(contracts.stateQueue.spendingScriptAddress, rootUnit);
  const rootDatum = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(root),
  );
  const genesis = await Effect.runPromise(
    SDK.getConfirmedStateFromStateQueueDatum(rootDatum),
  );
  const bond = SDK.getProtocolParameters("Preprod").required_bond;
  const publisher = deployment.publisherLucid;
  const publicationAddress = await publisher.wallet().address();
  let activeUnit = toUnit(
    contracts.activeOperators.policyId,
    SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorVkey,
  );
  let active: UTxO;
  const schedulerUnit = toUnit(
    contracts.scheduler.policyId,
    SDK.SCHEDULER_ASSET_NAME,
  );
  const onboardOperator = async () => {
    lucid.overrideUTxOs(await lucid.utxosAt(address));
    onStage("operator registration");
    await Effect.runPromise(
      registerOperatorProgram(
        lucid,
        contracts,
        bond,
        publisher,
        publicationAddress,
      ),
    );
    emulator.awaitSlot(180);
    onStage("operator activation");
    await Effect.runPromise(
      activateOperatorProgram(
        lucid,
        contracts,
        bond,
        publisher,
        publicationAddress,
      ),
    );
    active = await one(
      contracts.activeOperators.spendingScriptAddress,
      activeUnit,
    );
    const scheduler = await one(
      contracts.scheduler.spendingScriptAddress,
      schedulerUnit,
    );
    const registeredRoot = await one(
      contracts.registeredOperators.spendingScriptAddress,
      toUnit(
        contracts.registeredOperators.policyId,
        SDK.REGISTERED_OPERATORS_ROOT_ASSET_NAME,
      ),
    );
    const schedulerStart = BigInt(emulator.now() + 39_999);
    const appointedDatum = Data.to(
      {
        ActiveOperator: { operator: operatorVkey, start_time: schedulerStart },
      },
      SDK.SchedulerDatum,
    );
    onStage("scheduler appointment");
    const appointment = await lucid
      .newTx()
      .collectFrom([await plain()])
      .collectFrom([scheduler], (ctx) =>
        Data.to(
          {
            scheduler_input_index: SDK.requireInputIndex(
              ctx,
              scheduler,
              "scheduler appointment",
            ),
            scheduler_output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === contracts.scheduler.spendingScriptAddress &&
                output.assets[schedulerUnit] === 1n,
              "appointed scheduler",
            ),
            advancing_approach: {
              AppointFirstOperator: {
                new_shifts_operator_node_ref_input_index:
                  SDK.requireReferenceInputIndex(
                    ctx,
                    active,
                    "active operator",
                  ),
                registered_element_ref_input_index:
                  SDK.requireReferenceInputIndex(
                    ctx,
                    registeredRoot,
                    "registered root",
                  ),
              },
            },
          },
          SDK.SchedulerSpendRedeemer,
        ),
      )
      .readFrom([active, registeredRoot, reference("schedulerSpend")])
      .pay.ToContract(
        contracts.scheduler.spendingScriptAddress,
        { kind: "inline", value: appointedDatum },
        scheduler.assets,
      )
      .validFrom(emulator.now() - 60_000)
      .validTo(Number(schedulerStart + 1n))
      .complete({ localUPLCEval: true });
    await awaitConfirmed(
      await (await appointment.sign.withWallet().complete()).submit(),
    );
    emulator.awaitSlot(
      Math.max(
        0,
        Math.ceil((Number(schedulerStart) + 1 - emulator.now()) / 1000),
      ),
    );
  };
  await onboardOperator();
  const l2Address = credentialToAddress("Preprod", {
    type: "Key",
    hash: operatorVkey,
  });
  const deposit = await Effect.runPromise(
    SDK.buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, {
      l2Address,
      l2Datum: null,
      lovelace: 10_000_000n,
      additionalAssets: {},
      referenceScripts: { depositMinting: reference("depositMint") },
    }),
  );
  onStage("deposit publication");
  await awaitConfirmed(
    await (await deposit.tx.sign.withWallet().complete()).submit(),
  );
  const event = await one(
    contracts.deposit.spendingScriptAddress,
    deposit.metadata.depositAuthUnit,
  );
  const emptyEnd = BigInt(emulator.now() + 39_999);
  if (emptyEnd >= BigInt(deposit.metadata.inclusionTime))
    throw new Error(
      "Ordinary deposit must become eligible after the empty predecessor",
    );
  const base = await transitionTraceDepositRetainedFixture({
    operatorVkey,
    now: Number(emptyEnd) - 60_000,
    event,
    depositPolicyId: contracts.deposit.policyId,
    assetName: deposit.metadata.depositAssetName,
    honest,
  });
  const empty = await seal(base.predecessor.payload, {
    ...base.predecessor.header,
    startTime: genesis.data.endTime,
    endTime: emptyEnd,
    blockSlot: BigInt(lucid.unixTimeToSlot(Number(emptyEnd))),
  });
  const depositEnd = BigInt(
    Math.ceil(deposit.metadata.inclusionTime / 1000) * 1000 + 59_999,
  );
  const deposited = await seal(base.current.payload, {
    ...base.current.header,
    prevHeaderHash: empty.headerHash,
    startTime: emptyEnd,
    endTime: depositEnd,
    blockSlot: BigInt(lucid.unixTimeToSlot(Number(depositEnd))),
  });
  let anchor = root;
  let head: UTxO | undefined;
  let headUnit: string | undefined;
  const commits: string[] = [];
  const commit = async (block: Pick<typeof empty, "header" | "headerHash">) => {
    onStage(`header commit ${commits.length + 1}`);
    const confirmed = await one(
      contracts.stateQueue.spendingScriptAddress,
      rootUnit,
    );
    const activeInput = await one(
      contracts.activeOperators.spendingScriptAddress,
      activeUnit,
    );
    const hub = await one(
      contracts.hubOracle.spendingScriptAddress,
      toUnit(contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME),
    );
    const lock = await one(
      contracts.correctionLock.spendingScriptAddress,
      toUnit(contracts.hubOracle.policyId, SDK.CORRECTION_LOCK_ASSET_NAME),
    );
    if (
      lock.datum == null ||
      Data.from(lock.datum, SDK.CorrectionLockDatum) !== "Idle"
    )
      throw new Error("Cannot commit while correction lock is held");
    const continuedDatum = SDK.encodeLinkedListNodeView({
      key: { Key: { key: operatorVkey } },
      next: "Empty",
      data: Data.castTo(
        {
          bond_unlock_time:
            block.header.endTime +
            BigInt(MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs),
          inactivity_strikes: 0n,
        },
        SDK.ActiveOperatorDatum,
      ),
    });
    const builder = await Effect.runPromise(
      SDK.incompleteEmulatorCommitBlockHeaderTxProgram(
        lucid,
        {
          stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
          stateQueuePolicyId: contracts.stateQueue.policyId,
        },
        {
          anchorUTxO: await Effect.runPromise(
            SDK.utxoToStateQueueUTxO(anchor, contracts.stateQueue.policyId),
          ),
          newHeader: block.header,
          // Match the node's commit reserve: attaching availability status
          // enlarges this datum while the availability policy preserves value.
          headerNodeLovelace: 5_000_000n,
          additionalInputs: [await plain()],
          validFrom: BigInt(emulator.now() - 60_000),
          validTo: block.header.endTime + 1n,
          schedulerRefInput: await one(
            contracts.scheduler.spendingScriptAddress,
            schedulerUnit,
          ),
          correctionLockRefInput: {
            utxo: lock,
            datum: "Idle",
            assetName: SDK.CORRECTION_LOCK_ASSET_NAME,
          },
          ...(anchor.txHash === confirmed.txHash &&
          anchor.outputIndex === confirmed.outputIndex
            ? {}
            : {
                confirmedStateRefInput: confirmed,
                ...(head === undefined ||
                (head.txHash === anchor.txHash &&
                  head.outputIndex === anchor.outputIndex)
                  ? {}
                  : { headStateQueueNodeRefInput: head }),
              }),
          additionalRefInputs: [
            hub,
            reference("stateQueueSpend"),
            reference("stateQueueMint"),
            reference("activeOperatorsSpend"),
          ],
          activeOperatorInput: activeInput,
          activeOperatorSpendRedeemer: (ctx) =>
            Data.to(
              {
                UpdateBondHoldNewState: {
                  active_operator: operatorVkey,
                  active_node_input_index: SDK.requireInputIndex(
                    ctx,
                    activeInput,
                    "active node",
                  ),
                  active_node_output_index: SDK.requireUniqueOutputIndex(
                    ctx.outputs,
                    (output) =>
                      output.address ===
                        contracts.activeOperators.spendingScriptAddress &&
                      output.assets[activeUnit] === 1n,
                    "continued active node",
                  ),
                  hub_oracle_ref_input_index: SDK.requireReferenceInputIndex(
                    ctx,
                    hub,
                    "hub reference",
                  ),
                  state_queue_redeemer_index: SDK.requireMintRedeemerIndex(
                    ctx,
                    contracts.stateQueue.policyId,
                    "state queue mint",
                  ),
                },
              },
              SDK.ActiveOperatorSpendRedeemer,
            ),
          activeOperatorSpendingScript:
            contracts.activeOperators.spendingScript,
          continuedActiveOperatorOutput: {
            address: contracts.activeOperators.spendingScriptAddress,
            datum: continuedDatum,
            assets: activeInput.assets,
          },
          stateQueueSpendingScript: contracts.stateQueue.spendingScript,
          stateQueueMintingScript: contracts.stateQueue.mintingScript,
          yieldWitness: {
            referenceInput: reference("stateQueueCommitWithdraw"),
            script: contracts.stateQueue.yields.commit.withdrawalScript,
          },
        },
      ),
    );
    const signed = await (await builder.complete({ localUPLCEval: true })).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await awaitConfirmed(txHash);
    commits.push(txHash);
    anchor = await one(
      contracts.stateQueue.spendingScriptAddress,
      toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.headerHash,
      ),
    );
    headUnit ??= toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.headerHash,
    );
    head = await one(contracts.stateQueue.spendingScriptAddress, headUnit);
    active = await one(
      contracts.activeOperators.spendingScriptAddress,
      activeUnit,
    );
  };
  const attest = async (block: Awaited<ReturnType<typeof seal>>) => {
    const submit = async (builder: TxBuilder) => {
      const signed = await (
        await builder.complete({ localUPLCEval: true })
      ).sign
        .withWallet()
        .complete();
      const txHash = await signed.submit();
      await awaitConfirmed(txHash);
      return txHash;
    };
    const stateQueueUnit = toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.headerHash,
    );
    const freshTarget =
      async (): Promise<SDK.DaAttestationStateQueueTarget> => {
        const stateQueueUtxo = await Effect.runPromise(
          SDK.utxoToStateQueueUTxO(
            await one(
              contracts.stateQueue.spendingScriptAddress,
              stateQueueUnit,
            ),
            contracts.stateQueue.policyId,
          ),
        );
        const stateQueueNode = await Effect.runPromise(
          SDK.getStateQueueNodeFromStateQueueDatum(stateQueueUtxo.datum),
        );
        return { stateQueueUtxo, stateQueueNode, headerHash: block.headerHash };
      };
    const daParamsUtxo = await one(
      contracts.daParamsGovernor.spendingScriptAddress,
      SDK.daParamsUnit(contracts.daParamsGovernor),
    );
    const daParamsDatum = Data.from(daParamsUtxo.datum!, SDK.DaParamsDatum);
    const availabilityParameters = availabilityParametersFromManifest(
      deployment.manifest.availabilityChallenge,
    );
    const availabilityCommitment = SDK.buildDaAvailabilityCommitment({
      deploymentIdentity: contracts.hubOracle.policyId,
      headerHash: block.headerHash,
      payload: block.payloadEnvelopeCbor,
      bondOwner: paymentCredentialOf(address).hash,
      responseGeometry: availabilityParameters.response_geometry,
    });
    const referenceScripts: SDK.DaAttestationReferenceScripts = {
      daAttestationMinting: reference("daAttestationMint"),
      daAttestationSpending: reference("daAttestationSpend"),
      stateQueueMinting: reference("stateQueueMint"),
      stateQueueSpending: reference("stateQueueSpend"),
      availabilityChallengeMinting: reference("availabilityChallengeMint"),
      availabilityChallengeBondWithdrawal: reference(
        "availabilityChallengeBondWithdraw",
      ),
    };
    onStage(`DA attestation init ${block.headerHash}`);
    await submit(
      await Effect.runPromise(
        SDK.incompleteInitDaAttestationTxProgram(lucid, contracts, {
          daParamsUtxo,
          daParamsDatum,
          target: await freshTarget(),
          referenceScripts,
          attestationOutputLovelace: availabilityParameters.da_bond_lovelace,
          rescueBeneficiary: await Effect.runPromise(
            SDK.addressDataFromBech32(address),
          ),
          availabilityCommitment,
        }),
      ),
    );
    const fetchAttestation = async (): Promise<SDK.DaAttestationUtxo> => {
      const utxo = await one(
        contracts.daAttestation.spendingScriptAddress,
        SDK.daAttestationUnit(contracts.daAttestation, block.headerHash),
      );
      return { utxo, datum: Data.from(utxo.datum!, SDK.DaAttestationDatum) };
    };
    const message = SDK.daAvailabilityAttestationMessage(
      availabilityCommitment,
    );
    const witnesses = daLocalSigners(daSignerConfig).map((signer) => {
      const signerIndex = committeeSignerIndex(
        daParamsDatum.committee,
        signer.verificationKeyHex,
      );
      if (signerIndex === null)
        throw new Error(
          "Fixture signer is absent from the deployed DA committee",
        );
      return { signerIndex, signatureHex: signer.sign(message) };
    });
    onStage(`DA attestation signatures ${block.headerHash}`);
    await submit(
      await Effect.runPromise(
        SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
          daParamsUtxo,
          daParamsDatum,
          attestation: await fetchAttestation(),
          witnesses,
          referenceScripts,
        }),
      ),
    );
    const target = await freshTarget();
    const validFrom = BigInt(lucid.slotToUnixTime(lucid.currentSlot()));
    const deadline = block.header.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS;
    const validTo =
      validFrom + 120_000n < deadline ? validFrom + 120_000n : deadline;
    onStage(`DA attestation apply ${block.headerHash}`);
    const applyTxHash = await submit(
      await Effect.runPromise(
        SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
          lucid,
          contracts,
          {
            hubOracleRefInput: (
              await Effect.runPromise(
                SDK.fetchHubOracleUTxOProgram(lucid, {
                  hubOracleAddress: contracts.hubOracle.spendingScriptAddress,
                  hubOraclePolicyId: contracts.hubOracle.policyId,
                }),
              )
            ).utxo,
            daParamsUtxo,
            daParamsDatum,
            target,
            attestation: await fetchAttestation(),
            referenceScripts,
            validityRange: { validFrom, validTo },
          },
        ),
      ),
    );
    if (
      (await freshTarget()).stateQueueNode.da_attestation ===
      SDK.NO_DA_ATTESTATION
    )
      throw new Error(
        "Accepted DA attestation did not attach to the state queue",
      );
    return applyTxHash;
  };
  await commit(empty);
  emulator.awaitSlot(
    Math.max(
      0,
      Math.ceil((deposit.metadata.inclusionTime - emulator.now()) / 1000),
    ),
  );
  await commit(deposited);
  await attest(empty);
  await attest(deposited);
  let honestSuccessor:
    | Promise<Awaited<ReturnType<typeof seal>> & { commitTxHash: string }>
    | undefined;
  /** Publish new work only after the watcher's actual correction has completed. */
  const commitHonestSuccessor = ({
    beforeCommit = () => {},
  }: Readonly<{
    beforeCommit?: (
      block: Awaited<ReturnType<typeof seal>>,
    ) => void | Promise<void>;
  }> = {}) =>
    (honestSuccessor ??= (async () => {
      const fraudulentUnit = toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + deposited.headerHash,
      );
      if (
        (
          await lucid.utxosAtWithUnit(
            contracts.stateQueue.spendingScriptAddress,
            fraudulentUnit,
          )
        ).length !== 0
      )
        throw new Error(
          "Honest successor requires the fraudulent commitment to be removed first",
        );
      headUnit = toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + empty.headerHash,
      );
      anchor = await one(contracts.stateQueue.spendingScriptAddress, headUnit);
      const predecessorDatum = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(anchor),
      );
      const predecessor = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(predecessorDatum),
      );
      if (
        predecessorDatum.next !== "Empty" ||
        SDK.encodeHeaderCbor(predecessor).toString("hex") !==
          SDK.encodeHeaderCbor(empty.header).toString("hex")
      )
        throw new Error(
          "Corrected state queue does not end at the retained honest predecessor",
        );
      const previousOperator = operatorVkey;
      lucid = publisher;
      address = await lucid.wallet().address();
      operatorVkey = paymentCredentialOf(address).hash;
      if (operatorVkey === previousOperator)
        throw new Error("Continued production requires a second operator");
      activeUnit = toUnit(
        contracts.activeOperators.policyId,
        SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorVkey,
      );
      await onboardOperator();
      const liveEvent = await one(
        contracts.deposit.spendingScriptAddress,
        deposit.metadata.depositAuthUnit,
      );
      const actualDeposit = Data.from(liveEvent.datum!, SDK.DepositDatum);
      const endTime = BigInt(emulator.now() + 39_999);
      if (
        !(
          predecessor.endTime < actualDeposit.inclusion_time &&
          actualDeposit.inclusion_time <= endTime
        )
      )
        throw new Error(
          "The unspent ordinary deposit is not due in the corrected successor interval",
        );
      const honestFixture = await transitionTraceDepositRetainedFixture({
        operatorVkey,
        now: Number(predecessor.endTime) - 60_000,
        event: liveEvent,
        depositPolicyId: contracts.deposit.policyId,
        assetName: deposit.metadata.depositAssetName,
        honest: true,
      });
      const block = await seal(honestFixture.current.payload, {
        ...honestFixture.current.header,
        prevHeaderHash: empty.headerHash,
        prevUtxosRoot: predecessor.utxosRoot,
        startTime: predecessor.endTime,
        endTime,
        blockSlot: BigInt(lucid.unixTimeToSlot(Number(endTime))),
      });
      // Removal spent the predecessor to update its link and replaced the lock.
      // Refetch both through the ordinary commit path before constructing inputs.
      anchor = await one(contracts.stateQueue.spendingScriptAddress, headUnit);
      head = anchor;
      await beforeCommit(block);
      await commit(block);
      await attest(block);
      return { ...block, commitTxHash: commits.at(-1)! };
    })());
  return {
    predecessor: empty,
    current: deposited,
    commits,
    depositEvent: event,
    commitHonestSuccessor,
  };
};
