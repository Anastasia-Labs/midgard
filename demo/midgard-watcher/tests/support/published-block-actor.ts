import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type LucidEvolution,
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
import type { publishWorkflowDeploymentOnChain } from "midgard-node/tests/helpers/published-workflow-deployment";
import { daAttestationApplyValidityRangeProgram } from "midgard-node/transactions/da-attestation";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "midgard-node/transactions/register-active-operator";

export type PublishedWatcherDeployment = Awaited<
  ReturnType<typeof publishWorkflowDeploymentOnChain>
>;
export type PublishedWatcherBlock = {
  header: SDK.Header;
  headerHash: string;
  payloadEnvelopeCbor: Buffer;
};

/** A real operator's registration, header commitments and DA attestations. */
export const createPublishedWatcherBlockActor = async ({
  deployment,
  lucid,
  daSignerConfig,
  onStage = () => {},
}: {
  deployment: PublishedWatcherDeployment;
  lucid: LucidEvolution;
  daSignerConfig: DaLocalSignerConfig;
  onStage?: (name: string) => void;
}) => {
  const { contracts, chain, references } = deployment;
  const address = await lucid.wallet().address();
  const awaitConfirmed = async (txHash: string) => {
    await lucid.awaitTx(txHash, 500);
    // Operator onboarding maintains an explicit wallet snapshot. Refresh it
    // after direct SDK transactions so their successors use the live ledger.
    lucid.overrideUTxOs(await lucid.utxosAt(address));
  };
  const operatorVkey = paymentCredentialOf(address).hash;
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
  const bond = SDK.getProtocolParameters("Preprod").required_bond;
  const publisher = deployment.publisherLucid;
  const publicationAddress = await publisher.wallet().address();
  const activeUnit = toUnit(
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
    const activeBeforeActivation = await lucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      activeUnit,
    );
    if (activeBeforeActivation.length === 0) {
      const registrations = await Promise.all(
        (
          await lucid.utxosAt(
            contracts.registeredOperators.spendingScriptAddress,
          )
        ).map((utxo) =>
          Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(utxo)),
        ),
      );
      const matching = registrations.filter(
        (node) =>
          node.key !== "Empty" &&
          Data.castFrom(node.data, SDK.RegisteredOperatorDatum).operator ===
            operatorVkey,
      );
      if (matching.length !== 1 || matching[0]!.key === "Empty")
        throw new Error(
          "Expected one registered activation time for the journey operator",
        );
      const activationTime = Number(BigInt(`0x${matching[0]!.key.Key.key}`));
      await chain.awaitSlot(
        Math.max(0, Math.ceil((activationTime + 1 - chain.now()) / 1000)),
      );
    }
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
    const schedulerDatum = Data.from(scheduler.datum!, SDK.SchedulerDatum);
    if (schedulerDatum !== "NoActiveOperators") {
      if (schedulerDatum.ActiveOperator.operator !== operatorVkey)
        throw new Error(
          "A different operator owns the current scheduler appointment",
        );
      await chain.awaitSlot(
        Math.max(
          0,
          Math.ceil(
            (Number(schedulerDatum.ActiveOperator.start_time) +
              1 -
              chain.now()) /
              1000,
          ),
        ),
      );
      return;
    }
    const registeredRoot = await one(
      contracts.registeredOperators.spendingScriptAddress,
      toUnit(
        contracts.registeredOperators.policyId,
        SDK.REGISTERED_OPERATORS_ROOT_ASSET_NAME,
      ),
    );
    const schedulerStart = BigInt(chain.now() + 39_999);
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
      .validFrom(chain.now() - 60_000)
      .validTo(Number(schedulerStart + 1n))
      .complete({ localUPLCEval: true });
    await awaitConfirmed(
      await (await appointment.sign.withWallet().complete()).submit(),
    );
    await chain.awaitSlot(
      Math.max(0, Math.ceil((Number(schedulerStart) + 1 - chain.now()) / 1000)),
    );
  };
  const commit = async (
    block: Pick<PublishedWatcherBlock, "header" | "headerHash">,
    anchor: UTxO,
    head?: UTxO,
    onSigned?: (transaction: {
      txHash: string;
      signedCbor: string;
    }) => Promise<void>,
  ) => {
    onStage(`header commit ${block.headerHash}`);
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
          validFrom: BigInt(chain.now() - 60_000),
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
    const signedCbor = signed.toCBOR();
    const txHash = CML.hash_transaction(
      CML.Transaction.from_cbor_hex(signedCbor).body(),
    ).to_hex();
    await onSigned?.({ txHash, signedCbor });
    const submittedHash = await signed.submit();
    if (submittedHash !== txHash)
      throw new Error(
        "Submitted header hash differs from its signed transaction",
      );
    await awaitConfirmed(txHash);
    return txHash;
  };
  const attest = async (block: PublishedWatcherBlock) => {
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
    const existingTarget = await freshTarget();
    if (existingTarget.stateQueueNode.da_attestation !== SDK.NO_DA_ATTESTATION)
      return existingTarget.stateQueueUtxo.utxo.txHash;
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
    const existingAttestations = await lucid.utxosAtWithUnit(
      contracts.daAttestation.spendingScriptAddress,
      SDK.daAttestationUnit(contracts.daAttestation, block.headerHash),
    );
    if (existingAttestations.length === 0) {
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
    }
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
    const pending = await fetchAttestation();
    if (pending.datum.attestation_count === 0n) {
      onStage(`DA attestation signatures ${block.headerHash}`);
      await submit(
        await Effect.runPromise(
          SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
            daParamsUtxo,
            daParamsDatum,
            attestation: pending,
            witnesses,
            referenceScripts,
          }),
        ),
      );
    } else if (pending.datum.attestation_count < pending.datum.da_threshold) {
      throw new Error(
        "Journey attestation has an unexpected partial signature set",
      );
    }
    const target = await freshTarget();
    const validityRange = await Effect.runPromise(
      daAttestationApplyValidityRangeProgram({
        currentTime: BigInt(lucid.slotToUnixTime(lucid.currentSlot())),
        headerEndTime: block.header.endTime,
      }),
    );
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
            validityRange,
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
  return {
    address,
    operatorVkey,
    onboardOperator,
    commit,
    attest,
    awaitConfirmed,
  };
};
