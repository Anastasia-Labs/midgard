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
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "midgard-node/transactions/register-active-operator";
import { canActivateRegisteredOperatorImmediately } from "midgard-node/transactions/register-active-operator/activation";

import { verifyPublishedDaAttestationReceipt } from "./published-da-attestation-receipt.js";
import {
  type PublishedDaTargetCorrection,
  type PublishedDaTransactionRecord,
  type PublishedDaTransactionStep,
  type PublishedHeaderConsumption,
  reconcilePublishedDaTargetConsumption,
} from "./published-da-target-consumption.js";

export type PublishedWatcherDeployment = Awaited<
  ReturnType<typeof publishWorkflowDeploymentOnChain>
>;
export type PublishedWatcherBlock = {
  header: SDK.Header;
  headerHash: string;
  payloadEnvelopeCbor: Buffer;
};

/**
 * How a DA attestation finished: the apply receipt is authenticated, or the
 * target header's own fraud correction consumed it first and that correction
 * is authenticated instead. A target that vanished for any other reason throws.
 */
export type PublishedDaAttestationOutcome =
  | {
      kind: "attested";
      /** The transaction whose state queue output carries the attested header. */
      txHash: string;
    }
  | PublishedDaTargetCorrection;

export type PublishedDaAttestOptions = {
  /** DA transactions an earlier attempt already submitted for this header. */
  submitted?: readonly PublishedDaTransactionRecord[];
  /** Settles the exact retained bytes before any replacement DA construction. */
  reconcileSubmitted?(
    record: PublishedDaTransactionRecord,
  ): Promise<{ kind: "included" | "retired" }>;
  /** Called with each DA transaction after signing and before submission. */
  onSubmitted?(record: PublishedDaTransactionRecord): Promise<void>;
};

/**
 * The expected output was not observed before the local validity wait elapsed.
 * This is a reconciliation signal, not proof of canonical non-inclusion: the
 * indexer can lag, or another transaction can already have consumed the output.
 */
export class PublishedTransactionExpiredError extends Error {
  constructor(
    readonly label: string,
    readonly txHash: string,
    readonly expiryMs: number,
  ) {
    super(
      `${label} ${txHash} output was not observed after validity bound ${expiryMs}`,
    );
    this.name = "PublishedTransactionExpiredError";
  }
}

/** Submission failed after signing; only canonical recovery can settle the attempt. */
export class PublishedTransactionSubmissionError extends Error {
  constructor(
    readonly txHash: string,
    cause: unknown,
  ) {
    super(`Header submission ${txHash} is unresolved`, { cause });
    this.name = "PublishedTransactionSubmissionError";
  }
}

/** Grace after a validity upper bound for the indexer to publish the last eligible block. */
const EXPIRY_GRACE_MS = 60_000;
/** Bound on an unbounded-validity DA transaction reaching the chain. */
const DA_SUBMISSION_TIMEOUT_MS = 600_000;
/** Bound on the indexer publishing the spend of a target it already reports absent. */
const CONSUMPTION_INDEX_GRACE_MS = 120_000;
/**
 * How long a missing target may precede our own transaction's outputs before
 * it counts as consumed by someone else. A block-indexed provider flips both
 * at once; the emulator marks spent inputs at submission and publishes the
 * outputs at its next block, at most twenty slots later.
 */
const OWN_SPEND_VISIBILITY_GRACE_MS = 30_000;

/** A real operator's registration, header commitments and DA attestations. */
export const createPublishedWatcherBlockActor = async ({
  deployment,
  lucid,
  daSignerConfig,
  readConfirmedTransaction,
  readHeaderConsumptions,
  onStage = () => {},
}: {
  deployment: PublishedWatcherDeployment;
  lucid: LucidEvolution;
  daSignerConfig: DaLocalSignerConfig;
  readConfirmedTransaction?: (txHash: string) => Promise<{ cbor: string }>;
  /** Indexed spends of outputs carrying a unit; hints the reconciliation authenticates. */
  readHeaderConsumptions?: (
    unit: string,
  ) => Promise<PublishedHeaderConsumption[]>;
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
  /**
   * Wait for a transaction with a validity upper bound. Confirmation is the
   * appearance of its expected output; crossing the local bound plus indexing
   * grace without it requires canonical reconciliation before replacement.
   */
  const awaitLandedOrExpired = async ({
    label,
    txHash,
    expiryMs,
    landed,
  }: {
    label: string;
    txHash: string;
    expiryMs: number;
    landed: () => Promise<boolean>;
  }) => {
    for (;;) {
      if (await landed()) break;
      if (chain.now() > expiryMs + EXPIRY_GRACE_MS)
        throw new PublishedTransactionExpiredError(label, txHash, expiryMs);
      await chain.delaySlots(1);
    }
    lucid.overrideUTxOs(await lucid.utxosAt(address));
  };
  const outputLanded = (txHash: string) => async () =>
    (
      await lucid
        .config()
        .provider!.getUtxosByOutRef([{ txHash, outputIndex: 0 }])
    ).length === 1;
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
  /** Whether the operator's active node is currently published. A confirmed
   * slashing correction spends it, so a resumed journey must look again. */
  const operatorActive = async () =>
    (
      await lucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        activeUnit,
      )
    ).length === 1;
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
      const activeRoot = await one(
        contracts.activeOperators.spendingScriptAddress,
        toUnit(
          contracts.activeOperators.policyId,
          SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
        ),
      );
      const immediateActivation = canActivateRegisteredOperatorImmediately(
        {
          utxo: activeRoot,
          datum: await Effect.runPromise(
            SDK.getLinkedListNodeViewFromUTxO(activeRoot),
          ),
        },
        matching[0]!,
        contracts.activeOperators,
      );
      if (!immediateActivation) {
        const activationTime = Number(BigInt(`0x${matching[0]!.key.Key.key}`));
        await chain.awaitLedgerTime(activationTime + 1);
      }
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
    await appointScheduler();
  };
  const appointScheduler = async (): Promise<void> => {
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
      await chain.awaitLedgerTime(
        Number(schedulerDatum.ActiveOperator.start_time) + 1,
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
    const appointmentSigned = await appointment.sign.withWallet().complete();
    const appointmentHash = await appointmentSigned.submit();
    try {
      await awaitLandedOrExpired({
        label: "scheduler appointment",
        txHash: appointmentHash,
        expiryMs: Number(schedulerStart) + 1,
        landed: outputLanded(appointmentHash),
      });
    } catch (error) {
      if (!(error instanceof PublishedTransactionExpiredError)) throw error;
      onStage("scheduler appointment expired unminted; reappointing");
      lucid.overrideUTxOs(await lucid.utxosAt(address));
      return appointScheduler();
    }
    await chain.awaitLedgerTime(Number(schedulerStart) + 1);
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
    const unsigned = await builder.complete({ localUPLCEval: true });
    const signed = await unsigned.sign.withWallet().complete();
    const signedCbor = signed.toCBOR();
    const txHash = CML.hash_transaction(
      CML.Transaction.from_cbor_hex(signedCbor).body(),
    ).to_hex();
    await onSigned?.({ txHash, signedCbor });
    let submittedHash: string;
    try {
      submittedHash = await signed.submit();
    } catch (cause) {
      throw new PublishedTransactionSubmissionError(txHash, cause);
    }
    if (submittedHash !== txHash)
      throw new Error(
        "Submitted header hash differs from its signed transaction",
      );
    const headerUnit = toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.headerHash,
    );
    await awaitLandedOrExpired({
      label: `header commit ${block.headerHash}`,
      txHash,
      expiryMs: Number(block.header.endTime) + 1,
      landed: async () =>
        (
          await lucid.utxosAtWithUnit(
            contracts.stateQueue.spendingScriptAddress,
            headerUnit,
          )
        ).length === 1,
    });
    return txHash;
  };
  const attest = async (
    block: PublishedWatcherBlock,
    {
      submitted = [],
      reconcileSubmitted,
      onSubmitted,
    }: PublishedDaAttestOptions = {},
  ): Promise<PublishedDaAttestationOutcome> => {
    const records: PublishedDaTransactionRecord[] = [...submitted];
    const stateQueueUnit = toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.headerHash,
    );
    const attestationUnit = SDK.daAttestationUnit(
      contracts.daAttestation,
      block.headerHash,
    );
    const targetOutputs = async () => {
      const outputs = await lucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        stateQueueUnit,
      );
      if (outputs.length > 1)
        throw new Error(`Ambiguous published state: ${stateQueueUnit}`);
      return outputs;
    };
    const targetMissing = async () => (await targetOutputs()).length === 0;
    /** The live target, or undefined once something consumed it. */
    const freshTarget = async (): Promise<
      SDK.DaAttestationStateQueueTarget | undefined
    > => {
      const [output] = await targetOutputs();
      if (output === undefined) return undefined;
      const stateQueueUtxo = await Effect.runPromise(
        SDK.utxoToStateQueueUTxO(output, contracts.stateQueue.policyId),
      );
      const stateQueueNode = await Effect.runPromise(
        SDK.getStateQueueNodeFromStateQueueDatum(stateQueueUtxo.datum),
      );
      return { stateQueueUtxo, stateQueueNode, headerHash: block.headerHash };
    };
    /**
     * The target disappeared while its attestation was in progress. The only
     * acceptable explanation is the fraud correction of this exact header, so
     * authenticate that consumption and every DA transaction already sent.
     */
    const reconcile = async (): Promise<PublishedDaTargetCorrection> => {
      if (
        readHeaderConsumptions === undefined ||
        readConfirmedTransaction === undefined
      )
        throw new Error(
          `Expected one actual published state: ${stateQueueUnit}`,
        );
      onStage(`DA target ${block.headerHash} consumed; reconciling correction`);
      const deadline = chain.now() + CONSUMPTION_INDEX_GRACE_MS;
      let consumptions = await readHeaderConsumptions(stateQueueUnit);
      while (consumptions.length === 0 && chain.now() <= deadline) {
        await chain.delaySlots(1);
        consumptions = await readHeaderConsumptions(stateQueueUnit);
      }
      return reconcilePublishedDaTargetConsumption({
        headerHash: block.headerHash,
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        consumptions,
        attestationOutputs: await lucid.utxosAtWithUnit(
          contracts.daAttestation.spendingScriptAddress,
          attestationUnit,
        ),
        submitted: records,
        readConfirmedTransaction,
      });
    };
    /** Whether any output of the transaction is indexed: spent outputs vanish, wallet change stays. */
    const anyOutputLanded = async (txHash: string, signedCbor: string) => {
      const outputs = CML.Transaction.from_cbor_hex(signedCbor)
        .body()
        .outputs();
      return (
        (
          await lucid.config().provider!.getUtxosByOutRef(
            Array.from({ length: outputs.len() }, (_, outputIndex) => ({
              txHash,
              outputIndex,
            })),
          )
        ).length > 0
      );
    };
    /**
     * Sign, record, submit and wait. Init references the target and apply
     * spends it, so a correction that removes the target strands them; the
     * caller then reconciles instead of waiting for a block that cannot come.
     */
    const submit = async (
      step: PublishedDaTransactionStep,
      build: () => Promise<TxBuilder>,
      expiryMs?: number,
    ): Promise<PublishedDaTransactionRecord | "consumed"> => {
      const unsigned = await (await build()).complete({ localUPLCEval: true });
      const signed = await unsigned.sign.withWallet().complete();
      const signedCbor = signed.toCBOR();
      const txHash = CML.hash_transaction(
        CML.Transaction.from_cbor_hex(signedCbor).body(),
      ).to_hex();
      const record = { step, txHash, signedCbor };
      records.push(record);
      await onSubmitted?.(record);
      try {
        if ((await signed.submit()) !== txHash)
          throw new Error(
            "Submitted DA transaction changed its signed body hash",
          );
      } catch (error) {
        if (await targetMissing()) return "consumed";
        throw error;
      }
      const bound = expiryMs ?? chain.now() + DA_SUBMISSION_TIMEOUT_MS;
      let missingSince: number | undefined;
      for (;;) {
        if (await anyOutputLanded(txHash, signedCbor)) {
          lucid.overrideUTxOs(await lucid.utxosAt(address));
          return record;
        }
        // The apply spends the target itself, so its absence alone does not
        // name the spender; only an absence our outputs never follow does.
        if (await targetMissing()) {
          missingSince ??= chain.now();
          if (chain.now() - missingSince > OWN_SPEND_VISIBILITY_GRACE_MS)
            return "consumed";
        } else missingSince = undefined;
        if (chain.now() > bound + EXPIRY_GRACE_MS) {
          if (expiryMs !== undefined)
            throw new PublishedTransactionExpiredError(
              `DA ${step}`,
              txHash,
              expiryMs,
            );
          throw new Error(
            `DA ${step} ${txHash} reached no block within ${DA_SUBMISSION_TIMEOUT_MS}ms`,
          );
        }
        await chain.delaySlots(1);
      }
    };
    const daParamsUtxo = await one(
      contracts.daParamsGovernor.spendingScriptAddress,
      SDK.daParamsUnit(contracts.daParamsGovernor),
    );
    let existingTarget = await freshTarget();
    if (existingTarget === undefined) return reconcile();
    if (submitted.length > 0) {
      if (reconcileSubmitted === undefined)
        throw new Error(
          "Retained DA attempts require exact signed transaction reconciliation",
        );
      for (const record of submitted) await reconcileSubmitted(record);
      existingTarget = await freshTarget();
      if (existingTarget === undefined) return reconcile();
    }
    if (existingTarget.stateQueueNode.da_attestation !== SDK.NO_DA_ATTESTATION)
      return {
        kind: "attested",
        txHash: existingTarget.stateQueueUtxo.utxo.txHash,
      };
    // Reconcile retained attempts first, but do not fund init or signatures
    // when this immutable header can no longer accept the resulting apply.
    // The apply path checks again after those transactions have confirmed.
    await Effect.runPromise(
      SDK.daAttestationApplyValidityRangeProgram({
        currentTime: BigInt(lucid.slotToUnixTime(lucid.currentSlot())),
        headerEndTime: existingTarget.stateQueueNode.header.endTime,
      }),
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
    const existingAttestations = await lucid.utxosAtWithUnit(
      contracts.daAttestation.spendingScriptAddress,
      attestationUnit,
    );
    if (existingAttestations.length === 0) {
      const initTarget = await freshTarget();
      if (initTarget === undefined) return reconcile();
      onStage(`DA attestation init ${block.headerHash}`);
      const init = await submit("init", async () =>
        Effect.runPromise(
          SDK.incompleteInitDaAttestationTxProgram(lucid, contracts, {
            daParamsUtxo,
            daParamsDatum,
            target: initTarget,
            referenceScripts,
            attestationOutputLovelace: availabilityParameters.da_bond_lovelace,
            rescueBeneficiary: await Effect.runPromise(
              SDK.addressDataFromBech32(address),
            ),
            availabilityCommitment,
          }),
        ),
      );
      if (init === "consumed") return reconcile();
    }
    const fetchAttestation = async (): Promise<SDK.DaAttestationUtxo> => {
      const utxo = await one(
        contracts.daAttestation.spendingScriptAddress,
        attestationUnit,
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
      const signatures = await submit("signatures", async () =>
        Effect.runPromise(
          SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
            daParamsUtxo,
            daParamsDatum,
            attestation: pending,
            witnesses,
            referenceScripts,
          }),
        ),
      );
      if (signatures === "consumed") return reconcile();
    } else if (pending.datum.attestation_count < pending.datum.da_threshold) {
      throw new Error(
        "Journey attestation has an unexpected partial signature set",
      );
    }
    // The apply carries a bounded validity range. Like a commit, one that
    // closes unminted is rebuilt with a fresh interval; the fresh lookups
    // first notice a target that was corrected or attested meanwhile.
    let applied: PublishedDaTransactionRecord | "consumed";
    let attestation: SDK.DaAttestationUtxo;
    for (;;) {
      const target = await freshTarget();
      if (target === undefined) return reconcile();
      if (target.stateQueueNode.da_attestation !== SDK.NO_DA_ATTESTATION)
        return { kind: "attested", txHash: target.stateQueueUtxo.utxo.txHash };
      const validityRange = await Effect.runPromise(
        SDK.daAttestationApplyValidityRangeProgram({
          currentTime: BigInt(lucid.slotToUnixTime(lucid.currentSlot())),
          headerEndTime: block.header.endTime,
        }),
      );
      onStage(`DA attestation apply ${block.headerHash}`);
      attestation = await fetchAttestation();
      try {
        applied = await submit(
          "apply",
          async () =>
            Effect.runPromise(
              SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
                lucid,
                contracts,
                {
                  hubOracleRefInput: (
                    await Effect.runPromise(
                      SDK.fetchHubOracleUTxOProgram(lucid, {
                        hubOracleAddress:
                          contracts.hubOracle.spendingScriptAddress,
                        hubOraclePolicyId: contracts.hubOracle.policyId,
                      }),
                    )
                  ).utxo,
                  daParamsUtxo,
                  daParamsDatum,
                  target,
                  attestation,
                  referenceScripts,
                  validityRange,
                },
              ),
            ),
          Number(validityRange.validTo),
        );
        break;
      } catch (error) {
        if (!(error instanceof PublishedTransactionExpiredError)) throw error;
        onStage(`${error.label} expired unminted; rebuilding the apply`);
        lucid.overrideUTxOs(await lucid.utxosAt(address));
      }
    }
    if (applied === "consumed") return reconcile();
    await verifyPublishedDaAttestationReceipt({
      ...applied,
      readConfirmedTransaction,
      readLiveAttestation: async () => {
        const live = await freshTarget();
        if (live === undefined)
          throw new Error(
            `A proof already consumed header ${block.headerHash}; its DA receipt needs the confirmed-transaction reader`,
          );
        return live.stateQueueNode.da_attestation;
      },
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueueUnit,
      headerHash: block.headerHash,
      bondAssetName: SDK.daAvailabilityBondAssetName(
        SDK.outputReferenceFromUTxO(attestation.utxo),
      ),
    });
    return { kind: "attested", txHash: applied.txHash };
  };
  return {
    address,
    operatorVkey,
    operatorActive,
    onboardOperator,
    commit,
    attest,
    awaitConfirmed,
  };
};
