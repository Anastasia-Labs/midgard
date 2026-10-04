import { createHash } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core";
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  fetchCorrectionLockUTxOProgram,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  FraudProofTokenDatum,
  getHeaderFromStateQueueDatum,
  HUB_ORACLE_ASSET_NAME,
  incompleteRemoveFraudulentBlocksLinkTxProgram,
  incompleteRemoveLastFraudulentBlockHeaderTxProgram,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  SCHEDULER_ASSET_NAME,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseContractDeploymentInfo } from "./inspect-contracts.js";
import { parseHex } from "./json-file.js";
import {
  buildExplicitRemovalContracts,
  buildRemovalContracts,
} from "./remove-fraudulent-block.assemble-removal-contracts.js";
import {
  assertExactFraudSlashLovelaceConservation,
  fraudRemovalUsesWalletCoinSelection,
  fraudSlashEconomicsFromDeploymentManifest,
  fraudSlashFundingProofSource,
  readFraudSlashFundingAuthority,
  type RemoveFraudulentBlockFraudCategory,
  type RemoveFraudulentBlockLayout,
  resolveFraudSlashEconomics,
  slashFundingAuthorities,
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
} from "./remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import { resolveOperatorSlashingPlan } from "./remove-fraudulent-block.build-active-slashing-inputs.js";
import {
  loadStateQueueTopology,
  referenceScriptOutRefs,
  requireStateQueueHeaderHash,
  resolveReferenceScripts,
} from "./remove-fraudulent-block.load-state-queue-topology.js";
import { makeStateQueueRemoveMintRedeemer } from "./remove-fraudulent-block.make-state-queue-remove-mint-redeemer.js";
import {
  createLocalStateQueueMutationLeaseCoordinator,
  layoutToJson,
  type RemoveFraudulentBlockExplicitCategory,
  type RemoveTransactionKind,
  type RemoveTransactionResult,
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  type SubmitRemoveFraudulentBlockResult,
} from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import { resolveRegisteredOperatorRemovalWitness } from "./remove-fraudulent-block.resolve-registered-operator-removal-witness.js";
import { buildSlashingInputs } from "./remove-fraudulent-block.resolve-state-queue-slashing-approach.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  outRefLabel,
  requireSingletonUtxo,
  type ResolvedProverSigner,
} from "./runtime.js";
import { selectFeeInput } from "./step-support.js";
import { computeFraudProofReleaseEconomicsPolicyDigest } from "./workflow/release-economics-policy.js";
import {
  CapturedLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
  workflowTransactionInputOutRefs,
} from "./workflow/transaction-boundary.js";

export const submitRemoveFraudulentBlock = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  fraudCategory = "doubleSpend",
  fraudulentHeaderHash,
  awaitConfirmation = true,
  requireReferenceScripts = true,
  validFrom,
  validTo,
  stateQueueMutationLeaseCoordinator = createLocalStateQueueMutationLeaseCoordinator(),
  fraudProverRewardLovelace,
  preSubmitBoundary,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  /**
   * Canonical catalogue category resolved from the production manifest, or —
   * for a family that predates its catalogue registration — the explicit
   * already-resolved category record
   * (see {@link RemoveFraudulentBlockExplicitCategory}).
   */
  readonly fraudCategory?:
    | RemoveFraudulentBlockFraudCategory
    | RemoveFraudulentBlockExplicitCategory;
  readonly fraudulentHeaderHash: string;
  readonly awaitConfirmation?: boolean;
  readonly requireReferenceScripts?: boolean;
  readonly validFrom?: bigint;
  readonly validTo?: bigint;
  /**
   * Coordinates a non-tail removal's successor peels. Defaults to
   * {@link createLocalStateQueueMutationLeaseCoordinator}, which is the only
   * coordination mode: a removal that loses a race to a competing commit or
   * merge fails and must be re-run (the watcher's workflow orchestrator
   * retries it until confirmed). Callers such as the watcher may still pass
   * one explicitly.
   */
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  /**
   * Optional assertion of the deployment-manifest `fraudProverRewardLovelace`;
   * omission still routes the release profile's mandatory nonzero reward.
   */
  readonly fraudProverRewardLovelace?: bigint;
  /** Production workflow seam for each descendant/target removal tx. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitRemoveFraudulentBlockResult> => {
  const headerHash = parseHex(
    fraudulentHeaderHash,
    "--fraudulent-header-hash",
    28,
  );
  const canonicalManifest =
    typeof deploymentInfo === "object" &&
    deploymentInfo !== null &&
    "manifestId" in deploymentInfo
      ? structuredClone(verifyFinalizedDeploymentManifest(deploymentInfo))
      : null;
  if (canonicalManifest !== null && canonicalManifest.network !== network) {
    throw new Error(
      "fraud removal network differs from its finalized manifest",
    );
  }
  const deploymentDocument = canonicalManifest ?? deploymentInfo;
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentDocument);
  const deploymentEconomics =
    fraudSlashEconomicsFromDeploymentManifest(deploymentDocument);
  const contracts =
    typeof fraudCategory === "string"
      ? await buildRemovalContracts({
          blueprint,
          deploymentInfo: deploymentDocument,
          network,
          fraudCategory,
        })
      : buildExplicitRemovalContracts({
          deploymentInfo: deploymentDocument,
          network,
          category: fraudCategory,
        });
  if (
    contracts.fraudCategoryId.length !==
    FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT * 2
  ) {
    throw new Error(
      `${contracts.fraudCategory} fraud-proof category id has invalid length.`,
    );
  }

  const referenceScripts = await resolveReferenceScripts({
    lucid,
    deploymentInfo: parsedDeploymentInfo,
    requireReferenceScripts,
  });
  signer.selectWallet(lucid);

  const fraudProofAssetName = contracts.fraudCategoryId + headerHash;
  const fraudProofUnit = toUnit(
    contracts.fraudProofPolicyId,
    fraudProofAssetName,
  );
  const activeOperatorsRootUnit = toUnit(
    contracts.activeOperatorsPolicyId,
    ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  );
  const retiredOperatorsRootUnit = toUnit(
    contracts.retiredOperatorsPolicyId,
    RETIRED_OPERATORS_ROOT_ASSET_NAME,
  );
  const schedulerUnit = toUnit(
    contracts.schedulerPolicyId,
    SCHEDULER_ASSET_NAME,
  );
  const hubOracleUnit = toUnit(
    contracts.hubOraclePolicyId,
    HUB_ORACLE_ASSET_NAME,
  );
  const stateQueueConfig = {
    stateQueueAddress: contracts.stateQueueAddress,
    stateQueuePolicyId: contracts.stateQueuePolicyId,
  } as const;

  const [
    fraudProofUtxo,
    activeOperatorsRootUtxo,
    schedulerUtxo,
    hubOracleUtxo,
  ] = await Promise.all([
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
  ]);

  if (fraudProofUtxo.datum == null) {
    throw new Error(
      `Fraud-proof token UTxO ${outRefLabel(fraudProofUtxo)} is missing datum.`,
    );
  }
  const fraudProofDatum = Data.from(fraudProofUtxo.datum, FraudProofTokenDatum);

  if (
    fraudProverRewardLovelace !== undefined &&
    fraudProverRewardLovelace !== deploymentEconomics.fraudProverRewardLovelace
  ) {
    throw new Error(
      `Fraud-prover reward must equal deployment profile ${deploymentEconomics.profile} amount ${deploymentEconomics.fraudProverRewardLovelace.toString()} lovelace; found ${fraudProverRewardLovelace.toString()}.`,
    );
  }
  const fraudProverRewardPlan = {
    proverEnterpriseAddress: credentialToAddress(network, {
      type: "Key" as const,
      hash: fraudProofDatum.fraud_prover,
    }),
    lovelace: deploymentEconomics.fraudProverRewardLovelace,
  };

  let topology = await loadStateQueueTopology({
    lucid,
    stateQueueAddress: contracts.stateQueueAddress,
    stateQueuePolicyId: contracts.stateQueuePolicyId,
  });
  const initialTarget = topology.nodeByHeaderHash.get(headerHash);
  if (initialTarget === undefined) {
    throw new Error(`State queue does not contain block ${headerHash}.`);
  }
  const initialStateQueueRootOutRef = outRefLabel(topology.root.utxo);
  const initialTargetOutRef = outRefLabel(initialTarget.utxo);
  const initialTargetHasSuccessor =
    topology.successorByHeaderHash.has(headerHash);
  if (!awaitConfirmation && initialTargetHasSuccessor) {
    throw new Error(
      "Removing a non-tail fraudulent block requires --await-confirmation so each successor removal can be confirmed and refetched before the next transaction.",
    );
  }
  let stateQueueMutationLease: StateQueueMutationLease | undefined;
  let stateQueueMutationLeaseReleased = false;
  if (initialTargetHasSuccessor) {
    stateQueueMutationLease =
      await stateQueueMutationLeaseCoordinator.acquire();
    try {
      topology = await loadStateQueueTopology({
        lucid,
        stateQueueAddress: contracts.stateQueueAddress,
        stateQueuePolicyId: contracts.stateQueuePolicyId,
      });
      if (!topology.nodeByHeaderHash.has(headerHash)) {
        throw new Error(
          `State queue no longer contains block ${headerHash} after acquiring the mutation lease.`,
        );
      }
    } catch (error) {
      await stateQueueMutationLease.fail(formatUnknownError(error));
      throw error;
    }
  }

  const txValidityWindow = () => {
    const now = BigInt(Date.now());
    return {
      txValidFrom: validFrom ?? now - STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
      txValidTo: validTo ?? now + STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
    };
  };

  const submitRemovalTransaction = async ({
    kind,
    anchor,
    removed,
  }: {
    readonly kind: RemoveTransactionKind;
    readonly anchor: StateQueueUTxO;
    readonly removed: StateQueueUTxO;
  }): Promise<RemoveTransactionResult> => {
    await stateQueueMutationLease?.renew();
    const correctionLockInput = await Effect.runPromise(
      fetchCorrectionLockUTxOProgram(lucid, {
        correctionLockAddress: contracts.correctionLockAddress,
        hubOraclePolicyId: contracts.hubOraclePolicyId,
      }),
    );
    const removedHeaderHash = await requireStateQueueHeaderHash(removed);
    const removedHeader = await Effect.runPromise(
      getHeaderFromStateQueueDatum(removed.datum),
    );
    const fraudulentOperator = removedHeader.operatorVkey;
    const currentSchedulerUtxo = await requireSingletonUtxo({
      lucid,
      address: contracts.schedulerAddress,
      unit: schedulerUnit,
      label: "scheduler",
    });
    let txLayout: RemoveFraudulentBlockLayout | undefined;
    const operatorSlashingPlan = await resolveOperatorSlashingPlan({
      lucid,
      contracts,
      operator: fraudulentOperator,
      schedulerUtxo: currentSchedulerUtxo,
      activeOperatorsRootUnit,
      retiredOperatorsRootUnit,
    });
    const slashEconomics =
      operatorSlashingPlan.approach === "OperatorAlreadySlashed"
        ? null
        : resolveFraudSlashEconomics(
            deploymentEconomics,
            operatorSlashingPlan.removalPlan.node.utxo.assets.lovelace ?? 0n,
          );
    const { txValidFrom, txValidTo } = txValidityWindow();
    // The scheduler compares the new shift against the ledger's inclusive
    // upper bound, after Lucid converts validTo to an exclusive slot bound.
    const schedulerStartTime =
      BigInt(lucid.slotToUnixTime(lucid.unixTimeToSlot(Number(txValidTo)))) -
      1n;
    const registeredOperatorsElementForSlashing =
      operatorSlashingPlan.approach === "SlashActiveOperator" &&
      operatorSlashingPlan.schedulerPlan.kind === "rewind"
        ? await resolveRegisteredOperatorRemovalWitness({
            utxos: await lucid.utxosAt(contracts.registeredOperatorsAddress),
            address: contracts.registeredOperatorsAddress,
            policyId: contracts.registeredOperatorsPolicyId,
            inclusiveValidityUpperBound: schedulerStartTime,
          })
        : undefined;
    const slashingPlan = buildSlashingInputs({
      plan: operatorSlashingPlan,
      operator: fraudulentOperator,
      contracts,
      hubOracleUtxo,
      ...(registeredOperatorsElementForSlashing === undefined
        ? {}
        : {
            registeredOperatorsElementUtxo:
              registeredOperatorsElementForSlashing,
          }),
      ...(fraudProverRewardPlan === undefined
        ? {}
        : { fraudProverReward: fraudProverRewardPlan }),
    });
    // A legal operator bond tranche is exactly reward + slash fee.  Do not
    // add a wallet fee input to that branch: its change would be an unrelated
    // second payment to the prover whenever the submitter uses the prover's
    // enterprise wallet.  OperatorAlreadySlashed has no bond and still needs
    // an ordinary fee input for descendant cleanup transactions.
    const additionalInputs =
      slashEconomics === null
        ? [selectFeeInput(await lucid.wallet().getUtxos())]
        : [];
    const slashing = slashingPlan.buildSlashing(schedulerStartTime);
    if (slashEconomics !== null) {
      if (slashing.kind === "operatorAlreadySlashed") {
        throw new Error(
          "Bond-backed fraud slash unexpectedly resolved to OperatorAlreadySlashed.",
        );
      }
      assertExactFraudSlashLovelaceConservation({
        stateQueueAnchor: anchor,
        removedStateQueueNode: removed,
        slashing,
        economics: slashEconomics,
      });
    }
    const stateQueueMintRedeemer = makeStateQueueRemoveMintRedeemer({
      kind,
      anchor,
      removed,
      fraudulentOperator,
      fraudulentBlocksHeaderHash: headerHash,
      fraudProofRefInput: fraudProofUtxo,
      yieldRefInput: referenceScripts.stateQueueFraudRemovalWithdraw,
      slashing,
      contracts,
      onLayout: (layout) => {
        txLayout = layout;
      },
    });
    const tx =
      kind === "remove-successor"
        ? incompleteRemoveFraudulentBlocksLinkTxProgram(
            lucid,
            stateQueueConfig,
            {
              fraudulentBlockUTxO: anchor,
              removedBlockUTxO: removed,
              additionalInputs,
              validFrom: txValidFrom,
              validTo: txValidTo,
              fraudulentOperator,
              fraudulentBlocksHeaderHash: headerHash,
              fraudProofRefInput: fraudProofUtxo,
              fraudProofPolicyId: contracts.fraudProofPolicyId,
              hubOracleRefInput: hubOracleUtxo,
              correctionLockInput,
              correctionLockSpendingScript:
                contracts.correctionLockSpendingScript,
              additionalRefInputs: slashingPlan.additionalRefInputs,
              slashing,
              stateQueueSpendingScript: contracts.stateQueueSpendingScript,
              stateQueueMintingScript: contracts.stateQueueMintingScript,
              referenceScripts,
              yieldWitness: {
                referenceInput: referenceScripts.stateQueueFraudRemovalWithdraw,
                script: contracts.stateQueueFraudRemovalWithdrawalScript,
              },
              stateQueueMintRedeemer,
            },
          )
        : incompleteRemoveLastFraudulentBlockHeaderTxProgram(
            lucid,
            stateQueueConfig,
            {
              anchorUTxO: anchor,
              fraudulentBlockUTxO: removed,
              additionalInputs,
              validFrom: txValidFrom,
              validTo: txValidTo,
              fraudulentOperator,
              fraudulentBlocksHeaderHash: headerHash,
              fraudProofRefInput: fraudProofUtxo,
              fraudProofPolicyId: contracts.fraudProofPolicyId,
              hubOracleRefInput: hubOracleUtxo,
              correctionLockInput,
              correctionLockSpendingScript:
                contracts.correctionLockSpendingScript,
              additionalRefInputs: slashingPlan.additionalRefInputs,
              slashing,
              stateQueueSpendingScript: contracts.stateQueueSpendingScript,
              stateQueueMintingScript: contracts.stateQueueMintingScript,
              referenceScripts,
              yieldWitness: {
                referenceInput: referenceScripts.stateQueueFraudRemovalWithdraw,
                script: contracts.stateQueueFraudRemovalWithdrawalScript,
              },
              stateQueueMintRedeemer,
            },
          );
    const feeBoundTx =
      slashEconomics === null
        ? tx
        : tx.setMinFee(slashEconomics.exactFeeLovelace);
    const unsigned = await feeBoundTx.complete({
      // The bond-backed branch is already proven exactly balanced above.
      // Ordinary coin selection would add an unrelated wallet UTxO and let
      // CML absorb a sub-minimum change residual into the fee, violating the
      // exact F04 fee. Collateral selection remains independent and enabled.
      coinSelection: fraudRemovalUsesWalletCoinSelection(
        operatorSlashingPlan.approach,
      ),
      localUPLCEval: true,
    });
    if (txLayout === undefined) {
      throw new Error(
        "BuildTxWithRedeemer did not resolve remove-fraudulent-block layout.",
      );
    }

    const signed = await unsigned.sign.withWallet().complete();
    if (canonicalManifest !== null && slashEconomics !== null) {
      if (operatorSlashingPlan.approach === "OperatorAlreadySlashed") {
        throw new Error(
          "fraud slash funding authority omitted its operator bond",
        );
      }
      const transaction = signed.toTransaction();
      if (transaction.body().fee() !== slashEconomics.exactFeeLovelace) {
        throw new Error(
          "signed fraud slash fee differs from release economics",
        );
      }
      const inputOutRefs = [...workflowTransactionInputOutRefs(signed)].sort();
      const resolvedInputs = await lucid.utxosByOutRef(
        inputOutRefs.map((outRef) => ({
          txHash: outRef.slice(0, 64),
          outputIndex: Number(outRef.slice(65)),
        })),
      );
      const inputs = resolvedInputs
        .map((utxo) =>
          Object.freeze({
            outRef: outRefLabel(utxo),
            resolvedOutputCborHex: utxoToCore(utxo)
              .output()
              .to_canonical_cbor_hex(),
          }),
        )
        .sort((left, right) => left.outRef.localeCompare(right.outRef));
      if (
        inputs.length !== inputOutRefs.length ||
        inputs.some((input, index) => input.outRef !== inputOutRefs[index])
      ) {
        throw new Error(
          "signed fraud slash could not resolve its exact protocol inputs",
        );
      }
      const economicsPolicy = {
        profile: deploymentEconomics.profile,
        requiredBondLovelace:
          deploymentEconomics.requiredBondLovelace.toString(),
        slashingPenaltyLovelace:
          deploymentEconomics.slashingPenaltyLovelace.toString(),
        fraudProverRewardLovelace:
          deploymentEconomics.fraudProverRewardLovelace.toString(),
        inactivitySlashingPenaltyLovelace:
          deploymentEconomics.inactivitySlashingPenaltyLovelace.toString(),
        proverCollateralFloorLovelace:
          deploymentEconomics.proverCollateralFloorLovelace.toString(),
      };
      slashFundingAuthorities.set(
        signed,
        Object.freeze({
          deploymentFingerprint: canonicalManifest.manifestId,
          economicsPolicyDigest:
            computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
          category: contracts.fraudCategory,
          headerHash,
          ...fraudSlashFundingProofSource(fraudProofUtxo, fraudProofUnit),
          removedStateQueueOutRef: outRefLabel(removed.utxo),
          operatorOutRef: outRefLabel(
            operatorSlashingPlan.removalPlan.node.utxo,
          ),
          operatorBondLovelace: (
            operatorSlashingPlan.removalPlan.node.utxo.assets.lovelace ?? 0n
          ).toString(),
          tranche: slashEconomics.tranche,
          exactFeeLovelace: slashEconomics.exactFeeLovelace.toString(),
          rewardLovelace: slashEconomics.fraudProverRewardLovelace.toString(),
          rewardAddress: fraudProverRewardPlan.proverEnterpriseAddress,
          transactionHash: signed.toHash().toLowerCase(),
          transactionBodySha256: createHash("sha256")
            .update(Buffer.from(transaction.body().to_cbor_hex(), "hex"))
            .digest("hex"),
          signedTransactionCborHex: transaction.to_cbor_hex(),
          inputs: Object.freeze(inputs),
        }),
      );
      readFraudSlashFundingAuthority(signed);
    }
    const expectedTxHash = await reachFraudProofPreSubmitBoundary({
      signed,
      referenceScripts: workflowReferenceScriptsUsedByTransaction({
        signed,
        candidates: [
          {
            role: "correction-lock-spend",
            utxo: referenceScripts?.correctionLockSpend,
            expectedScript: contracts.correctionLockSpendingScript,
          },
          {
            role: "state-queue-spend",
            utxo: referenceScripts?.stateQueueSpend,
            expectedScript: contracts.stateQueueSpendingScript,
          },
          {
            role: "state-queue-mint",
            utxo: referenceScripts?.stateQueueMint,
            expectedScript: contracts.stateQueueMintingScript,
          },
          {
            role: "active-operators-spend",
            utxo: referenceScripts?.activeOperatorsSpend,
            expectedScript: contracts.activeOperatorsSpendingScript,
          },
          {
            role: "active-operators-mint",
            utxo: referenceScripts?.activeOperatorsMint,
            expectedScript: contracts.activeOperatorsMintingScript,
          },
          {
            role: "retired-operators-spend",
            utxo: referenceScripts?.retiredOperatorsSpend,
            expectedScript: contracts.retiredOperatorsSpendingScript,
          },
          {
            role: "retired-operators-mint",
            utxo: referenceScripts?.retiredOperatorsMint,
            expectedScript: contracts.retiredOperatorsMintingScript,
          },
          {
            role: "scheduler-spend",
            utxo: referenceScripts?.schedulerSpend,
            expectedScript: contracts.schedulerSpendingScript,
          },
        ],
      }),
      boundary: preSubmitBoundary,
    });
    readFraudSlashFundingAuthority(signed);
    const txHash = await signed.submit();
    if (txHash !== expectedTxHash) {
      throw new Error(
        `Provider returned transaction hash ${txHash}, expected ${expectedTxHash}.`,
      );
    }
    if (awaitConfirmation) {
      await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    }
    await stateQueueMutationLease?.renew();
    return {
      kind,
      txHash,
      removedHeaderHash,
      removedOperator: fraudulentOperator,
      stateQueueBlockOutRef: outRefLabel(removed.utxo),
      operatorNodeOutRef: slashingPlan.removedOperatorNodeOutRef,
      registeredOperatorsElementOutRef:
        slashingPlan.registeredOperatorsElementOutRef,
      slashingApproach: slashingPlan.approach,
      layout: layoutToJson(txLayout),
    };
  };

  try {
    const transactions: RemoveTransactionResult[] = [];
    while (true) {
      const proofedBlock = topology.nodeByHeaderHash.get(headerHash);
      if (proofedBlock === undefined) {
        throw new Error(
          `State queue no longer contains fraud-proved block ${headerHash} before final removal.`,
        );
      }
      const successor = topology.successorByHeaderHash.get(headerHash);
      if (successor === undefined) {
        break;
      }
      transactions.push(
        await submitRemovalTransaction({
          kind: "remove-successor",
          anchor: proofedBlock,
          removed: successor,
        }),
      );
      topology = await loadStateQueueTopology({
        lucid,
        stateQueueAddress: contracts.stateQueueAddress,
        stateQueuePolicyId: contracts.stateQueuePolicyId,
      });
    }

    const finalTarget = topology.nodeByHeaderHash.get(headerHash);
    if (finalTarget === undefined) {
      throw new Error(
        `State queue no longer contains fraud-proved block ${headerHash}.`,
      );
    }
    const finalAnchor = topology.predecessorByHeaderHash.get(headerHash);
    if (finalAnchor === undefined) {
      throw new Error(
        `State queue block ${headerHash} is not reachable from the confirmed-state root.`,
      );
    }
    transactions.push(
      await submitRemovalTransaction({
        kind: "remove-target",
        anchor: finalAnchor,
        removed: finalTarget,
      }),
    );
    const finalTransaction = transactions[transactions.length - 1]!;
    if (stateQueueMutationLease !== undefined) {
      await stateQueueMutationLease.release();
      stateQueueMutationLeaseReleased = true;
    }
    return {
      txHash: finalTransaction.txHash,
      walletSource: signer.source,
      proverAddress: fraudProverRewardPlan.proverEnterpriseAddress,
      fraudProver: fraudProofDatum.fraud_prover,
      fraudCategory: contracts.fraudCategory,
      fraudCategoryId: contracts.fraudCategoryId,
      fraudulentHeaderHash: headerHash,
      stateQueueBlockOutRef: initialTargetOutRef,
      stateQueueRootOutRef: initialStateQueueRootOutRef,
      fraudProofOutRef: outRefLabel(fraudProofUtxo),
      activeOperatorsRootOutRef: outRefLabel(activeOperatorsRootUtxo),
      activeOperatorNodeOutRef:
        finalTransaction.slashingApproach === "SlashActiveOperator"
          ? finalTransaction.operatorNodeOutRef
          : null,
      schedulerOutRef: outRefLabel(schedulerUtxo),
      hubOracleOutRef: outRefLabel(hubOracleUtxo),
      registeredOperatorsElementOutRef:
        transactions.find((tx) => tx.registeredOperatorsElementOutRef !== null)
          ?.registeredOperatorsElementOutRef ?? null,
      referenceScriptOutRefs: referenceScriptOutRefs(referenceScripts),
      transactions,
      layout: finalTransaction.layout,
      awaitedConfirmation: awaitConfirmation,
      stateQueueMutationLease:
        stateQueueMutationLease === undefined
          ? null
          : {
              token: stateQueueMutationLease.token,
              source: stateQueueMutationLease.source,
              released: stateQueueMutationLeaseReleased,
            },
    };
  } catch (error) {
    // A production workflow capture deliberately stops after the exact signed
    // body has passed local evaluation. Its adapter retains and renews the
    // acquired lease across durable intent and submission, so failing it here
    // would reopen the append/removal race in that crash boundary.
    if (error instanceof CapturedLocallyEvaluatedTransaction) {
      throw error;
    }
    if (
      stateQueueMutationLease !== undefined &&
      !stateQueueMutationLeaseReleased
    ) {
      await stateQueueMutationLease.fail(formatUnknownError(error));
    }
    throw error;
  }
};
