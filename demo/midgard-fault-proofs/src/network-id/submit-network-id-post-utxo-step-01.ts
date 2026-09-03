/** Authenticate a culpable post-block UTxO network violation and bind Q35. */
import {
  decodeMidgardAddressBytes,
  decodeMidgardLedgerOutputCommitment,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  NetworkIdStep01SpendRedeemerSchema,
  NetworkIdStep02DatumSchema,
  type NetworkIdStep02State,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunk,
  requireBuiltChunkReferenceIndices,
  walletInputsExcludingChunks,
} from "../proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireInitialStepDatum,
  selectFeeInput,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary.js";
import type { NetworkIdContracts } from "./contracts.js";
import type { PreparedNetworkIdPostUtxoProof } from "./prepare.js";
import {
  networkIdStepLabel,
  networkIdSubmitError,
  requireNetworkIdReferenceScript,
  requireNetworkIdThreadUtxo,
} from "./submit-common.js";
import type { SubmitNetworkIdStep01Result } from "./submit-network-id-step-01.js";

const STEP_LABEL = `${networkIdStepLabel(0)} post-UTxO`;

const inspectPrepared = ({
  prepared,
  expectedNetworkId,
}: {
  readonly prepared: PreparedNetworkIdPostUtxoProof;
  readonly expectedNetworkId: bigint;
}): void => {
  if (prepared.expectedNetworkId !== expectedNetworkId) {
    throw networkIdSubmitError(
      "prepared post-UTxO evidence targets a different deployed network id",
    );
  }
  const key = encodeMidgardSpendInputItem({
    txId: Buffer.from(prepared.outRef.transactionId, "hex"),
    outputIndex: Number(prepared.outRef.outputIndex),
  }).toString("hex");
  if (key !== prepared.outRefKeyCbor) {
    throw networkIdSubmitError(
      "prepared post-UTxO key does not canonically encode its out-ref",
    );
  }
  const descriptor = decodeMidgardLedgerOutputCommitment(
    Buffer.from(prepared.descriptorCbor, "hex"),
  );
  if (BigInt(descriptor.outputIndex) !== prepared.outRef.outputIndex) {
    throw networkIdSubmitError(
      "prepared post-UTxO descriptor index does not match its out-ref",
    );
  }
  const observed = BigInt(
    decodeMidgardAddressBytes(descriptor.address).networkId,
  );
  if (
    observed === expectedNetworkId ||
    prepared.faultClaim.observedNetworkId !== observed ||
    prepared.fault === "TransactionNetwork" ||
    typeof prepared.fault !== "object" ||
    !("OutputNetworkUtxo" in prepared.fault) ||
    prepared.fault.OutputNetworkUtxo.observed_network_id !== observed
  ) {
    throw networkIdSubmitError(
      "prepared post-UTxO descriptor does not contain the claimed network violation",
    );
  }
  if (prepared.predecessor !== "Introduced") {
    const previousCbor =
      prepared.predecessor.NetworkChanged.previous_descriptor_cbor;
    if (previousCbor === prepared.descriptorCbor) {
      throw networkIdSubmitError(
        "post-UTxO network-change evidence must change the descriptor",
      );
    }
    const previous = decodeMidgardLedgerOutputCommitment(
      Buffer.from(previousCbor, "hex"),
    );
    if (
      BigInt(previous.outputIndex) !== prepared.outRef.outputIndex ||
      BigInt(decodeMidgardAddressBytes(previous.address).networkId) !==
        expectedNetworkId
    ) {
      throw networkIdSubmitError(
        "post-UTxO predecessor descriptor is not an expected-network value for the same out-ref",
      );
    }
  }
};

export const submitNetworkIdPostUtxoStep01 = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  prepared,
  referenceScriptUtxo,
  publishedProofChunks,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: NetworkIdContracts;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly prepared: PreparedNetworkIdPostUtxoProof;
  /** Mandatory published Q35 step-01 reference script. */
  readonly referenceScriptUtxo: UTxO;
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNetworkIdStep01Result> => {
  inspectPrepared({
    prepared,
    expectedNetworkId: contracts.expectedNetworkId,
  });
  const { threadUtxo, threadToken } = await requireNetworkIdThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  const stepReference = requireNetworkIdReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  const [stateQueueBlockUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "state-queue block out-ref"),
      label: `${STEP_LABEL} state-queue block`,
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: `${STEP_LABEL} hub oracle`,
    }),
  ]);
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (
    headerHash !== threadToken.fraudulentHeaderHash ||
    headerHash !== prepared.headerHash
  ) {
    throw networkIdSubmitError(
      "thread, state-queue header, and prepared post-UTxO evidence do not identify one block",
    );
  }
  const node = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(getHeaderFromStateQueueDatum(node));
  if (header.utxosRoot !== prepared.postUtxosRoot) {
    throw networkIdSubmitError(
      `prepared post-UTxO root ${prepared.postUtxosRoot} does not match header.utxos_root ${header.utxosRoot}`,
    );
  }
  if (header.prevUtxosRoot !== prepared.prevUtxosRoot) {
    throw networkIdSubmitError(
      `prepared predecessor root ${prepared.prevUtxosRoot} does not match header.prev_utxos_root ${header.prevUtxosRoot}`,
    );
  }

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const carriedByChunks = chunks.length > 0;
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const chunkedVerifyScript = chunkedVerifyWithdrawalScript(blueprint);
  const chunkedVerifyRewardAddress = phasMembershipRewardAddress(
    network,
    chunkedVerifyScript,
  );
  const membershipCarriage = carriedByChunks
    ? witnessWithdrawalValidatorCarriage({
        script: chunkedVerifyScript,
        referenceUtxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
        label: `${STEP_LABEL} chunked post membership`,
      })
    : witnessWithdrawalValidatorCarriage({
        script: phasMembershipScript,
        referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
        label: `${STEP_LABEL} PHAS post membership`,
      });
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    ...chunks.map((chunk) => chunk.utxo),
    stepReference,
    ...membershipCarriage.referenceInputs,
  ];
  const resolvedChunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks,
    label: STEP_LABEL,
  });
  const state: NetworkIdStep02State = {
    bad_tx_id: prepared.outRef.transactionId,
    committed_tx_network_id: 255n,
    expected_network_id: contracts.expectedNetworkId,
    fault: prepared.fault,
    post_utxo: {
      out_ref: prepared.outRef,
      descriptor_cbor: prepared.descriptorCbor,
      prev_utxos_root: header.prevUtxosRoot,
      predecessor: prepared.predecessor,
    },
    forced_source_key: null,
  };
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state } as never,
    NetworkIdStep02DatumSchema,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let layout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks,
      derived: resolvedChunkIndices,
      label: STEP_LABEL,
    });
    const membership = carriedByChunks
      ? {
          PublishedChunkMembership: [
            {
              ordered_chunk_reference_input_indices: resolvedChunkIndices,
            },
          ],
        }
      : {
          RedeemerCarriedMembership: {
            membership_proof: prepared.membershipProof,
            membership_proof_script_redeemer_index:
              requireWithdrawalRedeemerIndex(
                ctx,
                phasRewardAddress,
                `${STEP_LABEL} PHAS post membership`,
              ),
          },
        };
    return Data.to(
      {
        Continue: [
          {
            tx_inclusion: null,
            post_utxo_membership: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              hub_ref_input_index: requireReferenceInputIndex(
                ctx,
                hubOracleUtxo,
                `${STEP_LABEL} hub oracle`,
              ),
              state_queue_node_ref_input_index: requireReferenceInputIndex(
                ctx,
                stateQueueBlockUtxo,
                `${STEP_LABEL} state-queue node`,
              ),
              out_ref: prepared.outRef,
              descriptor_cbor: prepared.descriptorCbor,
              membership,
              predecessor: prepared.predecessor,
            },
            forced_source: null,
            fault: prepared.fault,
          },
        ],
      } as never,
      NetworkIdStep01SpendRedeemerSchema,
    );
  }) satisfies BuildTxWithRedeemer;
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs);
  const withCarriage = carriedByChunks
    ? base.withdraw(chunkedVerifyRewardAddress, 0n, ((_ctx) =>
        chunkedMembershipClaimRedeemer({
          merkleRoot: prepared.postUtxosRoot,
          keyBytes: prepared.outRefKeyCbor,
          valueBytes: prepared.descriptorCbor,
          orderedChunkReferenceInputIndices: resolvedChunkIndices,
        })) satisfies BuildTxWithRedeemer)
    : base.withdraw(
        phasRewardAddress,
        0n,
        encodeRawPhasMembershipProofRedeemer({
          root: prepared.postUtxosRoot,
          keyBytes: prepared.outRefKeyCbor,
          valueBytes: prepared.descriptorCbor,
          membershipProofCbor: prepared.membershipProofCbor,
        }),
      );
  const paid = withCarriage.pay
    .ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await membershipCarriage
    .attach(paid)
    .complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw networkIdSubmitError("post-UTxO step-01 layout was not resolved");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "network-id-step-01",
        utxo: stepReference,
        expectedScript: contracts.steps[0].spendingScript,
      }),
      workflowReferenceScript({
        role: carriedByChunks
          ? "network-id-post-membership-chunked-verify"
          : "network-id-post-membership-phas",
        utxo: membershipCarriage.referenceInputs[0],
        expectedScript: carriedByChunks
          ? chunkedVerifyScript
          : phasMembershipScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw networkIdSubmitError(
      `provider returned transaction hash ${txHash}, expected ${expectedTxHash}`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudulentHeaderHash: headerHash,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    state,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
