/** Authenticate one accepted native-V1 transaction and bind a Q35 claim. */
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core";
import {
  HUB_ORACLE_ASSET_NAME,
  type NativeTxInclusionCarriage,
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
  parseSubmitStep01TxInclusion,
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary-v1.js";
import type { NetworkIdContracts } from "./contracts-v1.js";
import {
  findNetworkIdFaults,
  type RetainedDaNetworkIdEvidence,
} from "./evidence-v1.js";
import type { PreparedNetworkIdProof } from "./prepare-v1.js";
import {
  networkIdStepLabel,
  networkIdSubmitError,
  requireNetworkIdReferenceScript,
  requireNetworkIdThreadUtxo,
} from "./submit-common-v1.js";

const STEP_LABEL = networkIdStepLabel(0);

export type SubmitNetworkIdStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly state: NetworkIdStep02State;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitNetworkIdStep01 = async ({
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
  readonly prepared: PreparedNetworkIdProof;
  /** Mandatory published step-01 reference script. */
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNetworkIdStep01Result> => {
  if (prepared.expectedNetworkId !== contracts.expectedNetworkId) {
    throw networkIdSubmitError(
      "prepared evidence targets a different deployed network id",
    );
  }
  const canonicalTx = decodeMidgardNativeTxFullFromCanonicalCbor(
    Buffer.from(prepared.nativeTxCanonicalCbor, "hex"),
  );
  const canonicalCompactCbor = encodeMidgardNativeTxCompact(
    canonicalTx.compact,
  ).toString("hex");
  const canonicalTxId = computeMidgardNativeTxId(canonicalTx).toString("hex");
  if (
    canonicalCompactCbor !== prepared.nativeTxCompactCbor ||
    canonicalTxId !== prepared.badTxId
  ) {
    throw networkIdSubmitError(
      "prepared canonical transaction, compact transaction, and transaction id are not one item",
    );
  }
  const retained: RetainedDaNetworkIdEvidence = {
    source: "retained-da",
    evidenceSourceId: "prepared-canonical-block-evidence",
    nativeTxCanonicalCbor: prepared.nativeTxCanonicalCbor,
  };
  const detected = findNetworkIdFaults({
    evidence: retained,
    expectedNetworkId: contracts.expectedNetworkId,
  });
  if (
    !detected.some(
      (claim) =>
        claim.kind === prepared.faultClaim.kind &&
        (claim.kind === "transaction-network" ||
          (prepared.faultClaim.kind === "output-network" &&
            claim.outputIndex === prepared.faultClaim.outputIndex)),
    )
  ) {
    throw networkIdSubmitError(
      "prepared canonical transaction does not contain the requested fault",
    );
  }
  const txInclusion = parseSubmitStep01TxInclusion(prepared.txInclusion);
  if (
    txInclusion.nativeTxId !== prepared.badTxId ||
    txInclusion.nativeTxCompactCbor !== prepared.nativeTxCompactCbor
  ) {
    throw networkIdSubmitError(
      "prepared inclusion proof does not name the canonical fault transaction",
    );
  }
  requireNativeTxMatchesCompactCbor(txInclusion);
  if (txInclusion.nativeTx.validity_code !== 0n) {
    throw networkIdSubmitError(
      "only an accepted code-0 transaction can violate network-id rules",
    );
  }

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
      "thread, state-queue header, and prepared evidence do not identify one block",
    );
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(network, phasScript);
  const membershipCarriage = witnessWithdrawalValidatorCarriage({
    script: phasScript,
    referenceUtxo: witnessReferenceScripts.phasMembershipWithdraw,
    label: `${STEP_LABEL} PHAS membership`,
  });
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    stepReference,
    ...membershipCarriage.referenceInputs,
  ];
  const state: NetworkIdStep02State = {
    bad_tx_id: prepared.badTxId,
    committed_tx_network_id: txInclusion.nativeTx.body.network_id,
    expected_network_id: contracts.expectedNetworkId,
    fault: prepared.fault,
    post_utxo: null,
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
    const carriage: NativeTxInclusionCarriage = {
      RedeemerCarriedInclusion: [
        {
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
          native_tx_id: txInclusion.nativeTxId,
          l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
          transactions_phas_root: txInclusion.transactionsPhasRoot,
          tx_membership_proof: txInclusion.txMembershipProof,
          inclusion_proof_script_withdraw_redeemer_index:
            requireWithdrawalRedeemerIndex(
              ctx,
              phasRewardAddress,
              `${STEP_LABEL} membership`,
            ),
        },
      ],
    };
    return Data.to(
      {
        Continue: [
          {
            tx_inclusion: carriage,
            post_utxo_membership: null,
            forced_source: null,
            fault: prepared.fault,
          },
        ],
      } as never,
      NetworkIdStep01SpendRedeemerSchema,
    );
  }) satisfies BuildTxWithRedeemer;

  const chained = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
    .withdraw(
      phasRewardAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.l2TransactionSourceCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await membershipCarriage
    .attach(chained)
    .complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw networkIdSubmitError("step-01 layout was not resolved");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof network-id step-01",
        utxo: stepReference,
        expectedScript: contracts.steps[0].spendingScript,
      }),
      workflowReferenceScript({
        role: "membership proof withdrawal",
        utxo: witnessReferenceScripts.phasMembershipWithdraw,
        expectedScript: phasScript,
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
