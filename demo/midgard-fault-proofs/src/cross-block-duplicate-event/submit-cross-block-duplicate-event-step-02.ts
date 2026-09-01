import {
  assertConfirmedDuplicateEventV1,
  commitCountedRootProgram,
  type CommittedDuplicateEventProofV1,
  CrossBlockDuplicateEventStep02Datum,
  CrossBlockDuplicateEventStep02SpendRedeemer,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
  SettlementDatum,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { CrossBlockDuplicateEventContractsV1 } from "./contracts-v1.js";
import {
  crossBlockDuplicateEventSubmitError,
  requireCrossBlockDuplicateEventReferenceScriptV1,
  requireCrossBlockDuplicateEventStep02StateV1,
  requireCrossBlockDuplicateEventThreadV1,
} from "./submit-common-v1.js";

export type SubmitCrossBlockDuplicateEventStep02Result = {
  readonly txHash: string;
  readonly fraudProofOutRef: string;
  readonly fraudProofUnit: string;
  readonly challengedHeaderHash: string;
  readonly settledHeaderHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitCrossBlockDuplicateEventStep02 = async ({
  lucid,
  contracts,
  signer,
  threadOutRef,
  settlementOutRef,
  settledHeaderHash,
  settledEvent,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: CrossBlockDuplicateEventContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly settlementOutRef: string;
  readonly settledHeaderHash: string;
  readonly settledEvent: CommittedDuplicateEventProofV1;
  /** Mandatory published step-02 reference script. */
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitCrossBlockDuplicateEventStep02Result> => {
  if (!/^[0-9a-f]{56}$/u.test(settledHeaderHash)) {
    throw crossBlockDuplicateEventSubmitError(
      "settled header hash must be 28-byte lowercase hex",
    );
  }
  const [{ threadUtxo, threadToken }, settlementUtxo] = await Promise.all([
    requireCrossBlockDuplicateEventThreadV1({
      lucid,
      contracts,
      threadOutRef,
      stepIndex: 1,
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(settlementOutRef, "--settlement-out-ref"),
      label: "cross-block-duplicate-event confirmed settlement",
    }),
  ]);
  const state = requireCrossBlockDuplicateEventStep02StateV1({
    threadUtxo,
    signer,
    schema: CrossBlockDuplicateEventStep02Datum,
  });
  if (state.challenged_header_hash !== threadToken.fraudulentHeaderHash) {
    throw crossBlockDuplicateEventSubmitError(
      "step-02 state header does not match the computation-thread asset name",
    );
  }
  assertConfirmedDuplicateEventV1({ state, settledHeaderHash, settledEvent });
  const settlementUnit = toUnit(state.settlement_policy_id, settledHeaderHash);
  if (
    settlementUtxo.assets[settlementUnit] !== 1n ||
    settlementUtxo.datum == null
  ) {
    throw crossBlockDuplicateEventSubmitError(
      "settlement reference does not carry the authentic historical-header NFT and inline datum",
    );
  }
  const settlementDatum = Data.from(settlementUtxo.datum, SettlementDatum);
  const opening =
    "CommittedDuplicateDepositV1" in settledEvent
      ? {
          membership: settledEvent.CommittedDuplicateDepositV1.membership,
          domain: ROOT_DOMAINS.deposits,
          settlementRoot: settlementDatum.deposits_root,
        }
      : "CommittedDuplicateWithdrawalV1" in settledEvent
        ? {
            membership: settledEvent.CommittedDuplicateWithdrawalV1.membership,
            domain: ROOT_DOMAINS.withdrawals,
            settlementRoot: settlementDatum.withdrawals_root,
          }
        : {
            membership:
              settledEvent.CommittedDuplicateForcedTransactionV1.membership,
            domain: ROOT_DOMAINS.forcedTransactionsV1,
            settlementRoot: settlementDatum.forced_transactions_root,
          };
  const { membership, domain, settlementRoot } = opening;
  const derived = await Effect.runPromise(
    commitCountedRootProgram({
      domain,
      phasRoot: membership.phas_root,
      count: membership.count,
    }),
  );
  if (
    membership.domain !== domain ||
    membership.root !== settlementRoot ||
    derived !== settlementRoot
  ) {
    throw crossBlockDuplicateEventSubmitError(
      "settled event opening does not match the authentic settlement datum",
    );
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let layout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        fraudProofMintRedeemerIndex: bigint;
      }
    | undefined;
  let threadMintRedeemerIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "cross-block-duplicate-event step 02",
    );
    const resolved = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "cross-block-duplicate-event step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "cross-block-duplicate-event fraud-proof output",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        "cross-block-duplicate-event fraud-proof mint",
      ),
    };
    layout = resolved;
    return Data.to(
      {
        Continue: [
          {
            input_index: resolved.inputIndex,
            output_index: resolved.outputIndex,
            fraud_proof_mint_redeemer_index:
              resolved.fraudProofMintRedeemerIndex,
            settlement_ref_input_index: requireReferenceInputIndex(
              ctx,
              settlementUtxo,
              "cross-block-duplicate-event settlement",
            ),
            settled_event: settledEvent,
          },
        ],
      },
      CrossBlockDuplicateEventStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "cross-block-duplicate-event computation-thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "cross-block-duplicate-event fraud-proof mint",
    );
    threadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      "cross-block-duplicate-event computation-thread burn",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: threadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "cross-block-duplicate-event computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "cross-block-duplicate-event fraud-proof mint",
  });
  const referenceInputs = [
    settlementUtxo,
    requireCrossBlockDuplicateEventReferenceScriptV1({
      utxo: referenceScriptUtxo,
      contracts,
      stepIndex: 1,
    }),
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom(referenceInputs)
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, mintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [fraudProofUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(base),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined || threadMintRedeemerIndex === undefined) {
    throw crossBlockDuplicateEventSubmitError(
      "step-02 layout was not resolved",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof cross-block-duplicate-event step-02",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[1].spendingScript,
        },
        {
          role: "V1 fraud-proof computation-thread minting",
          utxo: witnessReferenceScripts?.computationThreadMint,
          expectedScript: contracts.computationThread.mintingScript,
        },
        {
          role: "V1 fraud-proof token minting",
          utxo: witnessReferenceScripts?.fraudProofMint,
          expectedScript: contracts.fraudProof.mintingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw crossBlockDuplicateEventSubmitError(
      `step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    fraudProofOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudProofUnit,
    challengedHeaderHash: state.challenged_header_hash,
    settledHeaderHash,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
