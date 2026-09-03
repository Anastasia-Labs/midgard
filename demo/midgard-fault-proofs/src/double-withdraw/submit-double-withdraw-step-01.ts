import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  DoubleWithdrawStep01SpendRedeemer,
  DoubleWithdrawStep02Datum,
  type DoubleWithdrawStep02State,
  doubleWithdrawStep02State,
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  type Header,
  HUB_ORACLE_ASSET_NAME,
  isPayableWithdrawalLeaf,
  OutputReference,
  Proof,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
  type RootMembershipProof,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseHex, requireRecord } from "../json-file.js";
import type { PreparedDoubleWithdrawInclusion } from "../prepare-double-withdraw.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import { requireInitialStepDatum, selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import {
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary-v1.js";
import type { DoubleWithdrawContracts } from "./contracts-v1.js";
import {
  doubleWithdrawSubmitError,
  requireDoubleWithdrawReferenceScript,
  requireDoubleWithdrawThreadUtxo,
} from "./submit-common-v1.js";

export type SubmitDoubleWithdrawInclusion = PreparedDoubleWithdrawInclusion & {
  readonly withdrawalMembershipProof: Proof;
};

export const parseSubmitDoubleWithdrawInclusion = (
  value: unknown,
): SubmitDoubleWithdrawInclusion => {
  const record = requireRecord(value, "--withdrawal-inclusion");
  const withdrawalIdCbor = parseHex(
    record.withdrawalIdCbor,
    "--withdrawal-inclusion.withdrawalIdCbor",
  );
  const withdrawalInfoCbor = parseHex(
    record.withdrawalInfoCbor,
    "--withdrawal-inclusion.withdrawalInfoCbor",
  );
  const withdrawalsPhasRoot = parseHex(
    record.withdrawalsPhasRoot,
    "--withdrawal-inclusion.withdrawalsPhasRoot",
    32,
  );
  const withdrawalMembershipProofCbor = parseHex(
    record.withdrawalMembershipProofCbor,
    "--withdrawal-inclusion.withdrawalMembershipProofCbor",
  );
  return {
    withdrawalIdCbor,
    withdrawalInfoCbor,
    withdrawalsPhasRoot,
    withdrawalMembershipProofCbor,
    withdrawalMembershipProof: Data.from(withdrawalMembershipProofCbor, Proof),
  };
};

export type DerivedDoubleWithdrawMembership = {
  readonly committedWithdrawal: RootMembershipProof<
    OutputReference,
    WithdrawalInfo
  >;
};

export const deriveDoubleWithdrawMembership = async ({
  header,
  inclusion,
}: {
  readonly header: Header;
  readonly inclusion: SubmitDoubleWithdrawInclusion;
}): Promise<DerivedDoubleWithdrawMembership> => {
  const root = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.withdrawals,
      phasRoot: inclusion.withdrawalsPhasRoot,
      count: header.withdrawalCount,
    }),
  );
  if (root !== header.withdrawalsRoot) {
    throw doubleWithdrawSubmitError(
      `withdrawals PHAS root does not open the on-chain counted root: derived=${root} header=${header.withdrawalsRoot}.`,
    );
  }
  const key = Data.from(inclusion.withdrawalIdCbor, OutputReference);
  const value = Data.from(inclusion.withdrawalInfoCbor, WithdrawalInfo);
  if (committedWithdrawalKeyBytes(key) !== inclusion.withdrawalIdCbor) {
    throw doubleWithdrawSubmitError(
      "withdrawal id bytes are not in canonical serialiseData form.",
    );
  }
  if (committedWithdrawalValueBytes(value) !== inclusion.withdrawalInfoCbor) {
    throw doubleWithdrawSubmitError(
      "withdrawal info bytes are not in canonical serialiseData form.",
    );
  }
  return {
    committedWithdrawal: {
      domain: ROOT_DOMAINS.withdrawals,
      root: header.withdrawalsRoot,
      phas_root: inclusion.withdrawalsPhasRoot,
      count: header.withdrawalCount,
      key,
      value,
      proof: inclusion.withdrawalMembershipProof,
    },
  };
};

export type SubmitDoubleWithdrawStep01Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly step02State: DoubleWithdrawStep02State;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitDoubleWithdrawStep01 = async ({
  lucid,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  inclusion,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: DoubleWithdrawContracts;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly inclusion: SubmitDoubleWithdrawInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitDoubleWithdrawStep01Result> => {
  const [{ threadUtxo, threadToken }, hubOracleUtxo, stateQueueBlockUtxo] =
    await Promise.all([
      requireDoubleWithdrawThreadUtxo({
        lucid,
        contracts,
        categoryId,
        stepIndex: 0,
        threadOutRef,
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOraclePolicyId),
        ),
        unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: "double-withdraw step-01 hub oracle",
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          stateQueueBlockOutRef,
          "--state-queue-block-out-ref",
        ),
        label: "double-withdraw step-01 state-queue block",
      }),
    ]);
  requireInitialStepDatum({ threadUtxo, signer });
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw doubleWithdrawSubmitError(
      `state-queue header ${stateQueueHeaderHash} does not match thread ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  const node = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(getHeaderFromStateQueueDatum(node));
  const { committedWithdrawal } = await deriveDoubleWithdrawMembership({
    header,
    inclusion,
  });
  if (!isPayableWithdrawalLeaf(committedWithdrawal.value)) {
    throw doubleWithdrawSubmitError(
      "step-01 refuses a non-payable first leaf; expected WithdrawalIsValid.",
    );
  }
  const step02State = doubleWithdrawStep02State({
    challengedHeaderHash: stateQueueHeaderHash,
    committedWithdrawal,
  });

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step02State },
    DoubleWithdrawStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let layout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        hubOracleRefInputIndex: bigint;
        stateQueueNodeRefInputIndex: bigint;
      }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "double-withdraw step-01");
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "double-withdraw step-01"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "double-withdraw step-01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "double-withdraw step-01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "double-withdraw step-01 state-queue node",
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            hub_ref_input_index: layout.hubOracleRefInputIndex,
            state_queue_node_ref_input_index:
              layout.stateQueueNodeRefInputIndex,
            committed_withdrawal: committedWithdrawal,
          },
        ],
      },
      DoubleWithdrawStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([hubOracleUtxo, stateQueueBlockUtxo])
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = base.readFrom([
    requireDoubleWithdrawReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[0].spendingScriptHash,
      stepIndex: 0,
    }),
  ]);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw doubleWithdrawSubmitError("step-01 layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof double-withdraw step-01",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.steps[0].spendingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw doubleWithdrawSubmitError(
      `step-01 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    step02State,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    hubOracleRefInputIndex: Number(layout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(layout.stateQueueNodeRefInputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
