import {
  commitCountedRootProgram,
  type CommittedDuplicateEventProofV1,
  CrossBlockDuplicateEventStep01SpendRedeemer,
  CrossBlockDuplicateEventStep02Datum,
  crossBlockDuplicateEventStep02StateV1,
  duplicateEventKindAndKeyV1,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
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
import type { CrossBlockDuplicateEventContractsV1 } from "./contracts-v1.js";
import {
  crossBlockDuplicateEventSubmitError,
  requireCrossBlockDuplicateEventReferenceScriptV1,
  requireCrossBlockDuplicateEventThreadV1,
} from "./submit-common-v1.js";

const requireOpeningMatchesHeaderV1 = async ({
  committedEvent,
  depositsRoot,
  depositCount,
  withdrawalsRoot,
  withdrawalCount,
}: {
  readonly committedEvent: CommittedDuplicateEventProofV1;
  readonly depositsRoot: string;
  readonly depositCount: bigint;
  readonly withdrawalsRoot: string;
  readonly withdrawalCount: bigint;
}): Promise<void> => {
  const deposit = "CommittedDuplicateDepositV1" in committedEvent;
  const membership = deposit
    ? committedEvent.CommittedDuplicateDepositV1.membership
    : committedEvent.CommittedDuplicateWithdrawalV1.membership;
  const expectedDomain = deposit
    ? ROOT_DOMAINS.deposits
    : ROOT_DOMAINS.withdrawals;
  const expectedCount = deposit ? depositCount : withdrawalCount;
  const expectedRoot = deposit ? depositsRoot : withdrawalsRoot;
  const derived = await Effect.runPromise(
    commitCountedRootProgram({
      domain: expectedDomain,
      phasRoot: membership.phas_root,
      count: expectedCount,
    }),
  );
  if (
    membership.domain !== expectedDomain ||
    membership.count !== expectedCount ||
    membership.root !== expectedRoot ||
    derived !== expectedRoot
  ) {
    throw crossBlockDuplicateEventSubmitError(
      "challenged counted-root opening does not match the authenticated state-queue header",
    );
  }
};

export type SubmitCrossBlockDuplicateEventStep01Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly challengedHeaderHash: string;
  readonly event: ReturnType<typeof duplicateEventKindAndKeyV1>;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitCrossBlockDuplicateEventStep01 = async ({
  lucid,
  network,
  contracts,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  committedEvent,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly contracts: CrossBlockDuplicateEventContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly committedEvent: CommittedDuplicateEventProofV1;
  /** Mandatory published step-01 reference script. */
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitCrossBlockDuplicateEventStep01Result> => {
  const [{ threadUtxo, threadToken }, hubUtxo, blockUtxo] = await Promise.all([
    requireCrossBlockDuplicateEventThreadV1({
      lucid,
      contracts,
      threadOutRef,
      stepIndex: 0,
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: "cross-block-duplicate-event hub oracle",
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: "cross-block-duplicate-event challenged state-queue block",
    }),
  ]);
  requireInitialStepDatum({ threadUtxo, signer });
  const challengedHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: blockUtxo,
  });
  if (challengedHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw crossBlockDuplicateEventSubmitError(
      "challenged state-queue header does not match the computation-thread asset name",
    );
  }
  const header = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(blockUtxo).pipe(
      Effect.flatMap(getHeaderV1FromStateQueueDatum),
    ),
  );
  await requireOpeningMatchesHeaderV1({
    committedEvent,
    depositsRoot: header.depositsRoot,
    depositCount: header.depositCount,
    withdrawalsRoot: header.withdrawalsRoot,
    withdrawalCount: header.withdrawalCount,
  });
  if (hubUtxo.datum == null) {
    throw crossBlockDuplicateEventSubmitError("hub oracle has no inline datum");
  }
  const hubDatum = Data.from(hubUtxo.datum, HubOracleDatum);
  const state = crossBlockDuplicateEventStep02StateV1({
    challengedHeaderHash,
    settlementPolicyId: hubDatum.settlement,
    committedEvent,
  });
  const outputDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    CrossBlockDuplicateEventStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: outputDatum,
    unit: threadToken.unit,
  });
  let layout: { inputIndex: bigint; outputIndex: bigint } | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "cross-block-duplicate-event step 01",
    );
    const resolved = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "cross-block-duplicate-event step 01",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "cross-block-duplicate-event step-02 output",
      ),
    };
    layout = resolved;
    return Data.to(
      {
        Continue: [
          {
            input_index: resolved.inputIndex,
            output_index: resolved.outputIndex,
            hub_ref_input_index: requireReferenceInputIndex(
              ctx,
              hubUtxo,
              "cross-block-duplicate-event hub oracle",
            ),
            state_queue_node_ref_input_index: requireReferenceInputIndex(
              ctx,
              blockUtxo,
              "cross-block-duplicate-event challenged block",
            ),
            committed_event: committedEvent,
          },
        ],
      },
      CrossBlockDuplicateEventStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      hubUtxo,
      blockUtxo,
      requireCrossBlockDuplicateEventReferenceScriptV1({
        utxo: referenceScriptUtxo,
        contracts,
        stepIndex: 0,
      }),
    ])
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadToken.unit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw crossBlockDuplicateEventSubmitError(
      "step-01 layout was not resolved",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    challengedHeaderHash,
    event: duplicateEventKindAndKeyV1(committedEvent),
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
