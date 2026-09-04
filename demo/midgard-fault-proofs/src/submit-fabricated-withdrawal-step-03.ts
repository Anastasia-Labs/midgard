/**
 * `fabricated-withdrawal` step-03 submitter (Goal task `Q40`, §9.1 output 8).
 *
 * Step 03 reads no chain state beyond the thread it spends: it opens step-02's
 * retained event-datum commitment and turns the authenticated verdict into the
 * named fault. Two things are re-run locally before any transaction is built, so
 * an unfinalizable thread is refused off-chain rather than on chain:
 *
 * - the **pairing** rule — a `WithdrawalIdentityAbsent` verdict admits only the
 *   `NoAuthenticContent` opening and a `WithdrawalEventObserved` verdict only
 *   `RetainedEventDatum`, because the cross pairs claim more than the evidence
 *   supports (`open_authentic_withdrawal_content_v1`); and
 * - the **establishment** rule that step 04 will re-apply
 *   (`isFabricatedWithdrawalFault`), including the
 *   `start_time < inclusion_time <= end_time` window, so a stale event cannot be
 *   walked one step further only to be refused at finalization.
 *
 * ### Why this step normalizes instead of demanding byte-identical CBOR
 *
 * The `fabricated-deposit` twin of this module refuses a supplied event datum whose
 * `Data.to` re-encoding is not byte-identical to the supplied hex. That check cannot
 * be reused here. A withdrawal event datum embeds the withdrawer's `l2_value` map,
 * and the two encoders disagree on exactly that: Plutus `serialise_data` — the
 * function step 02 hashed the reference input's datum through, and the one step 03
 * will hash the redeemer opening through — writes non-empty maps **definite**, while
 * Lucid's `Data.to` writes them indefinite. Demanding byte equality against
 * `Data.to` would therefore refuse the authentic datum as observed on chain and
 * accept only a form the script never hashes.
 *
 * So this module normalizes the supplied bytes to `serialise_data` form before
 * hashing, and lets the hash equality against step-02's retained commitment be the
 * authenticity gate — which is precisely the on-chain rule, since Plutus
 * re-serialises whatever wire form the redeemer arrived in.
 */
import {
  type FabricatedWithdrawalAuthenticContentOpening,
  type FabricatedWithdrawalFault,
  FabricatedWithdrawalStep03Datum,
  FabricatedWithdrawalStep03SpendRedeemer,
  type FabricatedWithdrawalStep03State,
  FabricatedWithdrawalStep04Datum,
  type FabricatedWithdrawalStep04State,
  fabricatedWithdrawalStep04State,
  isFabricatedWithdrawalFault,
  OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  withdrawalEventDatumBytes,
  withdrawalEventDatumCommitment,
  withdrawalInfoCommitment,
  WithdrawalOrderDatum,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { requireFabricatedReferenceScript } from "./fabricated-reference-script.js";
import { parseHex, readJsonFile, requireRecord } from "./json-file.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  FABRICATED_WITHDRAWAL_CATEGORY_LABEL,
  type FabricatedWithdrawalContracts,
} from "./submit-fabricated-withdrawal-step-01.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "./workflow/transaction-boundary.js";

/** The step-03 handoff: the opening the redeemer carries and the fault it yields. */
export type FabricatedWithdrawalStep03Handoff = {
  readonly opening: FabricatedWithdrawalAuthenticContentOpening;
  readonly fault: FabricatedWithdrawalFault;
  readonly step04State: FabricatedWithdrawalStep04State;
};

export const requireFabricatedWithdrawalStep03Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): FabricatedWithdrawalStep03State => {
  if (threadUtxo.datum === null || threadUtxo.datum === undefined) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} has no inline fabricated-withdrawal step-03 datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, FabricatedWithdrawalStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} belongs to fraud prover ${datum.fraud_prover}, not ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} carries no fabricated-withdrawal step-03 state.`,
    );
  }
  return datum.data;
};

/**
 * Decodes a supplied withdrawal event datum and reports the `serialise_data` bytes
 * the on-chain step will actually hash, whatever wire form the operator supplied.
 */
export const decodeWithdrawalEventDatumForOpening = (
  eventDatumCbor: string,
): {
  readonly eventDatum: WithdrawalOrderDatum;
  readonly serialisedBytes: string;
} => {
  const eventDatum = Data.from(eventDatumCbor, WithdrawalOrderDatum);
  return {
    eventDatum,
    serialisedBytes: withdrawalEventDatumBytes(eventDatum),
  };
};

/**
 * Pure twin of `open_authentic_withdrawal_content_v1` plus step-04's
 * establishment gate. Throws — fail-closed — on any pairing, commitment,
 * identity, equality or window failure.
 */
export const deriveFabricatedWithdrawalStep03Handoff = async ({
  state,
  eventDatumCbor,
}: {
  readonly state: FabricatedWithdrawalStep03State;
  readonly eventDatumCbor?: string;
}): Promise<FabricatedWithdrawalStep03Handoff> => {
  let opening: FabricatedWithdrawalAuthenticContentOpening;
  let fault: FabricatedWithdrawalFault;

  if (state.verdict === "WithdrawalIdentityAbsent") {
    if (eventDatumCbor !== undefined) {
      throw new Error(
        "Fabricated-withdrawal step 03 refuses a RetainedEventDatum opening on a WithdrawalIdentityAbsent verdict: the opening does not pair with the L1 verdict.",
      );
    }
    opening = "NoAuthenticContent";
    fault = "NonexistentWithdrawalIdentity";
  } else {
    if (eventDatumCbor === undefined) {
      throw new Error(
        "Fabricated-withdrawal step 03 refuses a NoAuthenticContent opening on a WithdrawalEventObserved verdict: it would convert a content dispute into a non-existence conviction.",
      );
    }
    const { event_datum_hash, event_inclusion_time } =
      state.verdict.WithdrawalEventObserved;
    const { eventDatum, serialisedBytes } =
      decodeWithdrawalEventDatumForOpening(eventDatumCbor);
    const suppliedHash = await Effect.runPromise(
      withdrawalEventDatumCommitment(eventDatum),
    );
    if (suppliedHash !== event_datum_hash) {
      throw new Error(
        `Supplied withdrawal event datum serialises to ${serialisedBytes} and hashes to ${suppliedHash}, not the commitment ${event_datum_hash} step 02 authenticated.`,
      );
    }
    const suppliedId = Data.to(eventDatum.event.id, OutputReference);
    const committedId = Data.to(state.committed_withdrawal_id, OutputReference);
    if (suppliedId !== committedId) {
      throw new Error(
        `Supplied withdrawal event datum names identity ${suppliedId}, not the committed identity ${committedId}.`,
      );
    }
    const authenticWithdrawalInfoHash = await Effect.runPromise(
      withdrawalInfoCommitment(eventDatum.event.info),
    );
    if (authenticWithdrawalInfoHash === state.committed_withdrawal_info_hash) {
      throw new Error(
        `Fabricated-withdrawal step 03 cannot classify a fault: the authentic withdrawal content hashes to ${authenticWithdrawalInfoHash}, which is exactly what the header committed.`,
      );
    }
    opening = { RetainedEventDatum: { event_datum: eventDatum } };
    fault = {
      MismatchedWithdrawalContent: {
        committed_withdrawal_info_hash: state.committed_withdrawal_info_hash,
        authentic_withdrawal_info_hash: authenticWithdrawalInfoHash,
        event_inclusion_time,
      },
    };
  }

  const step04State = fabricatedWithdrawalStep04State(state, fault);
  if (!isFabricatedWithdrawalFault(step04State)) {
    throw new Error(
      `Fabricated-withdrawal step 03 refuses to hand step 04 a fault it must reject: the authentic event is not due for the challenged block (${state.header_start_time.toString()} < inclusion_time <= ${state.header_end_time.toString()}).`,
    );
  }
  return { opening, fault, step04State };
};

export type SubmitFabricatedWithdrawalStep03CliConfig = SubmitProviderConfig & {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly authenticContentPath?: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitFabricatedWithdrawalAuthenticContent = {
  readonly eventDatumCbor: string;
};

export const parseSubmitFabricatedWithdrawalAuthenticContent = (
  value: unknown,
): SubmitFabricatedWithdrawalAuthenticContent => {
  const record = requireRecord(
    value,
    "fabricated-withdrawal authentic content",
  );
  return {
    eventDatumCbor: parseHex(
      record["eventDatumCbor"],
      "fabricated-withdrawal authentic content eventDatumCbor",
    ),
  };
};

export type SubmitFabricatedWithdrawalStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  readonly fourthStepAddress: string;
  readonly fault: FabricatedWithdrawalFault;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type FabricatedWithdrawalStep03Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

export const submitFabricatedWithdrawalStep03 = async ({
  lucid,
  contracts,
  signer,
  threadOutRef,
  eventDatumCbor,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FabricatedWithdrawalContracts;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly eventDatumCbor?: string;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFabricatedWithdrawalStep03Result> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "fabricated-withdrawal step-03 computation-thread UTxO",
  });
  if (threadUtxo.address !== contracts.steps[2].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at fabricated-withdrawal step 03.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: contracts.categoryId,
    categoryLabel: FABRICATED_WITHDRAWAL_CATEGORY_LABEL,
  });
  const state = requireFabricatedWithdrawalStep03Datum({ threadUtxo, signer });
  const handoff = await deriveFabricatedWithdrawalStep03Handoff({
    state,
    eventDatumCbor,
  });

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step04Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: handoff.step04State },
    FabricatedWithdrawalStep04Datum,
  );
  const step04OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: step04Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: FabricatedWithdrawalStep03Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "fabricated-withdrawal step 03");
    const layout: FabricatedWithdrawalStep03Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "fabricated-withdrawal step 03",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step04OutputMatches,
        "fabricated-withdrawal step 03 output",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            authentic_content: handoff.opening,
          },
        ],
      },
      FabricatedWithdrawalStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      requireFabricatedReferenceScript({
        utxo: referenceScriptUtxo,
        expectedScriptHash: contracts.steps[2].spendingScriptHash,
        categoryLabel: FABRICATED_WITHDRAWAL_CATEGORY_LABEL,
        stepIndex: 2,
      }),
    ])
    .pay.ToContract(
      contracts.steps[3].spendingScriptAddress,
      { kind: "inline", value: step04Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve fabricated-withdrawal step 03 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof fabricated-withdrawal step-03",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.steps[2].spendingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `fabricated-withdrawal step-03 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    fourthStepAddress: contracts.steps[3].spendingScriptAddress,
    fault: handoff.fault,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitFabricatedWithdrawalStep03FromFiles = async (
  config: SubmitFabricatedWithdrawalStep03CliConfig & {
    readonly contracts: FabricatedWithdrawalContracts;
    readonly referenceScriptUtxo: UTxO;
  },
): Promise<SubmitFabricatedWithdrawalStep03Result> => {
  const [authenticContentJson, lucid] = await Promise.all([
    config.authenticContentPath === undefined
      ? Promise.resolve(undefined)
      : readJsonFile(config.authenticContentPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitFabricatedWithdrawalStep03({
    lucid,
    contracts: config.contracts,
    signer,
    threadOutRef: config.threadOutRef,
    eventDatumCbor:
      authenticContentJson === undefined
        ? undefined
        : parseSubmitFabricatedWithdrawalAuthenticContent(authenticContentJson)
            .eventDatumCbor,
    referenceScriptUtxo: config.referenceScriptUtxo,
    awaitConfirmation: config.awaitConfirmation,
  });
};
