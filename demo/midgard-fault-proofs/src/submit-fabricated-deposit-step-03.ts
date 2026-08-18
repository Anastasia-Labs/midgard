/**
 * `fabricated-deposit` step-03 submitter (Goal task `Q39`, §9.1 output 8).
 *
 * Step 03 reads no chain state beyond the thread it spends: it opens step-02's
 * retained event-datum commitment and turns the authenticated verdict into the
 * named fault. Two things are re-run locally before any transaction is built, so
 * an unfinalizable thread is refused off-chain rather than on chain:
 *
 * - the **pairing** rule — a `DepositIdentityAbsent` verdict admits only the
 *   `NoAuthenticContent` opening and a `DepositEventObserved` verdict only
 *   `RetainedEventDatum`, because the cross pairs claim more than the evidence
 *   supports (`open_authentic_deposit_content_v1`); and
 * - the **establishment** rule that step 04 will re-apply
 *   (`isFabricatedDepositFaultV1`), including the
 *   `start_time < inclusion_time <= end_time` window, so a stale event cannot be
 *   walked one step further only to be refused at finalization.
 *
 * The supplied event datum is required to be *canonical* CBOR. On chain step 02
 * hashed the reference input's datum through `serialise_data`, whose output is
 * canonical, so hashing anything else here would silently disagree with the
 * commitment the thread carries.
 */
import {
  DepositDatum,
  depositEventDatumCommitmentV1,
  depositInfoCommitmentV1,
  type FabricatedDepositAuthenticContentOpeningV1,
  type FabricatedDepositFaultV1,
  FabricatedDepositStep03Datum,
  FabricatedDepositStep03SpendRedeemer,
  type FabricatedDepositStep03State,
  FabricatedDepositStep04Datum,
  type FabricatedDepositStep04State,
  fabricatedDepositStep04StateV1,
  isFabricatedDepositFaultV1,
  OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

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
  FABRICATED_DEPOSIT_CATEGORY_LABEL,
  type FabricatedDepositContractsV1,
} from "./submit-fabricated-deposit-step-01.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

/** The step-03 handoff: the opening the redeemer carries and the fault it yields. */
export type FabricatedDepositStep03HandoffV1 = {
  readonly opening: FabricatedDepositAuthenticContentOpeningV1;
  readonly fault: FabricatedDepositFaultV1;
  readonly step04State: FabricatedDepositStep04State;
};

export const requireFabricatedDepositStep03Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): FabricatedDepositStep03State => {
  if (threadUtxo.datum === null || threadUtxo.datum === undefined) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} has no inline fabricated-deposit step-03 datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, FabricatedDepositStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} belongs to fraud prover ${datum.fraud_prover}, not ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} carries no fabricated-deposit step-03 state.`,
    );
  }
  return datum.data;
};

/**
 * Decodes a supplied event datum and refuses non-canonical bytes, whose hash
 * cannot be the commitment step 02 retained.
 */
export const decodeCanonicalDepositEventDatumV1 = (
  eventDatumCbor: string,
): DepositDatum => {
  const decoded = Data.from(eventDatumCbor, DepositDatum);
  const canonical = Data.to(decoded, DepositDatum);
  if (canonical !== eventDatumCbor.toLowerCase()) {
    throw new Error(
      `Supplied deposit event datum is not canonical CBOR: re-encoding yields ${canonical}, not ${eventDatumCbor.toLowerCase()}.`,
    );
  }
  return decoded;
};

/**
 * Pure twin of `open_authentic_deposit_content_v1` plus step-04's
 * establishment gate. Throws — fail-closed — on any pairing, commitment,
 * identity, equality or window failure.
 */
export const deriveFabricatedDepositStep03HandoffV1 = async ({
  state,
  eventDatumCbor,
}: {
  readonly state: FabricatedDepositStep03State;
  readonly eventDatumCbor?: string;
}): Promise<FabricatedDepositStep03HandoffV1> => {
  let opening: FabricatedDepositAuthenticContentOpeningV1;
  let fault: FabricatedDepositFaultV1;

  if (state.verdict === "DepositIdentityAbsent") {
    if (eventDatumCbor !== undefined) {
      throw new Error(
        "Fabricated-deposit step 03 refuses a RetainedEventDatum opening on a DepositIdentityAbsent verdict: the opening does not pair with the L1 verdict.",
      );
    }
    opening = "NoAuthenticContent";
    fault = "NonexistentDepositIdentity";
  } else {
    if (eventDatumCbor === undefined) {
      throw new Error(
        "Fabricated-deposit step 03 refuses a NoAuthenticContent opening on a DepositEventObserved verdict: it would convert a content dispute into a non-existence conviction.",
      );
    }
    const { event_datum_hash, event_inclusion_time } =
      state.verdict.DepositEventObserved;
    const eventDatum = decodeCanonicalDepositEventDatumV1(eventDatumCbor);
    const suppliedHash = await Effect.runPromise(
      depositEventDatumCommitmentV1(eventDatum),
    );
    if (suppliedHash !== event_datum_hash) {
      throw new Error(
        `Supplied deposit event datum hashes to ${suppliedHash}, not the commitment ${event_datum_hash} step 02 authenticated.`,
      );
    }
    const suppliedId = Data.to(eventDatum.event.id, OutputReference);
    const committedId = Data.to(state.committed_deposit_id, OutputReference);
    if (suppliedId !== committedId) {
      throw new Error(
        `Supplied deposit event datum names identity ${suppliedId}, not the committed identity ${committedId}.`,
      );
    }
    const authenticDepositInfoHash = await Effect.runPromise(
      depositInfoCommitmentV1(eventDatum.event.info),
    );
    if (authenticDepositInfoHash === state.committed_deposit_info_hash) {
      throw new Error(
        `Fabricated-deposit step 03 cannot classify a fault: the authentic deposit content hashes to ${authenticDepositInfoHash}, which is exactly what the header committed.`,
      );
    }
    opening = { RetainedEventDatum: { event_datum: eventDatum } };
    fault = {
      MismatchedDepositContent: {
        committed_deposit_info_hash: state.committed_deposit_info_hash,
        authentic_deposit_info_hash: authenticDepositInfoHash,
        event_inclusion_time,
      },
    };
  }

  const step04State = fabricatedDepositStep04StateV1(state, fault);
  if (!isFabricatedDepositFaultV1(step04State)) {
    throw new Error(
      `Fabricated-deposit step 03 refuses to hand step 04 a fault it must reject: the authentic event is not due for the challenged block (${state.header_start_time.toString()} < inclusion_time <= ${state.header_end_time.toString()}).`,
    );
  }
  return { opening, fault, step04State };
};

export type SubmitFabricatedDepositStep03CliConfig = SubmitProviderConfig & {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly authenticContentPath?: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitFabricatedDepositAuthenticContent = {
  readonly eventDatumCbor: string;
};

export const parseSubmitFabricatedDepositAuthenticContent = (
  value: unknown,
): SubmitFabricatedDepositAuthenticContent => {
  const record = requireRecord(value, "fabricated-deposit authentic content");
  return {
    eventDatumCbor: parseHex(
      record["eventDatumCbor"],
      "fabricated-deposit authentic content eventDatumCbor",
    ),
  };
};

export type SubmitFabricatedDepositStep03Result = {
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
  readonly fault: FabricatedDepositFaultV1;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type FabricatedDepositStep03Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

export const submitFabricatedDepositStep03 = async ({
  lucid,
  contracts,
  signer,
  threadOutRef,
  eventDatumCbor,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FabricatedDepositContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly eventDatumCbor?: string;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFabricatedDepositStep03Result> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "fabricated-deposit step-03 computation-thread UTxO",
  });
  if (threadUtxo.address !== contracts.steps[2].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at fabricated-deposit step 03.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: contracts.categoryId,
    categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
  });
  const state = requireFabricatedDepositStep03Datum({ threadUtxo, signer });
  const handoff = await deriveFabricatedDepositStep03HandoffV1({
    state,
    eventDatumCbor,
  });

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step04Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: handoff.step04State },
    FabricatedDepositStep04Datum,
  );
  const step04OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: step04Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: FabricatedDepositStep03Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "fabricated-deposit step 03");
    const layout: FabricatedDepositStep03Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "fabricated-deposit step 03",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step04OutputMatches,
        "fabricated-deposit step 03 output",
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
      FabricatedDepositStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      contracts.steps[3].spendingScriptAddress,
      { kind: "inline", value: step04Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(contracts.steps[2].spendingScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve fabricated-deposit step 03 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
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

export const submitFabricatedDepositStep03FromFiles = async (
  config: SubmitFabricatedDepositStep03CliConfig & {
    readonly contracts: FabricatedDepositContractsV1;
  },
): Promise<SubmitFabricatedDepositStep03Result> => {
  const [authenticContentJson, lucid] = await Promise.all([
    config.authenticContentPath === undefined
      ? Promise.resolve(undefined)
      : readJsonFile(config.authenticContentPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitFabricatedDepositStep03({
    lucid,
    contracts: config.contracts,
    signer,
    threadOutRef: config.threadOutRef,
    eventDatumCbor:
      authenticContentJson === undefined
        ? undefined
        : parseSubmitFabricatedDepositAuthenticContent(authenticContentJson)
            .eventDatumCbor,
    awaitConfirmation: config.awaitConfirmation,
  });
};
