import { decodeMidgardAddressText } from "@al-ft/midgard-core/codec";
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import * as HistoryJournal from "../database/eventHistorySubmissions.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { decodeHistorySubmissionRequest } from "../transactions/event-history-submission.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
} from "../transactions/reference-scripts.js";
import { assetsToValue } from "../transactions/reserve-payout.js";
import * as SubmitWithdrawalTx from "../transactions/submit-withdrawal.js";
import {
  defaultMidgardNodeEndpoint,
  deriveWalletInfo,
  fetchNodeUtxosByOutRefs,
  lucidUtxoFromNodeUtxo,
  parseNodeEndpoint,
  resolveWalletSeedPhrase,
} from "./command-utils.js";
import {
  parseCardanoDatumCbor,
  parseWithdrawalTxOutRefLabel,
} from "./withdrawal-utils.js";

export type SubmitWithdrawalCliConfig = {
  readonly submissionId: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv: string;
  readonly l2OutRef: string;
  readonly l1Address: string;
  readonly l1Datum?: string;
  readonly refundAddress?: string;
  readonly refundDatum?: string;
  readonly orderLovelace?: string;
  readonly endpoint?: string;
};

export type SubmitWithdrawalCliResult = {
  readonly submissionId: string;
  readonly txHash: string;
  readonly withdrawalEventId: string;
  readonly withdrawalAssetName: string;
  readonly withdrawalAuthUnit: string;
  readonly l2OutRef: string;
  readonly l2Owner: string;
  readonly l2Value: Readonly<Record<string, bigint>>;
  readonly l1Address: string;
  readonly refundAddress: string;
  readonly walletSeedSource: string;
  readonly nodeEndpoint: string;
  readonly nonceInput: string;
  readonly validTo: number;
  readonly inclusionTime: number;
};

const parseOptionalPositiveLovelace = (
  value: string | undefined,
): bigint | undefined => {
  const normalized = value?.trim() ?? "";
  if (normalized.length === 0) {
    return undefined;
  }
  if (!/^\d+$/.test(normalized)) {
    throw new Error("--order-lovelace must be a positive integer.");
  }
  const parsed = BigInt(normalized);
  if (parsed <= 0n) {
    throw new Error("--order-lovelace must be greater than zero.");
  }
  return parsed;
};

const selectedUtxoPaymentKeyHash = (address: string): string => {
  const { paymentCredential } = decodeMidgardAddressText(address);
  if (paymentCredential.kind !== "PubKey") {
    throw new Error("Selected L2 UTxO must be owned by a key credential.");
  }
  return paymentCredential.hash.toString("hex");
};

export const withdrawalEventIdFromBuildMetadata = (
  metadata: SubmitWithdrawalTx.WithdrawalBuildMetadata,
): string => metadata.withdrawalEventIdCbor;

export const submitWithdrawalCommandProgram = ({
  config,
  assertWalletAddress,
}: {
  readonly config: SubmitWithdrawalCliConfig;
  readonly assertWalletAddress?: (walletAddress: string) => void;
}): Effect.Effect<
  SubmitWithdrawalCliResult,
  Error,
  NodeConfig | Lucid | MidgardContracts | Database
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;

    const parsedOutRef = parseWithdrawalTxOutRefLabel(
      config.l2OutRef,
      "--l2-out-ref",
    );
    const nodeEndpoint = parseNodeEndpoint(
      config.endpoint ?? defaultMidgardNodeEndpoint(),
    );
    const resolvedSeed = resolveWalletSeedPhrase({
      walletSeedPhrase: config.walletSeedPhrase,
      walletSeedPhraseEnv: config.walletSeedPhraseEnv,
    });
    const wallet = deriveWalletInfo(resolvedSeed, nodeConfig.NETWORK);
    assertWalletAddress?.(wallet.address);

    const saved = yield* HistoryJournal.retrieve(config.submissionId);
    const selectedState = yield* Effect.gen(function* () {
      if (Option.isSome(saved)) {
        if (
          saved.value.kind !== "Withdrawal" ||
          saved.value.wallet_address !== wallet.address ||
          saved.value.policy_id !== contracts.withdrawal.policyId
        )
          return yield* Effect.fail(
            new Error(
              "Submission ID belongs to a different kind, wallet or deployment",
            ),
          );
        const request = yield* Effect.try({
          try: () => decodeHistorySubmissionRequest(saved.value.request),
          catch: (cause) =>
            new Error(`Invalid saved withdrawal request: ${String(cause)}`),
        });
        const payload = Data.from(request.payloadCbor, SDK.EventHistoryPayload);
        if (!("WithdrawalPayload" in payload))
          return yield* Effect.fail(
            new Error("Saved request is not a withdrawal"),
          );
        const previous = payload.WithdrawalPayload.event.info.body;
        if (
          previous.l2_outref.transactionId !== parsedOutRef.txHash ||
          previous.l2_outref.outputIndex !== BigInt(parsedOutRef.outputIndex)
        )
          return yield* Effect.fail(
            new Error("Submission ID belongs to a different L2 output"),
          );
        return {
          owner: previous.l2_owner,
          assets: SDK.valueToAssets(previous.l2_value),
          valueCbor: plutusConstrFieldCbor(request.payloadCbor, [0, 1, 0, 2]),
        };
      }
      const matched = yield* Effect.tryPromise({
        try: () => fetchNodeUtxosByOutRefs(nodeEndpoint, [config.l2OutRef]),
        catch: (cause) =>
          new Error(`Failed to fetch selected L2 UTxO: ${String(cause)}`),
      });
      if (matched.length !== 1) {
        return yield* Effect.fail(
          new Error(
            `Expected exactly one spendable L2 UTxO for ${config.l2OutRef}, found ${matched.length.toString()}.`,
          ),
        );
      }
      const selected = matched[0]!;
      if (
        selected.txHash !== parsedOutRef.txHash ||
        selected.outputIndex !== parsedOutRef.outputIndex
      ) {
        return yield* Effect.fail(
          new Error("Node returned a UTxO that does not match --l2-out-ref."),
        );
      }
      return {
        owner: selectedUtxoPaymentKeyHash(selected.address),
        assets: selected.assets,
        valueCbor: Data.to(assetsToValue({ ...selected.assets }), SDK.Value),
      };
    });
    const selectedOwner = selectedState.owner;
    if (selectedOwner !== wallet.paymentKeyHash) {
      return yield* Effect.fail(
        new Error(
          `Selected L2 UTxO is owned by ${selectedOwner}, not withdrawal signer ${wallet.paymentKeyHash}.`,
        ),
      );
    }

    const l1Address = yield* SDK.addressDataFromBech32(config.l1Address);
    const refundAddress = yield* SDK.addressDataFromBech32(
      config.refundAddress ?? config.l1Address,
    );
    const body: SDK.WithdrawalBody = {
      l2_outref: parsedOutRef.outputReference,
      l2_owner: wallet.paymentKeyHash,
      l2_value: assetsToValue({ ...selectedState.assets }),
      l1_address: l1Address,
      l1_datum: "NoDatum",
    };
    const bodyCbor = replacePlutusConstrFieldCbor(
      replacePlutusConstrFieldCbor(
        Data.to(body, SDK.WithdrawalBody),
        [2],
        selectedState.valueCbor,
      ),
      [4],
      parseCardanoDatumCbor(config.l1Datum, "--l1-datum"),
    );
    const withdrawalReferenceScripts = yield* fetchReferenceScriptUtxosProgram(
      lucidService.api,
      lucidService.referenceScriptsAddress,
      referenceScriptTargetsByCommand(contracts).withdrawal,
      contracts.referenceScriptAuth,
    ).pipe(
      Effect.map((resolved) => ({
        withdrawalMinting: referenceScriptByName(
          resolved,
          "withdrawal minting",
        ),
      })),
    );

    yield* Effect.sync(() =>
      lucidService.api.selectWallet.fromSeed(wallet.seedPhrase),
    );
    const submitted = yield* SubmitWithdrawalTx.submitWithdrawalProgram(
      lucidService.api,
      contracts,
      {
        bodyCbor,
        signature: SDK.signWithdrawalBodyCbor(wallet.privateKey, bodyCbor),
        refundAddress,
        refundDatumCbor: parseCardanoDatumCbor(
          config.refundDatum,
          "--refund-datum",
        ),
        lovelace: parseOptionalPositiveLovelace(config.orderLovelace),
        referenceScripts: withdrawalReferenceScripts,
      },
      config.submissionId,
    );
    const withdrawalAssetName = submitted.metadata.withdrawalAuthUnit.slice(
      contracts.withdrawal.policyId.length,
    );

    return {
      submissionId: config.submissionId,
      txHash: submitted.txHash,
      withdrawalEventId: withdrawalEventIdFromBuildMetadata(submitted.metadata),
      withdrawalAssetName,
      withdrawalAuthUnit: submitted.metadata.withdrawalAuthUnit,
      l2OutRef: config.l2OutRef,
      l2Owner: wallet.paymentKeyHash,
      l2Value: { ...selectedState.assets },
      l1Address: config.l1Address,
      refundAddress: config.refundAddress ?? config.l1Address,
      walletSeedSource: wallet.seedSource,
      nodeEndpoint,
      nonceInput: `${submitted.metadata.nonceInput.txHash}#${submitted.metadata.nonceInput.outputIndex.toString()}`,
      validTo: submitted.metadata.validTo,
      inclusionTime: submitted.metadata.inclusionTime,
    };
  });

export const __submitWithdrawalTest = {
  lucidUtxoFromNodeUtxo,
  selectedUtxoPaymentKeyHash,
};
