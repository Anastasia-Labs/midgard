import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { assetsToValue } from "@/transactions/reserve-payout.js";
import * as SubmitWithdrawalTx from "@/transactions/submit-withdrawal.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
} from "@/transactions/reference-scripts.js";
import {
  defaultMidgardNodeEndpoint,
  deriveWalletInfo,
  fetchNodeUtxosByOutRefs,
  formatJson,
  lucidUtxoFromNodeUtxo,
  parseCardanoDatum,
  parseNodeEndpoint,
  parseTxOutRefLabel,
  resolveWalletSeedPhrase,
} from "@/commands/withdrawal-utils.js";
import { Lucid, MidgardContracts, NodeConfig } from "@/services/index.js";
import { signWithdrawalBody } from "@/withdrawal-signature.js";

export type SubmitWithdrawalCliConfig = {
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
  if (value === undefined || value.trim().length === 0) {
    return undefined;
  }
  if (!/^\d+$/.test(value.trim())) {
    throw new Error("--order-lovelace must be a positive integer.");
  }
  const parsed = BigInt(value.trim());
  if (parsed <= 0n) {
    throw new Error("--order-lovelace must be greater than zero.");
  }
  return parsed;
};

const selectedUtxoPaymentKeyHash = (address: string): string => {
  const details = getAddressDetails(address);
  const paymentCredential = details.paymentCredential;
  if (paymentCredential === undefined || paymentCredential.type !== "Key") {
    throw new Error("Selected L2 UTxO must be owned by a key credential.");
  }
  return paymentCredential.hash;
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
  NodeConfig | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;

    const parsedOutRef = parseTxOutRefLabel(config.l2OutRef, "--l2-out-ref");
    const nodeEndpoint = parseNodeEndpoint(
      config.endpoint ?? defaultMidgardNodeEndpoint(),
    );
    const resolvedSeed = resolveWalletSeedPhrase({
      walletSeedPhrase: config.walletSeedPhrase,
      walletSeedPhraseEnv: config.walletSeedPhraseEnv,
    });
    const wallet = deriveWalletInfo(resolvedSeed, nodeConfig.NETWORK);
    assertWalletAddress?.(wallet.address);

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
    const selectedOwner = selectedUtxoPaymentKeyHash(selected.address);
    if (selectedOwner.toLowerCase() !== wallet.paymentKeyHash.toLowerCase()) {
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
      l2_value: assetsToValue({ ...selected.assets }),
      l1_address: l1Address,
      l1_datum: parseCardanoDatum(config.l1Datum, "--l1-datum"),
    };
    const withdrawalReferenceScripts = yield* fetchReferenceScriptUtxosProgram(
      lucidService.api,
      lucidService.referenceScriptsAddress,
      referenceScriptTargetsByCommand(contracts).withdrawal,
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
        body,
        signature: signWithdrawalBody(wallet.privateKey, body),
        refundAddress,
        refundDatum: parseCardanoDatum(config.refundDatum, "--refund-datum"),
        lovelace: parseOptionalPositiveLovelace(config.orderLovelace),
        referenceScripts: withdrawalReferenceScripts,
      },
    );
    const withdrawalAssetName = submitted.metadata.withdrawalAuthUnit.slice(
      contracts.withdrawal.policyId.length,
    );

    return {
      txHash: submitted.txHash,
      withdrawalEventId: withdrawalEventIdFromBuildMetadata(submitted.metadata),
      withdrawalAssetName,
      withdrawalAuthUnit: submitted.metadata.withdrawalAuthUnit,
      l2OutRef: config.l2OutRef,
      l2Owner: wallet.paymentKeyHash,
      l2Value: { ...selected.assets },
      l1Address: config.l1Address,
      refundAddress: config.refundAddress ?? config.l1Address,
      walletSeedSource: wallet.seedSource,
      nodeEndpoint,
      nonceInput: `${submitted.metadata.nonceInput.txHash}#${submitted.metadata.nonceInput.outputIndex.toString()}`,
      validTo: submitted.metadata.validTo,
      inclusionTime: submitted.metadata.inclusionTime,
    };
  });

export const formatSubmitWithdrawalResult = (
  result: SubmitWithdrawalCliResult,
): string => formatJson(result);

export const __submitWithdrawalTest = {
  lucidUtxoFromNodeUtxo,
};
