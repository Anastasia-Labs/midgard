import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  coreToTxOutput,
  Data as LucidData,
  datumToHash,
  type LucidEvolution,
  type Network,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import { DepositSubmissionAttemptsDB } from "../database/index.js";

export type SubmitDepositReferenceScripts = SDK.SubmitDepositReferenceScripts;

export type SubmitDepositConfig = SDK.SubmitDepositConfig;

export type BuildDepositRequest = SubmitDepositConfig & {
  readonly fundingAddress: string;
  readonly fundingUtxos: readonly UTxO[];
};

export type BuiltUnsignedDepositTx = {
  readonly unsignedTxCbor: string;
};

export type DepositBuildMetadata = SDK.DepositBuildMetadata;

export class SubmitDepositError extends EffectData.TaggedError(
  "SubmitDepositError",
)<{
  message: string;
  cause: unknown;
}> {}

export const MAX_DEPOSIT_BUILD_FUNDING_UTXOS = 128;

export const MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES = 64;

export const MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS = 64;

export const buildUnsignedDepositTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: DepositBuildMetadata;
  },
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
> =>
  SDK.buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.catchTag("UserEventBuildError", (error) =>
      Effect.fail(
        new SubmitDepositError({
          message: error.message,
          cause: error.cause,
        }),
      ),
    ),
  );

const sortedAssetEntries = (assets: Assets): [string, string][] =>
  Object.entries(assets)
    .map(([unit, quantity]) => [unit, quantity.toString()] as [string, string])
    .sort(([left], [right]) => left.localeCompare(right));

const serializeAssets = (
  assets: Assets,
): DepositSubmissionAttemptsDB.SerializedAssets =>
  Object.fromEntries(sortedAssetEntries(assets));

const sameSerializedAssets = (
  left: DepositSubmissionAttemptsDB.SerializedAssets,
  right: DepositSubmissionAttemptsDB.SerializedAssets,
): boolean => {
  const entries = Object.entries(left);
  return (
    entries.length === Object.keys(right).length &&
    entries.every(([unit, amount]) => right[unit] === amount)
  );
};

const inputOutRefsFromCompletedTx = (
  tx: CML.Transaction,
): readonly string[] => {
  const inputs = tx.body().inputs();
  const outRefs: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    outRefs.push(
      `${input.transaction_id().to_hex()}#${Number(input.index()).toString()}`,
    );
  }
  return outRefs;
};

export const depositSubmissionAttemptFromCompletedTx = ({
  txHash,
  transactionCbor,
  metadata,
  config,
}: {
  readonly txHash: string;
  readonly transactionCbor: string;
  readonly metadata: DepositBuildMetadata;
  readonly config: SubmitDepositConfig;
}): DepositSubmissionAttemptsDB.InsertSubmittedInput => {
  const tx = CML.Transaction.from_cbor_hex(transactionCbor);
  if (CML.hash_transaction(tx.body()).to_hex() !== txHash)
    throw new Error(
      "Deposit transaction hash does not match its completed body",
    );
  const outputs = tx.body().outputs();
  const matches: Array<{
    readonly outputIndex: number;
    readonly assets: Assets;
  }> = [];

  for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
    const output = coreToTxOutput(outputs.get(outputIndex));
    if (output.address !== metadata.depositAddress) {
      continue;
    }
    if ((output.assets[metadata.depositAuthUnit] ?? 0n) !== 1n) {
      continue;
    }
    if (output.datum === undefined || output.datum === null) {
      throw new Error(
        `Deposit output ${txHash}#${outputIndex.toString()} is missing the inline deposit datum`,
      );
    }
    const node = LucidData.from(output.datum, SDK.EventHistoryNode);
    if (
      node.position === "Root" ||
      node.payload === "RootContent" ||
      !("Order" in node.payload)
    )
      throw new Error(
        "Expected an authenticated deposit Order in the completed transaction",
      );
    const facts = node.payload.Order.facts;
    const actualEventId = LucidData.to(facts.event_id, SDK.OutputReference);
    if (actualEventId !== metadata.depositEventId) {
      continue;
    }
    const key = datumToHash(actualEventId);
    if (
      node.position.Key[0] !== key ||
      metadata.depositAssetName !== key ||
      !metadata.depositAuthUnit.endsWith(key) ||
      metadata.depositAuthUnit.length !== 120 ||
      facts.structural_lovelace !== metadata.structuralLovelace ||
      facts.inclusion_time !== BigInt(metadata.inclusionTime) ||
      outputIndex !== metadata.orderOutputIndex
    )
      throw new Error(
        "Completed deposit Order does not match its full key, funding or inclusion metadata",
      );
    const assets = {
      ...output.assets,
      lovelace: (output.assets.lovelace ?? 0n) - facts.structural_lovelace,
    };
    if (assets.lovelace < 0n)
      throw new Error(
        "Completed deposit structural ADA exceeds its locked funds",
      );
    matches.push({ outputIndex, assets });
  }

  if (matches.length !== 1) {
    throw new Error(
      `Expected exactly one deposit output for event ${metadata.depositEventId} in completed tx ${txHash}; found ${matches.length.toString()}`,
    );
  }

  const match = matches[0]!;
  const expectedAssets: Assets = { ...match.assets };
  delete expectedAssets[metadata.depositAuthUnit];
  const expectedSerializedAssets = serializeAssets(expectedAssets);
  const requestedSerializedAssets = serializeAssets({
    ...config.additionalAssets,
    lovelace: config.lovelace,
  });
  if (
    !sameSerializedAssets(expectedSerializedAssets, requestedSerializedAssets)
  ) {
    throw new Error(
      `Deposit output ${txHash}#${match.outputIndex.toString()} does not match requested projected assets`,
    );
  }

  return {
    [DepositSubmissionAttemptsDB.Columns.TX_HASH]: Buffer.from(txHash, "hex"),
    [DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID]: Buffer.from(
      metadata.depositEventId,
      "hex",
    ),
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF]:
      `${txHash}#${match.outputIndex.toString()}`,
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_L2_ADDRESS]: config.l2Address,
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_LOVELACE]:
      config.lovelace.toString(),
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS]:
      expectedSerializedAssets,
    [DepositSubmissionAttemptsDB.Columns.METADATA]: {
      depositAddress: metadata.depositAddress,
      depositEventId: metadata.depositEventId,
      depositAssetName: metadata.depositAssetName,
      depositAuthUnit: metadata.depositAuthUnit,
      nonceInput: metadata.nonceInput,
      validTo: metadata.validTo,
      inclusionTime: metadata.inclusionTime,
      structuralLovelace: metadata.structuralLovelace.toString(),
      orderOutputIndex: metadata.orderOutputIndex,
      l2DatumCbor: config.l2Datum,
      transactionCbor,
    },
    [DepositSubmissionAttemptsDB.Columns.FUNDING_OUT_REFS]:
      inputOutRefsFromCompletedTx(tx),
  };
};

export type DepositSubmissionReconciliationResult = {
  readonly txHash: string;
  readonly depositEventId: string;
  readonly status:
    | "confirmed"
    | "reconciled_after_timeout"
    | "ambiguous"
    | "missing_attempt";
  readonly expectedDepositOutRef?: string;
  readonly depositRowsFound: number;
  readonly reconciledCount: number;
  readonly nextSafeAction: string;
};

/** Compare current L1 facts with the persisted submission intent, independent
 * of the mutable history output location. A cache row alone cannot confirm it. */
export const matchesDepositSubmissionIntent = (
  deposit: SDK.DepositUTxO,
  attempt: DepositSubmissionAttemptsDB.InsertSubmittedInput,
  network: Network,
): boolean => {
  const metadata = attempt[DepositSubmissionAttemptsDB.Columns.METADATA];
  if (metadata.l2DatumCbor === undefined)
    throw new Error("Deposit submission intent is missing its L2 datum");
  const expectedAddress = Effect.runSync(
    SDK.addressDataFromBech32(
      attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_L2_ADDRESS],
    ),
  );
  const infoCbor = deposit.infoCbor.toString("hex");
  const info = LucidData.from(infoCbor, SDK.DepositInfo);
  const datumCbor = (cbor: string | null) =>
    cbor === null
      ? null
      : aikenSerialisedPlutusDataCborPreservingMapOrder(cbor);
  return (
    deposit.idCbor.equals(
      attempt[DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID],
    ) &&
    deposit.utxo.address === metadata.depositAddress &&
    deposit.assetName === metadata.depositAssetName &&
    deposit.utxo.assets[metadata.depositAuthUnit] === 1n &&
    deposit.facts.inclusion_time === BigInt(metadata.inclusionTime) &&
    deposit.facts.structural_lovelace === BigInt(metadata.structuralLovelace) &&
    info.l2_network_id === (network === "Mainnet" ? 1n : 0n) &&
    LucidData.to(info.l2_address, SDK.AddressData) ===
      LucidData.to(expectedAddress, SDK.AddressData) &&
    datumCbor(
      info.l2_datum === null ? null : plutusConstrFieldCbor(infoCbor, [2, 0]),
    ) === datumCbor(metadata.l2DatumCbor) &&
    sameSerializedAssets(
      serializeAssets(deposit.originalAssets),
      attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS],
    )
  );
};
