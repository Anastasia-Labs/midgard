import {
  asLucidDataValue,
  asLucidSchema,
} from "@al-ft/midgard-core/lucid-data";
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type Assets,
  type Credential,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type OutputDatum,
  type TxBuilder,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { WithdrawalBody } from "./ledger-state.js";
import { MAX_VALIDITY_RANGE_LENGTH_MS } from "./protocol-parameters.js";
import { assetsEqual } from "./reserve-payout/assets.js";
import { fail, ReservePayoutTxError } from "./reserve-payout/errors.js";
import * as SDK from "./reserve-payout/primitives.js";
import { type ReservePayoutReferenceScripts } from "./reserve-payout/references.js";
import { requireUniqueOutputIndex } from "./tx-context-redeemer.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";
import { type EventHistoryRetirementWitness } from "./user-events/history.js";

type CommonBuilderConfig = {
  readonly hubOracleRefInput?: UTxO;
  readonly feeInput?: UTxO;
  readonly referenceScripts?: ReservePayoutReferenceScripts;
  readonly referenceScriptsAddress?: string;
};

export type AbsorbConfirmedDepositConfig = CommonBuilderConfig & {
  readonly deposit: SDK.DepositUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.RawRootMembershipProof;
  readonly confirmedRefInput?: UTxO;
  readonly nowMs?: number;
};

export type InitializePayoutConfig = CommonBuilderConfig & {
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.RawRootMembershipProof;
  readonly confirmedRefInput?: UTxO;
  readonly nowMs?: number;
};

export type AddReserveFundsConfig = CommonBuilderConfig & {
  readonly validTo?: number;
  readonly payoutInput: UTxO;
  readonly reserveInput: UTxO;
};

export type ConcludePayoutConfig = CommonBuilderConfig & {
  readonly validTo?: number;
  readonly payoutInput: UTxO;
};

export type RefundInvalidWithdrawalConfig = CommonBuilderConfig & {
  readonly withdrawal: SDK.WithdrawalUTxO;
  readonly settlementRefInput: UTxO;
  readonly membershipProof: SDK.RawRootMembershipProof;
  readonly confirmedRefInput?: UTxO;
  readonly nowMs?: number;
  readonly validityOverride: Exclude<
    SDK.WithdrawalValidity,
    "WithdrawalIsValid"
  >;
};

const encodeHexBytesData = (hex: string): unknown =>
  Data.from(Data.to(hex, asLucidSchema(Data.Bytes())));

export const requireNetwork = (
  lucid: LucidEvolution,
): Effect.Effect<Network, ReservePayoutTxError> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* fail(
        "Cardano network not found while preparing reserve/payout transaction",
        "Lucid network configuration is undefined",
      );
    }
    return network;
  });

const credentialFromAddressData = (credential: SDK.CredentialD): Credential => {
  if ("PublicKeyCredential" in credential) {
    return { type: "Key", hash: credential.PublicKeyCredential[0] };
  }
  return { type: "Script", hash: credential.ScriptCredential[0] };
};

export const addressDataToBech32 = (
  network: Network,
  address: SDK.AddressData,
): string => {
  const paymentCredential = credentialFromAddressData(
    address.paymentCredential,
  );
  if (address.stakeCredential === null) {
    return credentialToAddress(network, paymentCredential);
  }
  if ("Inline" in address.stakeCredential) {
    return credentialToAddress(
      network,
      paymentCredential,
      credentialFromAddressData(address.stakeCredential.Inline[0]),
    );
  }
  throw new Error(
    "Pointer stake credentials are not supported by node builders",
  );
};

export const withdrawalPayoutDatumCbor = (payloadCbor: string): string => {
  const bodyCbor = plutusConstrFieldCbor(payloadCbor, [0, 1, 0]);
  const body = Data.from(bodyCbor, WithdrawalBody);
  let payoutCbor = Data.to(
    {
      l2_value: body.l2_value,
      l1_address: body.l1_address,
      l1_datum: body.l1_datum,
    },
    SDK.PayoutDatum,
  );
  for (let field = 0; field < 3; field++)
    payoutCbor = replacePlutusConstrFieldCbor(
      payoutCbor,
      [field],
      plutusConstrFieldCbor(bodyCbor, [field + 2]),
    );
  return payoutCbor;
};

export const cardanoDatumCborToOutputDatum = (
  datumCbor: string,
): OutputDatum | undefined => {
  const datum = Data.from(datumCbor, SDK.CardanoDatum);
  if (datum === "NoDatum") {
    return undefined;
  }
  if ("DatumHash" in datum) {
    return {
      kind: "hash",
      value: datum.DatumHash.hash,
    };
  }
  return {
    kind: "inline",
    value: plutusConstrFieldCbor(datumCbor, [0]),
  };
};

export const payToAddressWithCardanoDatum = (
  tx: TxBuilder,
  address: string,
  datumCbor: string,
  assets: Assets,
): TxBuilder => {
  const outputDatum = cardanoDatumCborToOutputDatum(datumCbor);
  return outputDatum === undefined
    ? tx.pay.ToAddress(address, assets)
    : tx.pay.ToAddressWithData(address, outputDatum, assets);
};

export const encodeMembershipProofWithdrawalRedeemer = (
  keyCbor: string,
  valueCbor: string,
  proof: SDK.RootMembershipProof<unknown, unknown>,
): string => {
  const rootData = Data.from(Data.to(proof.phas_root, SDK.MerkleRoot));
  const keyData = encodeHexBytesData(keyCbor);
  const valueData = encodeHexBytesData(valueCbor);
  const proofData = Data.from(Data.to(proof.proof, asLucidSchema(SDK.Proof)));
  return Data.to(
    asLucidDataValue([rootData, keyData, valueData, proofData]),
    asLucidSchema(Data.Array(Data.Any())),
  );
};

export const outputHasNoDatum = (output: TxOutput): boolean =>
  output.datum == null && output.datumHash == null;

export const outputDatumMatches = (
  output: TxOutput,
  datumCbor: string,
): boolean => {
  const datum = Data.from(datumCbor, SDK.CardanoDatum);
  if (datum === "NoDatum") {
    return outputHasNoDatum(output);
  }
  if ("DatumHash" in datum) {
    return output.datumHash === datum.DatumHash.hash && output.datum == null;
  }
  return outputDatumCborMatches(output, plutusConstrFieldCbor(datumCbor, [0]));
};

export const reserveOutputIndex = (
  outputs: readonly TxOutput[],
  reserveAddress: string,
  reserveAssets: Assets,
  label: string,
): bigint =>
  requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === reserveAddress &&
      outputHasNoDatum(output) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, reserveAssets),
    label,
  );

export const outputWithDatumIndex = (
  outputs: readonly TxOutput[],
  address: string,
  datumCbor: string,
  assets: Assets,
  label: string,
): bigint => {
  return requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === address &&
      outputDatumCborMatches(output, datumCbor) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, assets),
    label,
  );
};

export const outputWithCardanoDatumIndex = (
  outputs: readonly TxOutput[],
  address: string,
  datumCbor: string,
  assets: Assets,
  label: string,
): bigint =>
  requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === address &&
      outputDatumMatches(output, datumCbor) &&
      output.scriptRef === undefined &&
      assetsEqual(output.assets, assets),
    label,
  );

export const requireResolvedLayout = <L>(
  layout: L | undefined,
  label: string,
): L => {
  if (layout === undefined) {
    throw new Error(`BuildTxWithRedeemer did not resolve ${label} layout.`);
  }
  return layout;
};

/** The continued predecessor stays protected until this window's upper bound
 * plus the deployment duration, so a short window lets the next retirement on
 * the same list follow soon after, as it does after an admission. */
export const HISTORY_RETIREMENT_VALIDITY_RANGE_MS = Math.min(
  180_000,
  Number(MAX_VALIDITY_RANGE_LENGTH_MS),
);

export type RetirementConfig = CommonBuilderConfig & {
  readonly settlementRefInput: UTxO;
  readonly confirmedRefInput?: UTxO;
  readonly membershipProof: SDK.RawRootMembershipProof;
  readonly nowMs?: number;
};

export type RetirementLayout = {
  readonly witness: EventHistoryRetirementWitness;
  readonly hubRefInputIndex: bigint;
  readonly settlementRefInputIndex: bigint;
  readonly retirementWithdrawalRedeemerIndex: bigint;
  readonly listWithdrawalRedeemerIndex: bigint;
  readonly burnRedeemerIndex: bigint;
  readonly payoutMintRedeemerIndex: bigint | null;
};
