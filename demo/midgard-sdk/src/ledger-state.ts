import { Data } from "@lucid-evolution/lucid";
import {
  AddressSchema,
  H32Schema,
  MerkleRootSchema,
  OutputReferenceSchema,
  POSIXTimeSchema,
  PubKeyHashSchema,
  ValueSchema,
} from "@/common.js";

export const HeaderHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });
export type HeaderHash = Data.Static<typeof HeaderHashSchema>;
export const HeaderHash = HeaderHashSchema as unknown as HeaderHash;

export const HeaderSchema = Data.Object({
  prevUtxosRoot: MerkleRootSchema,
  utxosRoot: MerkleRootSchema,
  transactionsRoot: MerkleRootSchema,
  depositsRoot: MerkleRootSchema,
  withdrawalsRoot: MerkleRootSchema,
  startTime: POSIXTimeSchema,
  endTime: POSIXTimeSchema,
  prevHeaderHash: HeaderHashSchema,
  operatorVkey: PubKeyHashSchema,
  protocolVersion: Data.Integer(),
});
export type Header = Data.Static<typeof HeaderSchema>;
export const Header = HeaderSchema as unknown as Header;

export const ConfirmedStateSchema = Data.Object({
  headerHash: HeaderHashSchema,
  prevHeaderHash: HeaderHashSchema,
  utxoRoot: MerkleRootSchema,
  startTime: POSIXTimeSchema,
  endTime: POSIXTimeSchema,
  protocolVersion: Data.Integer(),
});
export type ConfirmedState = Data.Static<typeof ConfirmedStateSchema>;
export const ConfirmedState = ConfirmedStateSchema as unknown as ConfirmedState;

export const CardanoDatumSchema = Data.Enum([
  Data.Literal("NoDatum"),
  Data.Object({
    DatumHash: Data.Object({
      hash: Data.Bytes(),
    }),
  }),
  Data.Object({
    InlineDatum: Data.Object({
      data: Data.Any(),
    }),
  }),
]);
export type CardanoDatum = Data.Static<typeof CardanoDatumSchema>;
export const CardanoDatum = CardanoDatumSchema as unknown as CardanoDatum;

export const DepositInfoSchema = Data.Object({
  l2Address: Data.Bytes(),
  l2Datum: Data.Nullable(Data.Bytes()),
});
export type DepositInfo = Data.Static<typeof DepositInfoSchema>;
export const DepositInfo = DepositInfoSchema as unknown as DepositInfo;

export const DepositEventSchema = Data.Object({
  id: OutputReferenceSchema,
  info: DepositInfoSchema,
});
export type DepositEvent = Data.Static<typeof DepositEventSchema>;
export const DepositEvent = DepositEventSchema as unknown as DepositEvent;

// Changed from hashes of body and witness set to full tx representation
// to enable tx order processing in node. Otherwise it is impossible to
// get utxos from tx orders.
export const MidgardTxCompactSchema = Data.Object({
  tx: Data.Bytes(),
  is_valid: Data.Boolean(),
});
export type MidgardTxCompact = Data.Static<typeof MidgardTxCompactSchema>;
export const MidgardTxCompact =
  MidgardTxCompactSchema as unknown as MidgardTxCompact;

export const TxOrderEventSchema = Data.Object({
  txOrderId: OutputReferenceSchema,
  midgardTx: MidgardTxCompactSchema,
});
export type TxOrderEvent = Data.Static<typeof TxOrderEventSchema>;
export const TxOrderEvent = TxOrderEventSchema as unknown as TxOrderEvent;

export const MidgardNetworkIdSchema = Data.Enum([
  Data.Literal("Mainnet"),
  Data.Literal("Testnet"),
]);
export type MidgardNetworkId = Data.Static<typeof MidgardNetworkIdSchema>;
export const MidgardNetworkId =
  MidgardNetworkIdSchema as unknown as MidgardNetworkId;

export const MidgardTxWitnessSetCompactSchema = Data.Object({
  addr_tx_wits: H32Schema,
  script_tx_wits: H32Schema,
  redeemer_tx_wits: H32Schema,
});
export type MidgardTxWitnessSetCompact = Data.Static<
  typeof MidgardTxWitnessSetCompactSchema
>;
export const MidgardTxWitnessSetCompact =
  MidgardTxWitnessSetCompactSchema as unknown as MidgardTxWitnessSetCompact;

export const ValidityRangeSchema = Data.Object({
  lower_bound: Data.Nullable(Data.Integer()),
  upper_bound: Data.Nullable(Data.Integer()),
});
export type ValidityRange = Data.Static<typeof ValidityRangeSchema>;
export const ValidityRange = ValidityRangeSchema as unknown as ValidityRange;

export const MidgardTxBodyCompactSchema = Data.Object({
  spend_inputs: H32Schema,
  reference_inputs: H32Schema,
  outputs: H32Schema,
  fee: Data.Integer(),
  validity_interval: ValidityRangeSchema,
  required_observers: H32Schema,
  required_signer_hashes: H32Schema,
  mint: H32Schema,
  script_integrity_hash: H32Schema,
  auxiliary_data_hash: H32Schema,
  network_id: MidgardNetworkIdSchema,
});
export type MidgardTxBodyCompact = Data.Static<
  typeof MidgardTxBodyCompactSchema
>;
export const MidgardTxBodyCompact =
  MidgardTxBodyCompactSchema as unknown as MidgardTxBodyCompact;

export const WithdrawalBodySchema = Data.Object({
  l2_outref: OutputReferenceSchema,
  l2_owner: Data.Bytes({ minLength: 28, maxLength: 28 }),
  l2_value: ValueSchema,
  l1_address: AddressSchema,
  l1_datum: CardanoDatumSchema,
});
export type WithdrawalBody = Data.Static<typeof WithdrawalBodySchema>;
export const WithdrawalBody = WithdrawalBodySchema as unknown as WithdrawalBody;

export const WithdrawalSignatureSchema = Data.Map(Data.Bytes(), Data.Bytes());
export type WithdrawalSignature = Data.Static<typeof WithdrawalSignatureSchema>;
export const WithdrawalSignature =
  WithdrawalSignatureSchema as unknown as WithdrawalSignature;

export const WithdrawalValiditySchema = Data.Enum([
  Data.Literal("WithdrawalIsValid"),
  Data.Literal("NonExistentWithdrawalUtxo"),
  Data.Literal("SpentWithdrawalUtxo"),
  Data.Literal("IncorrectWithdrawalOwner"),
  Data.Literal("IncorrectWithdrawalValue"),
  Data.Literal("IncorrectWithdrawalSignature"),
  Data.Literal("TooManyTokensInWithdrawal"),
]);
export type WithdrawalValidity = Data.Static<typeof WithdrawalValiditySchema>;
export const WithdrawalValidity =
  WithdrawalValiditySchema as unknown as WithdrawalValidity;

export const WithdrawalInfoSchema = Data.Object({
  body: WithdrawalBodySchema,
  signature: WithdrawalSignatureSchema,
  validity: WithdrawalValiditySchema,
});
export type WithdrawalInfo = Data.Static<typeof WithdrawalInfoSchema>;
export const WithdrawalInfo = WithdrawalInfoSchema as unknown as WithdrawalInfo;

export const WithdrawalEventSchema = Data.Object({
  id: OutputReferenceSchema,
  info: WithdrawalInfoSchema,
});
export type WithdrawalEvent = Data.Static<typeof WithdrawalEventSchema>;
export const WithdrawalEvent =
  WithdrawalEventSchema as unknown as WithdrawalEvent;
