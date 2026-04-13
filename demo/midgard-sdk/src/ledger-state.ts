import { Data } from "@lucid-evolution/lucid";
import {
  AddressSchema,
  MerkleRootSchema,
  OutputReferenceSchema,
  POSIXTimeSchema,
  PubKeyHashSchema,
  ValueSchema,
} from "./common.js";

/**
 * Ledger-state data schemas shared between the SDK and on-chain code.
 *
 * These definitions mirror the serialized structures consumed by Midgard
 * validators, so their field names and byte widths should stay aligned with the
 * on-chain counterparts.
 */
export const HeaderHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });
export type HeaderHash = Data.Static<typeof HeaderHashSchema>;
export const HeaderHash = HeaderHashSchema as unknown as HeaderHash;

/**
 * Header committed for each Midgard block.
 */
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

/**
 * Minimal confirmed-state view carried forward after block commitment.
 */
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

// HERE MERGED
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

export const TxOrderEventSchema = Data.Object({
  id: OutputReferenceSchema,
  tx: Data.Bytes(),
});
export type TxOrderEvent = Data.Static<typeof TxOrderEventSchema>;
export const TxOrderEvent = TxOrderEventSchema as unknown as TxOrderEvent;

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
