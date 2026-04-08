import { Data } from "@lucid-evolution/lucid";
import {
  MerkleRootSchema,
  POSIXTimeSchema,
  PubKeyHashSchema,
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
