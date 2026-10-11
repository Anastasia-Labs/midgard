/** The case table of the event-history admission agreement test: every order
 * shape admission must refuse because the node cannot decode or process it,
 * each next to an accepted control. */
import * as SDK from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";

import type { Group } from "./event-admission-agreement-emulator.node-decodes.js";
import type { Swap } from "./event-admission-agreement-emulator.sdk-reading.js";

export type Row = Readonly<{
  group: Group;
  name: string;
  admitted: boolean;
  /** Field replacements in the payload, by constructor-field path. */
  fields?: readonly (readonly [readonly number[], string])[];
  /** The external payload's reclamation credential. */
  reclaimAuth?: SDK.CredentialD;
  /** Withdrawal fields the SDK writes into the payload it funds. */
  withdrawal?: Readonly<{
    outputIndex?: bigint;
    l1Datum?: SDK.CardanoDatum;
    refundDatum?: SDK.CardanoDatum;
  }>;
  swap?: Swap;
}>;

const hash28 = "a7".repeat(28);
const datumHash = (bytes: number): SDK.CardanoDatum => ({
  DatumHash: { hash: "a8".repeat(bytes) },
});
const shortHash: Swap = { short: "a5".repeat(27), padded: "a5".repeat(28) };
const shortTxId: Swap = { short: "a6".repeat(31), padded: "a6".repeat(32) };
const key = (hash: string) => Data.to(new Constr(0, [hash]));
const script = (hash: string) => Data.to(new Constr(1, [hash]));
const noStake = Data.to(new Constr(1, []));
const inlineStake = (hash: string) =>
  Data.to(new Constr(0, [new Constr(0, [new Constr(0, [hash])])]));
/** An address with a pointer stake credential, in the SDK's (ledger) shape. */
const pointerAddress = Data.to(
  {
    paymentCredential: { PublicKeyCredential: [hash28] },
    stakeCredential: {
      Pointer: { slotNumber: 1n, transactionIndex: 2n, certificateIndex: 3n },
    },
  },
  SDK.AddressData,
);

// Payload paths. Deposit: payload.event.info.l2_address = [0, 1, 0].
// Withdrawal: payload.event.info.body = [0, 1, 0]; refund_address = [1].
const L2_ADDRESS = [0, 1, 0];
const BODY = [0, 1, 0];
const L1_ADDRESS = [...BODY, 3];
const REFUND_ADDRESS = [1];

/** Every order shape admission must refuse because the node cannot decode it,
 * each next to an accepted control that differs from it in one field. */
export const ROWS: readonly Row[] = [
  { group: "deposit", name: "the SDK's deposit", admitted: true },
  {
    group: "deposit",
    name: "a script payment credential",
    admitted: true,
    fields: [[[...L2_ADDRESS, 0], script(hash28)]],
  },
  {
    group: "deposit",
    name: "no stake credential",
    admitted: true,
    fields: [[[...L2_ADDRESS, 1], noStake]],
  },
  {
    group: "deposit",
    name: "a 27-byte payment key hash",
    admitted: false,
    fields: [[[...L2_ADDRESS, 0], key(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "deposit",
    name: "a 27-byte inline stake key hash",
    admitted: false,
    fields: [[[...L2_ADDRESS, 1], inlineStake(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "deposit",
    name: "a pointer stake credential",
    admitted: false,
    fields: [[L2_ADDRESS, pointerAddress]],
  },
  { group: "withdrawal", name: "the SDK's withdrawal", admitted: true },
  {
    group: "withdrawal",
    name: "an L1 address without stake credential",
    admitted: true,
    fields: [[[...L1_ADDRESS, 1], noStake]],
  },
  {
    group: "withdrawal",
    name: "a script refund credential",
    admitted: true,
    fields: [[[...REFUND_ADDRESS, 0], script(hash28)]],
  },
  {
    group: "withdrawal",
    name: "a 31-byte L2 transaction id",
    admitted: false,
    fields: [[[...BODY, 0, 0], Data.to(shortTxId.short)]],
    swap: shortTxId,
  },
  {
    group: "withdrawal",
    name: "a 27-byte L2 owner",
    admitted: false,
    fields: [[[...BODY, 1], Data.to(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "withdrawal",
    name: "a 27-byte L1 payment key hash",
    admitted: false,
    fields: [[[...L1_ADDRESS, 0], key(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "withdrawal",
    name: "a 27-byte L1 inline stake key hash",
    admitted: false,
    fields: [[[...L1_ADDRESS, 1], inlineStake(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "withdrawal",
    name: "a pointer L1 stake credential",
    admitted: false,
    fields: [[L1_ADDRESS, pointerAddress]],
  },
  {
    group: "withdrawal",
    name: "a 27-byte refund payment script hash",
    admitted: false,
    fields: [[[...REFUND_ADDRESS, 0], script(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "withdrawal",
    name: "a 27-byte refund inline stake key hash",
    admitted: false,
    fields: [[[...REFUND_ADDRESS, 1], inlineStake(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "withdrawal",
    name: "a pointer refund stake credential",
    admitted: false,
    fields: [[REFUND_ADDRESS, pointerAddress]],
  },
  {
    group: "withdrawal",
    name: "L2 output index 0",
    admitted: true,
    withdrawal: { outputIndex: 0n },
  },
  {
    group: "withdrawal",
    name: "L2 output index 65535",
    admitted: true,
    withdrawal: { outputIndex: 65_535n },
  },
  {
    group: "withdrawal",
    name: "L2 output index 65536",
    admitted: false,
    withdrawal: { outputIndex: 65_536n },
  },
  {
    group: "withdrawal",
    name: "L2 output index -1",
    admitted: false,
    withdrawal: { outputIndex: -1n },
  },
  {
    group: "withdrawal",
    name: "32-byte payout and refund datum hashes",
    admitted: true,
    withdrawal: { l1Datum: datumHash(32), refundDatum: datumHash(32) },
  },
  {
    group: "withdrawal",
    name: "a 31-byte payout datum hash",
    admitted: false,
    withdrawal: { l1Datum: datumHash(31) },
  },
  {
    group: "withdrawal",
    name: "a 33-byte payout datum hash",
    admitted: false,
    withdrawal: { l1Datum: datumHash(33) },
  },
  {
    group: "withdrawal",
    name: "a 31-byte refund datum hash",
    admitted: false,
    withdrawal: { refundDatum: datumHash(31) },
  },
  {
    group: "withdrawal",
    name: "a 33-byte refund datum hash",
    admitted: false,
    withdrawal: { refundDatum: datumHash(33) },
  },
  { group: "external deposit", name: "the SDK's payload", admitted: true },
  {
    group: "external deposit",
    name: "a script reclamation credential",
    admitted: true,
    reclaimAuth: { ScriptCredential: [hash28] },
  },
  {
    group: "external deposit",
    name: "a 27-byte reclamation key hash",
    admitted: false,
    reclaimAuth: { PublicKeyCredential: [shortHash.short] },
    swap: shortHash,
  },
];
