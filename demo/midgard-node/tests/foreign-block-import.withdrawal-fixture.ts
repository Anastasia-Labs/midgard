import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { foreignReplayMaterial } from "../src/landed-blocks/replay-foreign.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import { verifyAndImportBlock } from "../src/mpf/verified-block-import.js";
import { computeDaPayloadRoots } from "../src/workers/commit-block-header/da-payload.compute-da-payload-roots.js";

/**
 * A foreign block settling one honestly signed withdrawal of a ledger output
 * its owner holds. With `wrongSignature` the L1 event the follower knows
 * carries a signature over other bytes, while the block still commits the
 * honest verdict: the block's verdict is wrong.
 */
export const withdrawalFixture = async (
  options: { readonly wrongSignature?: boolean } = {},
) => {
  const privateKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x31));
  const owner = privateKey.to_public().hash().to_hex();
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(privateKey.to_public().hash()),
  )
    .to_address()
    .to_bech32();
  const value: SDK.Value = new Map([["", new Map([["", 7_000_000n]])]]);
  const body: SDK.WithdrawalBody = {
    l2_outref: { transactionId: "ab".repeat(32), outputIndex: 0n },
    l2_owner: owner,
    l2_value: value,
    l1_address: await Effect.runPromise(SDK.addressDataFromBech32(address)),
    l1_datum: "NoDatum",
  };
  const signature = SDK.signWithdrawalBody(privateKey, body);
  const info = (signed: SDK.WithdrawalSignature) =>
    Data.to(
      { body, signature: signed, validity: "WithdrawalIsValid" },
      SDK.WithdrawalInfo,
    );
  const input = {
    l2Owner: owner,
    l2ValueCbor: Data.to(value, SDK.Value),
    eventInfoCbor: info(signature),
    ledgerOutRef: encodeMidgardSpendInputItem({
      txId: Buffer.from(body.l2_outref.transactionId, "hex"),
      outputIndex: 0,
    }),
    ledgerOutput: encodeMidgardTxOutput({
      address: midgardAddressFromText(address),
      value: { lovelace: 7_000_000n, assets: new Map() },
    }),
  };
  const classified = await Effect.runPromise(
    SDK.classifyWithdrawalFromLedger(input),
  );
  if (classified.validity !== "WithdrawalIsValid")
    throw new Error(`withdrawal fixture classified ${classified.validity}`);
  const parentEntries = [
    { outref: input.ledgerOutRef, output: input.ledgerOutput },
  ];
  const prior = await Effect.runPromise(
    computeLedgerMpfRootFromLedgerEntries(parentEntries),
  );
  const id = { transactionId: "cd".repeat(32), outputIndex: 0n };
  const key = Data.to(id, SDK.OutputReference);
  const eventKey: SDK.EventKey = { WithdrawalEventKey: { withdrawal_id: id } };
  const counts: SDK.DaPayloadCounts = {
    withdrawalCount: 1n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 0n,
  };
  const empty = SDK.EMPTY_MERKLE_TREE_ROOT;
  const header: SDK.Header = {
    prevUtxosRoot: prior,
    utxosRoot: empty,
    withdrawalsRoot: empty,
    forcedTransactionsRoot: empty,
    transactionsRoot: empty,
    depositsRoot: empty,
    transitionTraceRoot: empty,
    eventToStepRoot: empty,
    validationTracesRoot: empty,
    ...counts,
    startTime: 1n,
    endTime: 2n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: "11".repeat(28),
    operatorVkey: "22".repeat(28),
    protocolVersion: 1n,
  };
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: "00".repeat(28),
      header,
      counts,
      deposits: [],
      transactions: [],
      transaction_preimages: [],
      cek_program_material: [],
      forced_transactions: [],
      forced_transaction_preimages: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      withdrawals: [[key, classified.settlementEventInfo.toString("hex")]],
      transition_trace: [
        [
          Data.to(0n),
          encodeTransitionStepCbor({
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "Withdrawal",
            pre_utxos_root: prior,
            post_utxos_root: empty,
          }).toString("hex"),
        ],
      ],
      event_to_step: [
        [
          Data.to(eventKey, SDK.EventKey),
          encodeEventToStepValueCbor({
            step_index: 0n,
            phase: "Withdrawal",
          }).toString("hex"),
        ],
      ],
    },
  };
  const roots = await Effect.runPromise(computeDaPayloadRoots(payload));
  payload.block_body.header = { ...header, ...roots, utxosRoot: empty };
  payload.block_body.header_hash = await Effect.runPromise(
    SDK.hashBlockHeader(payload.block_body.header),
  );
  const known = new Map([
    [
      key,
      {
        l2Outref: Data.to(body.l2_outref, SDK.OutputReference),
        l2Owner: owner,
        l2Value: input.l2ValueCbor,
        info:
          options.wrongSignature === true
            ? info([
                signature[0],
                privateKey.sign(new Uint8Array(32).fill(0xa5)).to_hex(),
              ])
            : input.eventInfoCbor,
      },
    ],
  ]);
  return {
    headerHash: payload.block_body.header_hash,
    imported: () =>
      Effect.runPromise(
        Effect.either(
          verifyAndImportBlock({
            ...foreignReplayMaterial(new Map(), known, new Map(), []),
            header: payload.block_body.header,
            headerHash: payload.block_body.header_hash,
            parentHeaderHash: header.prevHeaderHash,
            parentUtxosRoot: prior,
            parentEntries,
            payload,
            expectedNetworkId: 0n,
            minFeeA: 0n,
            minFeeB: 0n,
            blockSlot: 0n,
            blockTimeMs: 2,
            l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          }),
        ),
      ),
  };
};
