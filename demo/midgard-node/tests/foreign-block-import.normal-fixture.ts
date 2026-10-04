import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  replayValidationMachineEvent,
  validationMachineLedgerRoot,
} from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { encodeTransactionRootValue } from "../src/mpf/ledger-hydration.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import { countsFromLengths, headerFor } from "./da-payload.record.js";
import { rebind, verdict } from "./foreign-block-import.fixture.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";
import { buildNativeTx } from "./native-transaction-integration.build-native-tx.js";

/** Real signed native transfer on a fabricated funded prior ledger. The alias
 * control regenerates every semantic commitment using the wrong event identity. */
export const normalFixture = async (alias = false) => {
  const signer = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x17));
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(signer.to_public().hash()),
  ).to_address();
  const output = makeMidgardTxOutput(
    address,
    CML.Value.from_coin(3_000_000n),
  ).to_cbor_bytes();
  const spent = makeOutRefCbor(0x11);
  const tx = buildNativeTx({
    spendInputOutRefs: [spent],
    referenceInputOutRefs: [],
    outputCbors: [output],
    witnessMode: "valid",
    witnessSignerPrivateKey: signer,
  });
  const sourceKey = alias ? "a7".repeat(32) : tx.txId.toString("hex");
  const event: SDK.EventKey = { L2TransactionEventKey: { tx_id: sourceKey } };
  const eventCbor = Data.to(event, SDK.EventKey);
  const prior = (
    await validationMachineLedgerRoot([{ outRef: spent, output }])
  ).toString("hex");
  const replay = await Effect.runPromise(
    replayValidationMachineEvent({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(eventCbor, "hex"),
      canonicalTransactionCbor: tx.txCbor,
      ledgerWitnessEntries: [{ outRef: spent, output }],
      priorUtxosRoot: prior,
      blockEndTimeMs: 2,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      sourceKind: "normal",
    }),
  );
  if (replay.replayInput.expectedVerdict !== "accepted")
    throw new Error(`normal fixture was rejected: ${replay.rejection?.code}`);
  const d = replay.trace.tree.descriptor;
  const counts = {
    ...countsFromLengths({ transactions: 1 }),
    validationTraceCount: 1n,
  };
  const roots = Object.fromEntries(
    [
      "utxosRoot",
      "withdrawalsRoot",
      "forcedTransactionsRoot",
      "transactionsRoot",
      "depositsRoot",
      "transitionTraceRoot",
      "eventToStepRoot",
      "validationTracesRoot",
    ].map((key) => [key, SDK.EMPTY_MERKLE_TREE_ROOT]),
  ) as Parameters<typeof headerFor>[0];
  const header = { ...headerFor(roots, counts), prevUtxosRoot: prior };
  const payload: SDK.DaPayload = {
    version: 1n,
    block_body: {
      header,
      header_hash: "00".repeat(28),
      utxos: replay.statePatch.upsertedOutRefs.map(([key, value]) => [
        key,
        value.toString("hex"),
      ]),
      transactions: [
        [sourceKey, encodeTransactionRootValue(tx.txCbor).toString("hex")],
      ],
      transaction_preimages: [[sourceKey, tx.txCbor.toString("hex")]],
      withdrawals: [],
      forced_transactions: [],
      forced_transaction_preimages: [],
      deposits: [],
      cek_program_material: [],
      transition_trace: [
        [
          encodeTransitionIntegerCbor(0n).toString("hex"),
          encodeTransitionStepCbor({
            schema_version: 1n,
            step_index: 0n,
            event_key: event,
            phase: "L2Transaction",
            pre_utxos_root: prior,
            post_utxos_root: replay.replayInput.postUtxosRoot,
          }).toString("hex"),
        ],
      ],
      event_to_step: [
        [
          eventCbor,
          encodeEventToStepValueCbor({
            step_index: 0n,
            phase: "L2Transaction",
          }).toString("hex"),
        ],
      ],
      validation_traces: [
        [
          eventCbor,
          Data.to(
            {
              schema_version: BigInt(d.schemaVersion),
              machine_version: BigInt(d.machineVersion),
              trace_root: d.traceRoot.toString("hex"),
              step_count: BigInt(d.stepCount),
              initial_state_hash: d.initialStateHash.toString("hex"),
              terminal_state_hash: d.terminalStateHash.toString("hex"),
              verdict: "Accepted",
              rejection_code_hash: d.rejectionCodeHash.toString("hex"),
            },
            SDK.ValidationTraceDescriptor,
          ),
        ],
      ],
      validation_trace_witnesses: [],
      counts,
    },
  };
  const bound = await rebind(payload);
  return {
    payload: bound,
    sourceKey,
    transactionId: tx.txId.toString("hex"),
    imported: () =>
      verdict(bound, {
        parentUtxosRoot: prior,
        parentEntries: [{ outref: spent, output }],
      }),
  };
};
