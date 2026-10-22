import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { forcedVerdictForRejection } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import { buildPlaceholderValidationTraceMembers } from "../src/mpf/validation-trace.js";
import { verifyAndImportBlock } from "../src/mpf/verified-block-import.js";
import { computeDaPayloadRoots } from "../src/workers/commit-block-header/da-payload.js";
import { countsFromLengths, headerFor } from "./da-payload.record.js";
import { buildNativeTx } from "./native-transaction-integration.build-native-tx.js";
import { ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX } from "./native-transaction-integration.script-witness-item-to-versioned.js";

// A real canonical rejected forced transaction leaves the ledger unchanged but
// commits nonempty transaction, transition, mapping and validation roots.
export const fixture = async (
  options: {
    readonly withProgram?: boolean;
    readonly orderByte?: string;
    /** A forced transaction to carry instead, rejected with `rejection`. */
    readonly forced?: {
      readonly cbor: Buffer;
      readonly reason: SDK.RejectionReason;
    };
  } = {},
) => {
  const transaction = materializeMidgardForcedTxFromCanonical(
    buildNativeTx({
      spendInputOutRefs: [],
      referenceInputOutRefs: [],
      scriptWitnessItems:
        options.withProgram !== true
          ? []
          : [
              {
                language: "PlutusV3",
                scriptBytes: Buffer.from(
                  ALWAYS_SUCCEEDS_SPEND_SCRIPT_HEX,
                  "hex",
                ),
              },
            ],
    }).tx,
  );
  const cbor =
    options.forced?.cbor ?? encodeMidgardForcedTxCanonical(transaction);
  const reason = options.forced?.reason ?? "EmptyInputs";
  const { encodeForcedInclusionValueV1 } = await import(
    "../src/database/forcedTransactions.js"
  );
  const encoded = await Effect.runPromise(
    encodeForcedInclusionValueV1({
      nativeTxCbor: cbor,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      verdict: { ForcedTxInvalid: { reason } },
    }),
  );
  const orderId = {
    transactionId: (options.orderByte ?? "5a").repeat(32),
    outputIndex: 0n,
  };
  const key = Data.to(orderId, SDK.OutputReference);
  const eventKey: SDK.EventKey = {
    ForcedTransactionEventKey: { tx_order_id: orderId },
  };
  const eventCbor = Data.to(eventKey, SDK.EventKey);
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
  const counts = {
    ...countsFromLengths({ forcedTransactions: 1 }),
    validationTraceCount: 1n,
  };
  const header = headerFor(roots, counts);
  const [member] = await Effect.runPromise(
    buildPlaceholderValidationTraceMembers({
      transactions: [
        {
          eventKey,
          canonicalTransactionCbor: cbor,
          sourceKind: "forced",
          verdict: "rejected",
          rejectionReason: reason,
        },
      ],
    }),
  );
  if (member === undefined) throw new Error("missing fixture trace");
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: "00".repeat(28),
      header,
      deposits: [],
      withdrawals: [],
      transactions: [],
      transaction_preimages: [],
      forced_transactions: [[key, encoded.value.toString("hex")]],
      forced_transaction_preimages: [[key, cbor.toString("hex")]],
      transition_trace: [
        [
          encodeTransitionIntegerCbor(0n).toString("hex"),
          encodeTransitionStepCbor({
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "ForcedTransaction",
            pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
            post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          }).toString("hex"),
        ],
      ],
      event_to_step: [
        [
          eventCbor,
          encodeEventToStepValueCbor({
            step_index: 0n,
            phase: "ForcedTransaction",
          }).toString("hex"),
        ],
      ],
      validation_traces: [[eventCbor, member.valueCbor.toString("hex")]],
      validation_trace_witnesses: [],
      counts,
    },
  };
  return rebind(payload);
};

export const rebind = async (payload: SDK.DaPayload) => {
  const roots = await Effect.runPromise(computeDaPayloadRoots(payload));
  const header = {
    ...payload.block_body.header,
    ...roots,
    utxosRoot: payload.block_body.header.utxosRoot,
    // The header pins the validation-trace pair to the empty tree; the body
    // keeps the built members.
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    validationTraceCount: 0n,
  };
  return withHeader(payload, header);
};

const withHeader = async (payload: SDK.DaPayload, header: SDK.Header) => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  return {
    ...payload,
    block_body: { ...payload.block_body, header, header_hash: headerHash },
  };
};

/** The payload under a header that commits its built validation-trace pair. */
export const withBuiltValidationTracePair = async (payload: SDK.DaPayload) =>
  withHeader(payload, {
    ...payload.block_body.header,
    validationTracesRoot: (
      await Effect.runPromise(computeDaPayloadRoots(payload))
    ).validationTracesRoot,
    validationTraceCount: payload.block_body.counts.validationTraceCount,
  });

const imported = (
  payload: SDK.DaPayload,
  overrides: Partial<Parameters<typeof verifyAndImportBlock>[0]> = {},
) =>
  verifyAndImportBlock({
    header: payload.block_body.header,
    headerHash: payload.block_body.header_hash,
    parentHeaderHash: payload.block_body.header.prevHeaderHash,
    parentUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    parentEntries: [],
    payload,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    blockSlot: 0n,
    // The fixture block's node time; its traces were built at this clock.
    blockTimeMs: 2,
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    replayUserEvent: () => Effect.die("fixture has no user event"),
    verifyForcedSource: ({ source, canonicalTransactionCbor, rejection }) =>
      Effect.gen(function* () {
        const { encodeForcedInclusionValueV1 } = yield* Effect.promise(
          () => import("../src/database/forcedTransactions.js"),
        );
        const encoded = yield* encodeForcedInclusionValueV1({
          nativeTxCbor: canonicalTransactionCbor,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          verdict:
            rejection === undefined
              ? "ForcedTxValid"
              : forcedVerdictForRejection(rejection),
        });
        if (encoded.value.toString("hex") !== source[1])
          return yield* Effect.fail(new Error("forced verdict mismatch"));
      }),
    ...overrides,
  });

export const verdict = (
  payload: SDK.DaPayload,
  overrides: Partial<Parameters<typeof verifyAndImportBlock>[0]> = {},
) => Effect.runPromise(Effect.either(imported(payload, overrides)));
