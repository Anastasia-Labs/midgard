import {
  assertMidgardCekProgramMaterialBundle,
  decodeMidgardCekProgramMaterialDaEntry,
  encodeMidgardCekProgramEnvelope,
  type MidgardCekProgramEnvelope,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core/codec";
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  type MidgardForcedTxFull,
} from "@al-ft/midgard-core/codec/forced";
import { collectMidgardEventProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import { hexToBytes, normalizeHex } from "../utils/hex.js";
import {
  DaPayloadValidationError,
  decodeCanonicalData,
} from "./payload.da-payload-validation-error.js";
import {
  eventKeyFingerprint,
  l2EventKeyFingerprintFromTxId,
} from "./payload.source-event-fingerprints.js";
import { parseEventToStep } from "./payload.validate-trace-coverage.js";

/**
 * The L2 UTxO set immediately before a block: the parent block's post-state
 * (`body.utxos` of its payload, bound by `header.prevUtxosRoot`), or the
 * empty set before the first block. Keys are canonical out-ref item hex,
 * values are the exact output CBOR.
 */
export type PreBlockUtxos = readonly (readonly [string, Uint8Array])[];

type CanonicalTx =
  | ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor>
  | MidgardForcedTxFull;

type ReplayEvent =
  | { readonly kind: "withdrawal"; readonly spentOutRefHex: string | null }
  | {
      readonly kind: "forced";
      readonly fieldName: string;
      readonly tx: CanonicalTx;
      readonly effectful: boolean;
    }
  | {
      readonly kind: "normal";
      readonly fieldName: string;
      readonly tx: CanonicalTx;
    };

/**
 * The withdrawal value is the settled event info exactly as the event history
 * serialises it (map order preserved), authenticated by `withdrawals_root`,
 * so it is decoded without a Lucid re-encoding check.
 */
const decodeWithdrawalInfo = (
  valueHex: string,
  fieldName: string,
): SDK.WithdrawalInfo => {
  try {
    return LucidData.from(
      normalizeHex(valueHex, { fieldName }),
      SDK.WithdrawalInfo,
    ) as SDK.WithdrawalInfo;
  } catch (cause) {
    throw new DaPayloadValidationError(
      "malformed_trace",
      `failed to decode ${fieldName}`,
      { cause },
    );
  }
};

const outRefItemHexes = (
  preimageCbor: Uint8Array,
  fieldName: string,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, fieldName).map((item) =>
    Buffer.from(item).toString("hex"),
  );

const applyTransactionEffect = (
  state: Map<string, Uint8Array>,
  tx: CanonicalTx,
  fieldName: string,
): void => {
  for (const spent of outRefItemHexes(
    tx.body.spendInputsPreimageCbor,
    `${fieldName}.spend_inputs`,
  )) {
    state.delete(spent);
  }
  const txId = computeMidgardNativeTxId(tx.compact);
  const outputs = decodeMidgardNativeByteListPreimage(
    tx.body.outputsPreimageCbor,
    `${fieldName}.outputs`,
  );
  for (const [outputIndex, output] of outputs.entries()) {
    state.set(
      encodeMidgardSpendInputItem({ txId, outputIndex }).toString("hex"),
      output,
    );
  }
};

/**
 * Replays the block's events in `event_to_step` order from the state before
 * the block, resolving every event's inputs against the state immediately
 * before that event, as on Cardano. Each transaction's program set is the
 * shared per-event set (`collectMidgardEventProgramEnvelopes`) at its own
 * position, and the committed CEK program material must cover exactly the
 * union of those sets.
 *
 * A committed normal transaction must find every spent and reference input
 * at its position. A forced transaction is committed with its verdict
 * whatever it is, so an absent reference input contributes no program.
 */
export const validateDaPayloadEventProgramCoverage = (
  body: SDK.DaPayloadBody,
  preBlockUtxos: PreBlockUtxos,
): void => {
  const state = new Map<string, Uint8Array>(
    preBlockUtxos.map(([outRefHex, output]) => [
      normalizeHex(outRefHex, { fieldName: "pre-block utxos key" }),
      output,
    ]),
  );
  const stepByEvent = parseEventToStep(body);
  const stepOf = (fingerprint: string, fieldName: string): bigint => {
    const step = stepByEvent.get(fingerprint)?.step_index;
    if (step === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `${fieldName} has no event_to_step entry`,
      );
    }
    return step;
  };
  const events: { readonly step: bigint; readonly event: ReplayEvent }[] = [];

  for (const [index, [keyHex, valueHex]] of body.withdrawals.entries()) {
    const fieldName = `withdrawals[${index.toString()}]`;
    const withdrawalId = decodeCanonicalData<SDK.OutputReference>(
      keyHex,
      SDK.OutputReference as never,
      `${fieldName}.key`,
    );
    const info = decodeWithdrawalInfo(valueHex, `${fieldName}.value`);
    events.push({
      step: stepOf(
        eventKeyFingerprint({
          WithdrawalEventKey: { withdrawal_id: withdrawalId },
        }),
        fieldName,
      ),
      event: {
        kind: "withdrawal",
        spentOutRefHex:
          info.validity === "WithdrawalIsValid"
            ? encodeMidgardSpendInputItem({
                txId: hexToBytes(
                  info.body.l2_outref.transactionId,
                  `${fieldName}.l2_outref.transaction_id`,
                ),
                outputIndex: Number(info.body.l2_outref.outputIndex),
              }).toString("hex")
            : null,
      },
    });
  }

  const forcedPreimages = new Map(body.forced_transaction_preimages);
  for (const [
    index,
    [keyHex, valueHex],
  ] of body.forced_transactions.entries()) {
    const fieldName = `forced_transaction_preimages[${index.toString()}]`;
    const txOrderId = decodeCanonicalData<SDK.OutputReference>(
      keyHex,
      SDK.OutputReference as never,
      `forced_transactions[${index.toString()}].key`,
    );
    const forced = decodeCanonicalData<SDK.ForcedInclusionTxV1>(
      valueHex,
      SDK.ForcedInclusionTxV1Schema as never,
      `forced_transactions[${index.toString()}].value`,
    );
    events.push({
      step: stepOf(
        eventKeyFingerprint({
          ForcedTransactionEventKey: { tx_order_id: txOrderId },
        }),
        fieldName,
      ),
      event: {
        kind: "forced",
        fieldName,
        tx: decodeMidgardForcedTxFullFromCanonicalCbor(
          hexToBytes(forcedPreimages.get(keyHex) ?? "", fieldName),
        ),
        effectful: forced.verdict === "ForcedTxValid",
      },
    });
  }

  const preimages = new Map(body.transaction_preimages);
  for (const [index, [keyHex]] of body.transactions.entries()) {
    const fieldName = `transaction_preimages[${index.toString()}]`;
    const txId = normalizeHex(keyHex, {
      fieldName: `transactions[${index.toString()}].key`,
      byteLength: 32,
    });
    events.push({
      step: stepOf(l2EventKeyFingerprintFromTxId(txId), fieldName),
      event: {
        kind: "normal",
        fieldName,
        tx: decodeMidgardNativeTxFullFromCanonicalCbor(
          hexToBytes(preimages.get(keyHex) ?? "", fieldName),
        ),
      },
    });
  }

  // Deposits execute after every transaction, so no transaction resolves an
  // input against their outputs.
  events.sort((left, right) =>
    left.step < right.step ? -1 : left.step > right.step ? 1 : 0,
  );

  const programEnvelopes = new Map<string, MidgardCekProgramEnvelope>();
  const collect = (
    tx: CanonicalTx,
    fieldName: string,
    sourceKind: "normal" | "forced" = "normal",
  ): void => {
    let envelopes: readonly MidgardCekProgramEnvelope[];
    try {
      envelopes = collectMidgardEventProgramEnvelopes(
        tx,
        (outRefHex) => state.get(outRefHex),
        sourceKind,
      );
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        `${fieldName} has malformed V1 program envelopes`,
        { cause },
      );
    }
    for (const envelope of envelopes) {
      programEnvelopes.set(
        encodeMidgardCekProgramEnvelope(envelope).toString("hex"),
        envelope,
      );
    }
  };

  for (const { event } of events) {
    switch (event.kind) {
      case "withdrawal":
        if (event.spentOutRefHex !== null) state.delete(event.spentOutRefHex);
        break;
      case "forced":
        collect(event.tx, event.fieldName, "forced");
        if (event.effectful) {
          applyTransactionEffect(state, event.tx, event.fieldName);
        }
        break;
      case "normal": {
        for (const [role, preimageCbor] of [
          ["spend_inputs", event.tx.body.spendInputsPreimageCbor],
          ["reference_inputs", event.tx.body.referenceInputsPreimageCbor],
        ] as const) {
          for (const outRefHex of outRefItemHexes(
            preimageCbor,
            `${event.fieldName}.${role}`,
          )) {
            if (!state.has(outRefHex)) {
              throw new DaPayloadValidationError(
                "malformed_transaction",
                `${event.fieldName} ${role} entry ${outRefHex} is absent from the state immediately before the transaction`,
              );
            }
          }
        }
        collect(event.tx, event.fieldName);
        applyTransactionEffect(state, event.tx, event.fieldName);
        break;
      }
    }
  }

  try {
    const material = body.cek_program_material.map(([rootHex, valueHex]) =>
      decodeMidgardCekProgramMaterialDaEntry(
        hexToBytes(rootHex, "cek_program_material.root"),
        hexToBytes(valueHex, "cek_program_material.value"),
      ),
    );
    assertMidgardCekProgramMaterialBundle(
      [...programEnvelopes.values()],
      material,
    );
  } catch (cause) {
    throw new DaPayloadValidationError(
      "coverage_mismatch",
      "CEK program material does not exactly cover every program of every event at its position",
      { cause },
    );
  }
};
