import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusForcedTxCbor } from "@al-ft/midgard-core/consensus-validation";
import * as SDK from "@al-ft/midgard-sdk";
import { validatePhaseASingle } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { decodeDaPayloadStrict } from "da-committee-node/da/payload";
import { Effect, Logger } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import { forcedRejectionVerdict } from "../src/mpf/event-window.forced-verdict-for-rejection.js";
import {
  canonicalTransaction,
  TEST_ADDRESS,
} from "./forced-transactions.make-signed-effectful-transaction.js";
const sidecar = encodeMidgardCekProgramMaterialSidecar([]);
const time = new Date("2026-07-23T12:01:00.000Z");
const validation = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  bucketConcurrency: 1,
  slotForUnixTime: () => 100n,
};
const forced = (tx: ReturnType<typeof canonicalTransaction>) =>
  encodeMidgardForcedTxCanonical(materializeMidgardForcedTxFromCanonical(tx));
// Same admission boundary, but unsupported shapes must never reach the trace builder.
describe("kept forced admission screens", () => {
  const retained = [
    [
      "auxiliary hash",
      "E_AUX_DATA_FORBIDDEN",
      () => {
        const tx = canonicalTransaction();
        return forced({
          ...tx,
          body: { ...tx.body, auxiliaryDataHash: Buffer.alloc(32, 1) },
        });
      },
    ],
    [
      "witness envelope",
      "E_SCRIPT_PROGRAM_ENCODING",
      () => {
        const tx = canonicalTransaction();
        return forced({
          ...tx,
          witnessSet: {
            ...tx.witnessSet,
            scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
              { language: "MidgardV1", scriptBytes: Buffer.from("80", "hex") },
            ]),
          },
        });
      },
    ],
    [
      "reference envelope",
      "E_SCRIPT_PROGRAM_ENCODING",
      () => {
        const tx = canonicalTransaction();
        return forced({
          ...tx,
          body: {
            ...tx.body,
            outputsPreimageCbor: encodeCbor([
              encodeMidgardTxOutput({
                address: TEST_ADDRESS,
                value: { lovelace: 100_000_000n, assets: new Map() },
                script_ref: {
                  language: "MidgardV1",
                  scriptBytes: Buffer.from("80", "hex"),
                },
              }),
            ]),
          },
        });
      },
    ],
    [
      "value size",
      "E_VALUE_SIZE",
      () => {
        const tx = canonicalTransaction();
        const assets = new Map(
          Array.from(
            { length: 170 },
            (_, i) => [i.toString(16).padStart(64, "0"), 1n] as const,
          ),
        );
        return forced({
          ...tx,
          body: {
            ...tx.body,
            outputsPreimageCbor: encodeCbor([
              encodeMidgardTxOutput({
                address: TEST_ADDRESS,
                value: {
                  lovelace: 100_000_000n,
                  assets: new Map([["01".repeat(28), assets]]),
                },
              }),
            ]),
          },
        });
      },
    ],
  ] as const;
  it.each(retained)(
    "typed-stops %s at ingest with no encoded verdict",
    async (_name, code, bytes) => {
      const nativeTxCbor = bytes();
      expect(validateMidgardConsensusForcedTxCbor(nativeTxCbor)?.code).toBe(
        code,
      );
      const alarms: unknown[] = [];
      const alarmLogger = Logger.make(({ message }) => alarms.push(message));
      const result = await Effect.runPromise(
        Effect.either(
          ForcedTransactionsDB.encodeForcedInclusionValueV1({
            nativeTxCbor,
            verdict: "ForcedTxValid",
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ).pipe(
          Effect.provide(Logger.replace(Logger.defaultLogger, alarmLogger)),
        ),
      );
      expect(alarms).toHaveLength(1);
      expect(String(alarms[0])).toContain("no verdict encoded");
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        expect(result.left._tag).toBe("MidgardForcedTxAdmissionStopped");
        expect(result.left).toMatchObject({ code, retryable: false });
      }
      const phaseA = validatePhaseASingle(
        {
          txId: computeMidgardNativeTxId(
            decodeMidgardForcedTxFullFromCanonicalCbor(nativeTxCbor),
          ),
          txCbor: nativeTxCbor,
          sourceKind: "forced",
          arrivalSeq: 0n,
          createdAt: time,
          programMaterialSidecarCbor: sidecar,
        },
        { ...validation, concurrency: 1, strictnessProfile: "phase1_midgard" },
      );
      // The forced verdict writer, exercised separately, turns these unprovable
      // pre-screen codes into ForcedRejectionUnsupported; it cannot commit them.
      expect("ledgerTx" in phaseA).toBe(false);
      if ("ledgerTx" in phaseA)
        throw new Error("kept violation entered Phase B");
      expect(phaseA.code).toBe(code);
      const stopped = await Effect.runPromise(
        Effect.either(forcedRejectionVerdict(phaseA)),
      );
      expect(stopped._tag).toBe("Left");
      if (stopped._tag === "Left")
        expect(stopped.left._tag).toBe("ForcedRejectionStopped");
      const source = deriveMidgardForcedTxProofSource(
        decodeMidgardForcedTxFullFromCanonicalCbor(nativeTxCbor),
      );
      const txId = computeMidgardNativeTxId(
        decodeMidgardForcedTxFullFromCanonicalCbor(nativeTxCbor),
      ).toString("hex");
      const counts = {
        withdrawalCount: 0n,
        forcedTransactionCount: 1n,
        l2TransactionCount: 0n,
        depositCount: 0n,
        totalEventCount: 1n,
        transitionStepCount: 1n,
        validationTraceCount: 1n,
      };
      const header: SDK.Header = {
        blockSlot: 100n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: "00".repeat(28),
        operatorVkey: "00".repeat(28),
        startTime: 0n,
        endTime: 1n,
        prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        protocolVersion: 1n,
        ...counts,
      };
      const orderKey = Data.to(
        { transactionId: "22".repeat(32), outputIndex: 0n },
        SDK.OutputReference,
      );
      for (const verdict of [
        "ForcedTxValid",
        { ForcedTxInvalid: { reason: "EmptyInputs" } },
      ] satisfies SDK.OperatorVerdict[]) {
        // A hostile payload may claim either polarity. There is deliberately no
        // trace: the kept screen must stop before a verdict or descriptor can be admitted.
        const payload: SDK.DaPayload = {
          version: 1n,
          block_body: {
            header,
            header_hash: await Effect.runPromise(SDK.hashBlockHeader(header)),
            counts,
            utxos: [],
            transactions: [],
            transaction_preimages: [],
            forced_transactions: [
              [
                orderKey,
                Data.to(
                  {
                    tx_id: txId,
                    submitted_source: {
                      compact_cbor: source.compactCbor.toString("hex"),
                      witness_set_compact_cbor:
                        source.witnessSetCompactCbor.toString("hex"),
                      field_preimage_lengths_cbor:
                        source.fieldPreimageLengthsCbor.toString("hex"),
                    },
                    verdict,
                  },
                  SDK.ForcedInclusionTxV1,
                ),
              ],
            ],
            forced_transaction_preimages: [
              [orderKey, nativeTxCbor.toString("hex")],
            ],
            withdrawals: [],
            deposits: [],
            transition_trace: [],
            event_to_step: [],
            validation_traces: [],
            validation_trace_witnesses: [],
            cek_program_material: [],
          },
        };
        try {
          decodeDaPayloadStrict(SDK.encodeDaPayload(payload));
          throw new Error("kept shape admitted by committee");
        } catch (error) {
          expect(error).toMatchObject({
            _tag: "MidgardForcedTxAdmissionStopped",
            code,
            retryable: false,
          });
        }
      }
    },
  );
});
