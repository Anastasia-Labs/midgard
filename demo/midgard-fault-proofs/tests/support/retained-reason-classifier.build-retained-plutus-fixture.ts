import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  MIDGARD_CONSENSUS_PROFILE,
  midgardFieldCommitment,
  protectMidgardAddress,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical as forcedTraceBytes,
  materializeMidgardForcedTxFromCanonical as forcedTraceView,
} from "@al-ft/midgard-core/codec/forced";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import {
  encodeDaPayload,
  type EventKey,
  EventKeySchema,
  GENESIS_HEADER_HASH,
  hashBlockHeader,
} from "@al-ft/midgard-sdk";
import {
  buildMidgardCanonicalCekProgram,
  replayValidationMachineEvent,
} from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "../helpers/canonical-block-evidence-fixture.js";
import {
  buildRetainedValidationBlockFixture,
  type RetainedPlutusFixtureOptions,
  retainValidationTrace,
} from "./retained-reason-classifier.build-retained-validation-block-fixture.js";

/** Re-frames a predecessor fixture under another operator and block window. */
const framePredecessor = async (
  fixture: Awaited<ReturnType<typeof buildCanonicalBlockFixture>>,
  frame: NonNullable<RetainedPlutusFixtureOptions["predecessorFrame"]>,
) => {
  const header = {
    ...fixture.header,
    operatorVkey: frame.operatorVkey,
    startTime: frame.startTime,
    endTime: frame.endTime,
  };
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const payload = {
    ...fixture.payload,
    block_body: {
      ...fixture.payload.block_body,
      header,
      header_hash: headerHash,
    },
  };
  return {
    ...fixture,
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};

const buildRetainedPlutusFixture = async (
  claim: Parameters<typeof retainValidationTrace>[0]["claim"],
  flatProgramHex: string,
  options: RetainedPlutusFixtureOptions = {},
) => {
  const program = buildMidgardCanonicalCekProgram(
    Buffer.from(flatProgramHex, "hex"),
  );
  const script = {
    language: "PlutusV3" as const,
    scriptBytes: program.envelopeCbor,
  };
  const spent = options.ledgerInput?.outRef ?? outRefCbor(0x1d, 0n);
  const value = {
    lovelace: 10_000_000n,
    assets: new Map<string, Map<string, bigint>>(),
  };
  const inputOutput =
    options.ledgerInput?.output ??
    encodeMidgardTxOutput({
      address: protectMidgardAddress(
        Buffer.concat([
          Buffer.from([0x70]),
          Buffer.from(hashMidgardVersionedScript(script), "hex"),
        ]),
      ),
      value,
    });
  const output = encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
    value,
  });
  const redeemer = encodeMidgardRedeemerWitnessItem({
    purpose: "Spend",
    index: 0n,
    redeemerCbor: Buffer.from(Data.to(new Constr(0, [])), "hex"),
    executionUnits: { memory: 1_000_000_000n, steps: 1_000_000_000n },
  });
  const transaction = buildFixtureTransaction({
    spendInputs: [spent],
    outputs: [output],
    fee: 0n,
    networkId: 0n,
    ...(options.validityInterval === undefined
      ? {}
      : {
          validityIntervalStart: options.validityInterval.start,
          validityIntervalEnd: options.validityInterval.end,
        }),
    scriptWitnesses: [encodeMidgardVersionedScript(script)],
    redeemerWitnesses: [redeemer],
    scriptIntegrityHash: computeScriptIntegrityHashForLanguages(
      midgardFieldCommitment(encodeMidgardFieldPreimage([redeemer])),
      ["PlutusV3"],
    ),
  });
  const predecessor =
    options.predecessor ??
    (await (async () => {
      const fixture = await buildCanonicalBlockFixture({
        transactions: [],
        utxos: [{ key: spent, value: inputOutput }],
        prevHeaderHash: GENESIS_HEADER_HASH,
      });
      return options.predecessorFrame === undefined
        ? fixture
        : await framePredecessor(fixture, options.predecessorFrame);
    })());
  const sourceKind = options.sourceKind ?? "normal";
  const orderKey = options.orderKey ?? {
    transactionId: "52".repeat(32),
    outputIndex: 0n,
  };
  const eventKey: EventKey =
    sourceKind === "normal"
      ? {
          L2TransactionEventKey: { tx_id: transaction.txId },
        }
      : { ForcedTransactionEventKey: { tx_order_id: orderKey } };
  const blockEndTimeMs = options.blockEndTimeMs ?? 1_750_000_000_000;
  const blockSlot = options.blockSlot ?? 100n;
  const material = [...program.material.values()];
  const replay = await Effect.runPromise(
    replayValidationMachineEvent({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(
        Data.to(eventKey, asLucidSchema(EventKeySchema)),
        "hex",
      ),
      canonicalTransactionCbor:
        sourceKind === "forced"
          ? forcedTraceBytes(
              forcedTraceView(
                decodeMidgardNativeTxFullFromCanonicalCbor(
                  transaction.canonicalCbor,
                ),
              ),
            )
          : transaction.canonicalCbor,
      programMaterialSidecarCbor:
        encodeMidgardCekProgramMaterialSidecar(material),
      ...(sourceKind === "normal"
        ? { sourceKind: "normal" as const }
        : {
            sourceKind: "forced" as const,
          }),
      ledgerWitnessEntries: [{ outRef: spent, output: inputOutput }],
      priorUtxosRoot: predecessor.header.utxosRoot,
      blockEndTimeMs,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot,
    }),
  );
  const block = await buildRetainedValidationBlockFixture({
    subject:
      sourceKind === "normal"
        ? {
            kind: "normal",
            nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
              transaction.canonicalCbor,
            ),
          }
        : {
            kind: "forced",
            nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
              transaction.canonicalCbor,
            ),
            orderKey,
            verdict:
              claim.verdict === "accepted"
                ? "ForcedTxValid"
                : { ForcedTxInvalid: { reason: claim.reason } },
          },
    priorLedgerRoot: predecessor.header.utxosRoot,
    prevHeaderHash: predecessor.headerHash,
    blockEndTimeMs,
    blockSlot,
    ...(options.operatorVkey === undefined
      ? {}
      : { operatorVkey: options.operatorVkey }),
    ...(options.blockStartTimeMs === undefined
      ? {}
      : { blockStartTimeMs: options.blockStartTimeMs }),
    ...retainValidationTrace({
      trace: options.committedTrace?.(replay.trace) ?? replay.trace,
      eventKey,
      claim,
    }),
    programMaterialEntries: material.map((entry) => [
      Buffer.from(entry.root).toString("hex"),
      encodeMidgardCekProgramMaterialDaValue(entry).toString("hex"),
    ]),
    postLedgerEntries:
      (sourceKind === "forced" && claim.verdict === "rejected") ||
      replay.trace.verdict === "rejected"
        ? [{ outRef: spent, output: inputOutput }]
        : replay.statePatch.upsertedOutRefs.map(([outRef, output]) => ({
            outRef: Buffer.from(outRef, "hex"),
            output,
          })),
  });
  return { block, predecessor, replay, orderKey, transaction };
};

/** Existing UPLC 1.1.0 lambda(var0) from validation-machine-event-replay.test.ts. */
export const buildRetainedPlutusIdentityFixture = (
  claim: Parameters<typeof retainValidationTrace>[0]["claim"],
  options: RetainedPlutusFixtureOptions = {},
) => buildRetainedPlutusFixture(claim, "010100200101", options);

/** Exact existing bounded CEK unbound-variable refusal from cek-executor.test.ts. */
export const buildRetainedPlutusUnboundVariableFixture = (
  claim: Parameters<typeof retainValidationTrace>[0]["claim"],
  options: RetainedPlutusFixtureOptions = {},
) => buildRetainedPlutusFixture(claim, "0101000011", options);
