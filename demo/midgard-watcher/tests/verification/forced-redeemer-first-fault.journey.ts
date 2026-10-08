import { createHash } from "node:crypto";

import {
  encodeMidgardCekProgramMaterialSidecar,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { EventKey, ForcedInclusionTxV1 } from "@al-ft/midgard-sdk";
import {
  buildMidgardCanonicalCekProgram,
  replayValidationMachineEvent,
  validatePhaseASingle,
  validationMachineLedgerRoot,
} from "@al-ft/midgard-validation";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Exit } from "effect";
import { encodeForcedInclusionValueV1 } from "midgard-node/database/forcedTransactions.encode-forced-inclusion-value-v1";
import { type Entry } from "midgard-node/database/forcedTransactions.exact-forced-transaction-journal-member";
import { Columns } from "midgard-node/database/forcedTransactions.exact-forced-transaction-journal-member";
import { classifyForcedTransactions } from "midgard-node/mpf/event-window.classify-forced-transactions";
import { evaluateNormalBlockCandidates } from "midgard-node/mpf/process.evaluate-normal-block-candidates";
import { buildDeterministicValidationTraceMembers } from "midgard-node/mpf/validation-trace";

import { replayForcedTransitionEffect } from "../../src/verification/block-replay.replay-forced-transition-effect.js";
import { type ValidatedEventAuthority } from "../../src/verification/block-replay.watcher-block-replay-prior-state.js";
import { watcherForcedOperatorVerdict } from "../../src/verification/user-event.js";

export const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  blockEndTimeMs: 1750000000000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};
const validation = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  bucketConcurrency: 1,
  slotForUnixTime: () => 100n,
};
export async function journey(
  dataHex: string,
  priorCanonical: boolean,
  missingScript = false,
) {
  // Flat UPLC1.1.0: (lam x. x), also pinned by the core-step fixture.
  const program = buildMidgardCanonicalCekProgram(
    Buffer.from("010100200101", "hex"),
  );
  const script = plutusV3ScriptWitness(program.envelopeCbor),
    spent = outRefFromByte(0x4a);
  const entries = [
    {
      outRef: spent,
      output: makeProtectedScriptOutput(
        hashScriptWitness(script),
        FUNDED_OUTPUT_LOVELACE,
      ),
    },
  ];
  const redeemers = [
    ...(priorCanonical
      ? [
          {
            tag: 0,
            index: 0n,
            data: Buffer.from("d8799f01ff", "hex"),
            exUnits: [1000000000n, 1000000000n] as const,
          },
        ]
      : []),
    {
      tag: 0,
      index: 0n,
      data: Buffer.from(dataHex, "hex"),
      exUnits: [1000000000n, 1000000000n] as const,
    },
  ];
  const native = makeNativeTx({
    spendInputs: [spent],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    scriptWitnesses: missingScript ? [] : [script],
    scriptLanguages: ["PlutusV3"],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor(redeemers),
  });
  const txCbor = encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical(native.tx),
  );
  const sidecar = encodeMidgardCekProgramMaterialSidecar(
    missingScript ? [] : [...program.material.values()],
  );
  const intake = await Effect.runPromise(
    encodeForcedInclusionValueV1({
      nativeTxCbor: txCbor,
      verdict: "ForcedTxValid",
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    }),
  );
  const entry: Entry = {
    [Columns.NATIVE_TX_CBOR]: txCbor,
    [Columns.TX_ID]: intake.txId,
    [Columns.TX_COMPACT]: intake.txCompact,
    [Columns.TRANSACTION_COMMITMENT]: intake.transactionCommitment,
    [Columns.CONSENSUS_PROFILE_ID]: MIDGARD_CONSENSUS_PROFILE.profileId,
    [Columns.TX_ORDER_ID]: Buffer.alloc(36, 0x33),
    [Columns.INCLUSION_TIME]: new Date(0),
    [Columns.TX_ORDER_L1_TX_HASH]: Buffer.alloc(32, 0x33),
    [Columns.TX_ORDER_L1_OUTPUT_INDEX]: 0,
    [Columns.ASSET_NAME]: Buffer.alloc(0),
    [Columns.RAW_DATUM]: Buffer.alloc(0),
    [Columns.FORCED_INCLUSION_VALUE]: intake.value,
    [Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]: sidecar,
    [Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]: createHash("sha256")
      .update(sidecar)
      .digest(),
    [Columns.PROJECTED_HEADER_HASH]: null,
    [Columns.STATUS]: "awaiting",
  };
  const state = new Map(
    entries.map(({ outRef, output }) => [outRef.toString("hex"), output]),
  );
  const [classified] = await Effect.runPromise(
    classifyForcedTransactions({
      entries: [entry],
      initialState: state,
      effectiveEndTime: new Date(context.blockEndTimeMs),
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      validation,
      resolveProgramMaterialSidecar: () => Effect.succeed(sidecar),
    }),
  );
  const leaf = Data.from(
    classified!.entry[Columns.FORCED_INCLUSION_VALUE].toString("hex"),
    ForcedInclusionTxV1,
  );
  const arm = watcherForcedOperatorVerdict(leaf.verdict);
  const watcher = await replayForcedTransitionEffect({
    authority: {
      phase: "ForcedTransaction",
      canonicalNativeTxCbor: txCbor,
      programMaterialSidecarCbor: sidecar,
      committedForcedValidity: arm,
      eventKeyFingerprint: "review",
    } as ValidatedEventAuthority,
    state,
    phaseAConfig: {
      ...context,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    },
    phaseBConfig: {
      nowCardanoSlotNo: 100n,
      bucketConcurrency: 1,
      enforceScriptBudget: true,
    },
    step: {
      stepIndex: 0,
      phase: "ForcedTransaction",
      txId: null,
      eventKeyFingerprint: "review",
      preRoot: "00".repeat(32),
      postRoot: "00".repeat(32),
      eventToStepIndex: 0,
      eventToStepPhase: "ForcedTransaction",
    },
  });
  const replay = await Effect.runPromiseExit(
    replayValidationMachineEvent({
      ...context,
      sourceKind: "forced",
      canonicalTransactionCbor: txCbor,
      programMaterialSidecarCbor: sidecar,
      eventKeyCbor: Buffer.from(
        Data.to(
          {
            ForcedTransactionEventKey: {
              tx_order_id: { transactionId: "33".repeat(32), outputIndex: 0n },
            },
          },
          EventKey,
        ),
        "hex",
      ),
      ledgerWitnessEntries: entries,
      priorUtxosRoot: (await validationMachineLedgerRoot(entries)).toString(
        "hex",
      ),
    }),
  );
  const normalQueued = {
    sourceKind: "normal" as const,
    txId: native.txId,
    txCbor: native.txCbor,
    programMaterialSidecarCbor: sidecar,
    arrivalSeq: 0n,
    createdAt: new Date(context.blockEndTimeMs),
  };
  const phaseA = validatePhaseASingle(normalQueued, {
    ...context,
    strictnessProfile: "phase1_midgard",
    concurrency: 1,
  });
  const normal =
    "code" in phaseA
      ? {
          accepted: [],
          rejected: [phaseA],
          statePatch: { deletedOutRefs: [], upsertedOutRefs: [] },
        }
      : await Effect.runPromise(
          evaluateNormalBlockCandidates({
            candidates: [phaseA],
            state: new Map(state),
            blockSlot: context.blockSlot,
            bucketConcurrency: 1,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            scriptEvaluationsByTxId: new Map(),
          }),
        );
  const normalEventKey: SDK.EventKey = {
    L2TransactionEventKey: { tx_id: native.txId.toString("hex") },
  };
  const normalReplay =
    "code" in phaseA
      ? null
      : await Effect.runPromiseExit(
          replayValidationMachineEvent({
            ...context,
            sourceKind: "normal",
            canonicalTransactionCbor: native.txCbor,
            programMaterialSidecarCbor: sidecar,
            eventKeyCbor: Buffer.from(
              Data.to(normalEventKey, SDK.EventKey),
              "hex",
            ),
            ledgerWitnessEntries: entries,
            priorUtxosRoot: (
              await validationMachineLedgerRoot(entries)
            ).toString("hex"),
          }),
        );
  const retained = [];
  for (const eventReplay of [replay, normalReplay]) {
    if (eventReplay === null || Exit.isFailure(eventReplay)) continue;
    const input = eventReplay.value.replayInput;
    const eventKey = Data.from(
      input.eventKeyCbor.toString("hex"),
      SDK.EventKey,
    );
    retained.push(
      ...(await Effect.runPromise(
        buildDeterministicValidationTraceMembers({
          ...context,
          blockEndTime: new Date(context.blockEndTimeMs),
          transactions: [
            {
              ...input,
              eventKey,
              verdict: input.expectedVerdict,
              rejectionCode: input.expectedRejectionCode,
              ledgerOps: input.expectedLedgerOps,
              programMaterialSidecarCbor: sidecar,
            },
          ],
        }),
      )),
    );
  }
  return {
    classified,
    leaf,
    watcher,
    replay,
    txCbor,
    dataHex,
    normal,
    normalReplay,
    retained,
    normalTxCbor: native.txCbor,
  };
}
