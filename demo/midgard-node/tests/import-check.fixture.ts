/**
 * Foreign blocks of PlutusV3 spends, each committed Accepted whatever its
 * script does: a block's post-state applies every transaction, as its
 * operator committed it. Blocks chain: a block's outputs are spent by the
 * next block built on its ledger.
 */
import {
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { forcedVerdictForRejection } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildMidgardCanonicalCekProgram,
  type MidgardCekVerdictRequest,
  MidgardRedeemerTag,
  placeholderValidationTraceDescriptorV1,
  replayValidationVerdictEvent,
  validationMachineLedgerRoot,
  validationTransactionCommitmentV1,
} from "@al-ft/midgard-validation";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeRedeemersCbor,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import {
  Application,
  ErrorUPLC,
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { encodeTransactionRootValue } from "../src/mpf/ledger-hydration.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import { buildPlaceholderValidationTraceMembers } from "../src/mpf/validation-trace.js";
import { countsFromLengths, headerFor } from "./da-payload.record.js";
import { rebind, verdict } from "./foreign-block-import.fixture.js";

/** A script body: accepts, fails, loops until its budget is spent, or
 * accepts carrying `LARGE_SCRIPT_BYTES` of unused constants. */
export type ScriptBody = "accept" | "reject" | "loop" | "large";

/** The unused constants a "large" script carries, in 8 KiB constants. */
export const LARGE_SCRIPT_BYTES = 64 * 1024;

const omega = new Lambda(
  new Application(new UPLCVar(0), new UPLCVar(0)),
) as UPLCTerm;

/** `\ctx -> (\_ -> ... ctx) #c1 ... #ck`: unused constants. */
const carrying = (bytes: number): UPLCTerm => {
  const pieces = Math.ceil(bytes / 8_192);
  let body: UPLCTerm = new UPLCVar(pieces);
  for (let index = 0; index < pieces; index += 1)
    body = new Application(
      new Lambda(body),
      UPLCConst.byteString(new Uint8Array(8_192).fill(index + 1)),
    );
  return new Lambda(body);
};

const term = (body: ScriptBody): UPLCTerm =>
  body === "accept"
    ? new Lambda(new UPLCVar(0))
    : body === "reject"
      ? new Lambda(new ErrorUPLC())
      : body === "large"
        ? carrying(LARGE_SCRIPT_BYTES)
        : new Lambda(new Application(omega, omega));

/** A PlutusV3 program, its witness, and its unprotected script address. */
export const scriptOf = (body: ScriptBody) => {
  const program = buildMidgardCanonicalCekProgram(
    Buffer.from(UPLCEncoder.compile(new UPLCProgram([1, 1, 0], term(body)))),
  );
  const witness = plutusV3ScriptWitness(program.envelopeCbor);
  const hash = hashScriptWitness(witness);
  const address = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(CML.ScriptHash.from_hex(hash)),
    )
      .to_address()
      .to_raw_bytes(),
  );
  return {
    witness,
    address,
    material: [...program.material.values()],
  };
};

export type LedgerEntry = Readonly<{ outRef: Buffer; output: Buffer }>;

/** One spend: `input` (and each of `extra`, each a run of its own) is
 * locked by `body`'s script; its one output is locked by `next`'s script.
 * It also references `references` (outputs of the ledger it is built on). */
export type Spend = Readonly<{
  input: Buffer;
  extra?: readonly Buffer[];
  references?: readonly Buffer[];
  body: ScriptBody;
  next: ScriptBody;
  /** Declared cpu units (default 10^9). */
  cpu?: bigint;
}>;

/** The ledger entry locked by `body`'s script that a first block spends. */
export const fundedBy = (byte: number, body: ScriptBody): LedgerEntry => ({
  outRef: outRefFromByte(byte),
  output: makeOutput(FUNDED_OUTPUT_LOVELACE, scriptOf(body).address),
});

const buildSpend = (spend: Spend) => {
  const script = scriptOf(spend.body);
  const inputs = [spend.input, ...(spend.extra ?? [])];
  const tx = makeNativeTx({
    spendInputs: inputs,
    referenceInputs: spend.references ?? [],
    outputs: [
      makeOutput(
        FUNDED_OUTPUT_LOVELACE * BigInt(inputs.length),
        scriptOf(spend.next).address,
      ),
    ],
    omitVkeyWitness: true,
    scriptWitnesses: [script.witness],
    scriptLanguages: ["PlutusV3"],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor(
      inputs.map((_, index) => ({
        tag: MidgardRedeemerTag.Spend,
        index: BigInt(index),
        // The per-transaction memory cap, shared between the redeemers.
        exUnits: [
          BigInt(
            MIDGARD_CONSENSUS_PROFILE.limits
              .minSupportedTransactionExecutionMemoryUnits,
          ) / BigInt(inputs.length),
          spend.cpu ?? 1_000_000_000n,
        ],
      })),
    ),
  });
  return { tx, material: script.material };
};

const ROOT_KEYS = [
  "utxosRoot",
  "withdrawalsRoot",
  "forcedTransactionsRoot",
  "transactionsRoot",
  "depositsRoot",
  "transitionTraceRoot",
  "eventToStepRoot",
  "validationTracesRoot",
] as const;

const ledgerRoot = async (ledger: ReadonlyMap<string, Buffer>) =>
  (
    await validationMachineLedgerRoot(
      [...ledger].map(([key, output]) => ({
        outRef: Buffer.from(key, "hex"),
        output,
      })),
    )
  ).toString("hex");

/**
 * A block applying `spends` in order on `parent`, every transaction
 * committed Accepted. Returns the payload, its ledger after the block, each
 * transaction's script runs as its committed replay builds them, and an
 * import on `parent`.
 */
export const committedBlock = async (
  parent: readonly LedgerEntry[],
  spends: readonly Spend[],
) => {
  const ledger = new Map(
    parent.map((entry) => [entry.outRef.toString("hex"), entry.output]),
  );
  const prior = await ledgerRoot(ledger);
  const built = spends.map(buildSpend);
  const material = new Map<string, MidgardCekProgramMaterialEntry>();
  const steps: [string, string][] = [];
  const mapping: [string, string][] = [];
  const traces: [string, string][] = [];
  let pre = prior;
  const runs: MidgardCekVerdictRequest[][] = [];
  for (const [index, { tx, material: own }] of built.entries()) {
    for (const entry of own)
      material.set(Buffer.from(entry.root).toString("hex"), entry);
    const spent = [
      spends[index]!.input,
      ...(spends[index]!.extra ?? []),
      ...(spends[index]!.references ?? []),
    ];
    const txRuns: MidgardCekVerdictRequest[] = [];
    runs.push(txRuns);
    const replay = await Effect.runPromise(
      replayValidationVerdictEvent({
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        canonicalTransactionCbor: tx.txCbor,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(own),
        ledgerWitnessEntries: spent.map((outRef) => ({
          outRef,
          output: ledger.get(outRef.toString("hex"))!,
        })),
        blockTimeMs: 2,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 0n,
        sourceKind: "normal",
        evaluateScriptRun: (request) =>
          Effect.sync(() => {
            txRuns.push(request);
            return {
              kind: "accepted" as const,
              budget: { cpu: 0n, memory: 0n },
            };
          }),
      }),
    );
    if (replay.verdict !== "accepted")
      throw new Error(`fixture spend was rejected: ${replay.rejection?.code}`);
    for (const key of replay.statePatch.deletedOutRefs) ledger.delete(key);
    for (const [key, value] of replay.statePatch.upsertedOutRefs)
      ledger.set(key, value);
    const post = await ledgerRoot(ledger);
    const event: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: tx.txId.toString("hex") },
    };
    const eventCbor = Data.to(event, SDK.EventKey);
    steps.push([
      encodeTransitionIntegerCbor(BigInt(index)).toString("hex"),
      encodeTransitionStepCbor({
        schema_version: 1n,
        step_index: BigInt(index),
        event_key: event,
        phase: "L2Transaction",
        pre_utxos_root: pre,
        post_utxos_root: post,
      }).toString("hex"),
    ]);
    mapping.push([
      eventCbor,
      encodeEventToStepValueCbor({
        step_index: BigInt(index),
        phase: "L2Transaction",
      }).toString("hex"),
    ]);
    traces.push([
      eventCbor,
      Data.to(
        SDK.validationTraceDescriptorDataFromCore(
          placeholderValidationTraceDescriptorV1(
            {
              eventKeyCbor: Buffer.from(eventCbor, "hex"),
              transactionCommitment: validationTransactionCommitmentV1({
                canonicalTransactionCbor: tx.txCbor,
                sourceKind: "normal",
              }),
            },
            "accepted",
            null,
          ),
        ),
        SDK.ValidationTraceDescriptor,
      ),
    ]);
    pre = post;
  }
  const counts = {
    ...countsFromLengths({ transactions: built.length }),
    validationTraceCount: BigInt(built.length),
  };
  const header = {
    ...headerFor(
      Object.fromEntries(
        ROOT_KEYS.map((key) => [key, SDK.EMPTY_MERKLE_TREE_ROOT]),
      ) as Parameters<typeof headerFor>[0],
      counts,
    ),
    prevUtxosRoot: prior,
    utxosRoot: pre,
  };
  const payload = await rebind({
    version: 1n,
    block_body: {
      header,
      header_hash: "00".repeat(28),
      transactions: built.map(({ tx }) => [
        tx.txId.toString("hex"),
        encodeTransactionRootValue(tx.txCbor).toString("hex"),
      ]),
      transaction_preimages: built.map(({ tx }) => [
        tx.txId.toString("hex"),
        tx.txCbor.toString("hex"),
      ]),
      withdrawals: [],
      forced_transactions: [],
      forced_transaction_preimages: [],
      deposits: [],
      cek_program_material: [...material].map(([root, entry]) => [
        root,
        encodeMidgardCekProgramMaterialDaValue(entry).toString("hex"),
      ]),
      transition_trace: steps,
      event_to_step: mapping,
      validation_traces: traces,
      validation_trace_witnesses: [],
      counts,
    },
  });
  const after: LedgerEntry[] = [...ledger].map(([key, output]) => ({
    outRef: Buffer.from(key, "hex"),
    output,
  }));
  return {
    payload,
    headerHash: payload.block_body.header_hash,
    txIds: built.map(({ tx }) => tx.txId),
    /** Each transaction's script runs, in order. */
    runs,
    /** The output each transaction produced (index 0). */
    produced: built.map(({ tx }) => outRefFromTxId(tx.txId, 0n)),
    ledger: after,
    imported: () =>
      verdict(payload, {
        parentUtxosRoot: prior,
        parentEntries: parent.map((entry) => ({
          outref: entry.outRef,
          output: entry.output,
        })),
      }),
  };
};

/**
 * A block of one forced transaction spending `spend.input` with its script
 * (and its redeemer), committed with the verdict a full replay gives it on
 * `parent`. Returns the payload, that verdict, and an import on `parent`.
 */
export const forcedBlock = async (
  parent: readonly LedgerEntry[],
  spend: Spend,
) => {
  const { tx } = buildSpend(spend);
  const cbor = encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: tx.tx.version,
      body: tx.tx.body,
      witnessSet: tx.tx.witnessSet,
    }),
  );
  const ledger = new Map(
    parent.map((entry) => [entry.outRef.toString("hex"), entry.output]),
  );
  const prior = await ledgerRoot(ledger);
  const replay = await Effect.runPromise(
    replayValidationVerdictEvent({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      canonicalTransactionCbor: cbor,
      ledgerWitnessEntries: parent,
      blockTimeMs: 2,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      sourceKind: "forced",
    }),
  );
  if (replay.rejection === undefined)
    throw new Error("fixture forced spend was accepted");
  const committed = forcedVerdictForRejection(replay.rejection);
  if (typeof committed === "string")
    throw new Error("fixture forced verdict is not a rejection");
  const { encodeForcedInclusionValueV1 } = await import(
    "../src/database/forcedTransactions.js"
  );
  const encoded = await Effect.runPromise(
    encodeForcedInclusionValueV1({
      nativeTxCbor: cbor,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      verdict: committed,
    }),
  );
  const orderId = { transactionId: "5b".repeat(32), outputIndex: 0n };
  const key = Data.to(orderId, SDK.OutputReference);
  const eventKey: SDK.EventKey = {
    ForcedTransactionEventKey: { tx_order_id: orderId },
  };
  const eventCbor = Data.to(eventKey, SDK.EventKey);
  const [member] = await Effect.runPromise(
    buildPlaceholderValidationTraceMembers({
      transactions: [
        {
          eventKey,
          canonicalTransactionCbor: cbor,
          sourceKind: "forced",
          verdict: "rejected",
          rejectionReason: committed.ForcedTxInvalid.reason,
        },
      ],
    }),
  );
  if (member === undefined) throw new Error("missing fixture trace");
  const counts = {
    ...countsFromLengths({ forcedTransactions: 1 }),
    validationTraceCount: 1n,
  };
  const header = {
    ...headerFor(
      Object.fromEntries(
        ROOT_KEYS.map((rootKey) => [rootKey, SDK.EMPTY_MERKLE_TREE_ROOT]),
      ) as Parameters<typeof headerFor>[0],
      counts,
    ),
    prevUtxosRoot: prior,
    utxosRoot: prior,
  };
  const payload = await rebind({
    version: 1n,
    block_body: {
      header,
      header_hash: "00".repeat(28),
      transactions: [],
      transaction_preimages: [],
      withdrawals: [],
      forced_transactions: [[key, encoded.value.toString("hex")]],
      forced_transaction_preimages: [[key, cbor.toString("hex")]],
      deposits: [],
      cek_program_material: [],
      transition_trace: [
        [
          encodeTransitionIntegerCbor(0n).toString("hex"),
          encodeTransitionStepCbor({
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "ForcedTransaction",
            pre_utxos_root: prior,
            post_utxos_root: prior,
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
  });
  return {
    payload,
    rejection: replay.rejection,
    imported: () =>
      verdict(payload, {
        parentUtxosRoot: prior,
        parentEntries: parent.map((entry) => ({
          outref: entry.outRef,
          output: entry.output,
        })),
      }),
  };
};
