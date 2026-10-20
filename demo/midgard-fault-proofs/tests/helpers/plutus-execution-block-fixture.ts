/** One committed block whose single event spends a deposit-derived output.
 * The operator commits the event and applies its ledger effect (none for a
 * transaction stamped `TxIsInvalid`); every root is computed from the actual
 * outputs. The spend runs a PlutusV3 script (failing or passing), or is a key
 * spend with no redeemers. */
import {
  computeHash28,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  deriveCanonicalOriginalDepositTransitionEffect,
  MidgardRedeemerTag,
} from "@al-ft/midgard-validation";
import {
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeRedeemersCbor,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import type { LedgerEntry } from "../../src/ledger-at.js";
import { ledgerEntriesRoot } from "../../src/ledger-at.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { admitCompleteCanonicalReplayPredecessor } from "../../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  fixtureTransactionFromCanonicalCbor,
  reencodeFixturePayload,
} from "./canonical-block-evidence-fixture.js";

/** `(program 1.1.0 (lam ctx (error)))` in Flat, wrapped in one CBOR byte string. */
export const FAILING_SCRIPT = plutusV3ScriptWitness(
  encodeCbor(Buffer.from("0101002601", "hex")),
);
/** `(program 1.1.0 (lam ctx (con unit ())))` in Flat, wrapped in one CBOR byte string. */
export const PASSING_SCRIPT = plutusV3ScriptWitness(
  encodeCbor(Buffer.from("0101002499", "hex")),
);

export type PlutusExecutionScenario =
  | "failing"
  | "passing"
  | "forced_failing"
  | "no_redeemers"
  /** The failing spend, stamped `TxIsInvalid`. */
  | "failing_stamped_invalid"
  /** The passing spend, also requiring a signer that does not sign. */
  | "unsigned_required_signer";

export const SEED_LOVELACE = 20_000_000n;
const ownerKey = () => CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x63));
const ownerKeyHash = (): string => {
  const key = ownerKey();
  try {
    return computeHash28(Buffer.from(key.to_public().to_raw_bytes())).toString(
      "hex",
    );
  } finally {
    key.free();
  }
};

type SeedCredential =
  | { PublicKeyCredential: [string] }
  | { ScriptCredential: [string] };

/** The deposit event whose canonical insertion is a scenario's seed. */
const seedDepositEvent = (byte: number, paymentCredential: SeedCredential) =>
  ({
    id: {
      transactionId: byte.toString(16).padStart(2, "0").repeat(32),
      outputIndex: 0n,
    },
    info: {
      l2_address: {
        paymentCredential,
        stakeCredential: null,
      },
      l2_network_id: 0n,
      l2_datum: null,
    },
  }) satisfies SDK.DepositEvent;

/** A deposit's canonical ledger insertion, exactly as block production derives it. */
export const depositSeed = (
  byte: number,
  paymentCredential: SeedCredential,
): LedgerEntry => {
  const event = seedDepositEvent(byte, paymentCredential);
  const effect = deriveCanonicalOriginalDepositTransitionEffect({
    configuredNetwork: "Custom",
    eventId: event.id,
    l2Address: event.info.l2_address,
    l2NetworkId: event.info.l2_network_id,
    l2DatumCbor: null,
    originalAssets: { lovelace: SEED_LOVELACE },
  });
  const operation = effect.operations[0];
  if (operation?.type !== "insert")
    throw new Error("seed deposit has no canonical insertion");
  return { outRef: operation.outRefCbor, output: operation.outputCbor };
};

const DEFAULT_SCRIPT_SEED_BYTE = 0x61;
const KEY_SEED_BYTE = 0x62;

const scenarioScript = (scenario: PlutusExecutionScenario) =>
  scenario === "passing" || scenario === "unsigned_required_signer"
    ? PASSING_SCRIPT
    : FAILING_SCRIPT;

export const scriptCredential = (
  scenario: PlutusExecutionScenario,
): SeedCredential => ({
  ScriptCredential: [hashScriptWitness(scenarioScript(scenario))],
});
const keyCredential = (): SeedCredential => ({
  PublicKeyCredential: [ownerKeyHash()],
});

/**
 * The deposit that creates the output `scenario`'s transaction spends, as a
 * committed deposit block carries it. A script spend's seed is named by
 * `seedByte`, so one deposit block can seed several scenarios.
 */
export const plutusExecutionSeedDeposit = (
  scenario: PlutusExecutionScenario,
  seedByte: number = DEFAULT_SCRIPT_SEED_BYTE,
) => ({
  event:
    scenario === "no_redeemers"
      ? seedDepositEvent(KEY_SEED_BYTE, keyCredential())
      : seedDepositEvent(seedByte, scriptCredential(scenario)),
  originalAssets: { lovelace: SEED_LOVELACE },
  honest: true,
});

/** The transaction the scenario commits, over its seed output. */
export const scenarioTransaction = (
  scenario: PlutusExecutionScenario,
  seed: LedgerEntry,
) => {
  const outputs = [makeOutput(SEED_LOVELACE)];
  const key = ownerKey();
  try {
    if (scenario === "no_redeemers")
      return makeNativeTx({
        spendInputs: [seed.outRef],
        outputs,
        privateKey: key,
      });
    return makeNativeTx({
      spendInputs: [seed.outRef],
      outputs,
      privateKey: key,
      scriptWitnesses: [scenarioScript(scenario)],
      ...(scenario === "failing_stamped_invalid"
        ? { validity: "TxIsInvalid" as const }
        : {}),
      ...(scenario === "unsigned_required_signer"
        ? { requiredSignerItems: [Buffer.alloc(28, 0x71)] }
        : {}),
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
  } finally {
    key.free();
  }
};

const sorted = (rows: SDK.DaPayloadEntry[]): SDK.DaPayloadEntry[] =>
  rows.sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0));

const countedRoot = async (
  domain: SDK.RootDomain,
  rows: SDK.DaPayloadEntry[],
) =>
  (
    await buildCountedRoot(
      domain,
      rows.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    )
  ).root;

/** Header fields an emulator commit fixes: the operator, the covered
 * interval, its commit slot and the chain link. */
export type PlutusExecutionHeaderFields = Partial<
  Pick<
    SDK.Header,
    "operatorVkey" | "startTime" | "endTime" | "blockSlot" | "prevHeaderHash"
  >
>;

export type PlutusExecutionBlockOptions = {
  readonly header?: PlutusExecutionHeaderFields;
  /** The deposit byte of the spent seed (`plutusExecutionSeedDeposit`). */
  readonly seedByte?: number;
  /**
   * The predecessor block's ledger, which must hold the spent seed. By
   * default the scenario's script seed and a key-owned seed.
   */
  readonly predecessorEntries?: readonly LedgerEntry[];
};

export const plutusExecutionBlockFixture = async (
  scenario: PlutusExecutionScenario,
  {
    header: headerFields = {},
    seedByte = DEFAULT_SCRIPT_SEED_BYTE,
    predecessorEntries: requestedPredecessor,
  }: PlutusExecutionBlockOptions = {},
) => {
  const scriptSeed = depositSeed(seedByte, scriptCredential(scenario));
  const keySeed = depositSeed(KEY_SEED_BYTE, keyCredential());
  const predecessorEntries = requestedPredecessor ?? [scriptSeed, keySeed];
  const seed = scenario === "no_redeemers" ? keySeed : scriptSeed;
  if (!predecessorEntries.some((entry) => entry.outRef.equals(seed.outRef)))
    throw new Error("the predecessor ledger does not hold the spent seed");
  const native = scenarioTransaction(scenario, seed);
  const tx = fixtureTransactionFromCanonicalCbor({
    txId: native.txId.toString("hex"),
    canonicalCbor: native.txCbor,
  });
  const forced = scenario === "forced_failing";
  const forcedKey: SDK.OutputReference = {
    transactionId: "a1".repeat(32),
    outputIndex: 0n,
  };
  const eventKey: SDK.EventKey = forced
    ? { ForcedTransactionEventKey: { tx_order_id: forcedKey } }
    : { L2TransactionEventKey: { tx_id: tx.txId } };
  const phase: SDK.TransitionPhase = forced
    ? "ForcedTransaction"
    : "L2Transaction";

  const prevUtxosRoot = await ledgerEntriesRoot(predecessorEntries);
  const created = encodeMidgardSpendInputItem({
    txId: Buffer.from(tx.txId, "hex"),
    outputIndex: 0,
  });
  const createdEntry: LedgerEntry = {
    outRef: created,
    output: makeOutput(SEED_LOVELACE),
  };
  const postEntries =
    scenario === "failing_stamped_invalid"
      ? [...predecessorEntries]
      : [
          ...predecessorEntries.filter(
            (entry) => !entry.outRef.equals(seed.outRef),
          ),
          createdEntry,
        ];
  const utxosRoot = await ledgerEntriesRoot(postEntries);

  const forcedRows: SDK.DaPayloadEntry[] = [];
  const forcedPreimages: SDK.DaPayloadEntry[] = [];
  if (forced) {
    const full = materializeMidgardForcedTxFromCanonical(
      decodeMidgardNativeTxFullFromCanonicalCbor(tx.canonicalCbor),
    );
    const source = deriveMidgardForcedTxProofSource(full);
    const keyCbor = Data.to(forcedKey, SDK.OutputReference);
    forcedRows.push([
      keyCbor,
      Data.to(
        {
          tx_id: tx.txId,
          submitted_source: {
            compact_cbor: source.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              source.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              source.fieldPreimageLengthsCbor.toString("hex"),
          },
          verdict: "ForcedTxValid",
        } satisfies SDK.ForcedInclusionTxV1,
        SDK.ForcedInclusionTxV1,
      ),
    ]);
    forcedPreimages.push([
      keyCbor,
      encodeMidgardForcedTxCanonical(full).toString("hex"),
    ]);
  }
  const trace: SDK.DaPayloadEntry[] = [
    [
      Data.to(0n),
      Data.to(
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase,
          pre_utxos_root: prevUtxosRoot,
          post_utxos_root: utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ],
  ];
  const events: SDK.DaPayloadEntry[] = [
    [
      Data.to(eventKey, SDK.EventKey),
      Data.to({ step_index: 0n, phase }, SDK.EventToStepValue),
    ],
  ];

  const base = await buildCanonicalBlockFixture({
    transactions: forced ? [] : [tx],
    prevUtxosRoot,
  });
  const header: SDK.Header = {
    ...base.header,
    ...headerFields,
    forcedTransactionCount: BigInt(forcedRows.length),
    totalEventCount: 1n,
    transitionStepCount: 1n,
    utxosRoot,
    forcedTransactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forcedRows,
    ),
    transitionTraceRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      trace,
    ),
    eventToStepRoot: await countedRoot(SDK.ROOT_DOMAINS.eventToStep, events),
  };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const body = base.payload.block_body;
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...body,
      header,
      header_hash: headerHash,
      forced_transactions: sorted(forcedRows),
      forced_transaction_preimages: sorted(forcedPreimages),
      transition_trace: trace,
      event_to_step: events,
      counts: {
        ...body.counts,
        forcedTransactionCount: header.forcedTransactionCount,
        totalEventCount: header.totalEventCount,
        transitionStepCount: header.transitionStepCount,
      },
    },
  };
  const payloadEnvelopeCbor = await reencodeFixturePayload(payload);
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({ header, headerHash }),
    payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/plutus-execution",
      grade: "security",
    },
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      currentEvidence: evidence,
      ledgerAt: async (hash) =>
        hash === header.prevHeaderHash ? predecessorEntries : undefined,
    }),
  };
  return {
    evidence,
    /** The committed block's retained-DA payload envelope. */
    payloadEnvelopeCbor,
    context,
    eventKey,
    tx,
    predecessorEntries,
    seed,
    created: createdEntry,
    postEntries,
  };
};
