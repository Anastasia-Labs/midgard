/** One committed block whose single event spends a deposit-derived output.
 * The operator commits the event and applies its ledger effect (none for a
 * transaction stamped `TxIsInvalid`); every root is computed from the actual
 * outputs. The spend runs a PlutusV3 script (failing or passing), beside an
 * output a native script holds and a mint under its policy when asked, or
 * is a key spend with no redeemers. */
import {
  computeHash28,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { MidgardRedeemerTag } from "@al-ft/midgard-validation";
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
import {
  depositSeed,
  keyCredential,
  NATIVE_SPEND_SCRIPT,
  nativeMintAssets,
  nativeMintPreimageCbor,
  nativeSpendSeed,
  ownerKey,
  ownerKeyHash,
  SEED_LOVELACE,
  type SeedCredential,
  seedDepositEvent,
} from "./plutus-execution-seeds.js";

export {
  depositSeed,
  nativeSpendSeedDeposit,
  referenceSeedDeposits,
  SEED_LOVELACE,
} from "./plutus-execution-seeds.js";

/** `(program 1.1.0 (lam ctx (error)))` in Flat, wrapped in one CBOR byte string. */
export const FAILING_SCRIPT = plutusV3ScriptWitness(
  encodeCbor(Buffer.from("0101002601", "hex")),
);
/** `(program 1.1.0 (lam ctx (con unit ())))` in Flat, wrapped in one CBOR byte string. */
export const PASSING_SCRIPT = plutusV3ScriptWitness(
  encodeCbor(Buffer.from("0101002499", "hex")),
);

/** The per-transaction execution-unit cap admission enforces (the consensus
 * profile's Cardano `maxTxExUnits`), as `[memory, steps]`. */
export const ADMITTED_TX_EX_UNITS = [
  BigInt(MIDGARD_CONSENSUS_LIMITS.minSupportedTransactionExecutionMemoryUnits),
  BigInt(MIDGARD_CONSENSUS_LIMITS.minSupportedTransactionExecutionCpuUnits),
] as const;

/**
 * A passing PlutusV3 script that runs as long as admission allows:
 * `(program 1.1.0 (lam ctx [(lam _ (con unit ())) (countdown 5148 0)]))`,
 * where `countdown n acc = n == 0 ? acc : countdown (n - 1) acc` through the
 * strict fixpoint. 5148 rounds is the most whose run fits
 * `ADMITTED_TX_EX_UNITS` (16,497,794 memory, 391,317 CEK steps); in Flat,
 * wrapped in one CBOR byte string.
 */
export const LONGEST_PASSING_SCRIPT = plutusV3ScriptWitness(
  encodeCbor(
    Buffer.from(
      "010100232498ccc8c8c0088cc0080080048c0088cc0080080048894ccd5cd19b870024800040044cc00ccdc080124004002905c2824001",
      "hex",
    ),
  ),
);

/** A script spend's own script, in place of the scenario's, and, when
 * given, its declared execution units in place of the default units. */
export type ScriptSpend = Readonly<{
  script: MidgardVersionedScript;
  exUnits?: readonly [bigint, bigint];
}>;

export type PlutusExecutionScenario =
  | "failing"
  | "passing"
  | "forced_failing"
  | "no_redeemers"
  /** The failing spend, stamped `TxIsInvalid`. */
  | "failing_stamped_invalid"
  /** The passing spend, also requiring a signer that does not sign. */
  | "unsigned_required_signer";

const DEFAULT_SCRIPT_SEED_BYTE = 0x61;
const KEY_SEED_BYTE = 0x62;

/** The script `scenario`'s spend runs: `spend`'s when given, else the
 * passing script for a passing spend and the failing script otherwise. */
const scenarioScript = (
  scenario: PlutusExecutionScenario,
  spend?: ScriptSpend,
): MidgardVersionedScript =>
  spend?.script ??
  (scenario === "passing" || scenario === "unsigned_required_signer"
    ? PASSING_SCRIPT
    : FAILING_SCRIPT);

export const scriptCredential = (
  scenario: PlutusExecutionScenario,
  spend?: ScriptSpend,
): SeedCredential => ({
  ScriptCredential: [hashScriptWitness(scenarioScript(scenario, spend))],
});

/**
 * The deposit that creates the output `scenario`'s transaction spends, as a
 * committed deposit block carries it. A script spend's seed is named by
 * `seedByte`, so one deposit block can seed several scenarios; it is held
 * by `spend`'s script when given, by default the scenario's.
 */
export const plutusExecutionSeedDeposit = (
  scenario: PlutusExecutionScenario,
  seedByte: number = DEFAULT_SCRIPT_SEED_BYTE,
  spend?: ScriptSpend,
) => ({
  event:
    scenario === "no_redeemers"
      ? seedDepositEvent(KEY_SEED_BYTE, keyCredential())
      : seedDepositEvent(seedByte, scriptCredential(scenario, spend)),
  originalAssets: { lovelace: SEED_LOVELACE },
  honest: true,
});

/** Who signs a script spend beyond its owner: `extraSigners` other keys,
 * each adding an address witness, and how many times the spend lists the
 * owner's key among its required signers (`requiredOwner`). */
export type ScenarioSigners = Readonly<{
  extraSigners?: number;
  requiredOwner?: number;
}>;

/** How `scenarioTransaction` builds the script spend; every field defaults
 * to the plain scenario. */
export type ScenarioTransactionOptions = Readonly<{
  redeemerData?: Uint8Array;
  referenceInputs?: readonly Buffer[];
  signers?: ScenarioSigners;
  spend?: ScriptSpend;
  nativeSpend?: LedgerEntry;
  nativeMint?: boolean;
}>;

/** The `index`th key beyond the owner's. */
const extraSignerKey = (index: number) => {
  const bytes = Buffer.alloc(32, 0x73);
  bytes.writeUInt16BE(index);
  return CML.PrivateKey.from_normal_bytes(bytes);
};

/** The transaction the scenario commits, over its seed output; a script
 * spend's redeemer is `redeemerData` (Plutus Data CBOR) when given, it
 * reads `referenceInputs` (out-refs) as reference inputs, `signers` sign it
 * beside its owner, and it runs `spend`'s script (under `spend`'s units when
 * they are given) in place of the scenario's. With `nativeSpend`, the
 * transaction also spends that output, which sorts after the seed and which
 * the native script, witnessed first, holds; its one output carries both.
 * With `nativeMint`, it also mints an asset under the native script's
 * policy, witnessed first, into its one output. */
export const scenarioTransaction = (
  scenario: PlutusExecutionScenario,
  seed: LedgerEntry,
  {
    redeemerData,
    referenceInputs = [],
    signers: { extraSigners = 0, requiredOwner = 0 } = {},
    spend,
    nativeSpend,
    nativeMint = false,
  }: ScenarioTransactionOptions = {},
) => {
  const outputs = [
    makeOutput(
      nativeSpend === undefined ? SEED_LOVELACE : 2n * SEED_LOVELACE,
      undefined,
      nativeMintAssets(nativeMint),
    ),
  ];
  const key = ownerKey();
  const extraKeys = Array.from({ length: extraSigners }, (_, index) =>
    extraSignerKey(index),
  );
  try {
    if (scenario === "no_redeemers")
      return makeNativeTx({
        spendInputs: [seed.outRef],
        outputs,
        privateKey: key,
      });
    return makeNativeTx({
      spendInputs:
        nativeSpend === undefined
          ? [seed.outRef]
          : [seed.outRef, nativeSpend.outRef],
      referenceInputs,
      outputs,
      privateKey: key,
      scriptWitnesses: [
        ...(nativeSpend === undefined && !nativeMint
          ? []
          : [NATIVE_SPEND_SCRIPT]),
        scenarioScript(scenario, spend),
      ],
      ...(nativeMint ? { mintPreimageCbor: nativeMintPreimageCbor() } : {}),
      ...(scenario === "failing_stamped_invalid"
        ? { validity: "TxIsInvalid" as const }
        : {}),
      ...(scenario === "unsigned_required_signer"
        ? { requiredSignerItems: [Buffer.alloc(28, 0x71)] }
        : requiredOwner > 0
          ? {
              requiredSignerItems: Array.from({ length: requiredOwner }, () =>
                Buffer.from(ownerKeyHash(), "hex"),
              ),
            }
          : {}),
      ...(extraKeys.length > 0 ? { extraVkeyWitnessKeys: extraKeys } : {}),
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          ...(redeemerData === undefined ? {} : { data: redeemerData }),
          ...(spend?.exUnits === undefined ? {} : { exUnits: spend.exUnits }),
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
  } finally {
    key.free();
    for (const extra of extraKeys) extra.free();
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

/** How `plutusExecutionBlockFixture` builds the block; the script spend's
 * own options are `scenarioTransaction`'s. */
export type PlutusExecutionBlockOptions = Omit<
  ScenarioTransactionOptions,
  "nativeSpend"
> & {
  readonly header?: PlutusExecutionHeaderFields;
  /** The deposit byte of the spent seed (`plutusExecutionSeedDeposit`). */
  readonly seedByte?: number;
  /** The predecessor block's ledger, which must hold the spent seed; by
   * default the scenario's script seed and a key-owned seed. */
  readonly predecessorEntries?: readonly LedgerEntry[];
  /** Whether the script spend also spends the output the native script
   * holds (`nativeSpendSeedDeposit`), which the predecessor ledger must
   * hold. */
  readonly nativeSpend?: boolean;
};

export const plutusExecutionBlockFixture = async (
  scenario: PlutusExecutionScenario,
  {
    header: headerFields = {},
    seedByte = DEFAULT_SCRIPT_SEED_BYTE,
    predecessorEntries: requestedPredecessor,
    nativeSpend = false,
    ...spendOptions
  }: PlutusExecutionBlockOptions = {},
) => {
  const nativeMint = spendOptions.nativeMint ?? false;
  const scriptSeed = depositSeed(
    seedByte,
    scriptCredential(scenario, spendOptions.spend),
  );
  const keySeed = depositSeed(KEY_SEED_BYTE, keyCredential());
  const predecessorEntries = requestedPredecessor ?? [scriptSeed, keySeed];
  const seed = scenario === "no_redeemers" ? keySeed : scriptSeed;
  const spent = nativeSpend ? [seed, nativeSpendSeed()] : [seed];
  if (
    !spent.every(({ outRef }) =>
      predecessorEntries.some((entry) => entry.outRef.equals(outRef)),
    )
  )
    throw new Error("the predecessor ledger does not hold the spent seed");
  const native = scenarioTransaction(scenario, seed, {
    ...spendOptions,
    nativeSpend: nativeSpend ? spent[1] : undefined,
  });
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
    output: makeOutput(
      BigInt(spent.length) * SEED_LOVELACE,
      undefined,
      nativeMintAssets(nativeMint),
    ),
  };
  const postEntries =
    scenario === "failing_stamped_invalid"
      ? [...predecessorEntries]
      : [
          ...predecessorEntries.filter(
            (entry) => !spent.some(({ outRef }) => entry.outRef.equals(outRef)),
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
