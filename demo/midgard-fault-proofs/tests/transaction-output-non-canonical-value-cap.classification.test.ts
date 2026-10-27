/** An accepted transaction whose single output carries a 5,000- or
 * 5,001-byte Cardano value, classified through every installed category.
 * The transaction merges two Plutus-locked inputs whose values are each
 * under the cap, so value is preserved and every other family's input,
 * script and signature checks hold. */
import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  MIDGARD_CONSENSUS_PROFILE,
  midgardFieldCommitment,
  protectMidgardAddress,
} from "@al-ft/midgard-core";
import { midgardValueToCmlValue } from "@al-ft/midgard-core/codec";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type EventKey,
  EventKeySchema,
  GENESIS_HEADER_HASH,
} from "@al-ft/midgard-sdk";
import {
  MIDGARD_COINS_PER_UTXO_BYTE,
  minAdaLovelace,
} from "@al-ft/midgard-validation";
import { replayValidationMachineEvent } from "@al-ft/midgard-validation/tests/validation-machine-event-replay.trace";
import { Constr, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  createCompleteCanonicalReplayUnion,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { classifyHeader } from "../src/workflow/header-classifier.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  replayBudget,
  setup,
  transport,
} from "./support/catalogue-retained-classification.setup.js";
import {
  LONG_ASSET_NAME_LOVELACE,
  longAssetNameOutput,
  type LongAssetNameTail,
  longAssetNameValue,
} from "./support/long-asset-name-value.js";
import {
  buildRetainedValidationBlockFixture,
  placeholderValidationTraceEntries,
} from "./support/retained-reason-classifier.build-retained-validation-block-fixture.js";

afterEach(() => {
  transport.raw = undefined;
});

/** The accepting UPLC 1.1.0 program `(lam ctx [(lam _ ()) ctx])`. */
const script = {
  language: "PlutusV3" as const,
  scriptBytes: encodeCbor(Buffer.from("0101002324980041", "hex")),
};
const scriptAddress = protectMidgardAddress(
  Buffer.concat([
    Buffer.from([0x70]),
    Buffer.from(hashMidgardVersionedScript(script), "hex"),
  ]),
);
const spendRedeemer = (index: bigint) =>
  encodeMidgardRedeemerWitnessItem({
    purpose: "Spend",
    index,
    redeemerCbor: Buffer.from(Data.to(new Constr(0, [])), "hex"),
    executionUnits: { memory: 5_000_000n, steps: 1_000_000_000n },
  });

/**
 * A block accepting one transaction that spends the long names (with all but
 * one ADA of `inputLovelace`) and the tail asset (with one ADA) from two
 * script outputs, and pays them to one output of `outputLovelace` and no fee.
 */
const mergeFixture = async ({
  tailQuantity,
  inputLovelace = LONG_ASSET_NAME_LOVELACE,
  outputLovelace = inputLovelace,
}: {
  readonly tailQuantity: LongAssetNameTail;
  readonly inputLovelace?: bigint;
  readonly outputLovelace?: bigint;
}) => {
  const inputs = (
    [
      ["names", inputLovelace - 1_000_000n],
      ["tail", 1_000_000n],
    ] as const
  ).map(([part, lovelace], index) => ({
    outRef: outRefCbor(0x1d, BigInt(index)),
    output: encodeMidgardTxOutput({
      address: scriptAddress,
      value: longAssetNameValue({ lovelace, tailQuantity, part }),
    }),
  }));
  const output = longAssetNameOutput(tailQuantity, outputLovelace);
  const redeemers = [spendRedeemer(0n), spendRedeemer(1n)];
  const transaction = buildFixtureTransaction({
    spendInputs: inputs.map(({ outRef }) => outRef),
    outputs: [encodeMidgardTxOutput(output)],
    fee: 0n,
    networkId: 0n,
    scriptWitnesses: [encodeMidgardVersionedScript(script)],
    redeemerWitnesses: redeemers,
    scriptIntegrityHash: computeScriptIntegrityHashForLanguages(
      midgardFieldCommitment(encodeMidgardFieldPreimage(redeemers)),
      ["PlutusV3"],
    ),
  });
  const predecessor = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: inputs.map(({ outRef, output }) => ({ key: outRef, value: output })),
    prevHeaderHash: GENESIS_HEADER_HASH,
  });
  const eventKey: EventKey = {
    L2TransactionEventKey: { tx_id: transaction.txId },
  };
  const blockEndTimeMs = 1_750_000_000_000;
  const blockSlot = 100n;
  // The honest node rejects an over-cap output, so only the 5,000-byte twin
  // has an accepted replay. An operator accepting the 5,001-byte one cannot
  // derive a ledger descriptor for its output and keeps the spent inputs.
  const postLedgerEntries =
    tailQuantity === 256n
      ? inputs
      : await Effect.runPromise(
          replayValidationMachineEvent({
            l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            eventKeyCbor: Buffer.from(
              Data.to(eventKey, asLucidSchema(EventKeySchema)),
              "hex",
            ),
            canonicalTransactionCbor: transaction.canonicalCbor,
            sourceKind: "normal",
            ledgerWitnessEntries: inputs,
            priorUtxosRoot: predecessor.header.utxosRoot,
            blockTimeMs: blockEndTimeMs,
            expectedNetworkId: 0n,
            minFeeA: 0n,
            minFeeB: 0n,
            blockSlot,
          }),
        ).then(({ trace, statePatch }) => {
          if (trace.verdict !== "accepted")
            throw new Error("the 5,000-byte twin must be accepted");
          return statePatch.upsertedOutRefs.map(([outRef, value]) => ({
            outRef: Buffer.from(outRef, "hex"),
            output: value,
          }));
        });
  const block = await buildRetainedValidationBlockFixture({
    subject: {
      kind: "normal",
      nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
        transaction.canonicalCbor,
      ),
    },
    priorLedgerRoot: predecessor.header.utxosRoot,
    prevHeaderHash: predecessor.headerHash,
    blockEndTimeMs,
    blockSlot,
    ...placeholderValidationTraceEntries({
      eventKey,
      canonicalTransactionCbor: transaction.canonicalCbor,
      sourceKind: "normal",
      claim: { verdict: "accepted" },
    }),
    postLedgerEntries,
  });
  const fixture = {
    block,
    predecessor,
    transaction,
    orderKey: { transactionId: "52".repeat(32), outputIndex: 0n },
  };
  return { fixture, output };
};

const classify = async (input: Parameters<typeof mergeFixture>[0]) => {
  const { fixture, output } = await mergeFixture(input);
  const { classifyInput } = await setup(fixture, "normal");
  return {
    decision: await classifyHeader(classifyInput),
    headerHash: fixture.block.headerHash,
    txId: fixture.transaction.txId,
    output,
  };
};

const meetsMinAda = (output: ReturnType<typeof longAssetNameOutput>) =>
  output.value.lovelace >=
  minAdaLovelace(
    MIDGARD_COINS_PER_UTXO_BYTE,
    BigInt(encodeMidgardTxOutput(output).length),
  );

describe(
  "an accepted output at and over the 5,000-byte value cap through every installed category",
  replayBudget,
  () => {
    it("finds no fault in the 5,000-byte twin", async () => {
      const atCap = await classify({ tailQuantity: 24n });
      expect(
        midgardValueToCmlValue(atCap.output.value).to_cbor_bytes(),
      ).toHaveLength(5_000);
      expect(meetsMinAda(atCap.output)).toBe(true);
      expect(atCap.decision).toMatchObject({
        decision: "healthy",
        headerHash: atCap.headerHash,
      });
    });

    it.each([
      ["balanced and above the min-Ada floor", {}, true],
      [
        "below the min-Ada floor",
        { inputLovelace: 2_000_000n, outputLovelace: 2_000_000n },
        false,
      ],
      [
        "paying out more than its inputs hold",
        { outputLovelace: LONG_ASSET_NAME_LOVELACE + 1_000_000n },
        true,
      ],
    ] as const)(
      "selects transactionOutputNonCanonical for a 5,001-byte value %s",
      async (_label, amounts, minAdaMet) => {
        const overCap = await classify({ tailQuantity: 256n, ...amounts });
        expect(
          midgardValueToCmlValue(overCap.output.value).to_cbor_bytes(),
        ).toHaveLength(5_001);
        expect(meetsMinAda(overCap.output)).toBe(minAdaMet);
        expect(overCap.decision).toMatchObject({
          decision: "fault_detected",
          category: "transactionOutputNonCanonical",
          headerHash: overCap.headerHash,
        });
        if (overCap.decision.decision !== "fault_detected") return;
        expect(overCap.decision.detectionId).toContain(overCap.txId);
      },
    );
  },
);

describe("value conservation replay past an accepted 5,001-byte output", () => {
  it("leaves both the over-cap transaction and a later one to the output canonicality finding", async () => {
    const overCap = buildFixtureTransaction({
      spendInputs: [],
      outputs: [encodeMidgardTxOutput(longAssetNameOutput(256n))],
      fee: 0n,
    });
    // Balanced, so value conservation reaches it only through the ledger
    // replay of the transaction before it.
    const later = buildFixtureTransaction({ spendInputs: [], fee: 0n });
    const fixture = await buildCanonicalBlockFixture({
      transactions: [overCap, later],
      transitionSteps: [overCap, later].map((tx, index) => ({
        schema_version: 1n,
        step_index: BigInt(index),
        event_key: { L2TransactionEventKey: { tx_id: tx.txId } },
        phase: "L2Transaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      })),
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "test/value-cap-value-conservation",
        grade: "security",
      },
    });
    // Union members in catalogue order.
    const union = createCompleteCanonicalReplayUnion([
      VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
      TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
    ]);
    const { detections } = await union.replay(evidence, {});
    expect(
      detections.map(({ violationId, detectionId }) => ({
        violationId,
        transaction: detectionId.split(":")[2],
      })),
    ).toEqual([
      {
        violationId: "transaction-output-non-canonical",
        transaction: overCap.txId,
      },
    ]);
  });
});
