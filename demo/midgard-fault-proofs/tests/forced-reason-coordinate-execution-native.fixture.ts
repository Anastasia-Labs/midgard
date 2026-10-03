import {
  computeHash32,
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  encodeMidgardForcedTxCompact,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import {
  EventKeySchema,
  forcedVerdictSubject,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerMutationSteps,
} from "@al-ft/midgard-validation";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildExecutionSourceMachineAuthenticationFromRetainedDa } from "../src/execution-source-script-decoding/retained-witness.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { retainValidationTrace } from "./support/retained-reason-classifier.build-retained-validation-block-fixture.js";

/**
 * A forced transaction with two native mint executions. Execution 0 is the
 * inline `all []` policy, which is satisfied; execution 1 is a `sig` policy
 * over a key that signs nothing, sourced from a reference input's script
 * reference. A false witness-set script would be classified by its field-6
 * position, so the false policy is referenced rather than attached.
 */
export const buildExecutionNativeFixture = async () => {
  const trueScript = nativeScriptWitness({ type: "all", scripts: [] });
  // The key byte orders the false policy after the true one, so the true
  // policy is execution 0.
  const falseScript = nativeScriptWitness({
    type: "sig",
    keyHash: Buffer.alloc(28, 0x92),
  });
  const policies = [trueScript, falseScript].map((script) =>
    Buffer.from(hashScriptWitness(script), "hex"),
  );
  const assetName = Buffer.from("31", "hex");
  const spent = outRefFromByte(0x72);
  const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
  const reference = outRefFromByte(0x74, 7n);
  const referenceOutput = encodeMidgardTxOutput({
    ...decodeMidgardTxOutput(makeOutput(FUNDED_OUTPUT_LOVELACE)),
    script_ref: falseScript,
  });
  const output = makeOutput(
    FUNDED_OUTPUT_LOVELACE,
    undefined,
    new Map(
      policies.map((policy) => [
        policy.toString("hex"),
        new Map([[assetName.toString("hex"), 1n]]),
      ]),
    ),
  );
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    referenceInputs: [reference],
    outputs: [output],
    scriptWitnesses: [trueScript],
    mintPreimageCbor: makeMintPreimageCbor(
      new Map(policies.map((policy) => [policy, new Map([[assetName, 1n]])])),
    ),
  });
  const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
    transaction.txCbor,
  );
  const forced = materializeMidgardForcedTxFromCanonical(transaction.tx);
  const ledger = [
    { outRef: spent, output: spentOutput },
    { outRef: reference, output: referenceOutput },
  ];
  const mutations = await buildValidationMachineLedgerMutationSteps({
    initialEntries: ledger,
    operations: [{ type: "delete", key: spent }],
  });
  const priorLedgerRoot = mutations[0]!.preRoot.toString("hex");
  const orderKey = { transactionId: "73".repeat(32), outputIndex: 0n };
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: orderKey },
  } as const;
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: Buffer.from(
        Data.to(eventKey as never, EventKeySchema),
        "hex",
      ),
      sourceKind: "forced",
      blockEndTimeMs: 1_750_000_001_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId: transaction.txId,
      canonicalTransactionCbor: encodeMidgardForcedTxCanonical(forced),
      priorUtxosRoot: priorLedgerRoot,
      postUtxosRoot: priorLedgerRoot,
      ledgerWitnessEntries: ledger,
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: "E_NATIVE_SCRIPT_INVALID",
    }),
  );
  const compactWitnessSet = deriveMidgardNativeTxWitnessSetCompact(
    transaction.tx.witnessSet,
  );
  return {
    transaction,
    forced,
    nativeTx,
    nativeTxCompactCbor: encodeMidgardForcedTxCompact(
      materializeMidgardForcedTxFromCanonical(nativeTx).compact,
    ).toString("hex"),
    witnessSet: {
      addr_tx_wits_hash: Buffer.from(compactWitnessSet.addrTxWitsHash).toString(
        "hex",
      ),
      script_tx_wits_hash: Buffer.from(
        compactWitnessSet.scriptTxWitsHash,
      ).toString("hex"),
      redeemer_tx_wits_hash: Buffer.from(
        compactWitnessSet.redeemerTxWitsHash,
      ).toString("hex"),
    },
    addressWitnessItems: decodeMidgardFieldPreimage(
      transaction.tx.witnessSet.addrTxWitsPreimageCbor,
    ),
    scriptItems: [trueScript, falseScript].map(encodeMidgardVersionedScript),
    ledger,
    priorLedgerRoot,
    orderKey,
    eventKey,
    trace,
  };
};

export type ExecutionNativeFixture = Awaited<
  ReturnType<typeof buildExecutionNativeFixture>
>;

export const forcedReason = (executionIndex: number) =>
  ({
    ExecutionNativeScriptFalse: { execution_index: BigInt(executionIndex) },
  }) as const;

/**
 * The retained DA of a block committing the fixture under
 * `ExecutionNativeScriptFalse { executionIndex }`, and the machine
 * authentication of that execution read back from it.
 */
export const retainedExecution = async (
  fixture: ExecutionNativeFixture,
  executionIndex: number,
) => {
  const { descriptorEntries, retainedEntries } = retainValidationTrace({
    trace: fixture.trace,
    eventKey: fixture.eventKey,
    claim: { verdict: "rejected", reason: forcedReason(executionIndex) },
  });
  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptorEntries,
  );
  const machine = await buildExecutionSourceMachineAuthenticationFromRetainedDa(
    {
      eventKey: fixture.eventKey,
      executionIndex,
      authenticatedValidationTraceEntries: descriptorEntries,
      retainedValidationWitnessEntries: retainedEntries,
      expectedValidationTracesRoot: root.root,
      expectedPurposeKind: 1,
    },
  );
  const a = machine.authentication;
  const sourceLeaf =
    a.origin_kind === 0n
      ? hashMidgardInlineScriptSourceLeaf({
          sourceIndex: a.source_index,
          scriptLanguageTag: 0,
          scriptHash: Buffer.from(a.script_hash, "hex"),
          scriptTotalLength: Number(a.total_length),
          itemCommitment: Buffer.from(a.item_commitment, "hex"),
        })
      : hashMidgardReferenceScriptSourceLeaf({
          sourceKey: Buffer.from(a.source_key, "hex"),
          scriptLanguageTag: 0,
          scriptHash: Buffer.from(a.script_hash, "hex"),
          scriptTotalLength: Number(a.total_length),
          itemCommitment: Buffer.from(a.item_commitment, "hex"),
        });
  const scriptItem = fixture.scriptItems[executionIndex]!;
  const scriptBytes = decodeMidgardVersionedScript(scriptItem).scriptBytes;
  return {
    machine,
    scriptItem,
    /** The authenticated input the family's evidence is prepared from. */
    evidenceInput: (subject: ReturnType<typeof forcedVerdictSubject>) => ({
      finding: { subject, executionIndex },
      transactionIdHex: subject.transaction_id,
      sourceDescriptorHashHex: sourceLeaf.toString("hex"),
      scriptItemHashHex: computeHash32(scriptBytes).toString("hex"),
      scriptBytes,
      addressWitnessItems: fixture.addressWitnessItems,
      validityIntervalStart: fixture.transaction.tx.body.validityIntervalStart,
      validityIntervalEnd: fixture.transaction.tx.body.validityIntervalEnd,
    }),
  };
};

export const forcedTransactionId = (fixture: ExecutionNativeFixture) =>
  computeMidgardNativeTxId(fixture.forced);
