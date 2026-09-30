import {
  buildMidgardValidationTraceTree,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical as forcedTraceBytes,
  materializeMidgardForcedTxFromCanonical as forcedTraceView,
} from "@al-ft/midgard-core/codec/forced";
import {
  encodeRetainedValidationWitness,
  encodeRetainedValidationWitnessKey,
  type EventKey,
  EventKeySchema,
  type RejectionReason,
  ROOT_DOMAINS,
  ValidationAuxiliaryWitnessSchema,
  validationMachineStateDataFromCore,
  ValidationTraceDescriptorSchema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "../../../midgard-validation/tests/validation-fixtures.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { decoyMissingScriptSourceScript } from "./missing-script-source-shapes.js";

export type MissingScriptSourcePurposeKind = 0 | 1 | 2 | 3;

export type MissingScriptSourceLocation = "inline" | "reference";

export type MissingScriptSourceFixtureShape = Readonly<{
  /** Consensus purpose kind: spend=0, mint=1, observe=2, receive=3. */
  purposeKind: MissingScriptSourcePurposeKind;
  /** Where the required script is committed; `absent` for no source at all. */
  presentAt: MissingScriptSourceLocation | "absent";
  /** Position of the present source among its location's sources. */
  presentPosition?: "first" | "last";
  /** Decoy inline script witnesses (field 6) beside the required one. */
  inlineDecoys: number;
  /** Decoy reference inputs (field 1), each resolving to a reference script. */
  referenceDecoys: number;
  /**
   * `accepted`: the block commits the transaction as an accepted L2 event.
   * `forced`: the block commits a forced rejection under
   * `ScriptSourceMissing { purposeKind, 0 }`.
   */
  direction: "accepted" | "forced";
  /**
   * The transaction is genuinely valid (source present, no decoys) and the
   * machine accepts it: the honest accepted block a prover cannot convict.
   */
  honest?: boolean;
  /**
   * Spend only: a second spent input, first in field order but second in the
   * sorted spend namespace, locked by a script no source carries. The
   * machine rejects at that purpose, (0, 1), after the required one.
   */
  absentSecondSpend?: boolean;
}>;

const FAKE_LEDGER_ROOT = "33".repeat(32);

const REFERENCE_TX_ID = Buffer.alloc(32, 0x90);

const REFERENCE_OUTPUT_ADDRESS = Buffer.alloc(29, 0x61);

const FORCED_ORDER_KEY = { transactionId: "52".repeat(32), outputIndex: 0n };

export const missingScriptSourceReason = (
  purposeKind: MissingScriptSourcePurposeKind,
  purposeIndex = 0n,
): RejectionReason => ({
  ScriptSourceMissing: {
    purpose_kind: BigInt(purposeKind),
    purpose_index: purposeIndex,
  },
});

const referenceOutRef = (index: number) =>
  encodeMidgardSpendInputItem({ txId: REFERENCE_TX_ID, outputIndex: index });

const referenceOutput = (script: MidgardVersionedScript) =>
  encodeMidgardTxOutput({
    address: REFERENCE_OUTPUT_ADDRESS,
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    script_ref: script,
  });

/**
 * A canonical transaction whose purpose of the given kind requires one script
 * hash, the retained ScriptSources trace the validation machine produces for
 * it, and the DA payload rows (descriptor and retained witnesses) a block
 * commits. Nothing here is fabricated mid-thread: every proof-thread input
 * derives from these rows exactly as the production replay reads them.
 */
export const buildMissingScriptSourceFixture = async (
  shape: MissingScriptSourceFixtureShape,
) => {
  const {
    purposeKind,
    presentAt,
    presentPosition = "first",
    inlineDecoys,
    referenceDecoys,
    direction,
    honest = false,
    absentSecondSpend = false,
  } = shape;
  if (honest && (presentAt === "absent" || inlineDecoys + referenceDecoys > 0))
    throw new Error("an honest fixture commits exactly the required source");
  if (
    absentSecondSpend &&
    (purposeKind !== 0 || presentAt === "absent" || direction !== "forced")
  )
    throw new Error(
      "an absent second spend follows a present spend purpose in a forced fixture",
    );
  if (
    !honest &&
    presentAt !== "absent" &&
    inlineDecoys === 0 &&
    !absentSecondSpend
  )
    throw new Error(
      "a present-source fixture needs an unused inline decoy so the machine rejects after discovery",
    );
  const required =
    presentAt === "absent"
      ? nativeScriptWitness({ type: "sig", keyHash: Buffer.alloc(28, 0x44) })
      : nativeScriptWitness({ type: "all", scripts: [] });
  const requiredHashHex = hashScriptWitness(required);
  const requiredHash = Buffer.from(requiredHashHex, "hex");
  const inlineDecoyScripts = Array.from({ length: inlineDecoys }, (_v, i) =>
    decoyMissingScriptSourceScript(i),
  );
  const referenceDecoyScripts = Array.from(
    { length: referenceDecoys },
    (_v, i) => decoyMissingScriptSourceScript(100_000 + i),
  );
  const place = (
    location: MissingScriptSourceLocation,
    decoys: readonly MidgardVersionedScript[],
  ) =>
    presentAt !== location
      ? decoys
      : presentPosition === "last"
        ? [...decoys, required]
        : [required, ...decoys];
  const inlineScripts = place("inline", inlineDecoyScripts);
  const referenceScripts = place("reference", referenceDecoyScripts);
  const references = referenceScripts.map((script, index) => ({
    outRef: referenceOutRef(index),
    output: referenceOutput(script),
  }));
  const spent = outRefFromByte(0x31);
  const secondSpent = outRefFromByte(0x32);
  const secondSpentOutput = makeProtectedScriptOutput(
    hashScriptWitness(
      nativeScriptWitness({ type: "sig", keyHash: Buffer.alloc(28, 0x45) }),
    ),
    FUNDED_OUTPUT_LOVELACE,
  );
  const spentOutput =
    purposeKind === 0
      ? makeProtectedScriptOutput(requiredHashHex, FUNDED_OUTPUT_LOVELACE)
      : makeOutput(FUNDED_OUTPUT_LOVELACE);
  const mintAssetName = Buffer.from("31", "hex");
  const output =
    purposeKind === 3
      ? makeProtectedScriptOutput(requiredHashHex, FUNDED_OUTPUT_LOVELACE)
      : purposeKind === 1
        ? makeOutput(
            FUNDED_OUTPUT_LOVELACE,
            undefined,
            new Map([
              [requiredHashHex, new Map([[mintAssetName.toString("hex"), 1n]])],
            ]),
          )
        : makeOutput(FUNDED_OUTPUT_LOVELACE);
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: absentSecondSpend ? [secondSpent, spent] : [spent],
    referenceInputs: references.map(({ outRef }) => outRef),
    outputs: [output],
    scriptWitnesses: inlineScripts,
    ...(purposeKind === 1
      ? {
          mintPreimageCbor: makeMintPreimageCbor(
            new Map([[requiredHash, new Map([[mintAssetName, 1n]])]]),
          ),
        }
      : {}),
    ...(purposeKind === 2 ? { requiredObserverItems: [requiredHash] } : {}),
  });
  const orderKey = FORCED_ORDER_KEY;
  const eventKey = (
    direction === "accepted"
      ? { L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") } }
      : { ForcedTransactionEventKey: { tx_order_id: orderKey } }
  ) as EventKey;
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const ledgerWitnessEntries = [
    { outRef: spent, output: spentOutput },
    ...(absentSecondSpend
      ? [{ outRef: secondSpent, output: secondSpentOutput }]
      : []),
    ...references,
  ];
  const honestOperations = honest
    ? [
        { type: "delete" as const, key: spent },
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(transaction.txId),
          outputCbor: output,
        }),
      ]
    : [];
  const mutations = honest
    ? await buildValidationMachineLedgerMutationSteps({
        initialEntries: ledgerWitnessEntries,
        operations: honestOperations,
      })
    : [];
  const priorLedgerRoot = honest
    ? mutations[0]!.preRoot.toString("hex")
    : FAKE_LEDGER_ROOT;
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor,
      sourceKind: direction === "accepted" ? "normal" : "forced",

      blockEndTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor:
        direction === "forced"
          ? forcedTraceBytes(forcedTraceView(transaction.tx))
          : transaction.txCbor,
      priorUtxosRoot: priorLedgerRoot,
      postUtxosRoot: honest
        ? mutations.at(-1)!.postRoot.toString("hex")
        : FAKE_LEDGER_ROOT,
      ledgerWitnessEntries,
      expectedLedgerOps: honestOperations,
      ledgerMutationSteps: mutations,
      expectedVerdict: honest ? "accepted" : "rejected",
      expectedRejectionCode: honest
        ? null
        : presentAt === "absent" || absentSecondSpend
          ? "E_MISSING_REQUIRED_WITNESS"
          : "E_INVALID_FIELD_TYPE",
    }),
  );
  // The block commits the operator's verdict: an accepted event carries no
  // rejection code; a forced rejection carries the family's exact code.
  const committedRejectionHash =
    direction === "accepted"
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode("E_MISSING_REQUIRED_WITNESS");
  const tree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    direction === "accepted" ? "accepted" : "rejected",
    committedRejectionHash,
  );
  const descriptorData = {
    schema_version: BigInt(tree.descriptor.schemaVersion),
    machine_version: BigInt(tree.descriptor.machineVersion),
    trace_root: tree.descriptor.traceRoot.toString("hex"),
    step_count: BigInt(tree.descriptor.stepCount),
    initial_state_hash: tree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash: tree.descriptor.terminalStateHash.toString("hex"),
    verdict:
      direction === "accepted" ? ("Accepted" as const) : ("Rejected" as const),
    rejection_code_hash: tree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const descriptorEntries = [
    {
      key: eventKeyCbor,
      value: Buffer.from(
        Data.to(
          descriptorData as never,
          ValidationTraceDescriptorSchema as never,
        ),
        "hex",
      ),
    },
  ];
  const retainedEntries = trace.witnesses.flatMap((witness, stateIndex) => {
    if (
      witness.phase !== "scriptSources" ||
      (witness.auxiliary !== null &&
        witness.auxiliary.kind !== "scriptPurposeScan" &&
        witness.auxiliary.kind !== "scriptSourceScan")
    )
      return [];
    const key = encodeRetainedValidationWitnessKey({
      event_key: eventKey,
      execution_index: BigInt(stateIndex) - BigInt(trace.witnesses.length),
    });
    const auxiliary = Data.from(
      Data.to(validationAuxiliaryWitnessData(witness.auxiliary) as never),
      ValidationAuxiliaryWitnessSchema,
    );
    const value = encodeRetainedValidationWitness({
      machine_state: validationMachineStateDataFromCore(
        trace.states[stateIndex]!,
      ),
      trace_proof: validationTraceProofDataFromCore(tree.proofs[stateIndex]!),
      phase: 8n,
      program_counter: BigInt(witness.programCounter),
      witness_cbor: witness.cbor.toString("hex"),
      auxiliary,
    } as never);
    return [{ key, value }];
  });
  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptorEntries,
  );
  const sourceCount = inlineScripts.length + referenceScripts.length;
  const presentSourceIndex =
    presentAt === "absent"
      ? null
      : presentAt === "inline"
        ? presentPosition === "last"
          ? inlineScripts.length - 1
          : 0
        : inlineScripts.length +
          (presentPosition === "last" ? referenceScripts.length - 1 : 0);
  return {
    shape,
    transaction,
    eventKey,
    orderKey,
    trace,
    descriptorEntries,
    retainedEntries,
    expectedRoot: root.root,
    priorLedgerRoot,
    ledgerWitnessEntries,
    requiredHashHex,
    sourceCount,
    transactionSourceCount: inlineScripts.length,
    presentSourceIndex,
    reason: missingScriptSourceReason(purposeKind),
  };
};

export type MissingScriptSourceFixture = Awaited<
  ReturnType<typeof buildMissingScriptSourceFixture>
>;
