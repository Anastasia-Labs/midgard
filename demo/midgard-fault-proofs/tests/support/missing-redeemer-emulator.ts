import {
  buildMidgardValidationTraceTree,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  MidgardRedeemerTag,
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
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "../../../midgard-validation/tests/validation-fixtures.js";
import type { MissingRedeemerPurposeKind } from "../../src/missing-redeemer/family.js";
import {
  buildMissingRedeemerMaterialFromRetainedDa,
  type MissingRedeemerMaterial,
} from "../../src/missing-redeemer/replay.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";

/** §5.4 aggregate bound on one canonical field preimage: the largest field 8. */
export const MISSING_REDEEMER_MAXIMUM_FIELD_BYTES = 32_768;
/**
 * The maximum-shape item count: one full 16-item batch plus one item, so the
 * exact 32,768-byte certified field needs a real resumed grammar checkpoint
 * and a real resumed walk checkpoint, and its first batch is the widest
 * single-transaction scan the family can be asked to run.
 */
export const MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT = 17;

/**
 * A canonical CEK program envelope whose material sidecar the validation
 * machine accepts; the same bytes serve every purpose kind, and under the
 * MidgardV1 language they hash to a distinct script.
 */
const PROGRAM_ENVELOPE = Buffer.from(
  "85018301010058207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e021827",
  "hex",
);
const PROGRAM_MATERIAL_SIDECAR = Buffer.from(
  "82018282582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d47830100438200008258207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e582983010058248202582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d",
  "hex",
);
const MINT_ASSET_NAME = Buffer.from("aced", "hex");

export type MissingRedeemerFixtureShape = Readonly<{
  direction: "accepted" | "forced";
  purposeKind: MissingRedeemerPurposeKind;
  /** Where the purpose's Plutus source lives; receive scripts are inline only. */
  sourceLocation: "inline" | "reference";
  /**
   * Never-matching pointers that fill field 8 before the family's scan can
   * conclude. A present target always gets at least one so the producer's
   * machine still rejects downstream and never has to execute a script.
   */
  decoyRedeemers?: number;
  /** Exact field-8 preimage length; the last decoy's data absorbs the padding. */
  fieldBytes?: number;
  /**
   * Whether field 8 carries the exact target pointer. Defaults to the shape
   * an honest producer would reject wrongly (present under `forced`) or
   * accept wrongly (absent under `accepted`); the opposite is the honest
   * block the family must refuse to convict.
   */
  targetRedeemerPresent?: boolean;
}>;

export type MissingRedeemerFixture = Readonly<{
  shape: Required<MissingRedeemerFixtureShape>;
  transaction: ReturnType<typeof makeNativeTx>;
  script: MidgardVersionedScript;
  scriptHashHex: string;
  eventKey: SDK.EventKey;
  orderKey: SDK.OutputReference;
  subject: SDK.VerdictSubject;
  rejectionReason: {
    readonly RedeemerMissing: {
      readonly purpose_kind: bigint;
      readonly purpose_index: bigint;
    };
  };
  descriptorEntries: readonly { key: Buffer; value: Buffer }[];
  retainedEntries: readonly { key: Buffer; value: Buffer }[];
  validationTracesRoot: string;
  validationTraceCount: bigint;
  material: MissingRedeemerMaterial;
  fieldBytes: number;
}>;

const REDEEMER_TAG_BY_KIND = [
  MidgardRedeemerTag.Spend,
  MidgardRedeemerTag.Mint,
  MidgardRedeemerTag.Reward,
  MidgardRedeemerTag.Receiving,
] as const;

/** Canonical Plutus `B` data of exactly `length` payload bytes (chunked above 64). */
const canonicalBytesData = (length: number): Buffer => {
  if (length < 24)
    return Buffer.concat([Buffer.from([0x40 + length]), pad(length)]);
  if (length <= 64)
    return Buffer.concat([Buffer.from([0x58, length]), pad(length)]);
  const chunks: Buffer[] = [Buffer.from([0x5f])];
  let remaining = length;
  while (remaining > 0) {
    const take = Math.min(64, remaining);
    chunks.push(
      take < 24 ? Buffer.from([0x40 + take]) : Buffer.from([0x58, take]),
      pad(take),
    );
    remaining -= take;
  }
  chunks.push(Buffer.from([0xff]));
  return Buffer.concat(chunks);
};
const pad = (length: number): Buffer => Buffer.alloc(length, 0xa5);

type RedeemerItem = Parameters<typeof makeRedeemersCbor>[0][number];

/** Field 8 with the target pointer and decoys, padded to an exact length. */
const buildRedeemerField = ({
  targetTag,
  targetPresent,
  decoyRedeemers,
  fieldBytes,
}: {
  targetTag: number;
  targetPresent: boolean;
  decoyRedeemers: number;
  fieldBytes: number | undefined;
}): Buffer => {
  const items = (paddingLength: number): RedeemerItem[] => {
    const decoys: RedeemerItem[] = Array.from(
      { length: decoyRedeemers },
      (_, index) => ({
        tag: MidgardRedeemerTag.Spend,
        index: BigInt(1_000 + index),
        exUnits: [1_000n, 1_000n] as const,
        ...(index === decoyRedeemers - 1 && paddingLength > 0
          ? { data: canonicalBytesData(paddingLength) }
          : {}),
      }),
    );
    const target: RedeemerItem[] = targetPresent
      ? [{ tag: targetTag, index: 0n, exUnits: [1_000n, 1_000n] as const }]
      : [];
    return [...target, ...decoys].sort((left, right) =>
      left.tag === right.tag
        ? left.index < right.index
          ? -1
          : left.index > right.index
            ? 1
            : 0
        : left.tag - right.tag,
    );
  };
  if (fieldBytes === undefined) return makeRedeemersCbor(items(0));
  if (decoyRedeemers === 0)
    throw new Error("an exact field length needs a decoy to absorb padding");
  let paddingLength = Math.max(
    0,
    fieldBytes - makeRedeemersCbor(items(0)).length,
  );
  for (let round = 0; round < 8; round += 1) {
    const candidate = makeRedeemersCbor(items(paddingLength));
    if (candidate.length === fieldBytes) return candidate;
    paddingLength += fieldBytes - candidate.length;
    if (paddingLength < 0)
      throw new Error("field length target is below the unpadded field");
  }
  throw new Error(
    `could not size an exact ${fieldBytes.toString()}-byte redeemer field`,
  );
};

/**
 * One producer-committed block's retained validation DA for a transaction
 * whose exact purpose `(kind, 0)` has a Plutus source: the honest machine
 * trace, the producer's claimed descriptor, and the family's submission
 * material rebuilt from those public entries alone.
 *
 * Under `accepted`, the machine rejects at stage 10 (`E_MISSING_REQUIRED_WITNESS`)
 * and the producer claims acceptance. Under `forced`, the decoy makes the
 * machine reject downstream at the redeemer audit (`E_INVALID_FIELD_TYPE`)
 * and the producer claims the `RedeemerMissing` code; the producer's own
 * stage-10 selection of the purpose is what the family authenticates.
 */
export const buildMissingRedeemerFixture = async (
  requested: MissingRedeemerFixtureShape,
): Promise<MissingRedeemerFixture> => {
  const targetRedeemerPresent =
    requested.targetRedeemerPresent ?? requested.direction === "forced";
  const shape: Required<MissingRedeemerFixtureShape> = {
    ...requested,
    decoyRedeemers: requested.decoyRedeemers ?? (targetRedeemerPresent ? 1 : 0),
    fieldBytes: requested.fieldBytes ?? 0,
    targetRedeemerPresent,
  };
  if (targetRedeemerPresent && shape.decoyRedeemers === 0)
    throw new Error("a present target needs a decoy to stop execution");
  if (shape.purposeKind === 3 && shape.sourceLocation === "reference")
    throw new Error("MidgardV1 receive scripts have no reference carriage");
  const script: MidgardVersionedScript =
    shape.purposeKind === 3
      ? { language: "MidgardV1", scriptBytes: PROGRAM_ENVELOPE }
      : plutusV3ScriptWitness(PROGRAM_ENVELOPE);
  const scriptHashHex = hashScriptWitness(script);
  const scriptHash = Buffer.from(scriptHashHex, "hex");
  const spent = outRefFromByte(0x71);
  const reference = outRefFromByte(0x72);
  const spentOutput =
    shape.purposeKind === 0
      ? makeProtectedScriptOutput(scriptHashHex, FUNDED_OUTPUT_LOVELACE)
      : makeOutput(FUNDED_OUTPUT_LOVELACE);
  const mintedAssets = new Map([
    [scriptHashHex, new Map([[MINT_ASSET_NAME.toString("hex"), 1n]])],
  ]);
  const producedOutput =
    shape.purposeKind === 1
      ? makeOutput(FUNDED_OUTPUT_LOVELACE, undefined, mintedAssets)
      : shape.purposeKind === 3
        ? makeProtectedScriptOutput(scriptHashHex, FUNDED_OUTPUT_LOVELACE)
        : makeOutput(FUNDED_OUTPUT_LOVELACE);
  const referenceOutput = encodeMidgardTxOutput({
    address: Buffer.alloc(29, 0x61),
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    script_ref: script,
  });
  const redeemerTxWitsPreimageCbor = buildRedeemerField({
    targetTag: REDEEMER_TAG_BY_KIND[shape.purposeKind],
    targetPresent: targetRedeemerPresent,
    decoyRedeemers: shape.decoyRedeemers,
    fieldBytes: shape.fieldBytes === 0 ? undefined : shape.fieldBytes,
  });
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    referenceInputs: shape.sourceLocation === "reference" ? [reference] : [],
    outputs: [producedOutput],
    scriptWitnesses: shape.sourceLocation === "inline" ? [script] : [],
    redeemerTxWitsPreimageCbor,
    scriptLanguages: [
      script.language === "MidgardV1" ? "MidgardV1" : "PlutusV3",
    ],
    ...(shape.purposeKind === 1
      ? {
          mintPreimageCbor: makeMintPreimageCbor(
            new Map([[scriptHash, new Map([[MINT_ASSET_NAME, 1n]])]]),
          ),
        }
      : {}),
    ...(shape.purposeKind === 2
      ? { requiredObserverItems: [scriptHash], networkId: 0n }
      : {}),
  });
  const transactionId = transaction.txId.toString("hex");
  const orderKey = { transactionId: "52".repeat(32), outputIndex: 0n };
  const eventKey: SDK.EventKey =
    shape.direction === "accepted"
      ? { L2TransactionEventKey: { tx_id: transactionId } }
      : { ForcedTransactionEventKey: { tx_order_id: orderKey } };
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, SDK.EventKeySchema as never),
    "hex",
  );
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor,
      sourceKind: shape.direction === "accepted" ? "normal" : "forced",
      ...(shape.direction === "forced"
        ? { committedForcedVerdict: "rejected" as const }
        : {}),
      blockEndTimeMs: 1_800_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      programMaterialSidecarCbor: PROGRAM_MATERIAL_SIDECAR,
      priorUtxosRoot: "00".repeat(32),
      postUtxosRoot: "00".repeat(32),
      ledgerWitnessEntries: [
        { outRef: spent, output: spentOutput },
        ...(shape.sourceLocation === "reference"
          ? [{ outRef: reference, output: referenceOutput }]
          : []),
      ],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: targetRedeemerPresent
        ? "E_INVALID_FIELD_TYPE"
        : "E_MISSING_REQUIRED_WITNESS",
    }),
  );
  const tree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    shape.direction === "accepted" ? "accepted" : "rejected",
    shape.direction === "accepted"
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode("E_MISSING_REQUIRED_WITNESS"),
  );
  const descriptor: SDK.ValidationTraceDescriptor = {
    schema_version: BigInt(tree.descriptor.schemaVersion),
    machine_version: BigInt(tree.descriptor.machineVersion),
    trace_root: tree.descriptor.traceRoot.toString("hex"),
    step_count: BigInt(tree.descriptor.stepCount),
    initial_state_hash: tree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash: tree.descriptor.terminalStateHash.toString("hex"),
    verdict: shape.direction === "accepted" ? "Accepted" : "Rejected",
    rejection_code_hash: tree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const descriptorEntries = [
    {
      key: eventKeyCbor,
      value: Buffer.from(
        Data.to(
          descriptor as never,
          SDK.ValidationTraceDescriptorSchema as never,
        ),
        "hex",
      ),
    },
  ];
  // The family consumes the producer's purpose/source membership witnesses
  // and its stage-10 selection states; field-carriage auxiliaries need a
  // resolver over the committing transaction and are not family evidence.
  const retainedEntries = trace.witnesses.flatMap((witness, index) => {
    if (
      witness.phase !== "scriptSources" ||
      (witness.auxiliary !== null &&
        witness.auxiliary.kind !== "scriptPurposeScan" &&
        witness.auxiliary.kind !== "scriptSourceScan" &&
        witness.auxiliary.kind !== "redeemerScanBegin")
    )
      return [];
    const retained: SDK.RetainedValidationWitness = {
      machine_state: SDK.validationMachineStateDataFromCore(
        trace.states[index]!,
      ),
      trace_proof: SDK.validationTraceProofDataFromCore(tree.proofs[index]!),
      phase: 8n,
      program_counter: BigInt(witness.programCounter),
      witness_cbor: witness.cbor.toString("hex"),
      auxiliary: Data.from(
        Data.to(validationAuxiliaryWitnessData(witness.auxiliary) as never),
        SDK.ValidationAuxiliaryWitnessSchema,
      ) as unknown as SDK.ValidationAuxiliaryWitness,
    };
    return [
      {
        key: SDK.encodeRetainedValidationWitnessKey({
          event_key: eventKey,
          execution_index: -BigInt(index + 1),
        }),
        value: SDK.encodeRetainedValidationWitness(retained),
      },
    ];
  });
  const root = await buildCountedRoot(
    SDK.ROOT_DOMAINS.validationTraces,
    descriptorEntries,
  );
  const rejectionReason = {
    RedeemerMissing: {
      purpose_kind: BigInt(shape.purposeKind),
      purpose_index: 0n,
    },
  } as const;
  const subject =
    shape.direction === "accepted"
      ? SDK.acceptedVerdictSubject(transactionId)
      : SDK.forcedVerdictSubject({
          transactionId,
          sourceKey: orderKey,
          rejectionReason,
        });
  const material = await buildMissingRedeemerMaterialFromRetainedDa({
    eventKey,
    subject,
    purposeKind: shape.purposeKind,
    purposeIndex: 0,
    txCbor: transaction.txCbor,
    authenticatedValidationTraceEntries: descriptorEntries,
    retainedValidationWitnessEntries: retainedEntries,
    expectedValidationTracesRoot: root.root,
  });
  return {
    shape,
    transaction,
    script,
    scriptHashHex,
    eventKey,
    orderKey,
    subject,
    rejectionReason,
    descriptorEntries,
    retainedEntries,
    validationTracesRoot: root.root,
    validationTraceCount: root.count,
    material,
    fieldBytes: redeemerTxWitsPreimageCbor.length,
  };
};
