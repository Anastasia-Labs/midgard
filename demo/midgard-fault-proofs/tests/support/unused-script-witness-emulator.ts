import {
  buildMidgardValidationTraceTree,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardNativeScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
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
import type { CanonicalBlockEvidence } from "../../src/evidence/canonical-block-evidence.js";
import {
  requireLinearFaultInitialDatum,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { UnusedScriptWitnessContracts } from "../../src/unused-script-witness/contracts.js";
import { retainedScriptSourcesStage } from "../../src/unused-script-witness/retained-stage-twelve.js";
import {
  UnusedScriptAuthenticatedWitnessSchema,
  UnusedScriptBoundWitnessSchema,
  UnusedScriptPurposeOpeningSchema,
  UnusedScriptReverseScanSchema,
  UnusedScriptSourceOpeningSchema,
  UnusedScriptStep01RedeemerSchema,
  UnusedScriptStep02DatumSchema,
  UnusedScriptStep02RedeemerSchema,
  UnusedScriptStep03DatumSchema,
  UnusedScriptStep03RedeemerSchema,
  UnusedScriptStep04DatumSchema,
  UnusedScriptStep04RedeemerSchema,
  UnusedScriptStep05DatumSchema,
  UnusedScriptStep05RedeemerSchema,
  UnusedScriptStep06DatumSchema,
  UnusedScriptStep06RedeemerSchema,
} from "../../src/unused-script-witness/schemas.js";
import type { UnusedScriptWitnessAuthentication } from "../../src/unused-script-witness/submit-step-02.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";

const FAMILY = "unused-script-witness";

/** The rejection code the machine commits for an unused inline script. */
export const UNUSED_SCRIPT_WITNESS_REJECT_CODE =
  "E_INVALID_FIELD_TYPE" as const;

export type UnusedScriptWitnessFixtureSpec = Readonly<{
  /** Whether the accused event is a committed L2 transaction or a forced one. */
  direction: "accepted" | "forced";
  /** The verdict the operator committed for the event. */
  claimedVerdict: "accepted" | "rejected";
  /**
   * Whether the accused (last) inline script is selected by no purpose. When
   * true the machine stops its stage-11 source audit at that coordinate; when
   * false every inline source is used and the machine reaches the stage-12
   * terminal.
   */
  accusedUnused: boolean;
  /**
   * Inline field-6 scripts, each a distinct always-true native script. Every
   * script before the accused one is selected by its own spend purpose, so the
   * alternate-source walk of step 04 carries `sourceCount - 1` openings.
   */
  sourceCount: number;
  /** The accused field-6 coordinate; defaults to the last inline script. */
  accusedIndex?: number;
  /**
   * Further spend purposes under the first script, widening the purpose
   * frontier the reverse match of step 05 walks.
   */
  extraSpendPurposes?: number;
  /** Mint, receive and observer purposes under the second, third and fourth scripts. */
  allPurposeKinds?: boolean;
  /** Extra committed L2 transactions widening the validation-traces trie. */
  decoyTransactionCount?: number;
  /** Byte seeding the spent out-refs so fixtures in one harness stay distinct. */
  inputByte: number;
  operatorVkey: string;
  startTime: bigint;
}>;

export const unusedScriptWitnessReason = (scriptIndex: number) =>
  ({ UnusedScriptWitness: { script_index: BigInt(scriptIndex) } }) as const;

/** Distinct always-true native scripts: `all` over `index` trivial children. */
export const trivialNativeScript = (index: number): MidgardNativeScript => ({
  type: "all",
  scripts: Array.from({ length: index }, () => ({
    type: "all" as const,
    scripts: [],
  })),
});

/**
 * Builds the accused transaction, replays it through the deterministic
 * validation machine, commits it in a block whose validation-traces root is
 * the genuine descriptor tree, and exposes the retained DA entries the prover
 * reconstructs steps 02 to 05 from: every ScriptSources-phase state, so the
 * complete purpose frontier, the source scan, and the stage-11/12 audit seam
 * are all authenticated against the committed trace.
 */
export const buildUnusedScriptWitnessFixture = async (
  spec: UnusedScriptWitnessFixtureSpec,
) => {
  if (spec.sourceCount < 1 || spec.sourceCount > 0xffff)
    throw new Error("fixture source count must be within 1..65535");
  const allKinds = spec.allPurposeKinds ?? false;
  const scripts = Array.from({ length: spec.sourceCount }, (_, index) =>
    nativeScriptWitness(trivialNativeScript(index)),
  );
  const scriptHashes = scripts.map((script) => hashScriptWitness(script));
  const scriptIndex = spec.accusedIndex ?? spec.sourceCount - 1;
  if (scriptIndex < 0 || scriptIndex >= spec.sourceCount)
    throw new Error("fixture accused index is outside field 6");
  // An unused accused coordinate stops the machine's source audit there, so
  // later inline scripts are never reached; a used one needs every inline
  // script selected for the machine to commit the stage-12 terminal.
  const usedCount = spec.accusedUnused ? scriptIndex : spec.sourceCount;
  if (allKinds && usedCount < 4)
    throw new Error("every purpose kind needs four used inline scripts");
  // The key-held funding input sits at index 0 of the seeded transaction id;
  // every script spend is a later index of the same id, so the spend-inputs
  // preimage stays in canonical order. The accused script's own spend (when
  // it is used) is the last input, so the reverse match walks every purpose.
  const spentTxId = Buffer.alloc(32, spec.inputByte);
  const spent = outRefFromByte(spec.inputByte);
  const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
  let nextInputIndex = 1;
  const spendFor = (index: number) => ({
    outRef: outRefFromTxId(spentTxId, BigInt(nextInputIndex++)),
    output: makeProtectedScriptOutput(
      scriptHashes[index]!,
      FUNDED_OUTPUT_LOVELACE,
    ),
  });
  const scriptSpends = [
    ...Array.from({ length: Math.max(usedCount - 1, 0) }, (_, index) =>
      spendFor(index),
    ),
    ...Array.from({ length: spec.extraSpendPurposes ?? 0 }, () => spendFor(0)),
    ...(usedCount > 0 ? [spendFor(usedCount - 1)] : []),
  ];
  const mintPolicy = Buffer.from(scriptHashes[1] ?? "", "hex");
  const assetName = Buffer.from("cafe", "hex");
  const inputLovelace =
    FUNDED_OUTPUT_LOVELACE * BigInt(1 + scriptSpends.length);
  const outputs = allKinds
    ? [
        makeProtectedScriptOutput(scriptHashes[2]!, FUNDED_OUTPUT_LOVELACE),
        makeOutput(
          inputLovelace - FUNDED_OUTPUT_LOVELACE,
          undefined,
          new Map([
            [
              mintPolicy.toString("hex"),
              new Map([[assetName.toString("hex"), 1n]]),
            ],
          ]),
        ),
      ]
    : [makeOutput(inputLovelace)];
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent, ...scriptSpends.map(({ outRef }) => outRef)],
    outputs,
    scriptWitnesses: scripts,
    ...(allKinds
      ? {
          mintPreimageCbor: makeMintPreimageCbor(
            new Map([[mintPolicy, new Map([[assetName, 1n]])]]),
          ),
          requiredObserverItems: [Buffer.from(scriptHashes[3]!, "hex")],
          networkId: 0n,
        }
      : {}),
  });
  const nativeTxId = transaction.txId.toString("hex");
  for (const [label, bytes, bound] of [
    [
      "spend inputs",
      transaction.tx.body.spendInputsPreimageCbor.length,
      MIDGARD_CONSENSUS_LIMITS.maxSpendInputsPreimageBytes,
    ],
    [
      "outputs",
      transaction.tx.body.outputsPreimageCbor.length,
      MIDGARD_CONSENSUS_LIMITS.maxOutputsPreimageBytes,
    ],
    [
      "script witnesses",
      transaction.tx.witnessSet.scriptTxWitsPreimageCbor.length,
      MIDGARD_CONSENSUS_LIMITS.maxScriptWitnessesPreimageBytes,
    ],
  ] as const)
    if (bytes > bound)
      throw new Error(
        `fixture ${label} preimage (${bytes.toString()} bytes) exceeds the consensus bound ${bound.toString()}`,
      );
  const ledgerEntries = [
    { outRef: spent, output: spentOutput },
    ...scriptSpends,
  ];
  const allOperations = [
    ...ledgerEntries.map(({ outRef }) => ({
      type: "delete" as const,
      key: outRef,
    })),
    ...outputs.map((output, index) =>
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor: output,
      }),
    ),
  ];
  const mutations = await buildValidationMachineLedgerMutationSteps({
    initialEntries: ledgerEntries,
    operations: allOperations,
  });
  const machineAccepts = !spec.accusedUnused;
  const orderKey: SDK.OutputReference = {
    transactionId: spec.inputByte.toString(16).padStart(2, "0").repeat(32),
    outputIndex: 0n,
  };
  const eventKey: SDK.EventKey =
    spec.direction === "forced"
      ? { ForcedTransactionEventKey: { tx_order_id: orderKey } }
      : { L2TransactionEventKey: { tx_id: nativeTxId } };
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, SDK.EventKeySchema),
    "hex",
  );
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor,
      sourceKind: spec.direction === "forced" ? "forced" : "normal",
      committedForcedVerdict:
        spec.direction === "forced" ? spec.claimedVerdict : undefined,
      blockEndTimeMs: 1_750_000_001_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: mutations[0]!.preRoot.toString("hex"),
      postUtxosRoot: machineAccepts
        ? mutations.at(-1)!.postRoot.toString("hex")
        : mutations[0]!.preRoot.toString("hex"),
      ledgerWitnessEntries: ledgerEntries,
      expectedLedgerOps: machineAccepts ? allOperations : [],
      ledgerMutationSteps: machineAccepts ? mutations : [],
      expectedVerdict: machineAccepts ? "accepted" : "rejected",
      expectedRejectionCode: machineAccepts
        ? null
        : UNUSED_SCRIPT_WITNESS_REJECT_CODE,
    }),
  );
  const claimedTree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    spec.claimedVerdict,
    spec.claimedVerdict === "accepted"
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode(UNUSED_SCRIPT_WITNESS_REJECT_CODE),
  );
  const descriptor: SDK.ValidationTraceDescriptor = {
    schema_version: BigInt(claimedTree.descriptor.schemaVersion),
    machine_version: BigInt(claimedTree.descriptor.machineVersion),
    trace_root: claimedTree.descriptor.traceRoot.toString("hex"),
    step_count: BigInt(claimedTree.descriptor.stepCount),
    initial_state_hash: claimedTree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash:
      claimedTree.descriptor.terminalStateHash.toString("hex"),
    verdict: spec.claimedVerdict === "accepted" ? "Accepted" : "Rejected",
    rejection_code_hash:
      claimedTree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const descriptorCbor = Buffer.from(
    Data.to(descriptor as never, SDK.ValidationTraceDescriptorSchema),
    "hex",
  );
  // Every ScriptSources-phase state is retained: the purpose discovery, the
  // source scan and the stage-11/12 audit all live in that phase.
  const validationTraceWitnesses: SDK.DaPayloadEntry[] =
    trace.witnesses.flatMap((witness, index) => {
      if (witness.phase !== "scriptSources") return [];
      const retained: SDK.RetainedValidationWitness = {
        machine_state: SDK.validationMachineStateDataFromCore(
          trace.states[index]!,
        ),
        trace_proof: SDK.validationTraceProofDataFromCore(
          claimedTree.proofs[index]!,
        ),
        phase: 8n,
        program_counter: BigInt(witness.programCounter),
        witness_cbor: witness.cbor.toString("hex"),
        auxiliary: Data.from(
          Data.to(validationAuxiliaryWitnessData(witness.auxiliary) as never),
          SDK.ValidationAuxiliaryWitnessSchema,
        ) as unknown as SDK.ValidationAuxiliaryWitness,
      };
      return [
        [
          // The node's chronological negative coordinate domain for
          // ScriptSources controls (see midgard-node/src/mpf/validation-trace.ts).
          SDK.encodeRetainedValidationWitnessKey({
            event_key: eventKey,
            execution_index: BigInt(index) - BigInt(trace.witnesses.length),
          }).toString("hex"),
          SDK.encodeRetainedValidationWitness(retained).toString("hex"),
        ] as SDK.DaPayloadEntry,
      ];
    });
  const nativeTx =
    spec.direction === "forced"
      ? decodeMidgardNativeTxFullFromCanonicalCbor(transaction.txCbor)
      : transaction.tx;
  const block = await buildDecodingBlockFixture({
    operatorVkey: spec.operatorVkey,
    startTime: spec.startTime,
    priorLedgerRoot: mutations[0]!.preRoot.toString("hex"),
    subject:
      spec.direction === "forced"
        ? {
            kind: "forced",
            nativeTx,
            orderKey,
            verdict: {
              ForcedTxInvalid: {
                reason: unusedScriptWitnessReason(scriptIndex),
              },
            } as never,
          }
        : { kind: "normal", nativeTx },
    decoyTransactionCount: spec.decoyTransactionCount ?? 0,
  });
  const eventKeyHex = eventKeyCbor.toString("hex");
  const validationTraces: SDK.DaPayloadEntry[] =
    block.reconstruction.payload.block_body.validation_traces.map(
      ([key, value]) =>
        key === eventKeyHex
          ? [key, descriptorCbor.toString("hex")]
          : [key, value],
    );
  if (!validationTraces.some(([key]) => key === eventKeyHex))
    throw new Error("fixture block omitted the accused event");
  const root = await buildCountedRoot(
    SDK.ROOT_DOMAINS.validationTraces,
    validationTraces.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  const header: SDK.Header = {
    ...block.header,
    validationTracesRoot: root.root,
    validationTraceCount: root.count,
  };
  const preimages = new Map(
    block.reconstruction.payload.block_body.transaction_preimages,
  );
  const transactions = block.reconstruction.payload.block_body.transactions.map(
    ([nodeTxId, l2TransactionSourceCbor]) => ({
      nodeTxId,
      txCbor: preimages.get(nodeTxId),
      l2TransactionSourceCbor,
    }),
  );
  const retainedEntries = {
    authenticatedValidationTraceEntries: validationTraces.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    ),
    retainedValidationWitnessEntries: validationTraceWitnesses.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    ),
  };
  /** The canonical block evidence the production replay classifies. */
  const canonicalBlock = (headerHash: string): CanonicalBlockEvidence =>
    ({
      headerHash,
      header,
      reconstruction: {
        ...block.reconstruction,
        payload: {
          ...block.reconstruction.payload,
          block_body: {
            ...block.reconstruction.payload.block_body,
            validation_traces: validationTraces,
            validation_trace_witnesses: validationTraceWitnesses,
          },
        },
      },
      transactions,
    }) as unknown as CanonicalBlockEvidence;
  const subject =
    spec.direction === "forced"
      ? SDK.forcedVerdictSubject({
          transactionId: nativeTxId,
          sourceKey: orderKey,
          rejectionReason: unusedScriptWitnessReason(scriptIndex),
        })
      : SDK.acceptedVerdictSubject(nativeTxId);
  // The complete purpose frontier is opened by the stage-8 discovery; the
  // receive scan retains same-kind witnesses over another frontier.
  const purposeCount = trace.witnesses.filter(
    (witness) =>
      witness.phase === "scriptSources" &&
      witness.auxiliary?.kind === "scriptPurposeScan" &&
      retainedScriptSourcesStage(witness.cbor) === 8n,
  ).length;
  return Object.freeze({
    spec,
    transaction,
    nativeTxId,
    scriptIndex,
    scriptHashes,
    purposeCount,
    trace,
    eventKey,
    orderKey,
    subject,
    block,
    header,
    descriptor,
    retainedEntries,
    canonicalBlock,
  });
};

export type UnusedScriptWitnessFixture = Awaited<
  ReturnType<typeof buildUnusedScriptWitnessFixture>
>;

type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: UnusedScriptWitnessContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  referenceScriptUtxo: UTxO;
}>;

/**
 * A raw continuation: the exact datum and redeemer the test asks for reach
 * the validator, so every substitution is refused on chain rather than by an
 * off-chain builder guard.
 */
const continueRaw = async ({
  common,
  stepIndex,
  nextAddress,
  nextDatum,
  redeemerSchema,
  args,
}: {
  readonly common: Common;
  readonly stepIndex: number;
  readonly nextAddress: string;
  readonly nextDatum: string;
  readonly redeemerSchema: unknown;
  readonly args: (
    inputIndex: bigint,
    outputIndex: bigint,
  ) => Record<string, unknown>;
}) => {
  const { lucid, contracts, categoryId, signer, threadOutRef } = common;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const role = `raw step ${(stepIndex + 1).toString().padStart(2, "0")}`;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: common.referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex]!.spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: nextAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, role);
    const inputIndex = SDK.requireInputIndex(ctx, threadUtxo, role);
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      role,
    );
    return Data.to(
      { Continue: [args(inputIndex, outputIndex)] } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex]!.spendingScript,
    stepRole: role,
    nextAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error(`${role}: no layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

const stepState = <State>(common: Common, stepIndex: number, schema: unknown) =>
  requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef: common.threadOutRef,
  }).then(({ threadUtxo }) =>
    requireLinearFaultStepState<State>({
      threadUtxo,
      signer: common.signer,
      schema: schema as never,
      family: FAMILY,
      stepIndex,
    }),
  );

const datumOf = (common: Common, data: unknown, schema: unknown) =>
  Data.to(
    { fraud_prover: common.signer.paymentKeyHash, data } as never,
    schema as never,
  );

/** Step 01 over a forced leaf with no off-chain classification. */
export const submitUnusedStep01ForcedRaw = async ({
  header,
  membership,
  scriptIndex,
  direction,
  ...common
}: Common & {
  readonly header: SDK.Header;
  readonly membership: SDK.RootMembershipProof<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  >;
  readonly scriptIndex: bigint;
  readonly direction: bigint;
}) => {
  const verdict = membership.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error("raw forced step 01 needs a rejected leaf");
  const subject = {
    ...SDK.forcedVerdictSubject({
      transactionId: membership.value.tx_id,
      sourceKey: membership.key,
      rejectionReason: verdict.ForcedTxInvalid.reason,
    }),
    direction,
  };
  const { threadUtxo } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef: common.threadOutRef,
  });
  requireLinearFaultInitialDatum({
    threadUtxo,
    signer: common.signer,
    family: FAMILY,
  });
  return await continueRaw({
    common,
    stepIndex: 0,
    nextAddress: common.contracts.steps[1].spendingScriptAddress,
    nextDatum: datumOf(
      common,
      {
        subject,
        validation_traces_root: header.validationTracesRoot,
        validation_trace_count: header.validationTraceCount,
        script_index: scriptIndex,
      },
      UnusedScriptStep02DatumSchema,
    ),
    redeemerSchema: UnusedScriptStep01RedeemerSchema,
    args: (input_index, output_index) => ({
      source: {
        ForcedSource: {
          input_index,
          output_index,
          header,
          membership,
          direction,
        },
      },
      script_index: scriptIndex,
    }),
  });
};

/**
 * Step 02 with the supplied authentication handed to the validator verbatim;
 * the frontier counts and peaks of the next state come from the caller so a
 * substituted frontier is also refused on chain.
 */
export const submitUnusedStep02Raw = async ({
  authentication,
  frontiers,
  nextStepIndex = 2,
  ...common
}: Common & {
  readonly authentication: UnusedScriptWitnessAuthentication;
  readonly frontiers: Pick<
    Data.Static<typeof UnusedScriptAuthenticatedWitnessSchema>,
    "source_count" | "source_peaks" | "purpose_count" | "purpose_peaks"
  >;
  readonly nextStepIndex?: number;
}) => {
  const bound = await stepState<
    Data.Static<typeof UnusedScriptBoundWitnessSchema>
  >(common, 1, UnusedScriptStep02DatumSchema);
  const authenticated: Data.Static<
    typeof UnusedScriptAuthenticatedWitnessSchema
  > = {
    bound,
    prior_ledger_root: authentication.machine_state.prior_ledger_root,
    language_tag: authentication.language_tag,
    script_hash: authentication.script_hash,
    script_total_length: authentication.total_length,
    item_commitment: authentication.item_commitment,
    ...frontiers,
  };
  return await continueRaw({
    common,
    stepIndex: 1,
    nextAddress: common.contracts.steps[nextStepIndex]!.spendingScriptAddress,
    nextDatum: datumOf(common, authenticated, UnusedScriptStep03DatumSchema),
    redeemerSchema: UnusedScriptStep02RedeemerSchema,
    args: (input_index, output_index) => ({
      ...authentication,
      input_index,
      output_index,
    }),
  });
};

/** Step 03 with the initial scan state supplied by the caller. */
export const submitUnusedStep03Raw = async ({
  nextState,
  nextStepIndex = 3,
  ...common
}: Common & {
  readonly nextState: Data.Static<typeof UnusedScriptReverseScanSchema>;
  readonly nextStepIndex?: number;
}) =>
  await continueRaw({
    common,
    stepIndex: 2,
    nextAddress: common.contracts.steps[nextStepIndex]!.spendingScriptAddress,
    nextDatum: datumOf(common, nextState, UnusedScriptStep04DatumSchema),
    redeemerSchema: UnusedScriptStep03RedeemerSchema,
    args: (input_index, output_index) => ({ input_index, output_index }),
  });

type ScanState = Data.Static<typeof UnusedScriptReverseScanSchema>;
type SourceOpening = Data.Static<typeof UnusedScriptSourceOpeningSchema>;
type PurposeOpening = Data.Static<typeof UnusedScriptPurposeOpeningSchema>;

/** Step 04 with the openings, budget and next state supplied verbatim. */
export const submitUnusedStep04Raw = async ({
  openings,
  itemBudget,
  nextState,
  nextStepIndex,
  ...common
}: Common & {
  readonly openings: readonly SourceOpening[];
  readonly itemBudget: bigint;
  readonly nextState: ScanState;
  /** 3 keeps the self-loop, 4 hands over to step 05. */
  readonly nextStepIndex: 3 | 4;
}) =>
  await continueRaw({
    common,
    stepIndex: 3,
    nextAddress: common.contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum: datumOf(
      common,
      nextState,
      nextStepIndex === 4
        ? UnusedScriptStep05DatumSchema
        : UnusedScriptStep04DatumSchema,
    ),
    redeemerSchema: UnusedScriptStep04RedeemerSchema,
    args: (input_index, output_index) => ({
      input_index,
      output_index,
      openings,
      item_budget: itemBudget,
    }),
  });

/** Step 05 with the openings, budget and next datum supplied verbatim. */
export const submitUnusedStep05Raw = async ({
  openings,
  itemBudget,
  next,
  ...common
}: Common & {
  readonly openings: readonly PurposeOpening[];
  readonly itemBudget: bigint;
  readonly next:
    | { readonly kind: "scan"; readonly state: ScanState }
    | {
        readonly kind: "decision";
        readonly state: Data.Static<
          typeof UnusedScriptStep06DatumSchema
        >["data"];
      };
}) =>
  await continueRaw({
    common,
    stepIndex: 4,
    nextAddress:
      common.contracts.steps[next.kind === "decision" ? 5 : 4]
        .spendingScriptAddress,
    nextDatum: datumOf(
      common,
      next.state,
      next.kind === "decision"
        ? UnusedScriptStep06DatumSchema
        : UnusedScriptStep05DatumSchema,
    ),
    redeemerSchema: UnusedScriptStep05RedeemerSchema,
    args: (input_index, output_index) => ({
      input_index,
      output_index,
      openings,
      item_budget: itemBudget,
    }),
  });

/** Step 06 without the off-chain contradiction guard: the validator decides. */
export const submitUnusedStep06Raw = async ({
  witnessReferenceScripts,
  ...common
}: Common & {
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 5;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef: common.threadOutRef,
  });
  return await submitLinearFaultFinalize({
    lucid: common.lucid,
    family: FAMILY,
    stepIndex,
    step: common.contracts.steps[stepIndex],
    computationThread: common.contracts.computationThread,
    fraudProof: common.contracts.fraudProof,
    signer: common.signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: UnusedScriptStep06RedeemerSchema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo: common.referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

/** Reads the authenticated witness a step-03 thread currently carries. */
export const readUnusedAuthenticatedWitness = async (common: Common) =>
  await stepState<Data.Static<typeof UnusedScriptAuthenticatedWitnessSchema>>(
    common,
    2,
    UnusedScriptStep03DatumSchema,
  );

/** Reads the scan state a step-04/05 thread currently carries. */
export const readUnusedScanState = async (common: Common, stepIndex: 3 | 4) =>
  await stepState<ScanState>(
    common,
    stepIndex,
    stepIndex === 3
      ? UnusedScriptStep04DatumSchema
      : UnusedScriptStep05DatumSchema,
  );
