import {
  buildMidgardValidationTraceTree,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical as forcedTraceBytes,
  materializeMidgardForcedTxFromCanonical as forcedTraceView,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  MidgardRedeemerTag,
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
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
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
import type { ReceivePurposeLanguageContracts } from "../../src/receive-purpose-language/contracts.js";
import {
  AuthenticatedReceiveLanguageSchema,
  ReceivePurposeBoundExecutionSchema,
  ReceivePurposeStep01RedeemerSchema,
  ReceivePurposeStep02DatumSchema,
  ReceivePurposeStep02RedeemerSchema,
  ReceivePurposeStep03DatumSchema,
  ReceivePurposeStep03RedeemerSchema,
} from "../../src/receive-purpose-language/schemas.js";
import type { ReceivePurposeLanguageAuthentication } from "../../src/receive-purpose-language/submit-step-02.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";

const FAMILY = "receive-purpose-language";

/**
 * A PlutusV3 program whose receive execution the machine refuses with
 * `E_PLUTUS_SCRIPT_INVALID` ("ReceivingScript requires MidgardV1 context"),
 * and the CEK program-material sidecar that carries it. Shared with the
 * retained-DA unit test so both suites accuse the same committed script.
 */
export const PLUTUS_V3_RECEIVE_SCRIPT = Buffer.from(
  "85018301010058207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e021827",
  "hex",
);
export const PLUTUS_V3_RECEIVE_SIDECAR = Buffer.from(
  "82018282582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d47830100438200008258207d068efad94d2953eefe63951671327af75e08c963cd1f232b08966e6026bf5e582983010058248202582072c078cab22fca41a65b75e6dfcff21d6258a743068e190836bd227ad35dd99d",
  "hex",
);

export const RECEIVE_PURPOSE_REJECT_CODE = "E_PLUTUS_SCRIPT_INVALID" as const;

export type ReceivePurposeFixtureSpec = Readonly<{
  /** Whether the accused event is a committed L2 transaction or a forced one. */
  direction: "accepted" | "forced";
  /** The language of the accused receive purpose. */
  language: "plutusV3" | "native";
  /** The verdict the operator committed for the event. */
  claimedVerdict: "accepted" | "rejected";
  /**
   * Total script purposes: one receive purpose (the machine keys receive
   * purposes by receiving script, so one protected output is one purpose)
   * plus `purposeCount - 1` native spend purposes, each a distinct spent
   * out-ref protected by the trivial `all []` script, which is what widens
   * the purpose and execution frontiers. Spend purposes share one witness
   * because the machine's script-discovery bitmap caps distinct script
   * witnesses at 64 per transaction.
   */
  purposeCount: number;
  /** Extra committed L2 transactions widening the validation-traces trie. */
  decoyTransactionCount?: number;
  /** Byte seeding the spent out-ref so fixtures in one harness stay distinct. */
  inputByte: number;
  operatorVkey: string;
  startTime: bigint;
}>;

export const receivePurposeReason = (executionIndex: number) =>
  ({
    ReceivePurposePlutusV3Forbidden: {
      execution_index: BigInt(executionIndex),
    },
  }) as const;

/**
 * Builds the accused transaction, replays it through the deterministic
 * validation machine, commits it in a block whose validation-traces root is
 * the genuine descriptor tree, and exposes the retained DA entries a prover
 * reconstructs step 02 from.
 */
export const buildReceivePurposeFixture = async (
  spec: ReceivePurposeFixtureSpec,
) => {
  if (spec.purposeCount < 1 || spec.purposeCount > 0xffff)
    throw new Error("receive fixture purpose count must be within 1..65535");
  const trivialScript = nativeScriptWitness({ type: "all", scripts: [] });
  const script =
    spec.language === "plutusV3"
      ? plutusV3ScriptWitness(PLUTUS_V3_RECEIVE_SCRIPT)
      : trivialScript;
  const scriptHash = hashScriptWitness(script);
  const spendScriptHash = hashScriptWitness(trivialScript);
  // The key-held funding input sits at index 0 of the seeded transaction id;
  // every widening spend purpose is a later index of the same id, so the
  // spend-inputs preimage stays in canonical order.
  const spentTxId = Buffer.alloc(32, spec.inputByte);
  const spent = outRefFromByte(spec.inputByte);
  const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
  const scriptSpends = Array.from(
    { length: spec.purposeCount - 1 },
    (_, index) => ({
      outRef: outRefFromTxId(spentTxId, BigInt(index + 1)),
      output: makeProtectedScriptOutput(
        spendScriptHash,
        FUNDED_OUTPUT_LOVELACE,
      ),
    }),
  );
  const outputs = [
    makeProtectedScriptOutput(scriptHash, FUNDED_OUTPUT_LOVELACE),
    ...(scriptSpends.length > 0
      ? [makeOutput(FUNDED_OUTPUT_LOVELACE * BigInt(scriptSpends.length))]
      : []),
  ];
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent, ...scriptSpends.map(({ outRef }) => outRef)],
    outputs,
    scriptWitnesses:
      spec.language === "plutusV3" && scriptSpends.length > 0
        ? [script, trivialScript]
        : [script],
    ...(spec.language === "plutusV3"
      ? {
          redeemerTxWitsPreimageCbor: makeRedeemersCbor([
            {
              tag: MidgardRedeemerTag.Receiving,
              index: 0n,
              exUnits: [1_000_000n, 1_000_000n] as const,
            },
          ]),
          scriptLanguages: ["PlutusV3" as const],
        }
      : {}),
  });
  const nativeTxId = transaction.txId.toString("hex");
  // The shape must be one the consensus profile admits: every widened field
  // stays inside its aggregate preimage bound.
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
        `receive fixture ${label} preimage (${bytes.toString()} bytes) exceeds the consensus bound ${bound.toString()}`,
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
  const machineAccepts = spec.language === "native";
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

      blockEndTimeMs: 1_750_000_001_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId: transaction.txId,
      canonicalTransactionCbor:
        spec.direction === "forced"
          ? forcedTraceBytes(forcedTraceView(transaction.tx))
          : transaction.txCbor,
      ...(spec.language === "plutusV3"
        ? { programMaterialSidecarCbor: PLUTUS_V3_RECEIVE_SIDECAR }
        : {}),
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
        : RECEIVE_PURPOSE_REJECT_CODE,
    }),
  );
  const stateIndex = trace.witnesses.findIndex(
    ({ phase, auxiliary }) =>
      phase === "nativeScripts" &&
      auxiliary?.kind === "nativeExecutionDescriptor" &&
      auxiliary.purpose.purposeKind === 3,
  );
  if (stateIndex < 0)
    throw new Error("receive fixture replay has no receive descriptor");
  const witness = trace.witnesses[stateIndex]!;
  if (witness.auxiliary?.kind !== "nativeExecutionDescriptor")
    throw new Error("receive fixture retained the wrong auxiliary kind");
  const executionIndex = witness.auxiliary.executionIndex;
  if (!Number.isSafeInteger(executionIndex) || executionIndex < 0)
    throw new Error("receive fixture execution index is not an index");
  const claimedTree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    spec.claimedVerdict,
    spec.claimedVerdict === "accepted"
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCode(RECEIVE_PURPOSE_REJECT_CODE),
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
  const retainedKey: SDK.RetainedValidationWitnessKey = {
    event_key: eventKey,
    execution_index: BigInt(executionIndex),
  };
  const retainedValue: SDK.RetainedValidationWitness = {
    machine_state: SDK.validationMachineStateDataFromCore(
      trace.states[stateIndex]!,
    ),
    trace_proof: SDK.validationTraceProofDataFromCore(
      claimedTree.proofs[stateIndex]!,
    ),
    phase: 9n,
    program_counter: BigInt(witness.programCounter),
    witness_cbor: witness.cbor.toString("hex"),
    auxiliary: Data.from(
      Data.to(validationAuxiliaryWitnessData(witness.auxiliary) as never),
      SDK.ValidationAuxiliaryWitnessSchema,
    ) as unknown as SDK.ValidationAuxiliaryWitness,
  };
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
              ForcedTxInvalid: { reason: receivePurposeReason(executionIndex) },
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
    throw new Error("receive fixture block omitted the accused event");
  const validationTraceWitnesses: SDK.DaPayloadEntry[] = [
    [
      SDK.encodeRetainedValidationWitnessKey(retainedKey).toString("hex"),
      SDK.encodeRetainedValidationWitness(retainedValue).toString("hex"),
    ],
  ];
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
          rejectionReason: receivePurposeReason(executionIndex),
        })
      : SDK.acceptedVerdictSubject(nativeTxId);
  return Object.freeze({
    spec,
    transaction,
    nativeTxId,
    scriptHash,
    trace,
    stateIndex,
    executionIndex,
    eventKey,
    orderKey,
    subject,
    block,
    header,
    descriptor,
    retainedEntries,
    canonicalBlock,
    languageTag: (spec.language === "plutusV3" ? 3 : 0) as 0 | 3,
  });
};

export type ReceivePurposeFixture = Awaited<
  ReturnType<typeof buildReceivePurposeFixture>
>;

type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: ReceivePurposeLanguageContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  referenceScriptUtxo: UTxO;
}>;

/**
 * Step 01 over a forced leaf with no off-chain classification: the exact
 * redeemer the test asks for reaches the validator, so reason-coordinate,
 * header, membership and direction mutations are refused on chain.
 */
export const submitReceiveStep01ForcedRaw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  header,
  membership,
  executionIndex,
  direction,
  referenceScriptUtxo,
}: Common & {
  readonly header: SDK.Header;
  readonly membership: SDK.RootMembershipProof<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  >;
  readonly executionIndex: bigint;
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
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  requireLinearFaultInitialDatum({ threadUtxo, signer, family: FAMILY });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject,
        validation_traces_root: header.validationTracesRoot,
        validation_trace_count: header.validationTraceCount,
        execution_index: executionIndex,
      },
    } as never,
    ReceivePurposeStep02DatumSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex: 0,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, "raw step 01");
    const inputIndex = SDK.requireInputIndex(ctx, threadUtxo, "raw step 01");
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "raw step 01",
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                input_index: inputIndex,
                output_index: outputIndex,
                header,
                membership,
                direction,
              },
            },
            execution_index: executionIndex,
          },
        ],
      } as never,
      ReceivePurposeStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "raw step 01",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error("raw step 01: no layout");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/**
 * Step 02 with the supplied authentication handed to the validator verbatim
 * (no evidence cross-check), so every authentication-seam substitution is
 * refused by the script rather than by the builder.
 */
export const submitReceiveStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  authentication,
  referenceScriptUtxo,
}: Common & {
  readonly authentication: ReceivePurposeLanguageAuthentication;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const bound = requireLinearFaultStepState<
    Data.Static<typeof ReceivePurposeBoundExecutionSchema>
  >({
    threadUtxo,
    signer,
    schema: ReceivePurposeStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const authenticated: Data.Static<typeof AuthenticatedReceiveLanguageSchema> =
    {
      bound,
      prior_ledger_root: authentication.machine_state.prior_ledger_root,
      purpose_kind: 3n,
      purpose_index: authentication.purpose_index,
      source_index: authentication.source_index,
      origin_kind: authentication.origin_kind,
      source_key: authentication.source_key,
      language_tag: authentication.language_tag,
      script_hash: authentication.script_hash,
    };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: authenticated } as never,
    ReceivePurposeStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, "raw step 02");
    const inputIndex = SDK.requireInputIndex(ctx, threadUtxo, "raw step 02");
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "raw step 02",
    );
    return Data.to(
      {
        Continue: [
          {
            ...authentication,
            input_index: inputIndex,
            output_index: outputIndex,
          },
        ],
      } as never,
      ReceivePurposeStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: "raw step 02",
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error("raw step 02: no layout");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/** Step 03 without the off-chain contradiction guard: the validator decides. */
export const submitReceiveStep03Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: Common & {
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ReceivePurposeStep03RedeemerSchema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};
