/**
 * `executionSourceScriptDecoding` registered-chain emulator support (§5.3).
 *
 * The subject of this family is one execution of one native script source
 * inside a committed native transaction: the retained validation-machine
 * witness authenticates the purpose/source/execution frontiers, and the exact
 * inline field-6 item is opened by bounded-item chunk proofs. Fixtures here
 * build the canonical transaction, its deterministic machine trace, the
 * committed block and the retained evidence from canonical material only;
 * the stages drive the five registered validators on the shared Van Rossem
 * emulator parameters with local UPLC evaluation.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardValidationMerkleMembership,
  computeHash28,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeCbor,
  encodeMidgardVersionedScript,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardNativeScript,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  EventKeySchema,
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  type Header,
  OutputReference,
  Proof,
  type RejectionReason,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type RejectCode,
} from "@al-ft/midgard-validation";
import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  applyExecutionSourceScriptDecodingScripts,
  buildExecutionSourceMachineAuthentication,
  EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
  type ExecutionSourceAuthenticationData,
  type ExecutionSourceScriptDecodingContracts,
  type ExecutionSourceScriptDecodingEvidence,
  planExecutionSourceScriptDecodingStep04,
  prepareExecutionSourceScriptDecodingEvidence,
  readExecutionSourceScanState,
  submitExecutionSourceScriptDecodingCancel,
  submitExecutionSourceScriptDecodingInit,
  submitExecutionSourceScriptDecodingStep01Accepted,
  submitExecutionSourceScriptDecodingStep01Forced,
  submitExecutionSourceScriptDecodingStep01ForcedRaw,
  submitExecutionSourceScriptDecodingStep02,
  submitExecutionSourceScriptDecodingStep02Raw,
  submitExecutionSourceScriptDecodingStep03,
  submitExecutionSourceScriptDecodingStep03Raw,
  submitExecutionSourceScriptDecodingStep04,
  submitExecutionSourceScriptDecodingStep04Raw,
  submitExecutionSourceScriptDecodingStep05,
  submitExecutionSourceScriptDecodingStep05Raw,
} from "../../src/execution-source-script-decoding/index.js";
import type { VanRossemFitMeasurement } from "../../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { buildForcedTransactionLeafMembershipProof } from "../../src/transition-trace/witnesses.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./emulator/measurement.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./emulator/registered-chain.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export { network };
export const EXECUTION_SOURCE_CATEGORY_ID = "00000031";
/** The §5.4 aggregate cap on the field-6 preimage the source item lives in. */
export const EXECUTION_SOURCE_MAX_FIELD_BYTES = 32_768;
export const EXECUTION_SOURCE_REASON_ARMS = [
  "ExecutionNativeScriptMalformed",
  "ExecutionNativeScriptNodeLimit",
  "ExecutionNativeScriptDepthLimit",
] as const;
export type ExecutionSourceReasonArm =
  (typeof EXECUTION_SOURCE_REASON_ARMS)[number];
/** Twin of `rejection_code_of` for this family's three arms. */
export const EXECUTION_SOURCE_REJECTION_CODES: Record<
  ExecutionSourceReasonArm,
  RejectCode
> = {
  ExecutionNativeScriptMalformed: "E_INVALID_FIELD_TYPE",
  ExecutionNativeScriptNodeLimit: "E_NATIVE_SCRIPT_NODE_COUNT",
  ExecutionNativeScriptDepthLimit: "E_NATIVE_SCRIPT_DEPTH",
};
export const forcedReason = (arm: ExecutionSourceReasonArm): RejectionReason =>
  ({ [arm]: { execution_index: 0n } }) as never;

export type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
export type Measurement = CompleteSignedTransactionMeasurement;

// ## Script shapes

export const signatureScript = (fill = 2): MidgardNativeScript => ({
  type: "sig",
  keyHash: Buffer.alloc(28, fill),
});
/** `all []`: the trivially satisfied container every fixture can execute. */
export const emptyAllScript = (): MidgardNativeScript => ({
  type: "all",
  scripts: [],
});
/** `all [all [... all []]]` nested `depth` containers deep. */
export const nestedScript = (depth: number): MidgardNativeScript =>
  depth === 0
    ? emptyAllScript()
    : { type: "all", scripts: [nestedScript(depth - 1)] };
/**
 * `any [all [], sig × count]`: satisfied by its first child whatever the
 * signer set, so the canonical machine accepts the execution, while the
 * structural scan still walks every signature node.
 */
export const wideScript = (
  count: number,
  emptyContainers = 0,
): MidgardNativeScript => ({
  type: "any",
  scripts: [
    emptyAllScript(),
    ...Array.from({ length: count }, (_, index) =>
      signatureScript(4 + (index % 200)),
    ),
    ...Array.from({ length: emptyContainers }, emptyAllScript),
  ],
});
const field6Bytes = (script: MidgardNativeScript): number =>
  encodeMidgardVersionedScriptListPreimage([nativeScriptWitness(script)])
    .length;
/**
 * The widest `any [all [], sig × n, all [] × k]` whose field-6 preimage fits
 * the cap: as many signature nodes as fit, then three-byte containers up to
 * the exact cap, so the item spans all nine bounded chunks.
 */
export const maximumWideScript = (): {
  readonly script: MidgardNativeScript;
  readonly childCount: number;
  readonly fieldBytes: number;
} => {
  let low = 1;
  let high = 2_000;
  while (low < high) {
    const middle = Math.ceil((low + high) / 2);
    if (field6Bytes(wideScript(middle)) <= EXECUTION_SOURCE_MAX_FIELD_BYTES)
      low = middle;
    else high = middle - 1;
  }
  let fill = 0;
  while (
    field6Bytes(wideScript(low, fill + 1)) <= EXECUTION_SOURCE_MAX_FIELD_BYTES
  )
    fill += 1;
  return {
    script: wideScript(low, fill),
    childCount: low + fill + 1,
    fieldBytes: field6Bytes(wideScript(low, fill)),
  };
};

const cborBytesHead = (length: number): Buffer => {
  if (length < 24) return Buffer.from([0x40 | length]);
  if (length <= 0xff) return Buffer.from([0x58, length]);
  const head = Buffer.alloc(3);
  head[0] = 0x59;
  head.writeUInt16BE(length, 1);
  return head;
};
/** The versioned native item `[0, payload]` for arbitrary payload bytes. */
export const rawNativeItem = (payload: Buffer): Buffer =>
  Buffer.concat([
    Buffer.from([0x82, 0x00]),
    cborBytesHead(payload.length),
    payload,
  ]);
const rawNativePayload = (item: Buffer): Buffer => {
  if (item[0] !== 0x82 || item[1] !== 0x00 || item[2] === undefined)
    throw new Error("raw item is not a tag-0 versioned script");
  const additional = item[2] & 0x1f;
  if (additional < 24) return item.subarray(3, 3 + additional);
  if (additional === 24) return item.subarray(4, 4 + item[3]!);
  if (additional === 25) return item.subarray(5, 5 + item.readUInt16BE(3));
  throw new Error("raw item payload head is unsupported");
};
/** The zero-payload item whose single-item field-6 preimage is `fieldBytes`. */
export const malformedItemOfFieldBytes = (fieldBytes: number): Buffer => {
  for (let payload = fieldBytes; payload > fieldBytes - 16; payload -= 1) {
    const item = rawNativeItem(Buffer.alloc(payload, 0));
    if (encodeCbor([item]).length === fieldBytes) return item;
  }
  throw new Error(
    `no zero-payload item has a ${fieldBytes.toString()}-byte field`,
  );
};
/** The zero-payload item whose field-6 preimage is exactly the cap. */
export const maximumMalformedItem = (): Buffer =>
  malformedItemOfFieldBytes(EXECUTION_SOURCE_MAX_FIELD_BYTES);

// ## Registered chain

export const registeredContracts = async (harness: Harness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.executionSourceScriptDecoding;
  const category = harness.catalogue.categories.executionSourceScriptDecoding;
  expectRegisteredChainParity({
    registered,
    applied: applyExecutionSourceScriptDecodingScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  expect(category.categoryId).toBe(EXECUTION_SOURCE_CATEGORY_ID);
  const validators = familyStepsFromRegisteredChain(
    registered.steps,
    EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
  );
  const contracts: ExecutionSourceScriptDecodingContracts = {
    steps: validators,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
  };
  return { validators, contracts, catalogue: harness.catalogue, category };
};

export const makeExecutionSourceHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realExecutionSourceScriptDecoding: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  return { harness, ...(await registeredContracts(harness)) };
};
export type ExecutionSourceContext = Awaited<
  ReturnType<typeof makeExecutionSourceHarness>
>;

// ## Fit-ledger recorder

export const MAXIMUM_SHAPE =
  "32,768-byte field-6 preimage: zero-payload malformed item refused at its first token over the two-chunk window; widest any-of native script that fits the cap (16-step resumable scans across nine chunk windows); nested containers through the frame stack; cancellation from every step; mint and leased removal";

export const createMeasurementRecorder = () => {
  const measurements: VanRossemFitMeasurement[] = [];
  const names = new Set<string>();
  const record = (
    name: string,
    measurement: Measurement,
    { maximumShape = MAXIMUM_SHAPE } = {},
  ) => {
    expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
    expect(measurement.executionMemory, name).toBeGreaterThan(0n);
    expect(measurement.executionSteps, name).toBeGreaterThan(0n);
    if (names.has(name)) return;
    names.add(name);
    measurements.push({
      name,
      kind: "lifecycle",
      maximumShape,
      signedBytes: measurement.completeSignedBytes,
      memoryUnits: measurement.executionMemory,
      cpuUnits: measurement.executionSteps,
    });
  };
  const recordPublication = (stepIndex: number, measurement: Measurement) => {
    const name = `publish-step0${(stepIndex + 1).toString()}`;
    expect(measurement.completeSignedBytes, name).toBeLessThanOrEqual(15_872);
    if (names.has(name)) return;
    names.add(name);
    measurements.push({
      name,
      kind: "publication",
      maximumShape: "fully applied testnet validator",
      signedBytes: measurement.completeSignedBytes,
      memoryUnits: measurement.executionMemory,
      cpuUnits: measurement.executionSteps,
    });
  };
  return { measurements, record, recordPublication };
};
export type MeasurementRecorder = ReturnType<typeof createMeasurementRecorder>;

// ## Subjects

export type SubjectItem =
  | { readonly kind: "script"; readonly script: MidgardNativeScript }
  /** The exact versioned item bytes committed in field 6 (`[0, payload]`). */
  | { readonly kind: "raw"; readonly item: Buffer };

/**
 * The canonical machine's own verdict on the transaction: the deterministic
 * trace is rebuilt with whatever verdict and code the replay reports, so no
 * fixture ever asserts a classification the machine did not produce.
 */
const buildCanonicalTrace = async (
  input: Omit<
    Parameters<typeof buildDeterministicValidationMachineTrace>[0],
    | "expectedVerdict"
    | "expectedRejectionCode"
    | "expectedLedgerOps"
    | "ledgerMutationSteps"
    | "postUtxosRoot"
  > & {
    readonly accepted: {
      readonly expectedLedgerOps: Parameters<
        typeof buildDeterministicValidationMachineTrace
      >[0]["expectedLedgerOps"];
      readonly ledgerMutationSteps: Parameters<
        typeof buildDeterministicValidationMachineTrace
      >[0]["ledgerMutationSteps"];
      readonly postUtxosRoot: string;
    };
  },
) => {
  const { accepted, ...rest } = input;
  const attempt = (verdict: "accepted" | "rejected", code: RejectCode | null) =>
    Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...rest,
        expectedVerdict: verdict,
        expectedRejectionCode: code,
        ...(verdict === "accepted"
          ? accepted
          : {
              expectedLedgerOps: [],
              ledgerMutationSteps: [],
              postUtxosRoot: rest.priorUtxosRoot,
            }),
      }),
    );
  try {
    return {
      verdict: "accepted" as const,
      code: null,
      trace: await attempt("accepted", null),
    };
  } catch (error) {
    const match = /actual=rejected\/([A-Z_]+)/u.exec(
      error instanceof Error ? error.message : String(error),
    );
    if (match?.[1] === undefined) throw error;
    const code = match[1] as RejectCode;
    return {
      verdict: "rejected" as const,
      code,
      trace: await attempt("rejected", code),
    };
  }
};

export type SubjectFixture = Awaited<ReturnType<typeof buildSubjectFixture>>;

/**
 * One committed subject: the canonical transaction executing `item` as the
 * mint policy of its single output, its deterministic machine trace and the
 * retained step-02 authentication, the block committing it (accepted, or a
 * forced leaf rejected with `reason`), and the retained evidence.
 */
export const buildSubjectFixture = async ({
  harness,
  blockContext,
  direction,
  item,
  reason,
  seed = 0x72,
}: {
  readonly direction: "accepted" | "forced";
  readonly item: SubjectItem;
  readonly reason?: RejectionReason;
  readonly seed?: number;
} & (
  | { readonly harness: Harness; readonly blockContext?: never }
  | {
      readonly harness?: never;
      readonly blockContext: {
        readonly operatorVkey: string;
        readonly startTime: bigint;
      };
    }
)) => {
  const spent = outRefFromByte(seed);
  const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
  const witness = nativeScriptWitness(
    item.kind === "script" ? item.script : emptyAllScript(),
  );
  const scriptItem =
    item.kind === "script" ? encodeMidgardVersionedScript(witness) : item.item;
  const policyId =
    item.kind === "script"
      ? Buffer.from(hashScriptWitness(witness), "hex")
      : computeHash28(
          Buffer.concat([Buffer.from([0]), rawNativePayload(item.item)]),
        );
  const assetName = Buffer.from("31", "hex");
  const output = makeOutput(
    FUNDED_OUTPUT_LOVELACE,
    undefined,
    new Map([
      [policyId.toString("hex"), new Map([[assetName.toString("hex"), 1n]])],
    ]),
  );
  let transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs: [output],
    scriptWitnesses: [witness],
    mintPreimageCbor: makeMintPreimageCbor(
      new Map([[policyId, new Map([[assetName, 1n]])]]),
    ),
  });
  if (item.kind === "raw")
    transaction = encodeRecomputedNativeTx({
      ...transaction.tx,
      witnessSet: {
        ...transaction.tx.witnessSet,
        scriptTxWitsPreimageCbor: encodeCbor([item.item]),
      },
    });
  const nativeTx =
    direction === "forced"
      ? decodeMidgardNativeTxFullFromCanonicalCbor(transaction.txCbor)
      : transaction.tx;
  const allOperations = [
    { type: "delete" as const, key: spent },
    buildValidationMachineLedgerInsertOp({
      key: outRefFromTxId(transaction.txId),
      outputCbor: output,
    }),
  ];
  const mutations = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: allOperations,
  });
  const priorLedgerRoot = mutations[0]!.preRoot.toString("hex");
  const orderKey = {
    transactionId: (seed + 1).toString(16).padStart(2, "0").repeat(32),
    outputIndex: 0n,
  };
  const eventKey =
    direction === "forced"
      ? ({ ForcedTransactionEventKey: { tx_order_id: orderKey } } as const)
      : ({
          L2TransactionEventKey: { tx_id: transaction.txId.toString("hex") },
        } as const);
  const canonical = await buildCanonicalTrace({
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    eventKeyCbor: Buffer.from(
      Data.to(eventKey as never, EventKeySchema),
      "hex",
    ),
    sourceKind: direction === "forced" ? "forced" : "normal",
    committedForcedVerdict: direction === "forced" ? "rejected" : undefined,
    blockEndTimeMs: 1_750_000_001_000,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    blockSlot: 0n,
    transactionId: transaction.txId,
    canonicalTransactionCbor: transaction.txCbor,
    priorUtxosRoot: priorLedgerRoot,
    ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
    accepted: {
      expectedLedgerOps: allOperations,
      ledgerMutationSteps: mutations,
      postUtxosRoot: mutations.at(-1)!.postRoot.toString("hex"),
    },
  });
  if (direction === "forced" && reason === undefined)
    throw new Error("a forced subject needs the operator's typed reason");
  const arm =
    reason === undefined
      ? null
      : (Object.keys(reason)[0] as ExecutionSourceReasonArm);
  const authentication = await buildExecutionSourceMachineAuthentication({
    trace: canonical.trace,
    eventKey,
    claimedVerdict: direction === "forced" ? "rejected" : "accepted",
    claimedRejectionCode:
      arm === null ? null : EXECUTION_SOURCE_REJECTION_CODES[arm],
  });
  const { operatorVkey, startTime } = blockContext ?? {
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime: BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    ),
  };
  const block = await buildDecodingBlockFixture({
    operatorVkey,
    startTime,
    priorLedgerRoot,
    subject:
      direction === "forced"
        ? {
            kind: "forced",
            nativeTx,
            orderKey,
            verdict: { ForcedTxInvalid: { reason: reason! } },
          }
        : { kind: "normal", nativeTx },
  });
  const header: Header = {
    ...block.header,
    validationTracesRoot: authentication.validationTracesRoot,
    validationTraceCount: authentication.validationTraceCount,
  };
  const auxiliary = canonical.trace.witnesses.find(
    ({ phase, auxiliary }) =>
      phase === "nativeScripts" &&
      auxiliary?.kind === "nativeExecutionDescriptor",
  )?.auxiliary;
  if (auxiliary?.kind !== "nativeExecutionDescriptor")
    throw new Error("missing native execution descriptor");
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind: auxiliary.purpose.purposeKind,
    purposeIndex: auxiliary.purpose.purposeIndex,
    scriptHash: auxiliary.purpose.scriptHash,
    subject: auxiliary.purpose.subject,
  });
  const sourceLeaf = hashMidgardInlineScriptSourceLeaf({
    sourceIndex: BigInt(auxiliary.source.sourceIndex),
    scriptLanguageTag: 0,
    scriptHash: auxiliary.purpose.scriptHash,
    scriptTotalLength: auxiliary.source.scriptTotalLength,
    itemCommitment: auxiliary.source.scriptItemCommitment,
  });
  const executionLeaf = hashMidgardScriptExecutionLeaf({
    languageTag: 0,
    purposeLeaf,
    sourceLeaf,
    redeemerLeaf: auxiliary.redeemerLeaf,
  });
  const subject =
    direction === "forced"
      ? forcedVerdictSubject({
          transactionId: block.nativeTxId,
          sourceKey: orderKey,
          rejectionReason: reason!,
        })
      : acceptedVerdictSubject(block.nativeTxId);
  const descriptor: ExecutionSourceScriptDecodingEvidence["descriptor"] = {
    sourceIndex: auxiliary.source.sourceIndex,
    originKind: 0,
    sourceKeyHex: auxiliary.source.sourceKey.toString("hex"),
    languageTag: 0,
    scriptHashHex: auxiliary.purpose.scriptHash.toString("hex"),
    scriptItemHex: scriptItem.toString("hex"),
    purposeKind: auxiliary.purpose.purposeKind,
    purposeIndex: Number(auxiliary.purpose.purposeIndex),
    purposeSubjectHex: auxiliary.purpose.subject.toString("hex"),
    redeemerLeafHex: "",
    purposeMembership: buildMidgardValidationMerkleMembership([purposeLeaf], 0),
    sourceMembership: buildMidgardValidationMerkleMembership([sourceLeaf], 0),
    executionMembership: buildMidgardValidationMerkleMembership(
      [executionLeaf],
      0,
    ),
  };
  const evidence = prepareExecutionSourceScriptDecodingEvidence({
    finding: { subject, executionIndex: 0 },
    descriptor,
  });
  const membership =
    direction === "forced"
      ? await buildForcedTransactionLeafMembershipProof({
          reconstruction: block.reconstruction,
          eventKey,
        })
      : null;
  return {
    direction,
    scriptItem,
    transaction,
    canonicalVerdict: canonical.verdict,
    canonicalCode: canonical.code,
    trace: canonical.trace,
    authentication,
    block,
    header,
    eventKey,
    orderKey,
    subject,
    evidence,
    membership,
  };
};

/** Commits the fixture's header as the disputed state-queue block. */
export const commitSubjectBlock = async (
  { harness, catalogue }: ExecutionSourceContext,
  fixture: SubjectFixture,
) =>
  await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header: fixture.header,
  });

/**
 * A forced root that carries the same leaf but which the header never
 * committed: the membership proof verifies against its own root and the
 * counted-root binding to the header refuses it.
 */
export const foreignForcedMembership = async (
  membership: NonNullable<SubjectFixture["membership"]>,
) => {
  const key = Buffer.from(Data.to(membership.key, OutputReference), "hex");
  const value = Buffer.from(
    Data.to(membership.value as never, ForcedInclusionTxV1Schema as never),
    "hex",
  );
  const decoy = Buffer.from(
    Data.to(
      { transactionId: "ab".repeat(32), outputIndex: 7n },
      OutputReference,
    ),
    "hex",
  );
  const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    { key, value },
    { key: decoy, value },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(key, value);
  await trie.insert(decoy, value);
  return {
    ...membership,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    proof: Data.from((await trie.prove(key)).toCBOR().toString("hex"), Proof),
  };
};

// ## Stages

export const publishFamilyReferences = async (
  { harness, validators }: ExecutionSourceContext,
  recorder: MeasurementRecorder,
  label: string,
) => {
  const references: UTxO[] = [];
  for (const [index, step] of validators.entries()) {
    const published = await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: step.spendingScript,
      label: `${label}-${index.toString()}`,
    });
    recorder.recordPublication(index, published.publicationMeasurement);
    references.push(published.utxo);
  }
  return references;
};

export const makeExecutionSourceStages = ({
  context,
  setup,
  references,
}: {
  readonly context: ExecutionSourceContext;
  readonly setup: Awaited<ReturnType<typeof commitSubjectBlock>>;
  readonly references: readonly UTxO[];
}) => {
  const { harness, contracts, catalogue, category, validators } = context;
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const categoryId = category.categoryId;
  const captured = <T>(operation: () => Promise<T>) =>
    captureEmulatorSubmission(harness.emulator, operation);
  const reference = (stepIndex: number): UTxO => {
    const utxo = references[stepIndex];
    if (utxo === undefined) throw new Error("reference script absent");
    return utxo;
  };
  const common = { lucid, contracts, categoryId, signer };

  const init = async () =>
    await captured(() =>
      submitExecutionSourceScriptDecodingInit({
        ...common,
        blueprint: harness.realBlueprint,
        network,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        fraudulentHeaderHash: setup.headerHash,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  /** A fresh thread at step 01, for seams and cancellations. */
  const freshThread = async () => (await init()).result.nextThreadOutRef;
  const step01Accepted = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    executionIndex = 0n,
    txInclusion = fixture.block.txInclusion,
  ) => {
    if (txInclusion === null) throw new Error("accepted inclusion absent");
    return await captured(() =>
      submitExecutionSourceScriptDecodingStep01Accepted({
        ...common,
        blueprint: harness.realBlueprint,
        network,
        threadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        header: fixture.header,
        executionIndex,
        referenceScriptUtxo: reference(0),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  };
  const step01Forced = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    executionIndex = 0n,
  ) => {
    if (fixture.membership === null) throw new Error("forced leaf absent");
    const membership = fixture.membership;
    return await captured(() =>
      submitExecutionSourceScriptDecodingStep01Forced({
        ...common,
        threadOutRef,
        header: fixture.header,
        membership,
        executionIndex,
        referenceScriptUtxo: reference(0),
      }),
    );
  };
  const step01ForcedRaw = async (
    args: Omit<
      Parameters<typeof submitExecutionSourceScriptDecodingStep01ForcedRaw>[0],
      keyof typeof common | "referenceScriptUtxo"
    >,
  ) =>
    await submitExecutionSourceScriptDecodingStep01ForcedRaw({
      ...common,
      referenceScriptUtxo: reference(0),
      ...args,
    });
  const step02 = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    authentication: ExecutionSourceAuthenticationData = fixture.authentication
      .authentication,
  ) =>
    await captured(() =>
      submitExecutionSourceScriptDecodingStep02({
        ...common,
        threadOutRef,
        evidence: fixture.evidence,
        authentication,
        referenceScriptUtxo: reference(1),
      }),
    );
  const step02Raw = async (
    args: Omit<
      Parameters<typeof submitExecutionSourceScriptDecodingStep02Raw>[0],
      keyof typeof common | "referenceScriptUtxo"
    >,
  ) =>
    await submitExecutionSourceScriptDecodingStep02Raw({
      ...common,
      referenceScriptUtxo: reference(1),
      ...args,
    });
  const step03 = async (threadOutRef: string, fixture: SubjectFixture) =>
    await captured(() =>
      submitExecutionSourceScriptDecodingStep03({
        ...common,
        threadOutRef,
        evidence: fixture.evidence,
        referenceScriptUtxo: reference(2),
      }),
    );
  const step03Raw = async (
    args: Omit<
      Parameters<typeof submitExecutionSourceScriptDecodingStep03Raw>[0],
      keyof typeof common | "referenceScriptUtxo"
    >,
  ) =>
    await submitExecutionSourceScriptDecodingStep03Raw({
      ...common,
      referenceScriptUtxo: reference(2),
      ...args,
    });
  const scanState = async (threadOutRef: string, stepIndex: 3 | 4 = 3) =>
    (await readExecutionSourceScanState({ ...common, threadOutRef, stepIndex }))
      .state;
  const plan04 = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    direction?: 0 | 1,
  ) =>
    planExecutionSourceScriptDecodingStep04({
      contracts,
      state: await scanState(threadOutRef),
      evidence: fixture.evidence,
      ...(direction === undefined ? {} : { direction }),
    });
  const step04 = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    direction?: 0 | 1,
  ) =>
    await captured(() =>
      submitExecutionSourceScriptDecodingStep04({
        ...common,
        threadOutRef,
        evidence: fixture.evidence,
        ...(direction === undefined ? {} : { direction }),
        referenceScriptUtxo: reference(3),
      }),
    );
  /** Every scan transaction until the item scan closes. */
  const step04Loop = async (
    threadOutRef: string,
    fixture: SubjectFixture,
    onScan?: (
      captured: Awaited<ReturnType<typeof step04>>,
      index: number,
    ) => void,
    direction?: 0 | 1,
  ) => {
    let current = threadOutRef;
    let scans = 0;
    for (;;) {
      const scan = await step04(current, fixture, direction);
      onScan?.(scan, scans);
      scans += 1;
      current = scan.result.nextThreadOutRef;
      if (scan.result.closed) break;
    }
    return { threadOutRef: current, scans };
  };
  const step04Raw = async (
    args: Omit<
      Parameters<typeof submitExecutionSourceScriptDecodingStep04Raw>[0],
      keyof typeof common | "referenceScriptUtxo"
    >,
  ) =>
    await submitExecutionSourceScriptDecodingStep04Raw({
      ...common,
      referenceScriptUtxo: reference(3),
      ...args,
    });
  const step05 = async (threadOutRef: string, fixture: SubjectFixture) =>
    await captured(() =>
      submitExecutionSourceScriptDecodingStep05({
        ...common,
        threadOutRef,
        evidence: fixture.evidence,
        referenceScriptUtxo: reference(4),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const step05Raw = async (threadOutRef: string) =>
    await submitExecutionSourceScriptDecodingStep05Raw({
      ...common,
      threadOutRef,
      referenceScriptUtxo: reference(4),
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = async (threadOutRef: string, stepIndex: number) =>
    await captured(() =>
      submitExecutionSourceScriptDecodingCancel({
        ...common,
        threadOutRef,
        referenceScriptUtxo: reference(stepIndex),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const remove = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid,
      contracts: harness.contracts,
    });
    const base = buildRemovalDeploymentInfo(harness.contracts, catalogue, {
      removalReferenceScripts: removalReferences.published,
    });
    const deploymentInfo = {
      ...base,
      contracts: {
        ...base.contracts,
        ...Object.fromEntries(
          validators.map((step, index) => [
            index === 0
              ? "fraudProofExecutionSourceScriptDecoding"
              : `fraudProofExecutionSourceScriptDecodingStep0${(index + 1).toString()}`,
            {
              scriptHash: step.spendingScriptHash,
              contract: {
                type: step.spendingScript.type,
                cborHex: step.spendingScript.script,
              },
            },
          ]),
        ),
      },
    };
    const now = BigInt(harness.emulator.now());
    return await captured(() =>
      submitRemoveFraudulentBlock({
        lucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer,
        fraudCategory: "executionSourceScriptDecoding",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => ({
            token: "execution-source-script-decoding-emulator",
            source: "emulator",
            renew: async () => {},
            release: async () => {},
            fail: async () => {},
          }),
        },
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
  };
  return {
    init,
    freshThread,
    step01Accepted,
    step01Forced,
    step01ForcedRaw,
    step02,
    step02Raw,
    step03,
    step03Raw,
    scanState,
    plan04,
    step04,
    step04Loop,
    step04Raw,
    step05,
    step05Raw,
    cancel,
    remove,
  };
};
export type ExecutionSourceStages = ReturnType<
  typeof makeExecutionSourceStages
>;

export { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";
