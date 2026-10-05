import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardValidationMerkleMembership,
  computeHash28,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeCbor,
  encodeMidgardVersionedScript,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical as forcedTraceBytes,
  materializeMidgardForcedTxFromCanonical as forcedTraceView,
} from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
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
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
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
import { Data } from "@lucid-evolution/lucid";

import {
  buildExecutionSourceMachineAuthentication,
  type ExecutionSourceScriptDecodingEvidence,
  prepareExecutionSourceScriptDecodingEvidence,
} from "../../src/execution-source-script-decoding/index.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { buildForcedTransactionLeafMembershipProof } from "../../src/transition-trace/witnesses.js";
import {
  buildCanonicalTrace,
  emptyAllScript,
  EXECUTION_SOURCE_REJECTION_CODES,
  type ExecutionSourceContext,
  type ExecutionSourceReasonArm,
  type Harness,
  rawNativePayload,
  type SubjectItem,
} from "./execution-source-script-decoding-emulator.build-canonical-trace.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export type SubjectFixture = Awaited<ReturnType<typeof buildSubjectFixture>>;

/**
 * One committed subject: the canonical transaction executing `item` as the
 * mint policy of its single output, its deterministic machine trace and the
 * retained step-02 authentication, the block committing it (accepted, or a
 * forced leaf rejected with `reason`), and the retained evidence.
 */
export const buildSubjectReplay = async ({
  direction,
  item,
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

    blockEndTimeMs: 1_750_000_001_000,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    blockSlot: 0n,
    transactionId: transaction.txId,
    canonicalTransactionCbor:
      direction === "forced"
        ? forcedTraceBytes(forcedTraceView(transaction.tx))
        : transaction.txCbor,
    priorUtxosRoot: priorLedgerRoot,
    ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
    accepted: {
      expectedLedgerOps: allOperations,
      ledgerMutationSteps: mutations,
      postUtxosRoot: mutations.at(-1)!.postRoot.toString("hex"),
    },
  });
  return {
    canonical,
    transaction,
    nativeTx,
    scriptItem,
    priorLedgerRoot,
    orderKey,
    eventKey,
  };
};

/** Only replay that reached NativeScripts can authenticate this family. */
export const buildSubjectFixture = async (
  input: Parameters<typeof buildSubjectReplay>[0],
) => {
  const { harness, blockContext, direction, reason } = input;
  const {
    canonical,
    transaction,
    nativeTx,
    scriptItem,
    priorLedgerRoot,
    orderKey,
    eventKey,
  } = await buildSubjectReplay(input);
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
