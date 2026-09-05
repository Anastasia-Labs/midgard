/**
 * Emulator support for the `resolvedOutputNonCanonical` (`00000026`)
 * lifecycle: retained prior-ledger fixtures, accepted and forced block
 * commitment on the registered chain, the family submitters, and the raw
 * continuations that hand a substituted redeemer or successor to the
 * validator so every seam is refused on chain rather than by a builder.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  buildMidgardBoundedItem,
  computeMidgardNativeTxId,
  decodeMidgardDatum,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardLedgerOutputCommitment,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  AddressData,
  addressDataFromBech32,
  EventKeySchema,
  EventToStepValueSchema,
  type FieldOpening,
  type ForcedInclusionTxV1,
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  type Header,
  OutputReference,
  Proof,
  type RejectionReason,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
  type RootMembershipProof,
  TransitionStepSchema,
  ValidationTraceDescriptorSchema,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { submitCommittedFieldShapeInit } from "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import { submitRemoveFraudulentBlock } from "../../src/remove-fraudulent-block.js";
import {
  applyResolvedOutputNonCanonicalScripts,
  prepareResolvedOutputNonCanonicalEvidence,
  RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
  type ResolvedOutputCoordinate,
  type ResolvedOutputEvidence,
  type ResolvedOutputForcedSource,
  type ResolvedOutputNonCanonicalContracts,
  ResolvedOutputStep01RedeemerSchema,
  ResolvedOutputStep02DatumSchema,
  ResolvedOutputStep02RedeemerSchema,
  ResolvedOutputStep03DatumSchema,
  ResolvedOutputStep04DatumSchema,
  ResolvedOutputStep04RedeemerSchema,
  ResolvedOutputStep05DatumSchema,
  ResolvedOutputStep05RedeemerSchema,
  submitResolvedOutputNonCanonicalCancel,
  submitResolvedOutputNonCanonicalStep01Accepted,
  submitResolvedOutputNonCanonicalStep01Forced,
  submitResolvedOutputNonCanonicalStep02,
  submitResolvedOutputNonCanonicalStep03,
  submitResolvedOutputNonCanonicalStep04,
  submitResolvedOutputNonCanonicalStep05,
} from "../../src/resolved-output-non-canonical/index.js";
import { nativeTxFromCoreCompact } from "../../src/submit-step-01.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./emulator/measurement.js";
import { l2TransactionSourceCbor } from "./emulator/native-tx.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./emulator/registered-chain.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
import {
  ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  insertAdversarialMembershipSiblings,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./submit-init-emulator-fixtures.js";
import {
  h32,
  makeHeader,
  makeNativeTx,
  publishRemovalReferenceScripts,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./submit-init-emulator-shared.js";

export const network = "Custom" as const;
export const FAMILY = "resolved-output-non-canonical";
export const RESOLVED_OUTPUT_MAXIMUM_BYTES = 16_384;
/** Repeated input items that push field 0/1 into Certified carriage. */
export const MAXIMUM_INPUT_ITEM_COUNT = 800;
export const RESOLVED_OUTPUT_REASON_ARM = "InputSpentOutputNonCanonical";

const address = Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]);
const lovelaceOnly = { lovelace: 2_000_000n, assets: new Map() } as const;

// ---------------------------------------------------------------------------
// Retained prior-ledger output shapes
// ---------------------------------------------------------------------------

/** A canonical two-entry output shorter than one chunk. */
export const smallCanonicalOutput = (): Buffer =>
  encodeMidgardTxOutput({ address, value: lovelaceOnly });

/** The small canonical output followed by one trailing byte. */
export const smallMalformedOutput = (): Buffer =>
  Buffer.concat([smallCanonicalOutput(), Buffer.from([0])]);

const multiAssetValue = () => ({
  lovelace: 2_000_000n,
  assets: new Map(
    [0x0a, 0x0b].map((policyByte) => [
      Buffer.alloc(28, policyByte).toString("hex"),
      new Map([
        ["01", 5n],
        ["0203", 7n],
      ]),
    ]),
  ),
});

const canonicalFourEntryOutput = (scriptBytes: number): Buffer =>
  encodeMidgardTxOutput({
    address,
    value: multiAssetValue(),
    datum: decodeMidgardDatum(Buffer.from(Data.to("ab".repeat(7_000)), "hex")),
    script_ref: {
      language: "PlutusV3",
      scriptBytes: Buffer.alloc(scriptBytes, 0x6b),
    },
  });

/**
 * The largest canonical resolved output the ledger admits: address,
 * multi-asset value, datum payload and reference script, exactly 16,384 bytes.
 * The reference script is the tuning knob; its CBOR header is stable in the
 * range this shape uses, so one correction lands on the exact length.
 */
export const maximumCanonicalOutput = (): Buffer => {
  const probe = canonicalFourEntryOutput(7_000);
  const output = canonicalFourEntryOutput(
    7_000 + (RESOLVED_OUTPUT_MAXIMUM_BYTES - probe.length),
  );
  expect(output).toHaveLength(RESOLVED_OUTPUT_MAXIMUM_BYTES);
  return output;
};

/**
 * The accepted direction's maximum: a canonical four-entry prefix padded with
 * zero bytes to 16,384, so the structural scan walks every stage before the
 * trailing bytes fault it.
 */
export const maximumMalformedOutput = (): Buffer => {
  const prefix = canonicalFourEntryOutput(7_000);
  expect(prefix.length).toBeLessThan(RESOLVED_OUTPUT_MAXIMUM_BYTES);
  return Buffer.concat([
    prefix,
    Buffer.alloc(RESOLVED_OUTPUT_MAXIMUM_BYTES - prefix.length),
  ]);
};

/**
 * The compact descriptor a prior block committed for `output`. A canonical
 * output reconstructs its own descriptor; a malformed one is committed under
 * the small canonical template with the malformed item's length and
 * commitment, which is what an operator who accepted it must have done.
 */
export const descriptorFor = (outputIndex: number, output: Buffer): Buffer => {
  try {
    return buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex,
      outputCbor: output,
    }).descriptorCbor;
  } catch {
    const template = decodeMidgardLedgerOutputCommitment(
      buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex,
        outputCbor: smallCanonicalOutput(),
      }).descriptorCbor,
    );
    return encodeMidgardLedgerOutputCommitment({
      ...template,
      totalLength: output.length,
      itemCommitment: buildMidgardBoundedItem({
        fieldIndex: 2,
        itemIndex: outputIndex,
        bytes: output,
      }).commitment,
    });
  }
};

export type PriorLedgerFixture = Readonly<{
  priorRoot: string;
  priorTxId: string;
  outputIndex: number;
  outRefBytes: Buffer;
  output: Buffer;
  descriptorCbor: Buffer;
  proofCborHex: string;
  /** A sibling key the trie also holds, for membership substitution. */
  siblingProofCborHex: string;
  siblingKeyBytes: Buffer;
  siblingDescriptorCbor: Buffer;
}>;

/**
 * The predecessor ledger: the resolved output at `priorTxId#outputIndex`,
 * one canonical sibling entry, and (at the maximum shape) the adversarial
 * branch siblings that deepen the membership proof.
 */
export const buildPriorLedger = async ({
  output,
  outputIndex = 0,
  adversarialDepth,
  priorTxId = "ab".repeat(32),
}: {
  readonly output: Buffer;
  readonly outputIndex?: number;
  readonly adversarialDepth: boolean;
  readonly priorTxId?: string;
}): Promise<PriorLedgerFixture> => {
  const outRefBytes = encodeMidgardSpendInputItem({
    txId: Buffer.from(priorTxId, "hex"),
    outputIndex,
  });
  const descriptorCbor = descriptorFor(outputIndex, output);
  const siblingKeyBytes = encodeMidgardSpendInputItem({
    txId: Buffer.from("cd".repeat(32), "hex"),
    outputIndex: 1,
  });
  const siblingDescriptorCbor = descriptorFor(1, smallCanonicalOutput());
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(outRefBytes, descriptorCbor);
  await trie.insert(siblingKeyBytes, siblingDescriptorCbor);
  if (adversarialDepth) {
    await insertAdversarialMembershipSiblings({
      trie,
      targets: [{ key: outRefBytes, domain: 0x2601 }],
      branchLevels: ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    });
  }
  return {
    priorRoot: Buffer.from(trie.hash).toString("hex"),
    priorTxId,
    outputIndex,
    outRefBytes,
    output,
    descriptorCbor,
    proofCborHex: (await trie.prove(outRefBytes)).toCBOR().toString("hex"),
    siblingProofCborHex: (await trie.prove(siblingKeyBytes))
      .toCBOR()
      .toString("hex"),
    siblingKeyBytes,
    siblingDescriptorCbor,
  };
};

// ---------------------------------------------------------------------------
// The challenged transaction
// ---------------------------------------------------------------------------

/**
 * A native transaction whose field 0 (spend inputs) and field 1 (reference
 * inputs) are the supplied out-ref items. Field 1 takes the same 38-byte item
 * form as field 0.
 */
export const buildSubjectTransaction = ({
  spendInputCbors,
  referenceInputCbors = [],
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly referenceInputCbors?: readonly Buffer[];
}): MidgardNativeTxFull => {
  const base = makeNativeTx({
    spendInputCbors: [...spendInputCbors],
    fee: 7n,
    outputCbors: [],
  });
  if (referenceInputCbors.length === 0) return base;
  return materializeMidgardNativeTxFromCanonical({
    version: base.version,
    validity: base.validity,
    body: {
      ...base.body,
      referenceInputsPreimageCbor: encodeCbor([...referenceInputCbors]),
    },
    witnessSet: base.witnessSet,
  });
};

/** `count` copies of the resolved out-ref in the selected field, the rest empty. */
export const subjectTransactionFor = ({
  sourceKind,
  outRefBytes,
  count,
}: {
  readonly sourceKind: 0 | 1;
  readonly outRefBytes: Buffer;
  readonly count: number;
}): MidgardNativeTxFull => {
  const items = Array.from({ length: count }, () => outRefBytes);
  return buildSubjectTransaction(
    sourceKind === 0
      ? { spendInputCbors: items }
      : { spendInputCbors: [], referenceInputCbors: items },
  );
};

export const resolvedOutputReason = (
  coordinate: ResolvedOutputCoordinate,
): RejectionReason => ({
  InputSpentOutputNonCanonical: {
    source_kind: BigInt(coordinate.sourceKind),
    input_index: BigInt(coordinate.inputIndex),
  },
});

// ---------------------------------------------------------------------------
// Harness, registered chain and block commitment
// ---------------------------------------------------------------------------

export type ResolvedOutputContext = Awaited<
  ReturnType<typeof makeResolvedOutputContext>
>;

export const makeResolvedOutputContext = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realResolvedOutputNonCanonical: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.resolvedOutputNonCanonical;
  const category = harness.catalogue.categories.resolvedOutputNonCanonical;
  expectRegisteredChainParity({
    registered,
    applied: applyResolvedOutputNonCanonicalScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  const steps = familyStepsFromRegisteredChain(
    registered.steps,
    RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
  );
  const contracts: ResolvedOutputNonCanonicalContracts = {
    steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  return {
    harness,
    contracts,
    steps,
    catalogue: harness.catalogue,
    category,
  };
};

export type CommittedBlock = Readonly<{
  fraudulentBlockOutRef: string;
  headerHash: string;
  header: Header;
  nativeTx: MidgardNativeTxFull;
  nativeTxId: string;
  canonicalCbor: Buffer;
  /** Compact bytes step 02 anchors on (adjudicated for a forced leaf). */
  compactCborHex: string;
  witnessSetCompactCborHex: string;
  accepted?: {
    readonly txInclusion: Parameters<
      typeof submitResolvedOutputNonCanonicalStep01Accepted
    >[0]["txInclusion"];
    readonly transactionsRoot: string;
  };
  forced?: {
    readonly leaf: ForcedInclusionTxV1;
    readonly membership: RootMembershipProof<
      OutputReference,
      ForcedInclusionTxV1
    >;
    readonly reason: RejectionReason;
  };
}>;

/**
 * Commits a predecessor whose ledger root is the prior ledger, then the
 * challenged successor: an accepted block carrying `nativeTx` under its
 * transactions root, or a forced block whose counted forced-transactions
 * root carries the leaf rejected for `reason`.
 */
export const commitBlock = async ({
  context,
  nativeTx,
  priorRoot,
  reason,
}: {
  readonly context: ResolvedOutputContext;
  readonly nativeTx: MidgardNativeTxFull;
  readonly priorRoot: string;
  /** Present for a forced (rejected) leaf, absent for an accepted transaction. */
  readonly reason?: RejectionReason;
}): Promise<CommittedBlock> => {
  const { harness, catalogue } = context;
  const nativeTxId = computeMidgardNativeTxId(nativeTx).toString("hex");
  const canonicalCbor = encodeMidgardNativeTxCanonical(nativeTx);
  const witnessSetCompactCborHex = encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  ).toString("hex");

  let accepted: CommittedBlock["accepted"];
  let forced: CommittedBlock["forced"];
  let compactCborHex = encodeMidgardNativeTxCompact(nativeTx.compact).toString(
    "hex",
  );
  // The transactions trie carrying the subject. The accepted successor
  // commits it; the predecessor of either shape commits it too, as a valid
  // one-transaction block whose content the proofs never touch.
  const sourceCbor = l2TransactionSourceCbor(nativeTx);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(nativeTxId, "hex"),
    Buffer.from(sourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  if (reason === undefined) {
    accepted = {
      transactionsRoot,
      txInclusion: {
        nativeTxId,
        nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
        nativeTxCompactCbor: compactCborHex,
        l2TransactionSourceCbor: sourceCbor,
        transactionsPhasRoot: transactionsRoot,
        txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
        txMembershipProofCbor: proof.toCBOR().toString("hex"),
      },
    };
  }
  const predecessor = await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue,
    fixture: {
      transactionsRoot,
      l2TransactionCount: 1n,
      utxosRoot: priorRoot,
      headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS,
    },
  });
  const targetStart = emulatorSuccessorHeaderStart({
    predecessorEndTime: predecessor.header.endTime,
    emulator: harness.emulator,
  });
  let header: Header;
  if (reason === undefined) {
    header = {
      ...makeHeader(
        predecessor.header.operatorVkey,
        targetStart,
        await countedTransactionsRoot(transactionsRoot, 1n),
        1n,
      ),
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: priorRoot,
    };
  } else {
    const source = deriveMidgardNativeTxProofSource(
      adjudicateMidgardNativeTxFullValidity(nativeTx, "TxIsInvalid"),
    );
    const leaf: ForcedInclusionTxV1 = {
      tx_id: nativeTxId,
      source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: { ForcedTxInvalid: { reason } },
    };
    compactCborHex = leaf.source.compact_cbor;
    const key = transitionTraceOutRef("f1");
    const keyBytes = Buffer.from(Data.to(key, OutputReference), "hex");
    const valueBytes = Buffer.from(
      Data.to(leaf as never, ForcedInclusionTxV1Schema as never),
      "hex",
    );
    const counted = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
      { key: keyBytes, value: valueBytes },
    ]);
    const forcedStore = new Store(undefined);
    await forcedStore.ready();
    const forcedTrie = new Trie(forcedStore);
    await forcedTrie.insert(keyBytes, valueBytes);
    const membership: RootMembershipProof<
      OutputReference,
      ForcedInclusionTxV1
    > = {
      domain: ROOT_DOMAINS.forcedTransactionsV1,
      root: counted.root,
      phas_root: counted.phasRoot,
      count: counted.count,
      key,
      value: leaf,
      proof: Data.from(
        (await forcedTrie.prove(keyBytes)).toCBOR().toString("hex"),
        Proof,
      ),
    };
    forced = { leaf, membership, reason };
    // The header's other event commitments must carry the one forced event
    // too (`header_transition_commitments_v1_are_valid`): a rejected forced
    // transaction leaves the ledger at the prior root.
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: key } };
    const countedEntries = async (
      domain: Parameters<typeof buildCountedRoot>[0],
      entries: readonly (readonly [string, string])[],
    ) =>
      await buildCountedRoot(
        domain,
        entries.map(([entryKey, entryValue]) => ({
          key: Buffer.from(entryKey, "hex"),
          value: Buffer.from(entryValue, "hex"),
        })),
      );
    const [transitionRoot, eventRoot, validationRoot] = await Promise.all([
      countedEntries(ROOT_DOMAINS.transitionTrace, [
        transitionTraceDaEntry({
          key: 0n,
          keySchema: Data.Integer() as never,
          value: {
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "ForcedTransaction",
            pre_utxos_root: priorRoot,
            post_utxos_root: priorRoot,
          },
          valueSchema: TransitionStepSchema,
        }),
      ]),
      countedEntries(ROOT_DOMAINS.eventToStep, [
        transitionTraceDaEntry({
          key: eventKey,
          keySchema: EventKeySchema,
          value: { step_index: 0n, phase: "ForcedTransaction" },
          valueSchema: EventToStepValueSchema,
        }),
      ]),
      countedEntries(ROOT_DOMAINS.validationTraces, [
        transitionTraceDaEntry({
          key: eventKey,
          keySchema: EventKeySchema,
          value: {
            schema_version: 1n,
            machine_version: 1n,
            trace_root: h32("c1"),
            step_count: 1n,
            initial_state_hash: h32("c2"),
            terminal_state_hash: h32("c3"),
            verdict: "Rejected",
            rejection_code_hash: h32("c4"),
          },
          valueSchema: ValidationTraceDescriptorSchema,
        }),
      ]),
    ]);
    header = {
      ...makeHeader(predecessor.header.operatorVkey, targetStart),
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: priorRoot,
      utxosRoot: priorRoot,
      forcedTransactionsRoot: counted.root,
      transitionTraceRoot: transitionRoot.root,
      eventToStepRoot: eventRoot.root,
      validationTracesRoot: validationRoot.root,
      forcedTransactionCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
      validationTraceCount: 1n,
    };
  }
  const target = await submitSuccessorBlockTx({
    lucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    anchorBlockUnit: predecessor.stateQueueBlockUnit,
    header,
    hubOracle: predecessor.hubOracle,
    scheduler: predecessor.scheduler,
    activeOperatorNode: predecessor.activeOperatorNode,
    activeOperatorNodeUnit: predecessor.activeOperatorNodeUnit,
  });
  return {
    fraudulentBlockOutRef: target.successorOutRef,
    headerHash: target.successorHeaderHash,
    header,
    nativeTx,
    nativeTxId,
    canonicalCbor,
    compactCborHex,
    witnessSetCompactCborHex,
    ...(accepted === undefined ? {} : { accepted }),
    ...(forced === undefined ? {} : { forced }),
  };
};

/**
 * Evidence from the retained prior ledger and the committed transaction. For
 * the honest cases (where `prepare` refuses because the output agrees with
 * the verdict) `claim` builds the evidence under the contradicting subject
 * and then swaps in the honest subject, which is what a lying prover holds.
 */
export const resolvedOutputEvidence = ({
  block,
  prior,
  coordinate,
  claim,
}: {
  readonly block: CommittedBlock;
  readonly prior: PriorLedgerFixture;
  readonly coordinate: ResolvedOutputCoordinate;
  readonly claim?: "lying";
}): ResolvedOutputEvidence => {
  const subject: VerdictSubject =
    block.forced === undefined
      ? acceptedVerdictSubject(block.nativeTxId)
      : forcedVerdictSubject({
          transactionId: block.nativeTxId,
          sourceKey: block.forced.membership.key,
          rejectionReason: block.forced.reason,
        });
  const resolved = {
    priorRoot: prior.priorRoot,
    transactionId: prior.priorTxId,
    outputIndex: prior.outputIndex,
    descriptorCborHex: prior.descriptorCbor.toString("hex"),
    outputCborHex: prior.output.toString("hex"),
    membershipProofCborHex: prior.proofCborHex,
    membershipProof: Data.from(prior.proofCborHex, Proof),
  };
  if (claim === undefined) {
    return prepareResolvedOutputNonCanonicalEvidence({
      subject,
      coordinate,
      canonicalTransactionCbor: block.canonicalCbor,
      resolved,
    });
  }
  const contradicting: VerdictSubject =
    block.forced === undefined
      ? forcedVerdictSubject({
          transactionId: block.nativeTxId,
          sourceKey: transitionTraceOutRef("f1"),
          rejectionReason: resolvedOutputReason(coordinate),
        })
      : acceptedVerdictSubject(block.nativeTxId);
  const prepared = prepareResolvedOutputNonCanonicalEvidence({
    subject: contradicting,
    coordinate,
    canonicalTransactionCbor: block.canonicalCbor,
    resolved,
  });
  return Object.freeze({ ...prepared, subject });
};

export const forcedSourceOf = (
  block: CommittedBlock,
): ResolvedOutputForcedSource => {
  if (block.forced === undefined)
    throw new Error("block carries no forced leaf");
  return {
    header: block.header,
    membership: block.forced.membership,
    direction: 1n,
  };
};

// ---------------------------------------------------------------------------
// Stages
// ---------------------------------------------------------------------------

export type Captured<T> = Awaited<
  ReturnType<typeof captureEmulatorSubmission<T>>
>;

export const makeResolvedOutputStages = async (
  context: ResolvedOutputContext,
  block: CommittedBlock,
  onPublication?: (
    stepIndex: number,
    measurement: CompleteSignedTransactionMeasurement,
  ) => void,
) => {
  const { harness, contracts, steps, catalogue, category } = context;
  const references: UTxO[] = [];
  for (const [index, step] of steps.entries()) {
    const published = await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: step.spendingScript,
      label: `${FAMILY}-${index.toString()}`,
    });
    references.push(published.utxo);
    onPublication?.(index, published.publicationMeasurement);
  }
  const certificateReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: `${FAMILY}-certificate`,
    })
  ).utxo;
  const common = (threadOutRef: string, stepIndex: number) =>
    ({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      referenceScriptUtxo: references[stepIndex]!,
    }) as const;
  const capture = <T>(operation: () => Promise<T>) =>
    captureEmulatorSubmission(harness.emulator, operation);

  const init = async () =>
    await capture(() =>
      submitCommittedFieldShapeInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: contracts as never,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: block.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  const threadOf = (
    initialized: Captured<Awaited<ReturnType<typeof init>>["result"]>,
  ) =>
    `${initialized.result.txHash}#${initialized.result.firstStepOutputIndex.toString()}`;

  const step01Accepted = async (
    initialized: Captured<Awaited<ReturnType<typeof init>>["result"]>,
    evidence: ResolvedOutputEvidence,
    txInclusion = block.accepted?.txInclusion,
  ) => {
    if (txInclusion === undefined) throw new Error("block is not accepted");
    const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
      {
        txHash: initialized.result.txHash,
        outputIndex: initialized.result.firstStepOutputIndex,
      },
    ]);
    if (threadUtxo === undefined) throw new Error("init thread absent");
    return await capture(() =>
      submitResolvedOutputNonCanonicalStep01Accepted({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts,
        signer: harness.proverSigner,
        finding: evidence,
        threadUtxo,
        threadToken: {
          unit: initialized.result.computationThreadUnit,
          fraudulentHeaderHash: initialized.result.fraudulentHeaderHash,
        },
        stateQueueBlockOutRef: block.fraudulentBlockOutRef,
        txInclusion,
        referenceScriptUtxo: references[0]!,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  };

  const step01Forced = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
    forcedSource: ResolvedOutputForcedSource = forcedSourceOf(block),
  ) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalStep01Forced({
        ...common(threadOutRef, 0),
        finding: evidence,
        forcedSource,
      }),
    );

  /**
   * Step 01 over the forced leaf with no off-chain classification: the
   * subject is whatever the leaf and `direction` say, and the claimed
   * coordinate and prior root are handed to the validator verbatim.
   */
  const step01ForcedRaw = async ({
    threadOutRef,
    coordinate,
    priorRoot,
    header = block.header,
    membership = forcedSourceOf(block).membership,
    direction = 1n,
  }: {
    readonly threadOutRef: string;
    readonly coordinate: ResolvedOutputCoordinate;
    readonly priorRoot: string;
    readonly header?: Header;
    readonly membership?: RootMembershipProof<
      OutputReference,
      ForcedInclusionTxV1
    >;
    readonly direction?: bigint;
  }) => {
    const verdict = membership.value.verdict;
    const subject = {
      ...forcedVerdictSubject({
        transactionId: membership.value.tx_id,
        sourceKey: membership.key,
        rejectionReason:
          verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason,
      }),
      direction,
    };
    return await continueRaw({
      threadOutRef,
      stepIndex: 0,
      nextStepIndex: 1,
      nextData: {
        subject,
        source_kind: BigInt(coordinate.sourceKind),
        input_index: BigInt(coordinate.inputIndex),
        prior_root: priorRoot,
      },
      nextDatumSchema: ResolvedOutputStep02DatumSchema,
      redeemerSchema: ResolvedOutputStep01RedeemerSchema,
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
        source_kind: BigInt(coordinate.sourceKind),
        input_index: BigInt(coordinate.inputIndex),
      }),
    });
  };

  const step02 = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
    options: {
      readonly certificateUtxo?: UTxO;
      readonly publishedCarriageUtxos?: readonly UTxO[];
      readonly compactCborHex?: string;
    } = {},
  ) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalStep02({
        ...common(threadOutRef, 1),
        evidence,
        nativeTxCompactCbor: options.compactCborHex ?? block.compactCborHex,
        witnessSetCompactCbor: block.witnessSetCompactCborHex,
        certificateReferenceScriptUtxo: certificateReference,
        ...(options.certificateUtxo === undefined
          ? {}
          : { certificateUtxo: options.certificateUtxo }),
        ...(options.publishedCarriageUtxos === undefined
          ? {}
          : { publishedCarriageUtxos: options.publishedCarriageUtxos }),
      }),
    );

  /**
   * Step 02 with the honest Certified opening of the evidence's field, except
   * that the redeemer's certificate index names `otherCertificate` (a genuine
   * certificate for another field of the same transaction), which is also a
   * reference input. The builder's own certificate lookup is bypassed so the
   * field-opening door refuses the substitution itself. Returns the honest
   * carriage so the honest step 02 can reuse it.
   */
  const step02Raw = async ({
    threadOutRef,
    evidence,
    otherCertificate,
  }: {
    readonly threadOutRef: string;
    readonly evidence: ResolvedOutputEvidence;
    readonly otherCertificate: UTxO;
  }) => {
    const fieldIndex = evidence.coordinate.sourceKind;
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      block.canonicalCbor,
    );
    const planned = planFaultProofFieldOpening({
      fieldIndex,
      anchorTxId: block.nativeTxId,
      nativeTxCompactCbor: block.compactCborHex,
      itemCbors: decodeMidgardFieldPreimage(
        material.fieldPreimages[fieldIndex]!,
      ),
      owner: harness.proverSigner.paymentKeyHash,
      publish: false,
      label: `${FAMILY} raw field opening`,
    });
    expect(planned.plan.tier).toBe("Certified");
    harness.proverSigner.selectWallet(harness.proverLucid);
    const chunkUtxos = await publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label: `${FAMILY} raw field opening`,
    });
    const { certificateUtxo } = await certifyFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      network,
      signer: harness.proverSigner,
      planned,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      certificateMintingScript: contracts.fieldPreimageCertificateMintingScript,
      certificateReferenceScriptUtxo: certificateReference,
      chunkUtxos,
      compactCbor: block.compactCborHex,
      witnessSetCompactCbor: block.witnessSetCompactCborHex,
    });
    const stepReference = references[1]!;
    const referenceInputs = [
      ...chunkUtxos,
      stepReference,
      certificateUtxo,
      otherCertificate,
    ];
    const honest = faultProofFieldOpening({
      planned,
      referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${FAMILY} raw field opening`,
    });
    // The ledger orders reference inputs by `(txHash, outputIndex)`.
    const otherIndex = [...referenceInputs]
      .sort((left, right) =>
        left.txHash < right.txHash
          ? -1
          : left.txHash > right.txHash
            ? 1
            : left.outputIndex - right.outputIndex,
      )
      .findIndex(
        (utxo) =>
          utxo.txHash === otherCertificate.txHash &&
          utxo.outputIndex === otherCertificate.outputIndex,
      );
    expect(otherIndex).toBeGreaterThanOrEqual(0);
    if (!("BodyFieldOpening" in honest))
      throw new Error("input fields open through BodyFieldOpening");
    const carriage = honest.BodyFieldOpening.carriage;
    if (!("Certified" in carriage))
      throw new Error("raw field opening expected Certified carriage");
    const opening: FieldOpening = {
      BodyFieldOpening: {
        ...honest.BodyFieldOpening,
        carriage: {
          Certified: {
            ...carriage.Certified,
            cert_ref_input_index: BigInt(otherIndex),
          },
        },
      },
    };
    const submitted = await continueRaw({
      threadOutRef,
      stepIndex: 1,
      nextStepIndex: 2,
      nextData: {
        subject: evidence.subject,
        prior_root: evidence.resolved.priorRoot,
        out_ref: {
          transactionId: evidence.resolved.transactionId,
          outputIndex: BigInt(evidence.resolved.outputIndex),
        },
      },
      nextDatumSchema: ResolvedOutputStep03DatumSchema,
      redeemerSchema: ResolvedOutputStep02RedeemerSchema,
      args: (input_index, output_index) => ({
        input_index,
        output_index,
        opening,
      }),
      carriageUtxos: chunkUtxos,
      extraReferenceInputs: [certificateUtxo, otherCertificate],
    });
    return { ...submitted, chunkUtxos, certificateUtxo };
  };

  const step03 = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
  ) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalStep03({
        ...common(threadOutRef, 2),
        network,
        evidence,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );

  const step04 = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
  ) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalStep04({
        ...common(threadOutRef, 3),
        evidence,
      }),
    );

  /** Drives the self-loop from `threadOutRef` until the thread leaves step 04. */
  const reconstruct = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
    onTransition?: (
      captured: Captured<Awaited<ReturnType<typeof step04>>["result"]>,
      index: number,
    ) => void,
  ) => {
    let current = threadOutRef;
    let transitions = 0;
    for (;;) {
      const captured = await step04(current, evidence);
      onTransition?.(captured, transitions);
      transitions += 1;
      current = captured.result.nextThreadOutRef;
      if (captured.result.terminal) {
        return { threadOutRef: current, transitions, final: captured.result };
      }
    }
  };

  /** Step 04 with the action and successor state handed to the validator verbatim. */
  const step04Raw = async ({
    threadOutRef,
    evidence,
    action,
    nextData,
    nextStepIndex,
  }: {
    readonly threadOutRef: string;
    readonly evidence: ResolvedOutputEvidence;
    readonly action: unknown;
    /** Successor `ReconstructionV1` or `CanonicalVerdictV1` data. */
    readonly nextData: Record<string, unknown>;
    readonly nextStepIndex: 3 | 4;
  }) =>
    await continueRaw({
      threadOutRef,
      stepIndex: 3,
      nextStepIndex,
      nextData: { subject: evidence.subject, ...nextData },
      nextDatumSchema:
        nextStepIndex === 4
          ? ResolvedOutputStep05DatumSchema
          : ResolvedOutputStep04DatumSchema,
      redeemerSchema: ResolvedOutputStep04RedeemerSchema,
      args: (input_index, output_index) => ({
        input_index,
        output_index,
        action,
      }),
    });

  const step05 = async (
    threadOutRef: string,
    evidence: ResolvedOutputEvidence,
  ) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalStep05({
        ...common(threadOutRef, 4),
        evidence,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );

  /** Step 05 without the builder's off-chain contradiction check. */
  const step05Raw = async (threadOutRef: string) => {
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      family: FAMILY,
      stepIndex: 4,
      threadOutRef,
    });
    return await submitLinearFaultFinalize({
      lucid: harness.proverLucid,
      family: FAMILY,
      stepIndex: 4,
      step: contracts.steps[4],
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer: harness.proverSigner,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: ResolvedOutputStep05RedeemerSchema,
      buildFamilyArgs: (layout) => ({
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
      }),
      referenceScriptUtxo: references[4]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  };

  const continueRaw = async ({
    threadOutRef,
    stepIndex,
    nextStepIndex,
    nextData,
    nextDatumSchema,
    redeemerSchema,
    args,
    carriageUtxos = [],
    extraReferenceInputs = [],
  }: {
    readonly threadOutRef: string;
    readonly stepIndex: number;
    readonly nextStepIndex: number;
    readonly nextData: unknown;
    readonly nextDatumSchema: unknown;
    readonly redeemerSchema: unknown;
    readonly args: (
      inputIndex: bigint,
      outputIndex: bigint,
    ) => Record<string, unknown>;
    readonly carriageUtxos?: readonly UTxO[];
    readonly extraReferenceInputs?: readonly UTxO[];
  }) => {
    const lucid = harness.proverLucid;
    const signer = harness.proverSigner;
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      lucid,
      contracts,
      categoryId: category.categoryId,
      family: FAMILY,
      stepIndex,
      threadOutRef,
    });
    const role = `${FAMILY} raw step-0${(stepIndex + 1).toString()}`;
    const nextDatum = Data.to(
      { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
      nextDatumSchema as never,
    );
    const nextAddress = contracts.steps[nextStepIndex]!.spendingScriptAddress;
    const outputMatches = computationThreadOutputPredicate({
      address: nextAddress,
      datum: nextDatum,
      unit: threadToken.unit,
    });
    let outputIndex: bigint | undefined;
    const redeemer = ((ctx) => {
      requireOwnSpendPurpose(ctx, threadUtxo, role);
      const inputIndex = requireInputIndex(ctx, threadUtxo, role);
      outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, role);
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
      stepReference: requireLinearFaultReferenceScript({
        utxo: references[stepIndex]!,
        expectedScriptHash: contracts.steps[stepIndex]!.spendingScriptHash,
        family: FAMILY,
        stepIndex,
      }),
      stepScript: contracts.steps[stepIndex]!.spendingScript,
      stepRole: role,
      nextAddress,
      nextDatum,
      redeemer,
      carriageUtxos,
      extraReferenceInputs,
      awaitConfirmation: true,
    });
    if (outputIndex === undefined)
      throw new Error(`${role}: layout unresolved`);
    return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
  };

  const cancel = async (threadOutRef: string, stepIndex: number) =>
    await capture(() =>
      submitResolvedOutputNonCanonicalCancel({
        ...common(threadOutRef, stepIndex),
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );

  const remove = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferences.published },
    );
    const now = BigInt(harness.emulator.now());
    const removal = await capture(() =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudCategory: "resolvedOutputNonCanonical",
        fraudulentHeaderHash: block.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      }),
    );
    expect(removal.result.fraudCategoryId).toBe("00000026");
    return removal;
  };

  /**
   * A field-preimage certificate for another field of the same transaction,
   * minted the way step 02 mints its own, so a certificate seam substitution
   * reaches the validator with a genuine certificate for the wrong field.
   */
  const certifyField = async (fieldIndex: 0 | 1) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      block.canonicalCbor,
    );
    const planned = planFaultProofFieldOpening({
      fieldIndex,
      anchorTxId: block.nativeTxId,
      nativeTxCompactCbor: block.compactCborHex,
      itemCbors: decodeMidgardFieldPreimage(
        material.fieldPreimages[fieldIndex]!,
      ),
      owner: harness.proverSigner.paymentKeyHash,
      publish: false,
      label: `${FAMILY} other-field opening`,
    });
    expect(planned.plan.tier).toBe("Certified");
    harness.proverSigner.selectWallet(harness.proverLucid);
    const chunkUtxos = await publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label: `${FAMILY} other-field opening`,
    });
    return (
      await certifyFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        planned,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        certificateMintingScript:
          contracts.fieldPreimageCertificateMintingScript,
        certificateReferenceScriptUtxo: certificateReference,
        chunkUtxos,
        compactCbor: block.compactCborHex,
        witnessSetCompactCbor: block.witnessSetCompactCborHex,
      })
    ).certificateUtxo;
  };

  /** The live step-04 checkpoint a fresh process would resume from. */
  const readReconstruction = async (threadOutRef: string) => {
    const [txHash, index] = threadOutRef.split("#");
    const [utxo] = await harness.proverLucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(index) },
    ]);
    if (utxo?.datum == null) throw new Error("step-04 checkpoint absent");
    return (
      Data.from(utxo.datum, ResolvedOutputStep04DatumSchema as never) as {
        data: Data.Static<typeof ResolvedOutputStep04DatumSchema>["data"];
      }
    ).data!;
  };

  return {
    references,
    certificateReference,
    init,
    threadOf,
    step01Accepted,
    step01Forced,
    step01ForcedRaw,
    step02,
    step02Raw,
    step03,
    step04,
    step04Raw,
    reconstruct,
    step05,
    step05Raw,
    cancel,
    remove,
    certifyField,
    readReconstruction,
  };
};
