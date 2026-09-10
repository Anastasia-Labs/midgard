import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeScript,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, getAddressDetails } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  admitNativeScriptInvalidForcedArtifact,
  NATIVE_SCRIPT_INVALID_FORCED_ARTIFACT,
} from "../src/native-script-invalid/forced-artifact.js";
import { submitNativeScriptInvalidCancel } from "../src/native-script-invalid/submit-cancel.js";
import { submitNativeScriptInvalidInit } from "../src/native-script-invalid/submit-init.js";
import { submitNativeScriptInvalidStep01Forced } from "../src/native-script-invalid/submit-step-01-forced.js";
import { submitNativeScriptInvalidStep02 } from "../src/native-script-invalid/submit-step-02.js";
import { submitNativeScriptInvalidStep03 } from "../src/native-script-invalid/submit-step-03.js";
import { submitNativeScriptInvalidStep03StartSignerScan } from "../src/native-script-invalid/submit-step-03-staged.js";
import { submitNativeScriptInvalidStep04 } from "../src/native-script-invalid/submit-step-04.js";
import { submitNativeScriptInvalidStep05 } from "../src/native-script-invalid/submit-step-05.js";
import {
  admitNativeScriptInvalidWorkflowArtifact,
  prepareNativeScriptInvalidWorkflowArtifact,
} from "../src/native-script-invalid/workflow-artifact.js";
import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "../src/transition-trace/phas.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import { NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  makeNativeScriptInvalidEmulatorHarness,
  publishFinalFamilyReferenceScripts,
} from "./support/final-catalogue-emulator.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

type FitMeasurement = ReturnType<
  typeof import("./support/emulator/measurement.js").measureCompleteSignedTransaction
>;
const fitRows: {
  shape: string;
  stages: readonly FitMeasurement[];
  scriptIndex?: bigint;
  scriptFieldBytes?: number;
  signerFieldBytes?: number;
}[] = [];
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(fitRows).toHaveLength(8);
  const blueprint = await readFile(realBlueprintPath);
  const ledger = buildVanRossemFitLedger({
    category: "nativeScriptInvalid",
    blueprintSha256: createHash("sha256").update(blueprint).digest("hex"),
    compilerVersion: JSON.parse(blueprint.toString("utf8")).preamble.compiler
      .version,
    measurements: fitRows.flatMap(({ shape, stages }) =>
      stages.map((row, index) => ({
        name: `${shape}/${index.toString().padStart(3, "0")}`,
        kind:
          row.executionMemory === 0n && row.executionSteps === 0n
            ? ("publication" as const)
            : ("lifecycle" as const),
        maximumShape: shape,
        signedBytes: row.completeSignedBytes,
        memoryUnits: row.executionMemory,
        cpuUnits: row.executionSteps,
      })),
    ),
  });
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/native-script-invalid-wrongful-rejection-v1-fit-ledger.json",
        import.meta.url,
      ),
    ),
    ledger,
  );
});

const setup = async ({
  signerCount = 1,
  falseScript = false,
  invalidSignature = false,
  depth = 0,
  maximumField = false,
  fieldBytes = 32768,
  prefixCount = 0,
  deepScript = false,
  wrongReason = false,
}: {
  signerCount?: number;
  falseScript?: boolean;
  invalidSignature?: boolean;
  depth?: number;
  maximumField?: boolean;
  fieldBytes?: number;
  prefixCount?: number;
  deepScript?: boolean;
  wrongReason?: boolean;
} = {}) => {
  const h = await makeNativeScriptInvalidEmulatorHarness();
  const keys = Array.from({ length: signerCount }, (_, index) => {
    const seed = Buffer.alloc(32);
    seed.writeUInt32BE(index + 1, 28);
    return CML.PrivateKey.from_normal_bytes(seed);
  });
  // The 32-node domain bound makes every applicable native script <1024 bytes.
  let nativeScript: Parameters<typeof encodeMidgardNativeScript>[0] = {
    type: "all" as const,
    scripts: Array.from({ length: 31 }, () => ({
      type: "sig" as const,
      keyHash: Buffer.from(
        falseScript ? "ee".repeat(28) : keys[0]!.to_public().hash().to_hex(),
        "hex",
      ),
    })),
  };
  if (deepScript) {
    nativeScript = {
      type: "all",
      scripts:
        nativeScript.type === "all" ? nativeScript.scripts.slice(0, 17) : [],
    };
    for (let i = 0; i < 14; i++)
      nativeScript = { type: "all", scripts: [nativeScript] };
  }
  const scriptItem = encodeMidgardVersionedScript({
    language: "NativeCardano",
    nativeScript,
    scriptBytes: encodeMidgardNativeScript(nativeScript),
  });
  const emptyNative = { type: "all" as const, scripts: [] };
  const emptyItem = encodeMidgardVersionedScript({
    language: "NativeCardano",
    nativeScript: emptyNative,
    scriptBytes: encodeMidgardNativeScript(emptyNative),
  });
  const prefix = Array.from({ length: prefixCount }, () => emptyItem);
  let scripts = [...prefix, scriptItem];
  if (maximumField) {
    const decoy = (size: number) =>
      encodeMidgardVersionedScript({
        language: "PlutusV3",
        scriptBytes: Buffer.alloc(size, 0),
      });
    let padding = 32000;
    while (
      encodeMidgardFieldPreimage([decoy(padding), ...prefix, scriptItem])
        .length > fieldBytes
    )
      padding--;
    while (
      encodeMidgardFieldPreimage([decoy(padding + 1), ...prefix, scriptItem])
        .length <= fieldBytes
    )
      padding++;
    scripts = [decoy(padding), ...prefix, scriptItem];
  }
  const base = makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    scriptTxWitsPreimageCbor: encodeMidgardFieldPreimage(scripts),
    addrTxWitsPreimageCbor: encodeMidgardFieldPreimage([]),
    validityIntervalStart: 0n,
    validityIntervalEnd: 100n,
  });
  const id = computeMidgardNativeTxId(base);
  const witnesses = keys
    .map((key, index) => ({
      key,
      hash: key.to_public().hash().to_hex(),
      item: Buffer.concat([
        Buffer.from("825820", "hex"),
        key.to_public().to_raw_bytes(),
        Buffer.from("5840", "hex"),
        invalidSignature && index === 0
          ? Buffer.alloc(64)
          : key.sign(id).to_raw_bytes(),
      ]),
    }))
    .sort((a, b) => a.hash.localeCompare(b.hash));
  const submitted = materializeMidgardNativeTxFromCanonical({
    ...base,
    witnessSet: {
      ...base.witnessSet,
      addrTxWitsPreimageCbor: encodeMidgardFieldPreimage(
        witnesses.map((value) => value.item),
      ),
    },
  });
  const tx = adjudicateMidgardNativeTxFullValidity(submitted, "TxIsInvalid");
  const txId = computeMidgardNativeTxId(tx).toString("hex");
  const proofSource = deriveMidgardNativeTxProofSource(tx);
  const credential = getAddressDetails(
    await h.funderLucid.wallet().address(),
  ).paymentCredential!;
  const fixture = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + 120000,
      ) - 1,
  });
  const key = fixture.eventKey.ForcedTransactionEventKey.tx_order_id;
  const scriptIndex = BigInt(scripts.length - 1);
  const reason = wrongReason
    ? ("FeeBelowMinimum" as const)
    : { WitnessNativeScriptFalse: { script_index: scriptIndex } };
  const value = {
    tx_id: txId,
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason } },
  };
  const keyBytes = Buffer.from(Data.to(key, SDK.OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(value as never, SDK.ForcedInclusionTxV1Schema as never),
    "hex",
  );
  let root = await buildCountedRoot(SDK.ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(keyBytes, valueBytes);
  const deep =
    depth > 0
      ? syntheticDeepMembershipProof({
          key: keyBytes,
          value: valueBytes,
          branchLevels: depth,
        })
      : undefined;
  if (deep)
    root = {
      ...root,
      phasRoot: deep.transactionsPhasRoot,
      root: await commitCountedRoot({
        domain: root.domain,
        phasRoot: deep.transactionsPhasRoot,
        count: 1n,
      }),
    };
  const membership = {
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    key,
    value,
    proof: Data.from(
      deep?.proofCbor ?? (await trie.prove(keyBytes)).toCBOR().toString("hex"),
      SDK.Proof,
    ),
  };
  const header = {
    ...fixture.header,
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: root.count,
  };
  const seeded = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header,
  });
  const source = { header, membership, direction: 1n };
  const state = {
    subject: SDK.forcedVerdictSubject({
      transactionId: txId,
      sourceKey: key,
      rejectionReason: reason,
    }),
    bad_tx_id: txId,
    bad_tx_witness_set_hash:
      tx.compact.transactionWitnessSetHash.toString("hex"),
    validity_interval_start: 0n,
    validity_interval_end: 100n,
    grammar_checkpoint_hash: "",
    grammar_complete: false,
    script_checkpoint_hash: "",
  };
  const scriptPublications: ReturnType<
    typeof import("./support/emulator/measurement.js").measureCompleteSignedTransaction
  >[] = [];
  const refs = await publishFinalFamilyReferenceScripts({
    lucid: h.proverLucid,
    family: h.family,
    label: "native-script-invalid",
    onPublication: (_index, publication) =>
      scriptPublications.push(publication.publicationMeasurement),
  });
  const witness = deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet);
  const witnessSet = {
    addr_tx_wits_hash: witness.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: witness.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: witness.redeemerTxWitsHash.toString("hex"),
  };
  const common = {
    lucid: h.proverLucid,
    contracts: h.family,
    categoryId: h.category.categoryId,
    signer: h.proverSigner,
    nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
    witnessSet,
  };
  const artifact = {
    schemaVersion: NATIVE_SCRIPT_INVALID_FORCED_ARTIFACT,
    headerHash: seeded.headerHash,
    forcedIndex: 0,
    detectionId: `native-script-invalid:forced:0:${txId}:${scriptIndex}`,
    forcedSourceCbor: Data.to(
      source as never,
      SDK.NativeScriptInvalidForcedSourcePayloadSchema as never,
    ),
    fullTransactionCbor:
      encodeMidgardNativeTxCanonical(submitted).toString("hex"),
  };
  const init = () =>
    submitNativeScriptInvalidInit({
      lucid: h.proverLucid,
      blueprint: h.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
      network,
      signer: h.proverSigner,
      fraudulentBlockOutRef: seeded.fraudulentBlockOutRef,
      witnessReferenceScripts: h.witnessReferenceScripts,
    });
  const bind = (threadOutRef: string, overrides = {}) =>
    submitNativeScriptInvalidStep01Forced({
      ...common,
      threadOutRef,
      state,
      forcedSource: source,
      ...overrides,
      referenceScriptUtxo: refs[0]!,
    });
  const prepareArtifact = async () => {
    const entry = {
      key,
      value,
      keyBytes,
      valueBytes,
      fullTransactionCbor: encodeMidgardNativeTxCanonical(submitted),
    };
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: key } };
    const fingerprint = eventKeyFingerprint(eventKey);
    const block = {
      headerHash: seeded.headerHash,
      header,
      transactions: [],
      reconstruction: {
        forcedTransactions: [entry],
        rootData: { forcedTransactions: root },
        sourceEventsByFingerprint: new Map([
          [
            fingerprint,
            { phase: "ForcedTransaction", eventKey, fingerprint, entry },
          ],
        ]),
      },
    } as unknown as CanonicalBlockEvidence;
    const replay =
      await NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY.replay(block);
    const selected = replay.detections.find(
      (detection) => detection.detectionId === artifact.detectionId,
    );
    if (selected === undefined)
      throw new Error(
        "complete replay missed the forced native script contradiction",
      );
    return prepareNativeScriptInvalidWorkflowArtifact({
      evidence: block,
      classification: {
        schemaVersion: "midgard-fraud-proof-classification-v1",
        decision: "fault_detected",
        headerHash: block.headerHash,
        category: "nativeScriptInvalid",
        selected,
        detections: replay.detections,
        unprovableGaps: [],
      },
    });
  };
  return {
    h,
    refs,
    common,
    artifact,
    prepareArtifact,
    init,
    bind,
    seeded,
    state,
    source,
    scriptItem,
    scripts,
    witnesses,
    proofSource,
    scriptPublications,
    scriptIndex: scriptIndex,
  };
};

const run = async (
  f: Awaited<ReturnType<typeof setup>>,
  unsafeSkipLocalViolationCheckForTest = false,
  cancelAt?: "grammar" | "signer",
): Promise<void> => {
  const initial = await f.init();
  const reopened = unsafeSkipLocalViolationCheckForTest
    ? undefined
    : await admitNativeScriptInvalidForcedArtifact(
        JSON.parse(JSON.stringify(f.artifact)),
      );
  const bound = await f.bind(
    `${initial.txHash}#${initial.firstStepOutputIndex}`,
    reopened
      ? { state: reopened.evidence.state, forcedSource: reopened.forcedSource }
      : {},
  );
  const cancel = async (threadOutRef: string, index: number) => {
    await submitNativeScriptInvalidCancel({
      ...f.common,
      threadOutRef,
      referenceScriptUtxo: f.refs[index]!,
      witnessReferenceScripts: f.h.witnessReferenceScripts,
    });
    expect(
      await f.h.proverLucid.utxosAt(
        f.h.family.fraudProof.spendingScriptAddress,
      ),
    ).toHaveLength(0);
  };
  let threadOutRef = bound.nextThreadOutRef;
  const prepareField = async (
    fieldIndex: number,
    items: readonly Uint8Array[],
  ) => {
    const planned = planFaultProofFieldOpening({
      fieldIndex,
      anchorTxId: f.state.bad_tx_id,
      nativeTxCompactCbor: f.common.nativeTxCompactCbor,
      itemCbors: items,
      owner: f.h.proverSigner.paymentKeyHash,
      publish: true,
      witnessSet: f.common.witnessSet,
      anchorWitnessSetHash: f.state.bad_tx_witness_set_hash,
      label: "native-script maximum field",
    });
    const chunks = await publishFaultProofFieldCarriage({
      lucid: f.h.proverLucid,
      signer: f.h.proverSigner,
      planned,
      publisherAddress: f.h.proverSigner.address,
      label: "native-script field",
    });
    if (planned.plan.tier !== "Certified")
      return { publishedCarriageUtxos: chunks };
    const ref = await publishPlainReferenceScriptUtxo({
      lucid: f.h.funderLucid,
      script: f.h.contracts.fieldPreimageCertificate.mintingScript,
      label: "native-script field certificate",
    });
    const certified = await certifyFaultProofFieldCarriage({
      lucid: f.h.proverLucid,
      network,
      signer: f.h.proverSigner,
      planned,
      certificatePolicyId: f.h.contracts.fieldPreimageCertificate.policyId,
      certificateMintingScript:
        f.h.contracts.fieldPreimageCertificate.mintingScript,
      certificateReferenceScriptUtxo: ref.utxo,
      chunkUtxos: chunks,
      compactCbor: f.common.nativeTxCompactCbor,
      witnessSetCompactCbor:
        f.proofSource.witnessSetCompactCbor.toString("hex"),
    });
    return {
      publishedCarriageUtxos: chunks,
      certificateUtxo: certified.certificateUtxo,
    };
  };
  const scriptCarriage = await prepareField(6, f.scripts);
  const signerCarriage = await prepareField(
    7,
    f.witnesses.map((w) => w.item),
  );
  for (let i = 0; i < 1000; i++) {
    const next = await submitNativeScriptInvalidStep02({
      ...f.common,
      ...scriptCarriage,
      threadOutRef,
      scriptWitnessItems: f.scripts,
      scriptIndex: f.scriptIndex,
      referenceScriptUtxo: f.refs[1]!,
    });
    threadOutRef = next.nextThreadOutRef;
    if (cancelAt === "grammar" && next.nextStepIndex === 1) {
      await cancel(threadOutRef, 1);
      await run(f);
      return;
    }
    if (next.nextStepIndex === 2) break;
  }
  let proof;
  if (f.witnesses.length <= 28) {
    proof = await submitNativeScriptInvalidStep03({
      ...f.common,
      ...signerCarriage,
      unsafeSkipLocalViolationCheckForTest,
      threadOutRef,
      scriptItemCbor: f.scriptItem,
      addressWitnessItems: f.witnesses.map((w) => w.item),
      addressWitnessVerificationKeys: f.witnesses.map((w) =>
        w.key.to_public().to_raw_bytes(),
      ),
      referenceScriptUtxo: f.refs[2]!,
      witnessReferenceScripts: f.h.witnessReferenceScripts,
    });
  } else {
    const started = await submitNativeScriptInvalidStep03StartSignerScan({
      ...f.common,
      ...signerCarriage,
      threadOutRef,
      scriptItemCbor: f.scriptItem,
      addressWitnessItems: f.witnesses.map((w) => w.item),
      referenceScriptUtxo: f.refs[2]!,
    });
    threadOutRef = started.nextThreadOutRef;
    if (cancelAt === "signer") {
      await cancel(threadOutRef, 3);
      await run(f);
      return;
    }
    for (let i = 0; i < 1000; i++) {
      const advanced = await submitNativeScriptInvalidStep04({
        ...f.common,
        ...signerCarriage,
        threadOutRef,
        addressWitnessItems: f.witnesses.map((w) => w.item),
        referenceScriptUtxo: f.refs[3]!,
      });
      threadOutRef = advanced.nextThreadOutRef;
      if (advanced.action === "FinalizeSignerScan") break;
    }
    for (let i = 0; i < 1000; i++) {
      const advanced = await submitNativeScriptInvalidStep05({
        ...f.common,
        unsafeSkipLocalViolationCheckForTest,
        threadOutRef,
        scriptItemCbor: f.scriptItem,
        addressWitnessItems: f.witnesses.map((w) => w.item),
        referenceScriptUtxo: f.refs[4]!,
        witnessReferenceScripts: f.h.witnessReferenceScripts,
      });
      if ("fraudProofUnit" in advanced) {
        proof = advanced;
        break;
      }
      threadOutRef = advanced.nextThreadOutRef;
    }
    if (proof === undefined)
      throw new Error("script lifecycle did not finalize");
  }
  const removal = await publishRemovalReferenceScripts({
    lucid: f.h.proverLucid,
    contracts: f.h.contracts,
  });
  const now = BigInt(f.h.emulator.now());
  await submitRemoveFraudulentBlock({
    lucid: f.h.proverLucid,
    blueprint: f.h.realBlueprint,
    deploymentInfo: buildRemovalDeploymentInfo(f.h.contracts, f.h.catalogue, {
      removalReferenceScripts: removal.published,
    }),
    network,
    signer: f.h.proverSigner,
    fraudCategory: "nativeScriptInvalid",
    fraudulentHeaderHash: f.seeded.headerHash,
    requireReferenceScripts: true,
    validFrom: now > 120000n ? now - 120000n : 0n,
    validTo: now + 300000n,
  });
  expect(
    await f.h.proverLucid.utxosAtWithUnit(
      f.h.family.fraudProof.spendingScriptAddress,
      proof.fraudProofUnit,
    ),
  ).toHaveLength(1);
};

describe("native script wrongful forced rejection", () => {
  it.each([
    { shape: "direct-28-signers", signerCount: 28 },
    { shape: "staged-29-signers", signerCount: 29 },
    {
      shape: "last-raw-field",
      signerCount: 147,
      maximumField: true,
      fieldBytes: 15148,
      prefixCount: 64,
    },
    {
      shape: "first-certified-field",
      signerCount: 148,
      maximumField: true,
      fieldBytes: 15149,
      prefixCount: 64,
    },
    {
      shape: "maximum-fields-and-64-branch-source",
      signerCount: 318,
      maximumField: true,
      depth: 64,
      prefixCount: 64,
    },
    { shape: "maximum-native-depth", signerCount: 318, deepScript: true },
  ])(
    "measures $shape through permanent mint and removal",
    async (options) => {
      const f = await setup(options);
      const measured = await captureEmulatorSubmission(f.h.emulator, () =>
        run(f),
      );
      const stages = [...f.scriptPublications, ...measured.measurements];
      for (const row of stages) {
        expect(row.completeSignedBytes).toBeLessThanOrEqual(15872);
        expect(row.executionMemory).toBeLessThanOrEqual(13200000n);
        expect(row.executionSteps).toBeLessThanOrEqual(8000000000n);
      }
      fitRows.push({
        shape: options.shape,
        scriptIndex: f.scriptIndex,
        scriptFieldBytes: encodeMidgardFieldPreimage(f.scripts).length,
        signerFieldBytes: encodeMidgardFieldPreimage(
          f.witnesses.map((w) => w.item),
        ).length,
        stages,
      });
    },
    600000,
  );
  it("reopens durable evidence and convicts a true script through block removal", async () => {
    const f = await setup();
    const artifact = await f.prepareArtifact();
    const reopened = await admitNativeScriptInvalidForcedArtifact(
      JSON.parse(JSON.stringify(artifact)),
    );
    expect(reopened.evidence.state).toEqual(f.state);
    const workflow = await admitNativeScriptInvalidWorkflowArtifact(
      JSON.parse(JSON.stringify(artifact)),
    );
    expect(workflow.forced?.evidence.state).toEqual(f.state);
    expect(workflow.prepared.txInclusion).toBeUndefined();
    expect(workflow.witnessSetHash).toBe(f.state.bad_tx_witness_set_hash);
    await run(f);
  });
  it.each(["grammar", "signer"] as const)(
    "cancels at %s and restarts from admitted durable evidence",
    async (phase) => {
      const f = await setup({
        signerCount: 318,
        maximumField: true,
        prefixCount: 64,
      });
      const measured = await captureEmulatorSubmission(f.h.emulator, () =>
        run(f, false, phase),
      );
      const stages = [...f.scriptPublications, ...measured.measurements];
      for (const row of stages) {
        expect(row.completeSignedBytes).toBeLessThanOrEqual(15872);
        expect(row.executionMemory).toBeLessThanOrEqual(13200000n);
        expect(row.executionSteps).toBeLessThanOrEqual(8000000000n);
      }
      fitRows.push({ shape: `cancel-and-restart-${phase}`, stages });
    },
    600000,
  );
  it("refuses another exact authenticated rejection reason on chain", async () => {
    const f = await setup({ wrongReason: true });
    const initial = await f.init();
    await expect(
      f.bind(`${initial.txHash}#${initial.firstStepOutputIndex}`),
    ).rejects.toThrow();
  });
  it("rejects substituted source/header and another native script index on chain", async () => {
    const f = await setup({ prefixCount: 1 });
    const initial = await f.init();
    const outRef = `${initial.txHash}#${initial.firstStepOutputIndex}`;
    await expect(
      f.bind(outRef, {
        forcedSource: {
          ...f.source,
          header: {
            ...f.source.header,
            blockSlot: f.source.header.blockSlot + 1n,
          },
        },
      }),
    ).rejects.toThrow();
    await expect(
      f.bind(outRef, { forcedSource: { ...f.source, direction: 0n } }),
    ).rejects.toThrow();
    const bound = await f.bind(outRef);
    await expect(
      submitNativeScriptInvalidStep02({
        ...f.common,
        threadOutRef: bound.nextThreadOutRef,
        scriptWitnessItems: f.scripts,
        scriptIndex: 0n,
        referenceScriptUtxo: f.refs[1]!,
      }),
    ).rejects.toThrow();
  });
  it("rejects durable reason, index, header, source bytes and identity substitutions", async () => {
    const f = await setup({ prefixCount: 1 });
    for (const artifact of [
      { ...f.artifact, forcedSourceCbor: f.artifact.forcedSourceCbor + "00" },
      { ...f.artifact, headerHash: "11".repeat(28) },
      { ...f.artifact, detectionId: f.artifact.detectionId + ":changed" },
      {
        ...f.artifact,
        fullTransactionCbor: "ff" + f.artifact.fullTransactionCbor.slice(2),
      },
      {
        ...f.artifact,
        forcedSourceCbor: Data.to(
          {
            ...f.source,
            membership: {
              ...f.source.membership,
              value: {
                ...f.source.membership.value,
                verdict: { ForcedTxInvalid: { reason: "FeeBelowMinimum" } },
              },
            },
          } as never,
          SDK.NativeScriptInvalidForcedSourcePayloadSchema as never,
        ),
      },
    ])
      await expect(
        admitNativeScriptInvalidForcedArtifact(artifact),
      ).rejects.toThrow();
  });
  it.each([1, 29])(
    "refuses an honest false native script with %s signers on chain",
    async (signerCount) => {
      const f = await setup({ falseScript: true, signerCount });
      await expect(
        admitNativeScriptInvalidForcedArtifact(f.artifact),
      ).rejects.toThrow("no contradiction");
      await expect(run(f, true)).rejects.toThrow(
        /failed|Validation|Script|script/i,
      );
    },
    60_000,
  );
  it.each([1, 29])(
    "refuses a forged signer signature with %s signers on chain",
    async (signerCount) => {
      const f = await setup({ invalidSignature: true, signerCount });
      await expect(
        admitNativeScriptInvalidForcedArtifact(f.artifact),
      ).rejects.toThrow("no contradiction");
      await expect(run(f, true)).rejects.toThrow(
        /failed|Validation|Script|script/i,
      );
    },
    60_000,
  );
});
