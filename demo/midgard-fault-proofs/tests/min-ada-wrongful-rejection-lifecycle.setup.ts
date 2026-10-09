import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
} from "@al-ft/midgard-validation";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { MIN_ADA_FORCED_ARTIFACT } from "../src/min-ada/forced-artifact.js";
import { submitMinAdaInit } from "../src/min-ada/submit-init.js";
import { prepareMinAdaWorkflowArtifact } from "../src/min-ada/workflow-artifact.js";
import {
  buildCountedRoot,
  commitCountedRoot,
} from "../src/transition-trace/phas.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import { MIN_ADA_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { rows } from "./min-ada-wrongful-rejection-lifecycle.rows.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  makeMinAdaEmulatorHarness,
  publishFinalFamilyReferenceScripts,
} from "./support/final-catalogue-emulator.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  network,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

export let scenarioSequence = 0;

/**
 * `transactionOf` replaces the filler transaction built around the outputs,
 * and `namedOutputIndex` names an output other than the last one. A suite
 * that commits the verdict the node writes needs both: the filler spends no
 * input, which the node rejects before reading any output.
 */
export const setup = async ({
  transactionOf = (outputs: readonly Buffer[]): MidgardNativeTxFull =>
    makeNativeTx({ spendInputCbors: [], fee: 7n, outputCbors: [...outputs] }),
  namedOutputIndex = undefined as number | undefined,
  underfunded = false,
  depth = 0,
  wrongReason = false,
  outputBytes = 0,
  fieldBytes = 0,
  above = 0n,
  cancelAt = "",
  assetCount = 0,
  prefixCount = 0,
} = {}) => {
  const scenarioId = scenarioSequence++;
  const h = await makeMinAdaEmulatorHarness();
  const assets = new Map<string, Map<string, bigint>>();
  if (assetCount > 0) {
    const names = new Map<string, bigint>();
    for (let i = 0; i < assetCount; i++) {
      const name =
        i === 0
          ? Buffer.alloc(0)
          : i <= 256
            ? Buffer.from([i - 1])
            : Buffer.from([(i - 257) >> 8, (i - 257) & 255]);
      names.set(name.toString("hex"), i === assetCount - 1 ? 256n : 1n);
    }
    assets.set("44".repeat(28), names);
  }
  let padding = 0;
  const output = (lovelace: bigint) =>
    encodeMidgardTxOutput({
      ...(outputBytes === 0
        ? {}
        : {
            script_ref: {
              language: "PlutusV3" as const,
              scriptBytes: Buffer.alloc(padding),
            },
          }),
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
      value: { lovelace, assets },
    });
  if (outputBytes > 0) {
    while (output(2_000_000n).length < outputBytes) padding++;
    expect(output(2_000_000n).length).toBe(outputBytes);
  }
  let floor = 2_000_000n;
  for (let i = 0; i < 4; i++)
    floor = MIDGARD_COINS_PER_UTXO_BYTE * BigInt(output(floor).length + 160);
  const outputCbor = output(underfunded ? floor - 1n : floor + above);
  const prefix = Array.from({ length: prefixCount }, () =>
    encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x46)]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    }),
  );
  let outputs = [...prefix, outputCbor];
  if (fieldBytes > 0) {
    let pad = 0;
    const sibling = () =>
      encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x45)]),
        value: { lovelace: 2_000_000n, assets: new Map() },
        script_ref: { language: "PlutusV3", scriptBytes: Buffer.alloc(pad) },
      });
    while (
      encodeMidgardFieldPreimage([sibling(), ...prefix, outputCbor]).length <
      fieldBytes
    )
      pad++;
    outputs = [sibling(), ...prefix, outputCbor];
    expect(encodeMidgardFieldPreimage(outputs).length).toBe(fieldBytes);
  }
  const submitted = materializeMidgardNativeTxFromCanonical(
    transactionOf(outputs),
  );
  const tx = materializeMidgardForcedTxFromCanonical(submitted);
  const txId = computeMidgardNativeTxId(tx).toString("hex");
  const proofSource = deriveMidgardForcedTxProofSource(tx);
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
  const outputIndex = BigInt(namedOutputIndex ?? outputs.length - 1);
  const reason = wrongReason
    ? ("FeeBelowMinimum" as const)
    : { OutputBelowMinAda: { output_index: outputIndex } };
  const value = {
    tx_id: txId,
    submitted_source: {
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
  const scriptPublications: ReturnType<
    typeof import("./support/emulator/measurement.js").measureCompleteSignedTransaction
  >[] = [];
  const refs = await publishFinalFamilyReferenceScripts({
    lucid: h.proverLucid,
    family: h.family,
    label: "min-ada",
    onPublication: (_index, publication) =>
      scriptPublications.push(publication.publicationMeasurement),
  });

  const state = {
    grammar_checkpoint_hash: "",
    grammar_complete: false,
    walk_checkpoint_hash: "",
    direction: 1n,
    bad_tx_id: txId,
    fault: { MinAdaTx: { output_index: outputIndex } },
    post_utxo: null,
  };
  const material = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: Number(outputIndex),
    outputCbor: outputs[Number(outputIndex)]!,
  });
  if (assetCount === 1304)
    expect(material.descriptor.cardanoValueSize).toBe(5000);
  const prepared = {
    kind: "min-ada-forced" as const,
    headerHash: seeded.headerHash,
    badTxId: txId,
    badOutputIndex: outputIndex,
    nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
    nativeTxCanonicalCbor:
      encodeMidgardForcedTxCanonical(submitted).toString("hex"),
    outputItemCbors: outputs.map((item) => item.toString("hex")),
    descriptorCbor: material.descriptorCbor.toString("hex"),
    fault: state.fault,
    subject: SDK.forcedVerdictSubject({
      transactionId: txId,
      sourceKey: key,
      rejectionReason: reason,
    }),
    state,
  };
  const artifact = {
    schemaVersion: MIN_ADA_FORCED_ARTIFACT,
    headerHash: seeded.headerHash,
    forcedIndex: 0,
    detectionId: `min-ada:forced:0:${txId}:${outputIndex}`,
    forcedSourceCbor: Data.to(
      source as never,
      SDK.MinAdaForcedSourcePayloadSchema as never,
    ),
    fullTransactionCbor:
      encodeMidgardForcedTxCanonical(submitted).toString("hex"),
  };
  const common = {
    lucid: h.proverLucid,
    contracts: h.family,
    categoryId: h.category.categoryId,
    signer: h.proverSigner,
  };
  const shape = `scenario-${scenarioId}-assets-${assetCount}-cancel-${cancelAt}-output-${outputCbor.length}-field-${encodeMidgardFieldPreimage(outputs).length}-mpf-${depth}-above-${above}`;
  let measurementSequence = 0;
  const record = (
    name: string,
    kind: "publication" | "lifecycle",
    measurements: readonly CompleteSignedTransactionMeasurement[],
  ) =>
    measurements.forEach((m, i) =>
      rows.push({
        name: `${shape}/${name}/${measurementSequence++}/${i}`,
        kind,
        maximumShape: shape,
        signedBytes: m.completeSignedBytes,
        memoryUnits: m.executionMemory,
        cpuUnits: m.executionSteps,
      }),
    );
  record("family-publications", "publication", scriptPublications);
  record("yield-publications", "publication", [
    seeded.minAdaYieldReferenceScripts!.tx.publicationMeasurement,
    seeded.minAdaYieldReferenceScripts!.utxo.publicationMeasurement,
  ]);
  const capture = async <T>(
    name: string,
    operation: () => Promise<T>,
    kind: "publication" | "lifecycle" = "lifecycle",
  ) => {
    const captured = await captureEmulatorSubmission(
      h.emulator,
      operation,
    ).catch((error) => {
      console.error("MIN_ADA_PHASE", name, shape);
      throw error;
    });
    record(name, kind, captured.measurements);
    return captured.result;
  };
  const initialize = () =>
    capture("init", () =>
      submitMinAdaInit({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
        network,
        signer: h.proverSigner,
        fraudulentBlockOutRef: seeded.fraudulentBlockOutRef,
        witnessReferenceScripts: h.witnessReferenceScripts,
      }),
    );
  const init = await initialize();
  const prepareArtifact = async () => {
    const entry = {
      key,
      value,
      keyBytes,
      valueBytes,
      fullTransactionCbor: encodeMidgardForcedTxCanonical(submitted),
    };
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: key } };
    const fingerprint = eventKeyFingerprint(eventKey);
    const block = {
      headerHash: seeded.headerHash,
      header,
      transactions: [],
      reconstruction: {
        utxos: [],
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
    const replay = await MIN_ADA_COMPLETE_CANONICAL_REPLAY.replay(block);
    const selected = replay.detections.find(
      (d) => d.detectionId === artifact.detectionId,
    );
    if (!selected)
      throw new Error("installed complete replay missed forced minAda");
    if (depth > 0) return artifact;
    return prepareMinAdaWorkflowArtifact({
      evidence: block,
      predecessor: undefined,
      classification: {
        schemaVersion: "midgard-fraud-proof-classification-v1",
        decision: "fault_detected",
        headerHash: block.headerHash,
        category: "minAda",
        selected,
        detections: replay.detections,
        unprovableGaps: [],
      },
    });
  };
  return {
    h,
    common,
    capture,
    initialize,
    prepareArtifact,
    artifact,
    prepared,
    state,
    source,
    refs,
    seeded,
    threadOutRef: `${init.txHash}#${init.firstStepOutputIndex}`,
  };
};
