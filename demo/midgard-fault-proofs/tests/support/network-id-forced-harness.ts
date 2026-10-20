/**
 * Mock-free fixtures for the network-id forced (§5.2) workflow suites: the
 * authenticated forced contradiction, its durable artifact, and an adapter
 * wired to a stub L1 view.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash28,
  computeMidgardNativeTxId,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  encodeHeaderCbor,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  ForcedInclusionTxV1Schema,
  type NetworkIdFault,
  NetworkIdForcedScanDatumSchema,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type MintingPolicy,
  type UTxO,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { fieldPreimageCertificateAddress } from "../../src/field-opening.js";
import {
  createNetworkIdWorkflowAdapter,
  networkIdForcedArtifactFromPrepared,
  type NetworkIdWorkflowAdapterConfig,
} from "../../src/network-id/workflow-adapter.js";
import {
  detectNetworkIdWrongfulRejections,
  type PreparedNetworkIdWrongfulRejection,
} from "../../src/network-id/wrongful-rejection.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type JournalJsonObject,
} from "../../src/workflow/journal.js";
import type { FraudProofRawL1FamilyStage } from "../../src/workflow/raw-l1-family-derivation.js";
import type { SignedTransactionRecoveryObservation } from "../../src/workflow/signed-transaction-reconciliation.js";
import {
  h28,
  h32,
  outRefCbor,
} from "../helpers/canonical-block-evidence-fixture.js";
import { makeHeader } from "./emulator/header-fixtures.js";
import { makeNativeTx } from "./emulator/native-tx.js";

export const POLICY_ID = "91".repeat(28);
const THREAD_POLICY_ID = h28(0x13);
const CATEGORY_ID = "0000001c";
const SCRIPT: MintingPolicy = { type: "Native", script: "8200" };
export const SIGNER: ResolvedProverSigner = {
  source: "network-id-forced-test",
  address: "addr_test1_network_id_forced",
  paymentKeyHash: "81".repeat(28),
  selectWallet: () => undefined,
};
const STEP_01_ADDRESS = "network-step-01";
const FORCED_STEP_ADDRESS = "network-forced-step";
const FORCED_SCAN_ADDRESS = "network-forced-scan";
const STEP_02_ADDRESS = "network-step-02";
const STATE_QUEUE_ADDRESS = "state-queue";
export const CERTIFICATE_ADDRESS = fieldPreimageCertificateAddress({
  network: "Preview",
  certificatePolicyId: POLICY_ID,
});
export const CERTIFICATE_UNIT = `${POLICY_ID}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`;
export const STATE_QUEUE_OUT_REF = `${h32(0x73)}#0`;

export const utxo = ({
  txHash,
  outputIndex = 0,
  address = SIGNER.address,
  datum,
}: {
  readonly txHash: string;
  readonly outputIndex?: number;
  readonly address?: string;
  readonly datum?: string;
}): UTxO => ({
  txHash,
  outputIndex,
  address,
  assets: { lovelace: 5_000_000n },
  ...(datum === undefined ? {} : { datum }),
});

const output = (networkId: number) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([0x60 | networkId]),
      Buffer.alloc(28, 0x44),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });

/**
 * One authenticated forced leaf, its counted-root membership proof, and the
 * header that commits it — the exact material the planner would derive from a
 * reconstructed block.
 */
export const forcedPrepared = async ({
  outputNetworkIds = [0, 0],
  reason = "NetworkIdMismatch",
}: {
  readonly outputNetworkIds?: readonly number[];
  readonly reason?: "NetworkIdMismatch" | "EmptyInputs";
} = {}): Promise<PreparedNetworkIdWrongfulRejection> => {
  const invalid = materializeMidgardForcedTxFromCanonical(
    makeNativeTx({
      spendInputCbors: [outRefCbor(0x11, 0n)],
      fee: 0n,
      outputCbors: outputNetworkIds.map(output),
    }),
  );
  const txId = computeMidgardNativeTxId(invalid).toString("hex");
  const proofSource = deriveMidgardForcedTxProofSource(invalid);
  const key = { transactionId: h32(0x05), outputIndex: 0n };
  const leaf = {
    tx_id: txId,
    submitted_source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason } },
  } as const;
  const keyBytes = Buffer.from(Data.to(key, OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(leaf as never, ForcedInclusionTxV1Schema as never),
    "hex",
  );
  const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(keyBytes, valueBytes);
  const proof = await trie.prove(keyBytes);
  const header = {
    ...makeHeader(h28(0x44), 1_000),
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: root.count,
  };
  const headerHash = computeHash28(encodeHeaderCbor(header)).toString("hex");
  const block = {
    headerHash,
    header: { expectedNetworkId: 0n },
    reconstruction: {
      forcedTransactions: [
        {
          key,
          value: leaf,
          fullTransactionCbor: encodeMidgardForcedTxCanonical(invalid),
        },
      ],
    },
  } as never;
  const detection = detectNetworkIdWrongfulRejections({
    block,
    expectedNetworkId: 0n,
  })[0];
  if (detection === undefined) {
    throw new Error("fixture produced no authenticated wrongful rejection");
  }
  return {
    headerHash,
    expectedNetworkId: 0n,
    badTxId: txId,
    nativeTxCompactCbor: leaf.submitted_source.compact_cbor,
    outputsItemCbors: detection.evidence.outputsItemCbors,
    faultClaim: { kind: "forced-network-mismatch" },
    fault: "ForcedNetworkIdMismatch" as NetworkIdFault,
    subject: detection.evidence.subject,
    forcedSource: {
      header,
      membership: {
        domain: root.domain,
        root: root.root,
        phas_root: root.phasRoot,
        count: root.count,
        key,
        value: leaf,
        proof: Data.from(proof.toCBOR().toString("hex"), Proof),
      } as PreparedNetworkIdWrongfulRejection["forcedSource"]["membership"],
    },
    evidence: detection.evidence,
  };
};

export const workflowIdentity = (
  headerHash: string,
): FraudProofWorkflowIdentity => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  deploymentFingerprint: "d1".repeat(32),
  category: "networkId",
  target: { kind: "state_queue_header", headerHash },
});

/** The scan-thread datum the forced door writes, and the ones it becomes. */
export const scanDatum = (
  prepared: PreparedNetworkIdWrongfulRejection,
  state: "ready" | { readonly scanningAt: string },
): string => {
  const bound = {
    bad_tx_id: prepared.badTxId,
    committed_tx_network_id: prepared.evidence.committedNetworkId,
    expected_network_id: prepared.expectedNetworkId,
    forced_source_key: prepared.subject.source_key,
  };
  return Data.to(
    {
      fraud_prover: SIGNER.paymentKeyHash,
      data:
        state === "ready"
          ? { Ready: { bound } }
          : { Scanning: { bound, checkpoint_hash: state.scanningAt } },
    } as never,
    NetworkIdForcedScanDatumSchema as never,
  );
};

export const forcedHarness = ({
  prepared,
  stage,
  forcedDoorOccupied,
  forcedScanDatum,
  walletUtxos = [],
  certificateUtxos,
  observeSignedTransaction,
}: {
  /** The forced contradiction, or another direction's durable artifact. */
  readonly prepared:
    | PreparedNetworkIdWrongfulRejection
    | { readonly headerHash: string; readonly artifact: JournalJsonObject };
  readonly stage: FraudProofRawL1FamilyStage;
  readonly forcedDoorOccupied: boolean;
  /** Inline datum of the live scan thread, absent when the scan is empty. */
  readonly forcedScanDatum?: string;
  readonly walletUtxos?: readonly UTxO[];
  /**
   * Live view of the field-certificate address. When present, the exact
   * observer confirms a certificate out-ref only if it is served here with
   * the expected datum and token; absent, every exact observation confirms.
   */
  readonly certificateUtxos?: readonly UTxO[];
  /** Canonical signed-transaction recovery; absent in the bare raw-L1 stub. */
  readonly observeSignedTransaction?: (input: {
    readonly transactionHash: string;
    readonly signedTransactionCborHex: string;
  }) => Promise<SignedTransactionRecoveryObservation>;
}) => {
  const forcedUtxo = utxo({
    txHash: h32(0x77),
    address: FORCED_STEP_ADDRESS,
  });
  const scanUtxo = utxo({
    txHash: h32(0x78),
    address: FORCED_SCAN_ADDRESS,
    ...(forcedScanDatum === undefined ? {} : { datum: forcedScanDatum }),
  });
  const scanUtxos = forcedScanDatum === undefined ? [] : [scanUtxo];
  const lucid = {
    utxosAtWithUnit: async (address: string) =>
      address === FORCED_STEP_ADDRESS && forcedDoorOccupied
        ? [forcedUtxo]
        : address === FORCED_SCAN_ADDRESS
          ? scanUtxos
          : address === STATE_QUEUE_ADDRESS
            ? [utxo({ txHash: h32(0x73), address: STATE_QUEUE_ADDRESS })]
            : [],
    utxosAt: async (address: string) =>
      address === SIGNER.address
        ? walletUtxos
        : address === CERTIFICATE_ADDRESS
          ? (certificateUtxos ?? [])
          : [],
    utxosByOutRef: async (
      refs: readonly { txHash: string; outputIndex: number }[],
    ) =>
      refs.flatMap((ref) =>
        [...walletUtxos, ...(certificateUtxos ?? [])].filter(
          (utxo) =>
            utxo.txHash === ref.txHash && utxo.outputIndex === ref.outputIndex,
        ),
      ),
  } as unknown as LucidEvolution;
  const step = {
    spendingScript: SCRIPT,
    spendingScriptHash: h28(0x11),
    spendingScriptAddress: STEP_01_ADDRESS,
  };
  const config = {
    lucid,
    blueprint: {},
    network: "Preview",
    contracts: {
      steps: [step, { ...step, spendingScriptAddress: STEP_02_ADDRESS }],
      forcedStep: { ...step, spendingScriptAddress: FORCED_STEP_ADDRESS },
      forcedScan: { ...step, spendingScriptAddress: FORCED_SCAN_ADDRESS },
      expectedNetworkId: 0n,
      computationThread: { policyId: THREAD_POLICY_ID, mintingScript: SCRIPT },
      fraudProof: {
        policyId: h28(0x14),
        mintingScript: SCRIPT,
        spendingScriptAddress: "proof",
      },
      hubOraclePolicyId: h28(0x15),
      stateQueuePolicyId: h28(0x16),
      fieldPreimageCertificatePolicyId: POLICY_ID,
      fieldPreimageCertificateMintingScript: SCRIPT,
    },
    stateQueueAddress: STATE_QUEUE_ADDRESS,
    category: { categoryId: CATEGORY_ID, label: "network-id" },
    catalogue: {
      policyId: h28(0x17),
      spendingScriptAddress: "catalogue",
      root: h32(0x18),
    },
    signer: SIGNER,
    stepReferenceScripts: [
      utxo({ txHash: h32(0x19) }),
      utxo({ txHash: h32(0x1a) }),
    ],
    forcedStepReferenceScript: utxo({ txHash: h32(0x1b) }),
    forcedScanReferenceScript: utxo({ txHash: h32(0x1d) }),
    fieldPreimageCertificateReferenceScript: utxo({ txHash: h32(0x1c) }),
    witnessReferenceScripts: {},
    removal: {
      deploymentInfo: {},
      category: "networkId",
      isCurrentHead: async () => true,
    },
    rawL1: {
      publications: {
        observerVersion:
          "midgard-fraud-proof-authenticated-publication-observer-v1",
        observeExact: vi.fn(
          async (input: {
            readonly address: string;
            readonly expectedOutRef: string;
            readonly expectedDatumCbor: string;
            readonly expectedUnit?: string;
          }) =>
            certificateUtxos === undefined ||
            input.address !== CERTIFICATE_ADDRESS ||
            certificateUtxos.some(
              (candidate) =>
                `${candidate.txHash}#${candidate.outputIndex}` ===
                  input.expectedOutRef &&
                candidate.datum === input.expectedDatumCbor &&
                (input.expectedUnit === undefined ||
                  candidate.assets[input.expectedUnit] === 1n),
            )
              ? { kind: "confirmed" as const, outRef: input.expectedOutRef }
              : { kind: "not_found" as const },
        ),
      },
      transactionConfirmed: async () => true,
      observe: async () => stage,
      ...(observeSignedTransaction === undefined
        ? {}
        : { observeSignedTransaction }),
    },
    terminalFacts: async () => {
      throw new Error("terminal state is outside this test");
    },
  } as unknown as NetworkIdWorkflowAdapterConfig;
  const artifact =
    "artifact" in prepared
      ? prepared.artifact
      : (networkIdForcedArtifactFromPrepared(
          prepared,
        ) as unknown as JournalJsonObject);
  return {
    artifact,
    scanUtxos,
    publicationObserver: config.rawL1!.publications!,
    adapter: createNetworkIdWorkflowAdapter(config),
    observe: async () =>
      await createNetworkIdWorkflowAdapter(config).observe({
        identity: workflowIdentity(prepared.headerHash),
        workflowId: h32(0x51),
        artifact,
        entries: [],
      }),
  };
};
