import {
  deriveFieldPreimageCertification,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  fieldPreimagePublicationDatumCbor,
} from "@al-ft/midgard-sdk";
import type { MintingPolicy, OutRef, UTxO } from "@lucid-evolution/lucid";

import { fieldPreimageCertificateAddress } from "../../src/field-opening.certify-fault-proof-field-carriage.js";
import { planFaultProofFieldOpening } from "../../src/field-opening.plan-fault-proof-field-opening.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import type { DoubleSpendWorkflowStage } from "../../src/workflow/double-spend-adapter.js";
import type { FraudProofWorkflowJournalEntry } from "../../src/workflow/journal.fraud-proof-workflow-journal-event.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type JournalJsonObject,
} from "../../src/workflow/journal.fraud-proof-workflow-terminal.js";
import type { FraudProofFamilyWorkflowAdapter } from "../../src/workflow/orchestrator.js";
import { computeFraudProofRawL1PointId } from "../../src/workflow/raw-l1-snapshot.admit-fraud-proof-raw-l1-transaction.js";
import { h28, h32 } from "../helpers/canonical-block-evidence-fixture.js";
import { confirmedCarriageEntries } from "./confirmed-carriage-entries.js";

/** Shared fixtures of the tier-3 workflow action chain suite. */
export const POLICY_ID = "91".repeat(28);
export const HEADER_HASH = h28(0x71);
export const THREAD_OUT_REF = `${h32(0x72)}#0`;
export const STATE_QUEUE_OUT_REF = `${h32(0x73)}#0`;
export const SCRIPT: MintingPolicy = { type: "Native", script: "8200" };
export const SIGNER: ResolvedProverSigner = {
  source: "tier3-test",
  address: "addr_test1_tier3_funding",
  proofMaterialsAddress: "addr_test1_tier3_test",
  paymentKeyHash: "81".repeat(28),
  selectWallet: () => undefined,
};

export const chainPoint = (
  blockNo: number,
  slot: number,
  blockHash: string,
) => {
  const value = { blockNo: String(blockNo), slot: String(slot), blockHash };
  return { ...value, pointId: computeFraudProofRawL1PointId(value) };
};

export const stageObservation = (stage: DoubleSpendWorkflowStage) => ({
  provenance: {
    trustClass: "authenticated_cardano_l1",
    sourceId: "local-node-test",
    grade: "security",
  } as const,
  stage,
});

export const utxo = ({
  txHash,
  outputIndex,
  address = SIGNER.proofMaterialsAddress,
  datum,
  assets = { lovelace: 5_000_000n },
  scriptRef,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address?: string;
  readonly datum?: string;
  readonly assets?: Readonly<Record<string, bigint>>;
  readonly scriptRef?: MintingPolicy;
}): UTxO => ({
  txHash,
  outputIndex,
  address,
  assets,
  ...(datum === undefined ? {} : { datum }),
  ...(scriptRef === undefined ? {} : { scriptRef }),
});

export const publicationsFor = (
  plan: ReturnType<typeof planFaultProofFieldOpening>,
): readonly UTxO[] =>
  plan.plan.publications.map((publication, index) =>
    utxo({
      txHash: h32(0x20 + index),
      outputIndex: 0,
      datum: fieldPreimagePublicationDatumCbor(publication.bytes),
    }),
  );

export const certificateFor = ({
  plan,
  network,
}: {
  readonly plan: ReturnType<typeof planFaultProofFieldOpening>;
  readonly network: "Preview";
}): UTxO => {
  const certification = deriveFieldPreimageCertification(plan.plan);
  const unit = `${POLICY_ID}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`;
  return utxo({
    txHash: h32(0x61),
    outputIndex: 0,
    address: fieldPreimageCertificateAddress({
      network,
      certificatePolicyId: POLICY_ID,
    }),
    datum: certification.datumCbor,
    assets: { lovelace: 5_000_000n, [unit]: 1n },
  });
};

/** Ledger reads, by address and by out-ref, over the current outputs. */
export const ledgerReads = (
  live: () => readonly [materials: readonly UTxO[], others: readonly UTxO[]],
) => ({
  utxosAt: async (address: string) =>
    live()[address === SIGNER.proofMaterialsAddress ? 0 : 1],
  utxosByOutRef: async (outRefs: readonly OutRef[]) =>
    live()
      .flat()
      .filter(({ txHash, outputIndex }) =>
        outRefs.some(
          (ref) => ref.txHash === txHash && ref.outputIndex === outputIndex,
        ),
      ),
});

export const confirmedEntry = (
  actionId: string,
): FraudProofWorkflowJournalEntry =>
  ({
    event: { kind: "confirmed", actionId, txHash: h32(0x41) },
  }) as FraudProofWorkflowJournalEntry;

export const workflowIdentity = (
  category: "doubleSpend" | "networkId",
): FraudProofWorkflowIdentity => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  deploymentFingerprint: "d1".repeat(32),
  category,
  target: { kind: "state_queue_header", headerHash: HEADER_HASH },
});

export const observe = async ({
  adapter,
  category,
  artifact,
  entries = [],
  published = [],
}: {
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly category: "doubleSpend" | "networkId";
  readonly artifact: JournalJsonObject;
  readonly entries?: readonly FraudProofWorkflowJournalEntry[];
  /** Outputs of transactions the journal confirmed before `entries`. */
  readonly published?: readonly UTxO[];
}) => {
  const identity = workflowIdentity(category);
  const workflowId = h32(0x51);
  return await adapter.observe({
    identity,
    workflowId,
    artifact,
    entries: [
      ...confirmedCarriageEntries({ utxos: published, workflowId, identity }),
      ...entries,
    ],
  });
};
