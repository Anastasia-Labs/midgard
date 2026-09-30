import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import {
  ForcedInclusionTxV1Schema,
  HeaderSchema,
  type NetworkIdFault,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { nativeTxFromCoreCompact } from "../step-support.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
} from "../workflow/orchestrator.js";
import { type NetworkIdForcedScanStep } from "./forced-scan-plan.js";
import { type PreparedNetworkIdProof } from "./prepare.js";

export const ARTIFACT_VERSION =
  "midgard-network-id-workflow-artifact-v1" as const;

export const CATEGORY = "networkId";

/**
 * Direction discriminator for the durable artifact.
 *
 * The accepted direction predates it, so an artifact without the field is
 * exactly the accepted shape it always was; the forced direction is additive
 * and the schema version is unchanged.
 */
export type NetworkIdWorkflowDirection = "accepted" | "forced";

type NetworkIdWorkflowArtifact = {
  readonly schemaVersion: typeof ARTIFACT_VERSION;
  readonly direction?: "accepted";
  readonly headerHash: string;
  readonly expectedNetworkId: "0" | "1";
  readonly badTxId: string;
  readonly nativeTxCanonicalCbor: string;
  readonly nativeTxCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  readonly outputsItemCbors: readonly string[];
  readonly faultKind: "transaction-network" | "output-network";
  readonly outputIndex: string | null;
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
};

/**
 * §5.2 forced (wrongful-rejection) artifact.
 *
 * Everything the forced door and step 02 re-derive on chain is carried as the
 * authenticated bytes it is derived from: the counted-root membership proof
 * (header + leaf) as canonical `Data` CBOR, and the field-2 preimage. The
 * verdict subject, the transaction's own network id, the output network ids
 * and the item list are all re-derived on admission, never journaled as a
 * prover assertion.
 */
/**
 * Durable §5.2 journal artifact. It is the accepted artifact's sibling, not its
 * successor: the shared `schemaVersion` plus the additive `direction`
 * discriminator keeps every already-journaled accepted artifact admissible.
 */
export type NetworkIdForcedWorkflowArtifact = {
  readonly schemaVersion: typeof ARTIFACT_VERSION;
  readonly direction: "forced";
  readonly headerHash: string;
  readonly expectedNetworkId: "0" | "1";
  readonly badTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly outputsPreimageCbor: string;
  readonly outputsItemCbors: readonly string[];
  readonly forcedSourceCbor: string;
};

export const NetworkIdForcedSourceSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
});

export type WorkflowContext = Parameters<
  FraudProofFamilyWorkflowAdapter["observe"]
>[0];

export type ActionKind =
  | "init"
  | "step01"
  | "forced_step01"
  | "forced_bind"
  | "forced_scan_open"
  | "forced_scan_grammar"
  | "forced_scan_advance"
  | "step02"
  | "publish_field"
  | "certify_field"
  | "remove";

/** The three scan stages, named by the §10 phase each one drives. */
const FORCED_SCAN_KINDS = [
  "forced_scan_open",
  "forced_scan_grammar",
  "forced_scan_advance",
] as const;

type ForcedScanKind = (typeof FORCED_SCAN_KINDS)[number];

export const isForcedScanKind = (kind: ActionKind): kind is ForcedScanKind =>
  (FORCED_SCAN_KINDS as readonly string[]).includes(kind);

export const forcedScanKindFor = (
  step: NetworkIdForcedScanStep,
): ForcedScanKind =>
  step.kind === "open"
    ? "forced_scan_open"
    : step.kind === "advance"
      ? "forced_scan_advance"
      : "forced_scan_grammar";

export const requireString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`network-id workflow ${label} must be a non-empty string`);
  }
  return value;
};

export const actionKind = (action: FraudProofWorkflowAction): ActionKind => {
  const kind = action.input.kind;
  if (
    kind !== "init" &&
    kind !== "step01" &&
    kind !== "forced_step01" &&
    kind !== "forced_bind" &&
    kind !== "forced_scan_open" &&
    kind !== "forced_scan_grammar" &&
    kind !== "forced_scan_advance" &&
    kind !== "step02" &&
    kind !== "publish_field" &&
    kind !== "certify_field" &&
    kind !== "remove"
  ) {
    throw new Error(`network-id workflow action ${action.actionId} is unknown`);
  }
  return kind;
};

// Every action carries its kind twice: `kind` drives this adapter's own
// dispatch and `actionKind` is the stable action label the production funding
// reservation permit reads (it accepts `actionKind` or `stage`, never `kind`).
export const action = (
  kind: ActionKind,
  input: Readonly<Record<string, string>>,
  actionId = `network-id:${kind}:${Object.values(input).join(":")}`,
): FraudProofWorkflowAction => ({
  actionId,
  input: { kind, actionKind: kind, ...input },
});

export const contentActionId = ({
  base,
  entries,
}: {
  readonly base: string;
  readonly entries: WorkflowContext["entries"];
}): string => {
  const confirmations = entries.filter(
    (entry) =>
      entry.event.kind === "confirmed" &&
      (entry.event.actionId === base ||
        entry.event.actionId.startsWith(`${base}:heal:`)),
  ).length;
  return confirmations === 0
    ? base
    : `${base}:heal:${confirmations.toString()}`;
};

export const artifactFromPrepared = (
  prepared: PreparedNetworkIdProof,
): NetworkIdWorkflowArtifact => ({
  schemaVersion: ARTIFACT_VERSION,
  headerHash: prepared.headerHash,
  expectedNetworkId: prepared.expectedNetworkId.toString() as "0" | "1",
  badTxId: prepared.badTxId,
  nativeTxCanonicalCbor: prepared.nativeTxCanonicalCbor,
  nativeTxCompactCbor: prepared.nativeTxCompactCbor,
  l2TransactionSourceCbor: prepared.txInclusion.l2TransactionSourceCbor,
  outputsItemCbors: prepared.outputsItemCbors,
  faultKind: prepared.faultClaim.kind,
  outputIndex:
    prepared.faultClaim.kind === "output-network"
      ? prepared.faultClaim.outputIndex.toString()
      : null,
  transactionsPhasRoot: prepared.txInclusion.transactionsPhasRoot,
  txMembershipProofCbor: prepared.txInclusion.txMembershipProofCbor,
});

export const preparedFromArtifact = (
  value: WorkflowContext["artifact"],
): PreparedNetworkIdProof => {
  const artifact = value as unknown as NetworkIdWorkflowArtifact;
  if (artifact.schemaVersion !== ARTIFACT_VERSION) {
    throw new Error("network-id workflow artifact has an unsupported version");
  }
  const expectedNetworkId =
    artifact.expectedNetworkId === "0"
      ? 0n
      : artifact.expectedNetworkId === "1"
        ? 1n
        : undefined;
  if (expectedNetworkId === undefined) {
    throw new Error("network-id workflow artifact has an invalid network id");
  }
  const outputIndex =
    artifact.outputIndex === null ? undefined : BigInt(artifact.outputIndex);
  if (
    (artifact.faultKind === "transaction-network" &&
      outputIndex !== undefined) ||
    (artifact.faultKind === "output-network" && outputIndex === undefined)
  ) {
    throw new Error("network-id workflow artifact has an inconsistent fault");
  }
  const fault: NetworkIdFault =
    artifact.faultKind === "transaction-network"
      ? "TransactionNetwork"
      : { OutputNetwork: { output_index: outputIndex! } };
  return {
    headerHash: requireString(artifact.headerHash, "header hash"),
    expectedNetworkId,
    badTxId: requireString(artifact.badTxId, "transaction id"),
    nativeTxCanonicalCbor: requireString(
      artifact.nativeTxCanonicalCbor,
      "canonical transaction",
    ),
    nativeTxCompactCbor: requireString(
      artifact.nativeTxCompactCbor,
      "compact transaction",
    ),
    outputsItemCbors: [...artifact.outputsItemCbors],
    faultClaim:
      artifact.faultKind === "transaction-network"
        ? { kind: "transaction-network" }
        : { kind: "output-network", outputIndex: outputIndex! },
    fault,
    txInclusion: {
      nativeTxId: artifact.badTxId,
      nativeTx: nativeTxFromCoreCompact(
        decodeMidgardNativeTxCompact(
          Buffer.from(artifact.nativeTxCompactCbor, "hex"),
        ),
      ),
      nativeTxCompactCbor: artifact.nativeTxCompactCbor,
      l2TransactionSourceCbor: requireString(
        artifact.l2TransactionSourceCbor,
        "transaction source",
      ),
      transactionsPhasRoot: artifact.transactionsPhasRoot,
      txMembershipProofCbor: artifact.txMembershipProofCbor,
    },
  };
};

export type ForcedSourcePayload = Data.Static<
  typeof NetworkIdForcedSourceSchema
>;

export const proofSteps = (proof: ForcedSourcePayload["membership"]["proof"]) =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    }
    if ("Fork" in step) {
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    }
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });
