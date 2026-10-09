import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  credentialToAddress,
  Data,
  toUnit,
} from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  reconstructDaPayload,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";
import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import {
  admitFraudProofRawL1Snapshot,
  computeFraudProofRawL1SnapshotEvidenceDigest,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotAuthority,
  type FraudProofRawL1SnapshotRequest,
} from "../workflow/raw-l1-snapshot.js";

export type CrossBlockSettlementRecord = Readonly<{
  headerHash: string;
  outRef: string;
  policyId: string;
  datumCbor: string;
  payloadEnvelopeCbor: string;
  reconstruction: TransitionTraceReconstruction;
}>;
export type CrossBlockSettlementContext = Readonly<{
  schemaVersion: "midgard-cross-block-settlement-context-v1";
  challengedHeaderHash: string;
  deploymentFingerprint: string;
  boundaryPointId: string;
  contextDigest: string;
  /** Complete settlement evidence without the moving capture boundary. */
  evidenceDigest: string;
}>;
/**
 * The public retained-DA sources every settled block's payload is fetched
 * from: the same sources, through the same fetcher, as the challenged block
 * and its predecessor.
 */
export type CrossBlockSettlementRetainedDa = Readonly<{
  sources: readonly RetainedDaPayloadSource[];
  retries?: number;
}>;
type ContextData = Readonly<{
  authority: CrossBlockSettlementAuthority;
  retainedDa: CrossBlockSettlementRetainedDa;
  records: readonly CrossBlockSettlementRecord[];
}>;
const contextData = new WeakMap<CrossBlockSettlementContext, ContextData>();
const admittedAuthorities = new WeakSet<object>();
export type CrossBlockSettlementAuthority = Readonly<{
  deploymentFingerprint: string;
  capture(
    evidence: Pick<CanonicalBlockEvidence, "headerHash">,
    retainedDa: CrossBlockSettlementRetainedDa,
  ): Promise<CrossBlockSettlementContext>;
}>;
export const requireCrossBlockSettlementAuthority = (
  authority: CrossBlockSettlementAuthority,
) => {
  if (!admittedAuthorities.has(authority))
    throw new Error("cross-block settlement authority was not admitted");
  return authority;
};
export const crossBlockSettlementRecords = (
  evidence: Pick<CanonicalBlockEvidence, "headerHash">,
  context: CrossBlockSettlementContext,
) => {
  const data = contextData.get(context);
  if (
    data === undefined ||
    context.challengedHeaderHash !== evidence.headerHash
  )
    throw new Error(
      "cross-block settlement context is not admitted for challenged header",
    );
  return data.records;
};
export const refreshCrossBlockSettlementContext = async (
  evidence: Pick<CanonicalBlockEvidence, "headerHash">,
  context: CrossBlockSettlementContext,
) => {
  const data = contextData.get(context);
  if (data === undefined)
    throw new Error(
      "cross-block settlement context cannot be structurally revived",
    );
  return await data.authority.capture(evidence, data.retainedDa);
};
const sha = (value: string) => createHash("sha256").update(value).digest("hex");
type Binding = FraudProofWorkflowDeploymentBinding<"crossBlockDuplicateEvent">;
const scriptAddress = (binding: Binding, hash: string) =>
  credentialToAddress(binding.network, { type: "Script", hash });
const settlementAddress = (binding: Binding, address: SDK.AddressData) => {
  const credential = (value: SDK.CredentialD) =>
    "ScriptCredential" in value
      ? { type: "Script" as const, hash: value.ScriptCredential[0] }
      : { type: "Key" as const, hash: value.PublicKeyCredential[0] };
  if (
    address.stakeCredential !== null &&
    !("Inline" in address.stakeCredential)
  )
    throw new Error(
      "cross-block settlement address uses unsupported stake pointer",
    );
  return credentialToAddress(
    binding.network,
    credential(address.paymentCredential),
    address.stakeCredential === null
      ? undefined
      : credential(address.stakeCredential.Inline[0]),
  );
};
const hubFrom = (snapshot: FraudProofRawL1Snapshot, policyId: string) => {
  const unit = toUnit(policyId, SDK.HUB_ORACLE_ASSET_NAME);
  const hub = snapshot.scopes
    .find((scope) => scope.role === "hub_oracle")
    ?.utxos.filter(
      (raw) =>
        coreToTxOutput(CML.TransactionOutput.from_cbor_hex(raw.outputCbor))
          .assets[unit] === 1n,
    );
  if (hub?.length !== 1 || hub[0]!.datumCbor === null)
    throw new Error(
      "cross-block settlement authority lacks authentic singleton hub",
    );
  return Data.from(hub[0]!.datumCbor, SDK.HubOracleDatum);
};
const construct = ({
  binding,
  raw,
}: {
  binding: Binding;
  raw: FraudProofRawL1SnapshotAuthority;
}): CrossBlockSettlementAuthority => {
  const authority: CrossBlockSettlementAuthority = Object.freeze({
    deploymentFingerprint: binding.deploymentFingerprint,
    capture: async (
      evidence: Pick<CanonicalBlockEvidence, "headerHash">,
      retainedDa: CrossBlockSettlementRetainedDa,
    ) => {
      const hubPolicyId = binding.resolvedContracts.hubOraclePolicyId;
      const base: FraudProofRawL1SnapshotRequest = {
        deploymentIdentityDigest: binding.deploymentFingerprint,
        blueprintHash: binding.blueprintHash,
        finalityPolicyDigest: binding.releaseFinality.policyDigest,
        headerHash: evidence.headerHash,
        scopes: [
          { role: "hub_oracle", address: scriptAddress(binding, hubPolicyId) },
        ],
        historyUnits: [toUnit(hubPolicyId, SDK.HUB_ORACLE_ASSET_NAME)],
      };
      const read = async (request: FraudProofRawL1SnapshotRequest) =>
        admitFraudProofRawL1Snapshot({
          value: await raw.capture(request),
          request,
          releaseFinality: binding.releaseFinality,
        });
      const initial = await read(base);
      const firstHub = hubFrom(initial, hubPolicyId);
      const policyId = firstHub.settlement;
      const address = settlementAddress(binding, firstHub.settlement_addr);
      const scopes = [...base.scopes, { role: "settlement" as const, address }];
      const discovered = await read({ ...base, scopes });
      const settlementUnits = (snapshot: FraudProofRawL1Snapshot) =>
        [
          ...new Set(
            snapshot.scopes
              .find((scope) => scope.role === "settlement")!
              .utxos.flatMap((utxo) =>
                Object.entries(
                  coreToTxOutput(
                    CML.TransactionOutput.from_cbor_hex(utxo.outputCbor),
                  ).assets,
                )
                  .filter(
                    ([unit, quantity]) =>
                      unit.startsWith(policyId) && quantity !== 0n,
                  )
                  .map(([unit]) => unit),
              ),
          ),
        ].sort();
      const units = settlementUnits(discovered);
      // Every candidate NFT is authenticated against its exact raw creation
      // transaction at the final pinned boundary, not merely a scope row.
      const snapshot = await read({
        ...base,
        scopes,
        historyUnits: [...base.historyUnits, ...units],
      });
      if (JSON.stringify(settlementUnits(snapshot)) !== JSON.stringify(units))
        throw new Error("cross-block settlement set changed during capture");
      if (
        hubFrom(snapshot, hubPolicyId).settlement !== policyId ||
        settlementAddress(
          binding,
          hubFrom(snapshot, hubPolicyId).settlement_addr,
        ) !== address
      )
        throw new Error("cross-block settlement policy changed during capture");
      const records: CrossBlockSettlementRecord[] = [];
      for (const rawUtxo of snapshot.scopes.find(
        (scope) => scope.role === "settlement",
      )!.utxos) {
        const output = coreToTxOutput(
          CML.TransactionOutput.from_cbor_hex(rawUtxo.outputCbor),
        );
        const units = Object.entries(output.assets).filter(
          ([unit, quantity]) => unit.startsWith(policyId) && quantity !== 0n,
        );
        if (units.length === 0) continue;
        if (
          units.length !== 1 ||
          units[0]![1] !== 1n ||
          rawUtxo.datumCbor === null
        )
          throw new Error(
            "cross-block settlement has ambiguous NFT or missing datum",
          );
        const headerHash = units[0]![0].slice(policyId.length);
        if (!/^[0-9a-f]{56}$/u.test(headerHash))
          throw new Error("cross-block settlement NFT has invalid header hash");
        if (headerHash === evidence.headerHash) continue;
        // The settled block's bytes are fetched from public retained DA;
        // the reconstruction below binds them to the settled header hash and
        // the authenticated settlement datum's counted roots.
        const payload = await fetchRetainedDaPayloadByHeaderHash({
          headerHash,
          sources: retainedDa.sources,
          ...(retainedDa.retries === undefined
            ? {}
            : { retries: retainedDa.retries }),
        });
        const reconstruction = await reconstructDaPayload({
          payloadEnvelopeCbor: payload.payloadEnvelopeCbor,
          expectedHeaderHash: headerHash,
        });
        const datum = Data.from(rawUtxo.datumCbor, SDK.SettlementDatum);
        if (
          datum.deposits_root !== reconstruction.header.depositsRoot ||
          datum.withdrawals_root !== reconstruction.header.withdrawalsRoot ||
          datum.forced_transactions_root !==
            reconstruction.header.forcedTransactionsRoot ||
          datum.transactions_root !== reconstruction.header.transactionsRoot
        )
          throw new Error(
            "cross-block settled payload changed settlement counted roots",
          );
        records.push(
          Object.freeze({
            headerHash,
            outRef: rawUtxo.outRef,
            policyId,
            datumCbor: rawUtxo.datumCbor,
            payloadEnvelopeCbor: payload.payloadEnvelopeCbor.toString("hex"),
            reconstruction,
          }),
        );
      }
      records.sort(
        (a, b) =>
          a.headerHash.localeCompare(b.headerHash) ||
          a.outRef.localeCompare(b.outRef),
      );
      if (
        new Set(records.map((record) => record.headerHash)).size !==
        records.length
      )
        throw new Error(
          "cross-block historical settlement identity is ambiguous",
        );
      const context: CrossBlockSettlementContext = Object.freeze({
        schemaVersion: "midgard-cross-block-settlement-context-v1",
        challengedHeaderHash: evidence.headerHash,
        deploymentFingerprint: binding.deploymentFingerprint,
        boundaryPointId: snapshot.cursor.point.pointId,
        evidenceDigest: sha(
          JSON.stringify({
            schemaVersion: "midgard-cross-block-settlement-evidence-v1",
            deploymentFingerprint: binding.deploymentFingerprint,
            challengedHeaderHash: evidence.headerHash,
            settlementPolicyId: policyId,
            settlementAddress: address,
            rawEvidenceDigest:
              computeFraudProofRawL1SnapshotEvidenceDigest(snapshot),
            records: records.map(
              ({
                headerHash,
                outRef,
                policyId,
                datumCbor,
                payloadEnvelopeCbor,
              }) => ({
                headerHash,
                outRef,
                policyId,
                datumCbor,
                payloadEnvelopeSha256: createHash("sha256")
                  .update(Buffer.from(payloadEnvelopeCbor, "hex"))
                  .digest("hex"),
              }),
            ),
          }),
        ),
        contextDigest: sha(
          JSON.stringify({
            headerHash: evidence.headerHash,
            boundary: snapshot.cursor.point.pointId,
            records: records.map(
              ({
                headerHash,
                outRef,
                policyId,
                datumCbor,
                payloadEnvelopeCbor,
              }) => ({
                headerHash,
                outRef,
                policyId,
                datumCbor,
                payloadEnvelopeSha256: createHash("sha256")
                  .update(Buffer.from(payloadEnvelopeCbor, "hex"))
                  .digest("hex"),
              }),
            ),
          }),
        ),
      });
      contextData.set(context, {
        authority,
        retainedDa: Object.freeze({
          sources: Object.freeze([...retainedDa.sources]),
          ...(retainedDa.retries === undefined
            ? {}
            : { retries: retainedDa.retries }),
        }),
        records: Object.freeze(records),
      });
      return context;
    },
  });
  admittedAuthorities.add(authority);
  return authority;
};
export const createCrossBlockSettlementAuthority = ({
  binding,
  l1,
}: {
  binding: Binding;
  l1: FraudProofL1Source;
}) =>
  construct({
    binding,
    raw: l1.snapshotAuthority({
      releaseFinality: binding.releaseFinality,
      observationDepth: "release_finality",
    }),
  });

/** Test-only raw Cardano transport seam. Production constructs the
 * fault-proof L1 source above; settled payloads come from retained DA. All snapshot and
 * root admission runs identically; this seam supplies no verdict callback. */
export const unsafeCreateCrossBlockSettlementAuthorityFromRawForTest =
  construct;
