import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  type Credential,
  credentialToAddress,
  Data,
  type Network,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { createFraudProofFamilyLocalKupmiosL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { LocalKupmiosCheckpointChangedError } from "../workflow/local-kupmios-raw-l1-authority.js";
import {
  admitFraudProofRawL1Snapshot,
  computeFraudProofRawL1SnapshotEvidenceDigest,
  type FraudProofRawL1SnapshotAuthority,
  type FraudProofRawL1SnapshotRequest,
  type FraudProofRawL1Utxo,
} from "../workflow/raw-l1-snapshot.js";

export type TransitionTraceL1Events = Readonly<{
  headerHash: string;
  snapshotDigest: string;
  evidenceDigest: string;
}>;
export type TransitionTraceL1Event = Readonly<{
  utxo: UTxO;
  assetName: string;
}> &
  (
    | Readonly<{ kind: "forcedTransaction" }>
    | Readonly<{
        kind: "deposit" | "withdrawal";
        history: Readonly<{ commitmentCbor: string; openingCbor: string }>;
        retainedDataUtxo?: UTxO;
      }>
  );
type AdmittedTransitionTraceL1Events = Readonly<{
  events: readonly TransitionTraceL1Event[];
  hub: UTxO;
  network: Network;
  depositPolicyId: string;
  snapshot: ReturnType<typeof admitFraudProofRawL1Snapshot>;
}>;
const admitted = new WeakMap<
  object,
  Readonly<{
    convenience: AdmittedTransitionTraceL1Events;
    raw: AdmittedTransitionTraceL1Events;
  }>
>();
// Applied only to the detached result of successful admission. The snapshot
// and all derived convenience views have one immutable owner, so reading an
// admitted handle cannot require repeating transaction and history validation.
const freezeAdmittedEvents = (value: unknown): void => {
  if (value === null || typeof value !== "object") return;
  for (const child of Object.values(value)) freezeAdmittedEvents(child);
  Object.freeze(value);
};

const utxo = (raw: FraudProofRawL1Utxo): UTxO => {
  const [txHash, index] = raw.outRef.split("#");
  return {
    ...coreToTxOutput(CML.TransactionOutput.from_cbor_hex(raw.outputCbor)),
    txHash: txHash!,
    outputIndex: Number(index),
  };
};

const eventAddress = (network: Network, address: SDK.AddressData): string => {
  const credential = (value: SDK.CredentialD): Credential =>
    "PublicKeyCredential" in value
      ? { type: "Key", hash: value.PublicKeyCredential[0] }
      : { type: "Script", hash: value.ScriptCredential[0] };
  if (address.stakeCredential === null)
    return credentialToAddress(network, credential(address.paymentCredential));
  if ("Inline" in address.stakeCredential)
    return credentialToAddress(
      network,
      credential(address.paymentCredential),
      credential(address.stakeCredential.Inline[0]),
    );
  throw new Error(
    "Authenticated event deployment contains an unsupported pointer address",
  );
};

/** Captures release-final raw address coverage, then complete histories for each
 * event NFT discovered there. Rechecking the hub in the final snapshot prevents
 * mixing event policies across observations. No supplied event verdict is read. */
export const captureTransitionTraceL1Events = async ({
  binding,
  authority,
}: {
  binding: FraudProofWorkflowDeploymentBinding<"transitionTrace">;
  authority: FraudProofRawL1SnapshotAuthority;
}): Promise<TransitionTraceL1Events> => {
  const finality = binding.releaseFinality;
  const hubPolicy = binding.resolvedContracts.hubOraclePolicyId;
  const hubAddress = credentialToAddress(
    binding.network,
    scriptHashToCredential(hubPolicy),
  );
  const hubUnit = toUnit(hubPolicy, SDK.HUB_ORACLE_ASSET_NAME);
  const base = {
    deploymentIdentityDigest: finality.deploymentIdentityDigest,
    blueprintHash: finality.blueprintHash,
    finalityPolicyDigest: finality.policyDigest,
    headerHash: binding.definition.headerHash,
  };
  const capture = async (
    scopes: FraudProofRawL1SnapshotRequest["scopes"],
    historyUnits: readonly string[],
  ) => {
    const request = { ...base, scopes, historyUnits };
    return admitFraudProofRawL1Snapshot({
      value: await authority.capture(request),
      request,
      releaseFinality: finality,
      observationDepth: "inclusion",
    });
  };
  const hubScope = { role: "hub_oracle", address: hubAddress } as const;
  const first = await capture([hubScope], [hubUnit]);
  const readHub = (snapshot: typeof first) => {
    const matches = snapshot.scopes
      .find((scope) => scope.role === "hub_oracle")!
      .utxos.map(utxo)
      .filter((entry) => entry.assets[hubUnit] === 1n);
    if (matches.length !== 1 || matches[0]!.datum == null)
      throw new Error(
        "Transition replay needs the unique authenticated hub oracle",
      );
    return {
      utxo: matches[0]!,
      outRef: `${matches[0]!.txHash}#${matches[0]!.outputIndex}`,
      datum: matches[0]!.datum!,
      data: Data.from(matches[0]!.datum!, SDK.HubOracleDatum),
    };
  };
  const hub = readHub(first);
  const definitions = [
    {
      kind: "deposit",
      role: "deposit_event",
      policy: hub.data.deposit,
      address: hub.data.deposit_addr,
    },
    {
      kind: "withdrawal",
      role: "withdrawal_event",
      policy: hub.data.withdrawal,
      address: hub.data.withdrawal_addr,
    },
    {
      kind: "forcedTransaction",
      role: "forced_transaction_event",
      policy: hub.data.tx_order,
      address: hub.data.tx_order_addr,
    },
  ] as const;
  const historyDeployment =
    binding.resolvedContracts.contracts.transitionTrace?.history;
  if (historyDeployment === undefined)
    throw new Error(
      "Transition event capture requires applied history parameters",
    );
  const scopes = [
    {
      role: "deposit_history_data" as const,
      address: historyDeployment.retentionAddresses.deposit,
    },
    {
      role: "withdrawal_history_data" as const,
      address: historyDeployment.retentionAddresses.withdrawal,
    },
    hubScope,
    ...definitions.map((entry) => ({
      role: entry.role,
      address: eventAddress(binding.network, entry.address),
    })),
  ];
  const discovery = await capture(scopes, [hubUnit]);
  const units = new Set([hubUnit]);
  for (const definition of definitions)
    for (const raw of discovery.scopes.find(
      (scope) => scope.role === definition.role,
    )!.utxos) {
      for (const unit of Object.keys(utxo(raw).assets))
        if (unit.startsWith(definition.policy)) units.add(unit);
    }
  const captureCoveredEvents = async () => {
    for (let attempt = 0; attempt < 3; attempt += 1) {
      const snapshot = await capture(scopes, [...units].sort());
      const finalHub = readHub(snapshot);
      if (hub.outRef !== finalHub.outRef || hub.datum !== finalHub.datum)
        throw new Error(
          "Transition replay hub changed while admitting event history",
        );
      const events: TransitionTraceL1Event[] = [];
      const newUnits = new Set<string>();
      const scopedUtxos = new Map(
        snapshot.scopes.map((scope) => [scope.address, scope.utxos.map(utxo)]),
      );
      const provider = {
        utxosAt: async (address: string) => {
          const outputs = scopedUtxos.get(address);
          if (outputs === undefined)
            throw new Error(
              "Transition retained-data address was not captured",
            );
          return [...outputs];
        },
      };
      for (const definition of definitions) {
        const outputs = snapshot.scopes
          .find((scope) => scope.role === definition.role)!
          .utxos.map(utxo);
        if (definition.kind !== "forcedTransaction") {
          const deployment: SDK.EventHistoryDeployment = {
            policyId: definition.policy,
            address: eventAddress(binding.network, definition.address),
            retentionAddress:
              historyDeployment.retentionAddresses[definition.kind],
            inlineLimitBytes: historyDeployment.inlineLimitBytes,
          };
          for (const anchor of SDK.authenticateHistoryNodes(
            outputs,
            deployment,
          )) {
            const unit = definition.policy + (anchor.key ?? "");
            if (!units.has(unit)) newUnits.add(unit);
            // Roots, fillers and pointer-only continuations are list structure.
            if (
              anchor.node.payload === "RootContent" ||
              !("Order" in anchor.node.payload)
            )
              continue;
            const witness = await SDK.fetchEventHistoryWitness(
              provider,
              deployment,
              anchor.node.payload.Order.facts.event_id,
            );
            if (witness.kind !== "Present")
              throw new Error("Transition Order has no authenticated presence");
            const captured = SDK.captureEventHistoryWitness(
              witness,
              definition.policy,
              definition.kind === "deposit" ? "Deposit" : "Withdrawal",
            );
            events.push({
              kind: definition.kind,
              utxo: witness.anchor.utxo,
              assetName: witness.anchor.key!,
              history: {
                commitmentCbor: Data.to(
                  captured.commitment,
                  SDK.EventHistoryCommitment,
                ),
                openingCbor: captured.openingCbor,
              },
              ...(witness.retainedDataUtxo === undefined
                ? {}
                : { retainedDataUtxo: witness.retainedDataUtxo }),
            });
          }
          continue;
        }
        for (const event of outputs) {
          const tokens = Object.entries(event.assets).filter(([unit]) =>
            unit.startsWith(definition.policy),
          );
          if (tokens.length === 0) continue;
          if (
            tokens.length !== 1 ||
            tokens[0]![1] !== 1n ||
            event.datum == null
          )
            throw new Error(
              "Transition replay event NFT coverage is ambiguous",
            );
          Data.from(event.datum, SDK.TxOrderDatum);
          if (!units.has(tokens[0]![0])) newUnits.add(tokens[0]![0]);
          events.push({
            kind: definition.kind,
            utxo: event,
            assetName: tokens[0]![0].slice(56),
          });
        }
      }
      if (newUnits.size === 0) return { snapshot, finalHub, events };
      // Address discovery and history acquisition pin independent snapshots.
      // A new NFT requires its full history at a newly admitted common point.
      for (const unit of newUnits) units.add(unit);
    }
    throw new LocalKupmiosCheckpointChangedError(
      "Transition replay event NFT coverage kept growing during history acquisition",
    );
  };
  const { snapshot, finalHub, events } = await captureCoveredEvents();
  const handle = Object.freeze({
    headerHash: base.headerHash,
    snapshotDigest: createHash("sha256")
      .update(JSON.stringify(snapshot))
      .digest("hex"),
    evidenceDigest: computeFraudProofRawL1SnapshotEvidenceDigest(snapshot),
  });
  const owned = structuredClone({
    events,
    hub: finalHub.utxo,
    network: binding.network,
    depositPolicyId: hub.data.deposit,
    snapshot,
  });
  freezeAdmittedEvents(owned);
  // Preserve each reader's ordering: raw readers follow scope/NFT order;
  // convenience readers have always sorted all event out-refs together.
  const convenience = Object.freeze({
    ...owned,
    events: Object.freeze(
      [...owned.events].sort(
        (left, right) =>
          left.utxo.txHash.localeCompare(right.utxo.txHash) ||
          left.utxo.outputIndex - right.utxo.outputIndex,
      ),
    ),
  });
  admitted.set(handle, Object.freeze({ convenience, raw: owned }));
  return handle;
};

export const requireTransitionTraceL1Events = (
  handle: TransitionTraceL1Events,
) => {
  const value = admitted.get(handle);
  if (value === undefined)
    throw new Error(
      "Transition replay requires freshly admitted raw L1 events",
    );
  return value.convenience;
};

/** Read the immutable result of exact raw admission. A different capture gets
 * a different opaque handle and must pass admission independently. */
export const readFreshTransitionTraceL1Events = (
  handle: TransitionTraceL1Events,
) => {
  requireTransitionTraceL1Events(handle);
  return admitted.get(handle)!.raw;
};

export type TransitionTraceEventAuthority = Readonly<{
  deploymentFingerprint: string;
}>;
const eventAuthorities = new WeakMap<
  object,
  (headerHash: string) => Promise<TransitionTraceL1Events>
>();
export const createTransitionTraceEventAuthority = ({
  binding,
  source,
}: {
  binding: FraudProofWorkflowDeploymentBinding<"transitionTrace">;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
}): TransitionTraceEventAuthority => {
  const port = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const authority = port.rawL1;
  if (authority === undefined)
    throw new Error(
      "Transition event authority requires local raw L1 observations",
    );
  return createEventAuthorityFromRaw({ binding, authority });
};

const createEventAuthorityFromRaw = ({
  binding,
  authority,
}: {
  binding: FraudProofWorkflowDeploymentBinding<"transitionTrace">;
  authority: FraudProofRawL1SnapshotAuthority;
}): TransitionTraceEventAuthority => {
  const handle = Object.freeze({
    deploymentFingerprint: binding.deploymentFingerprint,
  });
  eventAuthorities.set(handle, (headerHash) =>
    captureTransitionTraceL1Events({
      binding: {
        ...binding,
        definition: { ...binding.definition, headerHash },
      },
      authority,
    }),
  );
  return handle;
};
/** Test-only transport injection; every capture still admits exact raw L1 evidence. */
export const unsafeCreateTransitionTraceEventAuthorityFromRawForTest =
  createEventAuthorityFromRaw;

export const requireTransitionTraceEventAuthority = (
  handle: TransitionTraceEventAuthority,
) => {
  const capture = eventAuthorities.get(handle);
  if (capture === undefined)
    throw new Error("Transition event authority was not admitted");
  return capture;
};
