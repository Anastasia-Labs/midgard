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
import {
  admitFraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotAuthority,
  type FraudProofRawL1SnapshotRequest,
  type FraudProofRawL1Utxo,
} from "../workflow/raw-l1-snapshot.js";

export type TransitionTraceL1Events = Readonly<{
  headerHash: string;
  snapshotDigest: string;
}>;
export type TransitionTraceL1Event = Readonly<{
  kind: "deposit" | "withdrawal" | "forcedTransaction";
  utxo: UTxO;
  assetName: string;
}>;
const admitted = new WeakMap<
  object,
  Readonly<{
    events: readonly TransitionTraceL1Event[];
    hub: UTxO;
    network: Network;
    depositPolicyId: string;
    snapshot: ReturnType<typeof admitFraudProofRawL1Snapshot>;
  }>
>();
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
    releaseIdentityDigest: finality.releaseIdentityDigest,
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
  const scopes = [
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
  const snapshot = await capture(scopes, [...units].sort());
  const finalHub = readHub(snapshot);
  if (hub.outRef !== finalHub.outRef || hub.datum !== finalHub.datum)
    throw new Error(
      "Transition replay hub changed while admitting event history",
    );
  const events: TransitionTraceL1Event[] = [];
  for (const definition of definitions)
    for (const raw of snapshot.scopes.find(
      (scope) => scope.role === definition.role,
    )!.utxos) {
      const event = utxo(raw);
      const tokens = Object.entries(event.assets).filter(([unit]) =>
        unit.startsWith(definition.policy),
      );
      if (tokens.length === 0) continue;
      if (
        tokens.length !== 1 ||
        tokens[0]![1] !== 1n ||
        !units.has(tokens[0]![0]) ||
        event.datum == null
      )
        throw new Error(
          "Transition replay event NFT coverage changed or is ambiguous",
        );
      // Strict datum decoding rejects arbitrary deposits to a user-event address.
      if (definition.kind === "deposit")
        Data.from(event.datum, SDK.DepositDatum);
      else if (definition.kind === "withdrawal")
        Data.from(event.datum, SDK.WithdrawalOrderDatum);
      else Data.from(event.datum, SDK.TxOrderDatum);
      events.push({
        kind: definition.kind,
        utxo: event,
        assetName: tokens[0]![0].slice(56),
      });
    }
  const handle = Object.freeze({
    headerHash: base.headerHash,
    snapshotDigest: createHash("sha256")
      .update(JSON.stringify(snapshot))
      .digest("hex"),
  });
  admitted.set(handle, {
    events: events.sort(
      (left, right) =>
        left.utxo.txHash.localeCompare(right.utxo.txHash) ||
        left.utxo.outputIndex - right.utxo.outputIndex,
    ),
    hub: finalHub.utxo,
    network: binding.network,
    depositPolicyId: hub.data.deposit,
    snapshot,
  });
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
  return value;
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
export const requireTransitionTraceEventAuthority = (
  handle: TransitionTraceEventAuthority,
) => {
  const capture = eventAuthorities.get(handle);
  if (capture === undefined)
    throw new Error("Transition event authority was not admitted");
  return capture;
};
